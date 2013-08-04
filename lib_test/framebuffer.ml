(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Rfb
open Lwt

module Server = Rfb.Make(Rfb_lwt)
open Server

let w = 640
let h = 480

module Coord = struct
  type t = int * int
  let compare ((x1,y1): t) ((x2,y2): t) =
    if x1 < x2 then -1
    else if x1 > x2 then 1
    else if y1 < y2 then -1
    else if y1 > y2 then 1
    else 0
end

module CoordMap = Map.Make(Coord)
 
module Console = struct
  type t = {
    chars: int CoordMap.t;
    cursor: Coord.t;
    rows: int;
    cols: int;
  }

  let make rows cols =
    let chars = CoordMap.empty in
    let cursor = 0, 0 in
    { rows; cols; chars; cursor }

  let output_char (t: t) c =
    if c = '\n'
    then { t with chars = t.chars; cursor = fst t.cursor + 1, 0 }
    else
      let chars = CoordMap.add t.cursor (int_of_char c) t.chars in
      (* TODO: scrolling *)
      let cursor =
        if snd t.cursor = t.cols - 1
        then fst t.cursor + 1, 0
        else fst t.cursor, snd t.cursor + 1 in
      { t with chars = chars; cursor = cursor }

  let output_string (t: t) s =
    let s' = String.length s in
    let rec loop i t =
      if i = s' - 1 then t else loop (i + 1) (output_char t s.[i]) in
    loop 0 t

  let dump (t: t) =
    for row = 0 to t.rows - 1 do
      for col = 0 to t.cols - 1 do
        try
          let c = CoordMap.find (row, col) t.chars in
          print_string (String.make 1 (char_of_int c))
        with Not_found -> ()
      done;
      print_string "\n";
    done
end

let height_of_font font =
  let open Pcf in
  let open Accelerator in
  let open Metrics in
  let a = get_accelerator font in
  a.min_bounds.character_ascent + a.min_bounds.character_descent

let width_of_font font =
  let open Pcf in
  let a = get_accelerator font in
  a.Accelerator.min_bounds.Metrics.character_width

let make_full_update bpp console font =
  (* Update the whole thing *)
  let font_width = width_of_font font in
  let font_height = height_of_font font in
  let bytes_per_pixel = bpp / 8 in
  let buffer = String.create (w * h * bytes_per_pixel) in
  let bytes_per_line = w * bytes_per_pixel in
  for row = 0 to console.Console.rows - 1 do
    for col = 0 to console.Console.cols - 1 do
      try
        let c = CoordMap.find (row, col) console.Console.chars in
        let e = Pcf.Encoding.of_int c in
        match Pcf.Glyph.get_bitmap font e with
        | None -> ()
        | Some pixels ->
          Array.iteri
            (fun row' row_data ->
              Array.iteri
                (fun col' pixel ->
                  let y = row * font_height + row' in
                  let x = col * font_width + col' in
                  let ofs = y * bytes_per_line + x * bytes_per_pixel in
                  let c = if pixel then 0xffffff else 0x0 in
                  buffer.[ofs + 0] <- char_of_int (c lsr 16);
                  buffer.[ofs + 1] <- char_of_int ((c lsr 8) land 0xff);
                  buffer.[ofs + 2] <- char_of_int (c land 0xff);
                  if bytes_per_pixel = 32
                  then buffer.[ofs + 3] <- char_of_int 0
                ) row_data
            ) pixels
      with Not_found -> ()
    done
  done;
  let raw = { FramebufferUpdate.Raw.buffer = buffer } in
  { FramebufferUpdate.x = 0; y = 0; w = w; h = h;
    encoding = FramebufferUpdate.Encoding.Raw raw }


let console = ref (Console.make 0 0)
let console_m = Lwt_mutex.create ()
let console_c = Lwt_condition.create ()
let update_console f =
  Lwt_mutex.with_lock console_m
    (fun () ->
      console := f !console;
      Lwt_condition.broadcast console_c ();
      return ()
    )
let wait_for_update c =
  Lwt_mutex.with_lock console_m
    (fun () ->
      lwt () = while_lwt !console = c do Lwt_condition.wait console_c ~mutex:console_m done in
      return !console
    )

let server (s: Lwt_unix.file_descr) font = 
  lwt () = Server.handshake w h s in

  let bpp = ref 32 in
  let client_remembers = ref (Console.make 0 0) in
  let m = Lwt_mutex.create () in
  let framebuffer_thread = ref None in

  let framebuffer_thread_body () =
    while_lwt true do
      lwt new_console = wait_for_update !client_remembers in
      Lwt_mutex.with_lock m
        (fun () ->
          let update = make_full_update !bpp new_console font in
          lwt () = Rfb_lwt.really_write s (FramebufferUpdate.marshal [ update ]) in
          client_remembers := new_console;
          return ()
        )
     done in

  while_lwt true do
    lwt req = Request.unmarshal s in
    Lwt_mutex.with_lock m (fun () ->
    print_endline ("<- " ^ (Request.prettyprint req));
    match req with
    | Request.SetPixelFormat pf ->
        Printf.printf "Setting pixel format to %d bpp\n" pf.PixelFormat.bpp;
	bpp := pf.PixelFormat.bpp;
        return ()
    | Request.KeyEvent { KeyEvent.down = false; key = key } ->
        (try_lwt
          let code = char_of_int (Int32.to_int key) in
          update_console
            (fun c ->
              Console.output_char c code
            )
        with _ ->
          Printf.printf "Ignoring keycode: %lx\n%!" key;
          return ())
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = true } ->
      if !framebuffer_thread = None
      then framebuffer_thread := Some (framebuffer_thread_body ());
      return ()
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = false } ->
      let c = !console in
      let update = make_full_update !bpp c font in
      client_remembers := c;
      print_endline ("-> " ^ (FramebufferUpdate.prettyprint update));
      Rfb_lwt.really_write s (FramebufferUpdate.marshal [ update ]);
    | _ ->
	print_endline "<- ^^ ignoring";
        return ()
    )
  done
  
let main () = 
  if Array.length Sys.argv <> 2 then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr " %s <PCF font file>\n" Sys.argv.(0);
    exit 1;
  end;
  let font = match Pcf_unix.of_file Sys.argv.(1) with
  | None ->
    Printf.fprintf stderr "Failed to read PCF format font data from %s\n" Sys.argv.(1);
    exit 2
  | Some pcf -> pcf in

  let a = Pcf.get_accelerator font in
  if not a.Pcf.Accelerator.terminal_font
  then Printf.fprintf stderr "WARNING: font is not claiming to be a terminal font\n%!";
  let cols = w / (width_of_font font) in
  let rows = h / (height_of_font font) in
  Printf.fprintf stderr "Font has dimensions %d x %d\n%!" (width_of_font font) (height_of_font font);
  Printf.fprintf stderr "Setting rows to %d and cols to %d\n%!" rows cols;

  let c = Console.make rows cols in
  console := Console.output_string c "hello world\n";
  let _ =
    while_lwt true do
      lwt () = Lwt_unix.sleep 0.25 in
      let t = "all work and no play makes Dave a dull boy. " in
      for_lwt i = 0 to String.length t - 1 do
        lwt () = Lwt_unix.sleep 0.1 in
        update_console (fun c -> Console.output_char c t.[i])
      done
    done in

  let port = 5902 in
  let s = Lwt_unix.socket Lwt_unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.handle_unix_error (Unix.setsockopt (Lwt_unix.unix_file_descr s) Unix.SO_REUSEADDR) true;
  Lwt_unix.bind s (Lwt_unix.ADDR_INET (Unix.inet_addr_any, port));
  let port = begin match Lwt_unix.getsockname s with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> failwith "Failed to discover local port"
  end in
  Printf.printf "Listening on local port %d\n" port; flush stdout;
  Lwt_unix.listen s 5;
  lwt x = Lwt_unix.accept s in
  server (fst x) font

let _ = Lwt_main.run (main ())
