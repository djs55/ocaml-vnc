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

open Textconsole

module Server = Rfb.Make(Rfb_lwt)
open Server

let w = 640
let h = 480

let debug = ref true

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

let write_raw_char bpp font highlight c =
  let font_width = width_of_font font in
  let font_height = height_of_font font in
  let bytes_per_pixel = bpp / 8 in
  let buffer = String.create (font_width * font_height * bytes_per_pixel) in
  let e = Pcf.Encoding.of_int c in
  let pixels = match Pcf.Glyph.get_bitmap font e with
  | Some pixels -> pixels
  | None ->
    (* unusual to have a character but not glyph (provided
       the font is decent) *)
    Array.init font_height (fun _ -> Array.create font_width false) in
  Array.iteri
    (fun row row_data ->
      Array.iteri
        (fun col pixel ->
          let ofs = (row * font_width + col) * bytes_per_pixel in
          let c = if (pixel && not highlight)||(not pixel && highlight) then 0xffffff else 0x0 in
          buffer.[ofs + 0] <- char_of_int (c lsr 16);
          buffer.[ofs + 1] <- char_of_int ((c lsr 8) land 0xff);
          buffer.[ofs + 2] <- char_of_int (c land 0xff);
          if bytes_per_pixel = 32
          then buffer.[ofs + 3] <- char_of_int 0
        ) row_data
    ) pixels;
  FramebufferUpdate.Encoding.Raw { FramebufferUpdate.Raw.buffer = buffer }

let make_full_update bpp drawing_operations screen font incremental x y w h =
  let updates = ref [] in
  let bytes_per_pixel = bpp / 8 in
  let painted_already = Hashtbl.create 128 in
  let font_width = width_of_font font in
  let font_height = height_of_font font in

  (* convert the pixel co-ordinates into relative text co-ordinates *)
  let x' = x / font_width and y' = y / font_height in
  let w = w + (x - x' * font_width) and h = h + (y - y' * font_height) in
  let w' = (w + font_width - 1) / font_width in
  let h' = (h + font_height - 1) / font_height in

  let open FramebufferUpdate in
  let push (row, col) encoding =
    let x = col * font_width in
    let y = row * font_height in
    let update = {
      x; y; w = font_width; h = font_height; encoding;
    } in
    updates := update :: !updates in

  let char (row, col) { Delta.highlight = highlight; char = char } =
    if row >= y' && (row < (y' + h')) && (col >= x') && (col < (x' + w')) then begin
      let encoding = match char with
        | None ->
          Encoding.RRE {
            RRE.background = String.make bytes_per_pixel (if highlight then '\255' else '\000');
            rectangles = []
          }
        | Some c ->
          if Hashtbl.mem painted_already (c, highlight) then begin
            let row, col = Hashtbl.find painted_already (c, highlight) in
            let x = col * font_width in
            let y = row * font_height in
            Encoding.CopyRect { CopyRect.x; y }
          end else begin
            Hashtbl.replace painted_already (c, highlight) (row, col);
            write_raw_char bpp font highlight c
          end
      in
      push (row, col) encoding
    end in

  let scroll lines =
    if lines > 0 then begin
      for y = 1 to font_height do
        let encoding = Encoding.CopyRect { CopyRect.x = 0; y = lines } in
        let update = {
          x = 0; y = 0; w = w; h = h - lines; encoding
        } in
        updates := update :: !updates;
      done
    end else if lines < 0 then begin
      let encoding = Encoding.CopyRect { CopyRect.x = 0; y = 0 } in
      let update = {
        x = 0; y = -lines * font_height; w = w; h = -lines * font_height; encoding
      } in
      updates := update :: !updates
    end in
  if incremental then begin
    List.iter (function
      | Delta.Update (coord, x) -> char coord x
      | Delta.Scroll x -> scroll x
    ) drawing_operations
  end else begin
    for row = y' to y' + h' - 1 do
      for col = x' to x' + w' - 1 do
        try
          let c = CoordMap.find (row, col) screen.Screen.chars in
          char (row, col) { Delta.char = Some c; highlight = (screen.Screen.cursor = Some (row, col)) }
        with Not_found ->
          char (row, col) { Delta.char = None; highlight = (screen.Screen.cursor = Some (row, col)) }
      done
    done
  end;
  (* Updates must be ordered or else we may issue a CopyRect before the
     underlying data is written. *)
  List.rev !updates

let console = ref (Console.make 0)
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

let server (s: Lwt_unix.file_descr) window font = 
  lwt () = Server.handshake "console" w h s in

  let bpp = ref 32 in
  let client_remembers = ref (Console.make 0) in
  let m = Lwt_mutex.create () in
  let framebuffer_thread = ref None in

  let start_new_thread = ref true in

  let one_background_incremental_update () =
      start_new_thread := false;
      let updates = ref [] in
      lwt () =
        while_lwt !updates = [] do
          lwt new_console = wait_for_update !client_remembers in
          let drawing_operations = Delta.draw window !client_remembers window new_console in
          let visible_console = Screen.make new_console window in
          updates := make_full_update !bpp drawing_operations visible_console font true 0 0 w h;
          client_remembers := new_console;
          return ()
        done in
      start_new_thread := true;
      Lwt_mutex.with_lock m
        (fun () ->
          if !debug then begin
            print_endline "-> FramebufferUpdate";
            List.iter
              (fun x ->
                print_endline (FramebufferUpdate.prettyprint x);
              ) !updates;
          end;
          lwt () = Rfb_lwt.really_write s (FramebufferUpdate.marshal !updates) in
          return ()
        )
     in

  while_lwt true do
    lwt req = Request.unmarshal s in
    Lwt_mutex.with_lock m (fun () ->
    if !debug then print_endline ("<- " ^ (Request.prettyprint req));
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
      if !start_new_thread then ignore(one_background_incremental_update ());
      return ()
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = false; x; y; width; height } ->
      let c = !console in
      let visible_console = Screen.make c window in
      let update = make_full_update !bpp [] visible_console font false x y width height in
      client_remembers := c;
      if !debug then begin
        print_endline "-> FramebufferUpdate";
        List.iter
          (fun x ->
            print_endline (FramebufferUpdate.prettyprint x);
          ) update;
      end;
      Rfb_lwt.really_write s (FramebufferUpdate.marshal update);
    | _ ->
	if !debug then print_endline "<- ^^ ignoring";
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
  let window = Window.make rows in
  Printf.fprintf stderr "Font has dimensions %d x %d\n%!" (width_of_font font) (height_of_font font);
  Printf.fprintf stderr "Setting rows to %d and cols to %d\n%!" rows cols;

  console := Console.make cols;
  let _ =
    let j = ref 0 in
    while_lwt true do
      lwt () = Lwt_unix.sleep 0.05 in
      let t = Printf.sprintf "%d: all work and no play makes Dave a dull boy.\n" !j in
      incr j;
      for_lwt i = 0 to String.length t - 1 do
        lwt () = Lwt_unix.sleep 0.01 in
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
  server (fst x) window font

let _ = Lwt_main.run (main ())
