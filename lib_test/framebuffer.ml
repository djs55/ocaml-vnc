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

let write_raw_char pf font highlight c =
  let font_width = width_of_font font in
  let font_height = height_of_font font in
  let bytes_per_pixel = PixelFormat.bytes_per_pixel pf in
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
          let r, g, b =
            if (pixel && not highlight) || (not pixel && highlight)
            then 0xff, 0xff, 0xff
            else 0x0,  0x0,  0x0 in
          let encoded = Pixel.encode pf r g b in
          Pixel.write pf buffer ofs encoded;
        ) row_data
    ) pixels;
  FramebufferUpdate.Encoding.Raw { FramebufferUpdate.Raw.buffer = buffer }

let make_update pf drawing_operations font x y w h =
  let bytes_per_pixel = PixelFormat.bytes_per_pixel pf in
  let font_width = width_of_font font in
  let font_height = height_of_font font in

  (* convert the pixel co-ordinates into relative text co-ordinates *)
  let x' = x / font_width and y' = y / font_height in
  let w = w + (x - x' * font_width) and h = h + (y - y' * font_height) in
  let w' = (w + font_width - 1) / font_width in
  let h' = (h + font_height - 1) / font_height in

  let open FramebufferUpdate in

  let character (row, col) encoding =
    (* the cursor can end up just outside the visible window. Perhaps we
       should generalise this into clipping on framebuffer coords? *)
    if row >= y' && (row < (y' + h')) && (col >= x') && (col < (x' + w')) then [{
      x = col * font_width; y = row * font_height;
      w = font_width; h = font_height; encoding;
    }] else [] in
 
  let copy (from_row, from_col) =
      let x = from_col * font_width in
      let y = from_row * font_height in
      Encoding.CopyRect { CopyRect.x; y } in

  let write { char = char; highlight = highlight } = match char with
   | None ->
      let r, g, b = if highlight then 0xff, 0xff, 0xff else 0x0, 0x0, 0x0 in
      let encoded = Pixel.encode pf r g b in
      let background = String.create bytes_per_pixel in
      Pixel.write pf background 0 encoded;
      Encoding.RRE { RRE.background; rectangles = [] }
   | Some c ->
     write_raw_char pf font highlight c in

  let scroll lines =
    let down = {
      x = 0; y = 0; w = w; h = h - lines;
      encoding = Encoding.CopyRect { CopyRect.x = 0; y = lines }
    } in
    let up = {
      x = 0; y = -lines * font_height; w = w; h = -lines * font_height;
      encoding = Encoding.CopyRect { CopyRect.x = 0; y = 0 }
    } in
    let rec animate acc = function
      | 0 -> acc
      | n -> animate ((if lines > 0 then down else up) :: acc) (n-1) in
    animate [] font_height in

    List.rev (List.fold_left (fun acc d -> match d with
      | Delta.Update (coord, x) ->
        character coord (write x) @ acc
      | Delta.Copy (coord, from) ->
        character coord (copy from) @ acc
      | Delta.Scroll lines ->
        scroll lines @ acc
    ) [] drawing_operations)

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
  let pf = ref (PixelFormat.true_colour_default Sys.big_endian) in
  let buf = ref (Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout 409600))) in

  lwt () = Server.handshake "console" !pf w h !buf s in

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
          let drawing_operations = Delta.draw false window !client_remembers window new_console in
          updates := make_update !pf drawing_operations font 0 0 w h;
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
          lwt () = Rfb_lwt.really_write s (FramebufferUpdate.marshal_at !updates !buf) in
          return ()
        )
     in

  while_lwt true do
    lwt req = Request.unmarshal s !buf in
    Lwt_mutex.with_lock m (fun () ->
    if !debug then print_endline ("<- " ^ (Request.prettyprint req));
    match req with
    | Request.SetPixelFormat pf' ->
        Printf.printf "Setting pixel format to %s\n" (PixelFormat.to_string pf');
	pf := pf';
        let max_size_needed = w * h * (PixelFormat.bytes_per_pixel !pf) + (FramebufferUpdate.sizeof []) in
        if max_size_needed > (Cstruct.len !buf)
        then buf := Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout max_size_needed));
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
      let drawing_operations = Delta.draw true window c window c in
      let update = make_update !pf drawing_operations font x y width height in
      client_remembers := c;
      if !debug then begin
        print_endline "-> FramebufferUpdate";
        List.iter
          (fun x ->
            print_endline (FramebufferUpdate.prettyprint x);
          ) update;
      end;
      Rfb_lwt.really_write s (FramebufferUpdate.marshal_at update !buf);
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
