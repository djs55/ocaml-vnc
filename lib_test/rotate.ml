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

module Server = Rfb.Make(Rfb_unix)
open Server

let w = 640
let h = 480

let display_buffer = ref (Array.create (w * h) 0)
let update_buffer  = ref (Array.create (w * h) 0)

let update_counter = ref 0
let update_counter_m = Mutex.create ()
let update_counter_c = Condition.create ()

let get_display_buffer () = !display_buffer
let get_update_buffer () = !update_buffer
let switch () =
  let d = !display_buffer and u = !update_buffer in
  display_buffer := u;
  update_buffer := d
let post_update () =
  incr update_counter;
  Condition.signal update_counter_c
let wait_update last_update =
  Mutex.lock update_counter_m;
  while !update_counter = last_update do
    Condition.wait update_counter_c update_counter_m
  done;
  let this_update = !update_counter in
  Mutex.unlock update_counter_m;
  this_update

let make_update pf =
  let d = get_display_buffer () in
  (* Update the whole thing *)
  let bytes_per_pixel = PixelFormat.bytes_per_pixel pf in
  let buffer = String.create (w * h * bytes_per_pixel) in
  let bytes_per_line = w * bytes_per_pixel in
  for y = 0 to h - 1 do
    let y' = y * bytes_per_line in
    for x = 0 to w - 1 do
      let x' = x * bytes_per_pixel in
      let r = d.(y * w + x) lsr 16 in
      let g = (d.(y * w + x) lsr 8) land 0xff in
      let b = d.(y * w + x) land 0xff in
      let encoded = Pixel.encode pf r g b in
      Pixel.write pf buffer (y' + x') encoded
    done
  done;
  let raw = { FramebufferUpdate.Raw.buffer = buffer } in
  { FramebufferUpdate.x = 0; y = 0; w = w; h = h;
    encoding = FramebufferUpdate.Encoding.Raw raw }

let square t x y =
  if x < 100 || x > 540 || y < 100 || y > 380
  then 0
  else 0xffffff

let hatch t x y =
  if ((x / 32) + (y / 32)) mod 2 = 0
  then 0x666666
  else 0xbbbbbb

let raster t f =
  let d = get_update_buffer () in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      d.(y * w + x) <- f t x y
    done
  done

let clear d =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      d.(y * w + x) <- 0x0
    done
  done

let plot f t =
  let d = get_update_buffer () in
  clear d;
  let c' = float_of_int (h / 2) /. 2. in
  for x = 0 to w - 1 do
    let y = int_of_float (f t (float_of_int x /. (float_of_int w)) *. c' +. (float_of_int (h / 2)))  in
    d.(y * w + x) <- 0xffffff
  done

let pi = 4. *. atan 1.

let sinusoid =
  let c = 2. *. pi in
  fun t x ->
    sin ((x +. t) *. c)

let plot2 f t =
  let d = get_update_buffer () in
  clear d;
  for i = 0 to 4000 do
    d.(f t (float_of_int i /. 4000.)) <- 0xffffff
  done
 
let test =
  let c = 2. *. pi in
  let w_size = float_of_int w *. 0.4 in
  let h_size = float_of_int h *. 0.4 in
  fun t i ->
    let x = int_of_float (sin ((0.1 *. t +. i) *. c) *. w_size +. (float_of_int (w/2))) in
    let y = int_of_float (cos ((0.1 *. t +. 4. *. i) *. c) *. h_size +. (float_of_int (h/2))) in
    y * w + x


let animate fps f =
  let epoch_start = Unix.gettimeofday () in
  let ideal_delay = 1. /. fps in
  while true do
    let start = Unix.gettimeofday () in (* TODO: use monotonic clock *)
    f (start -. epoch_start);
    switch ();
    post_update ();
    let took = Unix.gettimeofday () -. start in
    if took < ideal_delay
    then Thread.delay (ideal_delay -. took)
  done

let server (s: Unix.file_descr) =
  let pf = ref (PixelFormat.true_colour_default Sys.big_endian) in
  let buf = ref (Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout 4096))) in

  Server.handshake "rotate" !pf w h !buf s;

  let last_update_seen = ref !update_counter in
  while true do
    let req = Request.unmarshal s in
    (* print_endline ("<- " ^ (Request.prettyprint req)); *)
    match req with
    | Request.SetPixelFormat pf' ->
	pf := pf';
        let max_size_needed = w * h * (PixelFormat.bytes_per_pixel !pf) + (FramebufferUpdate.sizeof []) in
        if max_size_needed > (Cstruct.len !buf)
        then buf := Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout max_size_needed))
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = true } ->
      last_update_seen := wait_update !last_update_seen;
      let update = make_update !pf in
      (* print_endline ("-> " ^ (FramebufferUpdate.prettyprint update)); *)
      Rfb_unix.really_write s (FramebufferUpdate.marshal_at [ update ] !buf)
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = false } ->
      let update = make_update !pf in
      print_endline ("-> " ^ (FramebufferUpdate.prettyprint update));
      Rfb_unix.really_write s (FramebufferUpdate.marshal_at [ update ] !buf);
    | _ ->
	print_endline "<- ^^ ignoring";
  done
  
let _ = 
  let port = 5902 in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.handle_unix_error (Unix.setsockopt s Unix.SO_REUSEADDR) true;
  Unix.handle_unix_error (Unix.bind s) (Unix.ADDR_INET (Unix.inet_addr_any, port));
  let port = begin match Unix.getsockname s with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> failwith "Failed to discover local port"
  end in
  Printf.printf "Listening on local port %d\n" port; flush stdout;
  Unix.handle_unix_error (Unix.listen s) 5;
  let _ = Thread.create (animate 50.) (plot2 test) in
  let fd, _ = Unix.accept s in
  server fd

