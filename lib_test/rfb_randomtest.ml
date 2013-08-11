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

let w = 640
let h = 480

module Server = Rfb.Make(Rfb_unix)
open Server

let server (s: Unix.file_descr) =
  let pf = PixelFormat.true_colour_default Sys.big_endian in
  Server.handshake "random" pf w h s;

  let bpp = ref 32 in
  let update_thread = ref None in
  while true do
    let req = Request.unmarshal s in
    print_endline ("<- " ^ (Request.prettyprint req));
    match req with
    | Request.SetPixelFormat pf ->
	bpp := pf.PixelFormat.bpp;
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = true } ->
      (* send a copyrect *)
      let w' = Random.int (w - 1) + 1 and h' = Random.int (h - 1) + 1 in
      let x' = Random.int (w - w') and y' = Random.int (h - h') in
      let x'' = Random.int (w - w') and y'' = Random.int (h - h') in
      let cr = { FramebufferUpdate.CopyRect.x = x'' ; y = y'' } in
      let update = { FramebufferUpdate.x = x'; y = y'; w = w'; h = h';
                     encoding = FramebufferUpdate.Encoding.CopyRect cr } in
      print_endline ("-> " ^ (FramebufferUpdate.prettyprint update));
      Rfb_unix.really_write s (FramebufferUpdate.marshal [ update ])
    | Request.FrameBufferUpdateRequest { FramebufferUpdateRequest.incremental = false } ->
      (* Update the whole thing *)
      let buffer = String.create (w * h * !bpp / 8) in
      for i = 0 to String.length buffer - 1 do
        buffer.[i] <- char_of_int (Random.int 255)
      done;
      let raw = { FramebufferUpdate.Raw.buffer = buffer } in
      let update = { FramebufferUpdate.x = 0; y = 0; w = w; h = h;
                     encoding = FramebufferUpdate.Encoding.Raw raw } in
      print_endline ("-> " ^ (FramebufferUpdate.prettyprint update));
      Rfb_unix.really_write s (FramebufferUpdate.marshal [ update ]);
    | _ ->
	print_endline "<- ^^ ignoring";
  done
  

