(*
 * Copyright (C) 2011-2013 Citrix Inc
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
open OUnit

let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout 4096))

let check c hex =
  assert_equal ~printer:string_of_int (List.length hex) (Cstruct.len c);
  List.iteri (fun i x ->
    assert_equal ~printer:(fun x -> Printf.sprintf "at offset %d: %d" i x) x (Cstruct.get_uint8 c i)
  ) hex

let protocolversion () =
  let ver = { ProtocolVersion.major = 3; minor = 3 } in
  let expected = [ 0x52; 0x46; 0x42; 0x20; 0x30; 0x30; 0x33; 0x2e; 0x30; 0x30; 0x33; 0x0a ] in
  let buf' = ProtocolVersion.marshal_at ver buf in
  check buf' expected

let securitytype_none () =
  let x = SecurityType.NoSecurity in
  let expected = [ 0; 0; 0; 1 ] in
  let buf' = SecurityType.marshal_at x buf in
  check buf' expected

let clientinit () =
  let x = false in
  let expected = [ 0 ] in
  let buf' = ClientInit.marshal_at x buf in
  check buf' expected

let example_pixelformat = {
  PixelFormat.bpp = PixelFormat.BPP_8;
  depth = 13;
  big_endian = false;
  true_colour = false;
  red_max_n = 3;
  green_max_n = 4;
  blue_max_n = 5;
  red_shift = 6;
  green_shift = 7;
  blue_shift = 8;
}

let example_pixelformat_expected = [ 8; 13; 0; 0; 0; 7; 0; 15; 0; 31; 6; 7; 8; 0; 0; 0]

let serverinit () =
  let x = { ServerInit.width = 7; height = 11; pixelformat = example_pixelformat; name = "hello" } in
  let expected = [ 0; 7; 0; 11 ] @ example_pixelformat_expected @ [ 0; 0; 0; 5; int_of_char 'h'; int_of_char 'e'; int_of_char 'l'; int_of_char 'l'; int_of_char 'o' ] in
  let buf' = ServerInit.marshal_at x buf in
  check buf' expected

let setpixelformat () =
  let expected = [ 0; 0; 0; 0; ] @ example_pixelformat_expected in
  let buf' = SetPixelFormat.marshal_at example_pixelformat buf in
  check buf' expected

let fbu_0 () =
  let expected = [ 0; 0; 0; 0 ] in
  let buf' = FramebufferUpdate.marshal_at [] buf in
  check buf' expected

let fbu_copyrect () =
  let x = [ { FramebufferUpdate.x = 16; y = 32; w = 48; h = 64; encoding = FramebufferUpdate.Encoding.CopyRect {FramebufferUpdate.CopyRect.x = 640; y = 480 } } ] in
  let expected = [ 0; 0; 0; 1; 0; 16; 0; 32; 0; 48; 0; 64; 0; 0; 0; 1; 640 / 0x100; 640 mod 0x100; 480 / 0x100; 480 mod 0x100 ] in
  let buf' = FramebufferUpdate.marshal_at x buf in
  check buf' expected

let fbu_raw () =
  let x = [ { FramebufferUpdate.x = 16; y = 32; w = 48; h = 64; encoding = FramebufferUpdate.Encoding.Raw {FramebufferUpdate.Raw.buffer = "\001\002\003\004"} } ] in
  let expected = [ 0; 0; 0; 1; 0; 16; 0; 32; 0; 48; 0; 64; 0; 0; 0; 0; 1; 2; 3; 4 ] in
  let buf' = FramebufferUpdate.marshal_at x buf in
  check buf' expected

let setcolourmapentries () =
  let x = { SetColourMapEntries.first_colour = 2; map = [ (1,2,3); (4,5,6) ] } in
  let expected = [ 1; 0; 0; 2; 0; 2; 0; 1; 0; 2; 0; 3; 0; 4; 0; 5; 0; 6 ] in
  let buf' = SetColourMapEntries.marshal_at x buf in
  check buf' expected

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test rfb parser";

  let suite = "vhd" >:::
    [
      "protocolversion" >:: protocolversion;
      "securitytype_none" >:: securitytype_none;
      "clientinit" >:: clientinit;
      "serverinit" >:: serverinit;
      "setpixelformat" >:: setpixelformat;
      "fbu_0" >:: fbu_0;
      "fbu_copyrect" >:: fbu_copyrect;
      "fbu_raw" >:: fbu_raw;
      "setcolourmapentries" >:: setcolourmapentries;
    ] in
  run_test_tt ~verbose:!verbose suite

