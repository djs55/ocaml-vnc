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
    assert_equal ~printer:string_of_int x (Cstruct.get_uint8 c i)
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
    ] in
  run_test_tt ~verbose:!verbose suite

