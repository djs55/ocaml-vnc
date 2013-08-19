(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

type t =
  | Query_device_code
  | Report_device_code of string
  | Query_device_status
  | Report_device_ok
  | Report_device_failure
  | Query_cursor_position
  | Report_cursor_position of int * int
  | String of string

let parse f =
  let tostring chars = String.concat "" (List.map (fun x -> String.make 1 x) chars) in
  match f () with
  | '\027' -> begin match f () with
    | '[' ->
      (* semicolon-separated, possibly quote-escaped arguments terminated
         by a character *)
      (* FIXME: is there a quoting convention within strings? *)
      let rec loop complete_args current_arg quoting =
        let c = f () in match quoting, c with
        | false, '"' -> loop complete_args current_arg true
        | true, '"' -> loop complete_args current_arg false
        | false, ';' -> loop (tostring (List.rev current_arg) :: complete_args) [] false
        | false, ('a'..'z' | 'A'..'Z')  ->
          let complete_args = if current_arg = [] then complete_args else tostring (List.rev current_arg) :: complete_args in
          List.rev complete_args, c
        | _, c -> loop complete_args (c :: current_arg) quoting in
      let args, c = loop [] [] false in
      begin match args, c with
      | [], 'c' -> Query_device_code
      | [ code0 ], 'c' when code0.[String.length code0 - 1 ] = '0' -> Report_device_code (String.sub code0 0 (String.length code0 - 1))
      | [ "5" ], 'n' -> Query_device_status
      | [ "3" ], 'n' -> Report_device_failure
      | [ "0" ], 'n' -> Report_device_ok
      | [ "3" ], 'n' -> Report_device_failure
      | [ "6" ], 'n' -> Query_cursor_position
      | [ row; col ], 'R' -> Report_cursor_position (int_of_string row, int_of_string col)
      | _, _ -> String (tostring [ '\027'; '[' ] ^ (String.concat ";" args) ^ (tostring [ c ]))
      end
    | c -> String (tostring [ '\027'; c ])
    end
  | c -> String (tostring [ c ])

let tests = [
 "\027[c", Query_device_code;
 "\027[440c", Report_device_code "44";
]

let stream_of_string x =
  let i = ref (-1) in
  fun () ->
    incr i;
    if !i >= (String.length x) then raise End_of_file;
    x.[!i]

let _ = List.iter (fun (s, r) ->
  let x = stream_of_string s in
  assert (parse x = r)
) tests
