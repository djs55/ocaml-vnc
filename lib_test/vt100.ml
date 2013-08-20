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

type colour =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let colour_of_relative_int = function
  | 0 -> Some Black
  | 1 -> Some Red
  | 2 -> Some Green
  | 3 -> Some Yellow
  | 4 -> Some Blue
  | 5 -> Some Magenta
  | 6 -> Some Cyan
  | 7 -> Some White
  | _ -> None

type attribute =
  | Reset
  | Bright
  | Dim
  | Underscore
  | Blink
  | Reverse
  | Hidden
  | Foreground of colour
  | Background of colour

let attribute_of_int = function
  | 0 -> Some Reset
  | 1 -> Some Bright
  | 2 -> Some Dim
  | 3 -> Some Underscore
  | 4 -> Some Blink
  | 5 -> Some Reverse
  | 6 -> Some Hidden
  | x ->
    begin match colour_of_relative_int (x - 30) with
    | Some x -> Some (Foreground x)
    | None ->
      begin match colour_of_relative_int (x - 40) with
      | Some x -> Some (Background x)
      | None -> None
      end
    end

type t =
  | Query_device_code
  | Report_device_code of string
  | Query_device_status
  | Report_device_ok
  | Report_device_failure
  | Query_cursor_position
  | Report_cursor_position of int * int
  | String of string
  | Reset_device
  | Enable_line_wrap
  | Disable_line_wrap
  | Font_set_g0
  | Font_set_g1
  | Cursor_home of int * int
  | Cursor_up of int
  | Cursor_down of int
  | Cursor_forward of int
  | Cursor_backward of int
  | Force_cursor_position of int * int
  | Save_cursor
  | Unsave_cursor
  | Save_cursor_and_attrs
  | Restore_cursor_and_attrs
  | Scroll_screen of (int * int) option
  | Scroll_down
  | Scroll_up
  | Set_tab
  | Clear_tab
  | Clear_all_tabs
  | Erase_end_of_line
  | Erase_start_of_line
  | Erase_line
  | Erase_down
  | Erase_up
  | Erase_screen
  | Print_screen
  | Print_line
  | Stop_print_log
  | Start_print_log
  | Set_key_definition of int * string
  | Set_attribute_mode of attribute list


let parse f =
  let tostring chars = String.concat "" (List.map (fun x -> String.make 1 x) chars) in
  match f () with
  | '\027' -> begin match f () with
    | 'c' -> Reset_device
    | '(' -> Font_set_g0
    | ')' -> Font_set_g1
    | '7' -> Save_cursor_and_attrs
    | '8' -> Restore_cursor_and_attrs
    | 'D' -> Scroll_down
    | 'M' -> Scroll_up
    | 'H' -> Set_tab
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
      | [], 's' -> Save_cursor
      | [], 'u' -> Unsave_cursor
      | [], 'r' -> Scroll_screen None
      | [], 'g' -> Clear_tab
      | [], 'i' -> Print_screen
      | [], 'K' -> Erase_end_of_line
      | [], 'J' -> Erase_down
      | [ code0 ], 'c' when code0.[String.length code0 - 1 ] = '0' -> Report_device_code (String.sub code0 0 (String.length code0 - 1))
      | [ "5" ], 'n' -> Query_device_status
      | [ "3" ], 'n' -> Report_device_failure
      | [ "0" ], 'n' -> Report_device_ok
      | [ "3" ], 'n' -> Report_device_failure
      | [ "6" ], 'n' -> Query_cursor_position
      | [ "3" ], 'g' -> Clear_all_tabs
      | [ "7" ], 'h' -> Enable_line_wrap
      | [ "7" ], 'l' -> Disable_line_wrap
      | [ "1" ], 'i' -> Print_line
      | [ "4" ], 'i' -> Stop_print_log
      | [ "5" ], 'i' -> Start_print_log
      | [ "1" ], 'K' -> Erase_start_of_line
      | [ "2" ], 'K' -> Erase_line
      | [ "1" ], 'J' -> Erase_up
      | [ "2" ], 'J' -> Erase_screen
      | [ row; col ], 'R' -> Report_cursor_position (int_of_string row, int_of_string col)
      | [ row; col ], 'H' -> Cursor_home (int_of_string row, int_of_string col)
      | [ row; col ], 'f' -> Force_cursor_position (int_of_string row, int_of_string col)
      | [ r1; r2 ], 'r' -> Scroll_screen (Some (int_of_string r1, int_of_string r2))
      | [ count ], 'A' -> Cursor_up (int_of_string count)
      | [ count ], 'B' -> Cursor_down (int_of_string count)
      | [ count ], 'C' -> Cursor_forward (int_of_string count)
      | [ count ], 'D' -> Cursor_backward (int_of_string count)
      | [ key; string ], 'p' -> Set_key_definition (int_of_string key, string)
      | attrs, 'm' ->
        let int_opts = List.map (fun x -> try Some (int_of_string x) with _ -> None) attrs in
        let flatten xs = List.rev (List.fold_left (fun acc x -> match x with Some y -> y :: acc | None -> acc) [] xs) in
        let ints = flatten int_opts in
        let attr_opts = List.map attribute_of_int ints in
        Set_attribute_mode (flatten attr_opts)
      | _, _ -> String (tostring [ '\027'; '[' ] ^ (String.concat ";" args) ^ (tostring [ c ]))
      end
    | c -> String (tostring [ '\027'; c ])
    end
  | c -> String (tostring [ c ])

let tests = [
 "\027[c", Query_device_code;
 "\027[440c", Report_device_code "44";
 "\027[5n", Query_device_status;
 "\027[0n", Report_device_ok;
 "\027[3n", Report_device_failure;
 "\027[6n", Query_cursor_position;
 "\027[1;2R", Report_cursor_position(1, 2);
 "\027c", Reset_device;
 "\027[7h", Enable_line_wrap;
 "\027[7l", Disable_line_wrap;
 "\027(", Font_set_g0;
 "\027)", Font_set_g1;
 "\027[2;3H", Cursor_home(2, 3);
 "\027[4A", Cursor_up 4;
 "\027[5B", Cursor_down 5;
 "\027[6C", Cursor_forward 6;
 "\027[7D", Cursor_backward 7;
 "\027[1;2f", Force_cursor_position(1, 2);
 "\027[s", Save_cursor;
 "\027[u", Unsave_cursor;
 "\0277", Save_cursor_and_attrs;
 "\0278", Restore_cursor_and_attrs;
 "\027[r", Scroll_screen None;
 "\027[1;2r", Scroll_screen (Some (1, 2));
 "\027D", Scroll_down;
 "\027M", Scroll_up;
 "\027H", Set_tab;
 "\027[g", Clear_tab;
 "\027[3g", Clear_all_tabs;
 "\027[K", Erase_end_of_line;
 "\027[1K", Erase_start_of_line;
 "\027[2K", Erase_line;
 "\027[J", Erase_down;
 "\027[1J", Erase_up;
 "\027[2J", Erase_screen;
 "\027[i", Print_screen;
 "\027[1i", Print_line;
 "\027[4i", Stop_print_log;
 "\027[5i", Start_print_log;
 "\027[23;\"hello\"p", Set_key_definition(23, "hello");
 "\027[0;1;31;42m", Set_attribute_mode [ Reset; Bright; Foreground Red; Background Green ];
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
