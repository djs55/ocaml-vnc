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
let debug = ref true

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

module CoordSet = Set.Make(Coord)

module Console = struct
  type t = {
    chars: int CoordMap.t;
    cursor: Coord.t; (* XXX: should this be moved to the Window *)
    cols: int;
  }

  let make cols =
    let chars = CoordMap.empty in
    let cursor = 0, 0 in
    { cols; chars; cursor }

  let view t start_row rows =
    let chars = CoordMap.fold (fun (row, col) char acc ->
      if row >= start_row && (row < (start_row + rows))
      then CoordMap.add (row - start_row, col) char acc
      else acc
    ) t.chars CoordMap.empty in
    let cursor = fst t.cursor - start_row, snd t.cursor in
    let cols = t.cols in
    { chars; cursor; cols }

  let output_char (t: t) c =
    if c = '\n'
    then { t with chars = t.chars; cursor = fst t.cursor + 1, 0 }
    else
      let chars = CoordMap.add t.cursor (int_of_char c) t.chars in
      let cursor =
        if snd t.cursor = t.cols - 1
        then fst t.cursor + 1, 0
        else fst t.cursor, snd t.cursor + 1 in
      { t with chars = chars; cursor = cursor }

  let output_string (t: t) s =
    let s' = String.length s in
    let rec loop i t =
      if i = s' then t else loop (i + 1) (output_char t s.[i]) in
    loop 0 t
end

module Window = struct
  type position =
    | Fixed of int      (* starting row *)
    | End               (* following end of console *)

  type t = {
    position: position;
    rows: int;          (* visible *)
  }

  let make rows = {
    position = End;
    rows;
  }

  let get_scroll_offset t console = match t.position with
    | Fixed x -> x
    | End -> max 0 (fst console.Console.cursor - t.rows + 1)

  let get_visible t console =
    Console.view console (get_scroll_offset t console) t.rows

  let dump t console =
    for row = 0 to t.rows - 1 do
      for col = 0 to console.Console.cols - 1 do
        try
          let c = CoordMap.find (row, col) console.Console.chars in
          print_string (String.make 1 (char_of_int c))
        with Not_found -> ()
      done;
      print_string "\n"
    done
end

module Delta = struct
  type t =
    | Write of int CoordMap.t
    | Erase of CoordSet.t
    | Scroll of int

  let update_chars a b =
    let chars_to_draw = CoordMap.fold (fun coord char acc ->
      if CoordMap.mem coord a.Console.chars && CoordMap.find coord a.Console.chars = char
      then acc (* already present *)
      else CoordMap.add coord char acc
    ) b.Console.chars CoordMap.empty in
    let chars_to_erase = CoordMap.fold (fun coord char acc ->
      if CoordMap.mem coord b.Console.chars 
      then acc (* still present *)
      else CoordSet.add coord acc
    ) a.Console.chars CoordSet.empty in
    [
      Write chars_to_draw;
      Erase chars_to_erase
    ]

  let apply window console d =
    let scroll_offset = Window.get_scroll_offset window console in
    match d with
    | Write chars_to_draw ->
      let chars = CoordMap.fold (fun coord char acc ->
        let coord' = fst coord + scroll_offset, snd coord in
        CoordMap.add coord' char acc
      ) chars_to_draw console.Console.chars in
      { console with Console.chars }
    | Erase chars_to_erase ->
      let chars = CoordSet.fold (fun coord acc ->
        let coord' = fst coord + scroll_offset, snd coord in
        CoordMap.remove coord' acc
      ) chars_to_erase console.Console.chars in
      { console with Console.chars }
    | Scroll _ -> console

  let draw initial_window initial_console current_window current_console =
    (* draw the new content into the current window *)
    let offset = Window.get_scroll_offset initial_window initial_console in
    let fixed_initial_window = { initial_window with Window.position = Window.Fixed offset } in
    let a = Window.get_visible fixed_initial_window initial_console in
    let b = Window.get_visible fixed_initial_window current_console in
    let update_current_window = update_chars a b in
    (* reflect these changes in 'initial_console' *)
    let initial_console = List.fold_left (apply initial_window) initial_console update_current_window in

    (* scroll the window *)
    let initial_scroll_offset = Window.get_scroll_offset initial_window initial_console in
    let final_scroll_offset = Window.get_scroll_offset current_window current_console in
    let change_scroll_offset = final_scroll_offset - initial_scroll_offset in
    let scroll = Scroll change_scroll_offset in

    (* draw any new revealed content *)
    let a = Window.get_visible current_window initial_console in
    let b = Window.get_visible current_window current_console in
    let final_reveal = update_chars a b in

    update_current_window @ [ scroll ] @ final_reveal
end

let debug () = 
  let c = ref (Console.make 80) in
  let w = Window.make 10 in
  for i = 0 to 100 do
    c := Console.output_string !c (Printf.sprintf "%d: hello world\n" i);
    Unix.sleep 1;
    Printf.fprintf stdout "Iteration %d\n%!" i;
    Window.dump w (Window.get_visible w !c);
    flush stdout;
  done
