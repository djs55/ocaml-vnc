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
    max_chars: int;  (* maximum number of stored characters *)
    cursor: Coord.t; (* XXX: should this be moved to the Window *)
    cols: int;
  }

  let make ?(max_chars = 80 * 100) cols =
    let chars = CoordMap.empty in
    let cursor = 0, 0 in
    { cols; chars; max_chars; cursor }

  let output_char (t: t) c =
    let chars =
      if CoordMap.cardinal t.chars >= t.max_chars
      then CoordMap.remove (fst (CoordMap.min_binding t.chars)) t.chars
      else t.chars in
    if c = '\n'
    then { t with chars = chars; cursor = fst t.cursor + 1, 0 }
    else
      let chars = CoordMap.add t.cursor (int_of_char c) chars in
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

end

module Screen = struct
  type t = {
    chars: int CoordMap.t;
    cursor: Coord.t option;
    rows: int;
    cols: int;
  }

  let make console window =
    let start_row = Window.get_scroll_offset window console in
    let rows = window.Window.rows in
    let cols = console.Console.cols in

    let chars = CoordMap.fold (fun (row, col) char acc ->
      if row >= start_row && (row < (start_row + rows))
      then CoordMap.add (row - start_row, col) char acc
      else acc
    ) console.Console.chars CoordMap.empty in
    let y, x = fst console.Console.cursor - start_row, snd console.Console.cursor in
    let cursor = if y < 0 || y >= rows || x < 0 || x >= cols then None else Some (y, x) in 
    { chars; cursor; rows; cols }

  let dump t =
    for row = 0 to t.rows - 1 do
      for col = 0 to t.cols - 1 do
        try
          let c = CoordMap.find (row, col) t.chars in
          print_string (String.make 1 (char_of_int c))
        with Not_found -> ()
      done;
      print_string "\n"
    done
end

module Delta = struct
  type cell = {
    char: int option;
    highlight: bool;
  }

  type t =
    | Update of cell CoordMap.t
    | Scroll of int

  let difference a b =
    let cells = CoordMap.empty in
    (* unhighlight the old cursor location *)
Printf.printf "a.cursor = %s b.cursor = %s\n%!" (match a.Screen.cursor with None -> "None" | Some (row, col) -> string_of_int row ^ "," ^ (string_of_int col)) (match b.Screen.cursor with None -> "None" | Some (row, col) -> string_of_int row ^ "," ^ (string_of_int col));

    let cells = match a.Screen.cursor with
      | None -> cells
      | Some x -> CoordMap.add x { char = if CoordMap.mem x b.Screen.chars then Some (CoordMap.find x b.Screen.chars) else None; highlight = false } cells in
    let cells = match b.Screen.cursor with
      | None -> cells
      | Some x -> CoordMap.add x { char = if CoordMap.mem x b.Screen.chars then Some (CoordMap.find x b.Screen.chars) else None; highlight = true } cells in
    let cells = CoordMap.fold (fun coord char acc ->
      if CoordMap.mem coord a.Screen.chars && CoordMap.find coord a.Screen.chars = char
      then acc (* already present *)
      else CoordMap.add coord { char = Some char; highlight = b.Screen.cursor = Some coord }  acc
    ) b.Screen.chars cells in
    let cells = CoordMap.fold (fun coord char acc ->
      if CoordMap.mem coord b.Screen.chars 
      then acc (* still present *)
      else CoordMap.add coord { char = None; highlight = b.Screen.cursor = Some coord } acc
    ) a.Screen.chars cells in
    [ Update cells ]
  
let apply screen d =
    match d with
    | Update cells ->
      let chars, cursor = CoordMap.fold (fun coord cell (chars, cursor) ->
        let chars = match cell.char with
          | Some char -> CoordMap.add coord char screen.Screen.chars
          | None -> CoordMap.remove coord screen.Screen.chars in
        let cursor = if cell.highlight then Some coord else cursor in
        chars, cursor
      ) cells (screen.Screen.chars, screen.Screen.cursor) in
      { screen with Screen.chars; cursor }
    | Scroll lines ->
      let chars = CoordMap.fold (fun coord char acc ->
        let coord' = fst coord + lines, snd coord in
        CoordMap.add coord' char acc
      ) screen.Screen.chars CoordMap.empty in
      let cursor = match screen.Screen.cursor with
      | None -> None
      | Some x -> Some (fst x + lines, snd x) in
      { screen with Screen.chars; Screen.cursor }

  let draw initial_window initial_console current_window current_console =
    (* without moving the window, refresh the currently visible content *)
    let offset = Window.get_scroll_offset initial_window initial_console in
    let fixed_initial_window = { initial_window with Window.position = Window.Fixed offset } in
    let a = Screen.make initial_console fixed_initial_window in
    let b = Screen.make current_console fixed_initial_window in
    let update_current_window = difference a b in
    
    (* scroll the window *)
    let initial_scroll_offset = Window.get_scroll_offset initial_window initial_console in
    let final_scroll_offset = Window.get_scroll_offset current_window current_console in
    let change_scroll_offset = final_scroll_offset - initial_scroll_offset in
    let scroll = Scroll change_scroll_offset in
    
    (* draw any new revealed content *)
    let a = apply b scroll in
    let b = Screen.make current_console current_window in
    let final_reveal = difference a b in

    update_current_window @ [ scroll ] @ final_reveal
end

let debug () = 
  let c = ref (Console.make 80) in
  let w = Window.make 10 in
  for i = 0 to 100 do
    c := Console.output_string !c (Printf.sprintf "%d: hello world\n" i);
    Unix.sleep 1;
    Printf.fprintf stdout "Iteration %d\n%!" i;
    let s = Screen.make !c w in
    Screen.dump s;
    flush stdout;
  done
