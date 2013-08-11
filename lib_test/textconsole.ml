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
  let compare ((row1,col1): t) ((row2,col2): t) =
    if row1 < row2 then -1
    else if row1 > row2 then 1
    else if col1 < col2 then -1
    else if col1 > col2 then 1
    else 0

  let to_string (row, col) = Printf.sprintf "{ row=%d; col=%d }" row col
end

module CoordMap = Map.Make(Coord)

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

type cell = {
  char: int option;
  highlight: bool;
}

let string_of_cell c =
  Printf.sprintf "char = %s; highlight = %b"
    (match c.char with None -> "None" | Some x -> String.make 1 (char_of_int x))
  c.highlight

module CellMap = Map.Make(struct
  type t = cell
  let compare = compare
end)

module Screen = struct
  type t = {
    cells: cell CoordMap.t;
    coords: Coord.t CellMap.t;
    rows: int;
    cols: int;
  }

  let make console window =
    let start_row = Window.get_scroll_offset window console in
    let rows = window.Window.rows in
    let cols = console.Console.cols in
    let cells, coords = CoordMap.fold (fun (row, col) char (cells, coords) ->
      if row >= start_row && (row < (start_row + rows))
      then
        let cell = { char = Some char; highlight = console.Console.cursor = (row, col) } in
        let coord = row - start_row, col in
        let cells = CoordMap.add coord cell cells in
        let coords = CellMap.add cell coord coords in
        cells, coords
      else cells, coords
    ) console.Console.chars (CoordMap.empty, CellMap.empty) in
    (* add the cursor (which may be on an empty cell) *)
    let cells, coords =
      let cursor = console.Console.cursor in
      let cursor = fst cursor - start_row, snd cursor in
      if CoordMap.mem cursor cells
      then cells, coords
      else
        let cell = { char = None; highlight = true } in
        let cells = CoordMap.add cursor cell cells in
        let coords = CellMap.add cell cursor coords in
        cells, coords in
    { cells; coords; rows; cols }

  let dump t =
    for row = 0 to t.rows - 1 do
      for col = 0 to t.cols - 1 do
        try
          let c = CoordMap.find (row, col) t.cells in
          print_string (String.make 1 (match c.char with None -> ' ' | Some x -> char_of_int x))
        with Not_found -> ()
      done;
      print_string "\n"
    done
end

module Delta = struct
  type t =
    | Update of Coord.t * cell
    | Copy  of Coord.t * Coord.t
    | Scroll of int

  let to_string = function
    | Scroll x -> Printf.sprintf "Scroll %d" x
    | Update (coord, cell) ->
      Printf.sprintf "Update %s %s"
        (Coord.to_string coord) (string_of_cell cell)
    | Copy (coord, from) ->
      Printf.sprintf "Copy   %s from %s"
        (Coord.to_string coord) (Coord.to_string from)

  let rec apply screen d =
    match d with
    | Update (coord, cell) ->
      (* If we change a cell which is already bound then we
         must invalidate any entry in our 'cells' map. *)
      let coords =
        if not(CoordMap.mem coord screen.Screen.cells)
        || (CoordMap.find coord screen.Screen.cells = cell)
        then screen.Screen.coords
        else
          let existing = CoordMap.find coord screen.Screen.cells in
          if CellMap.find existing screen.Screen.coords = coord
          then CellMap.remove existing screen.Screen.coords
          else screen.Screen.coords in
      let cells = CoordMap.add coord cell screen.Screen.cells in
      { screen with Screen.cells; coords }
    | Copy (coord, from) ->
      let cell = CoordMap.find from screen.Screen.cells in
      apply screen (Update (coord, cell))
    | Scroll lines ->
      let cells = CoordMap.fold (fun coord cell acc ->
        let coord' = fst coord - lines, snd coord in
        CoordMap.add coord' cell acc
      ) screen.Screen.cells CoordMap.empty in
      let coords = CellMap.fold (fun cell coord acc ->
        let coord' = fst coord - lines, snd coord in
        CellMap.add cell coord' acc
      ) screen.Screen.coords CellMap.empty in
      { screen with Screen.cells; coords }

  let difference invalidate a b =

    let draw coord map =
      if CoordMap.mem coord b.Screen.cells
      then CoordMap.add coord (CoordMap.find coord b.Screen.cells) map
      else CoordMap.add coord { char = None; highlight = false } map in

    let cells = CoordMap.empty in
    let cells =
      if invalidate then begin
        let rec loop acc row col =
          let row, col = if col = a.Screen.cols then row + 1, 0 else row, col in
          if row >= a.Screen.rows then acc
          else loop (draw (row, col) acc) row (col + 1) in
        loop CoordMap.empty 0 0
      end else begin
        let cells = CoordMap.fold (fun coord cell acc ->
          if CoordMap.mem coord a.Screen.cells && CoordMap.find coord a.Screen.cells = cell
          then acc (* already present *)
          else draw coord acc
        ) b.Screen.cells cells in
        let cells = CoordMap.fold (fun coord cell acc ->
          if CoordMap.mem coord b.Screen.cells
          then acc (* still present *)
          else draw coord acc
        ) a.Screen.cells cells
        in cells
      end in
    let a =
      if invalidate
      then { a with Screen.coords = CellMap.empty }
      else a in
    fst (CoordMap.fold (fun coord cell (drawing_ops, a) ->
      let op =
        if CellMap.mem cell a.Screen.coords
        then Copy (coord, CellMap.find cell a.Screen.coords)
        else Update (coord, cell) in
      let a = apply a op in
      op :: drawing_ops, a
    ) cells ([], a))
  
  let draw invalidate initial_window initial_console current_window current_console =
    (* without moving the window, refresh the currently visible content *)
    let offset = Window.get_scroll_offset initial_window initial_console in
    let fixed_initial_window = { initial_window with Window.position = Window.Fixed offset } in
    let a = Screen.make initial_console fixed_initial_window in
    let b = Screen.make current_console fixed_initial_window in
    let update_current_window = difference invalidate a b in
    
    (* scroll the window *)
    let initial_scroll_offset = Window.get_scroll_offset initial_window initial_console in
    let final_scroll_offset = Window.get_scroll_offset current_window current_console in
    let change_scroll_offset = final_scroll_offset - initial_scroll_offset in

    let the_rest =
      if change_scroll_offset = 0
      then []
      else
        let scroll = Scroll change_scroll_offset in
    
        (* draw any new revealed content *)
        let a = apply b scroll in
        let b = Screen.make current_console current_window in
        let final_reveal = difference invalidate a b in
        scroll :: final_reveal in

    update_current_window @ the_rest
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
