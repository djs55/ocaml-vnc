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

module Coord : sig
  (** A 2D co-ordinate, (row, column) *)

  type t = int * int

  val compare: t -> t -> int
end

module CoordMap : sig
  include module type of Map.Make(Coord)
end

module CoordSet : sig
  include module type of Set.Make(Coord)
end

module Console : sig
  (** Represents the entire console contents. *)

  type t = {
    chars: int CoordMap.t;  (** entire contents stored as a sparse map *)
    max_chars: int;         (** maximum scrollback size in characters *)
    cursor: Coord.t;        (** the cursor position *)
    cols: int;              (** the width in characters, used for wrapping the cursor *)
  }

  val make: ?max_chars:int -> int -> t
  (** [make ?max_chars cols] creates an empty console with width [cols]
      and maximum scrollback [max_chars] *)

  val output_char: t -> char -> t
  (** [output_char t c] writes [c] at the cursor position in [t] and moves
      the cursor *)

  val output_string: t -> string -> t
  (** [output_string t s] writes [s] at the cursor position in [t] and moves
      the cursor. *)
end

module Window : sig
  (** Represents the portion of the console that we want to see. *)

  type position =
    | Fixed of int      (* starting row *)
    | End               (* following end of console *)
  (** Describes how the visible portion should change as the console changes
      in size. *)

  type t = {
    position: position;
    rows: int;          (** visible rows *)
  }

  val make: int -> t
  (** [make rows] makes a Window of size [rows] set to follow the end of
      the console as more data is produced. *)

end

module Screen : sig
  (** Represents a set of visible characters *)
  type t = {
    chars: int CoordMap.t; (** the visible characters in screen co-ordinates *)
    cursor: Coord.t;       (** the cursor position in screen co-ordinates *)
    rows: int;             (** the number of visible rows *)
    cols: int;             (** the number of visible columns *)
  }

  val make: Console.t -> Window.t -> t
  (** [make console window] extracts the visible part of [console] according
      to [window] *)

end

module Delta : sig
  (** Represents a drawing operation to update the state of a Screen *)

  type t =
    | Write of int CoordMap.t (** characters to be written *)
    | Erase of CoordSet.t     (** characters to be erased *)
    | Scroll of int           (** a number of lines to scroll (+ve means down) *)

  val apply: Screen.t -> t -> Screen.t
  (** [apply screen t] applies the drawing operation [t] to [screen] *)

  val draw: Window.t -> Console.t -> Window.t -> Console.t -> t list
  (** [draw initial_window initial_console final_window final_console]
      constructs a list of drawing operations which, when applied to
      [Screen.make initial_console initial_window] will transform it into
      [Screen.make final_console final_window] *)

end

