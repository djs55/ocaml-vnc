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

open Lwt

type 'a t = 'a Lwt.t

let (>>=) = Lwt.(>>=)
let return = Lwt.return

type fd = Lwt_unix.file_descr

(** Really read, raising End_of_file if no more data *)
let really_read fd n = 
  let buf = String.make n '\000' in
  let rec rread fd buf ofs len = 
    lwt n = Lwt_unix.read fd buf ofs len in
    if n = 0 then raise End_of_file;
    if n < len then rread fd buf (ofs + n) (len - n) else return () in
  lwt () = rread fd buf 0 n in
  let result = Cstruct.create n in
  Cstruct.blit_from_string buf 0 result 0 n;
  return result
let really_write fd buf =
(*
  Printf.printf "About to write %d bytes\n" (String.length buf);
  let buf' = String.length buf in
  if buf' < (16 * 10) then begin
    let c = Cstruct.create buf' in
    Cstruct.blit_from_string buf 0 c 0 buf';
    Cstruct.hexdump c;
  end;
*)
  let rec rwrite fd buf ofs len =
    lwt n = Lwt_unix.write fd buf ofs len in
    if n = 0 then raise End_of_file;
    if n < len then rwrite fd buf (ofs + n) (len - n) else return () in
  lwt () = rwrite fd buf 0 (String.length buf) in
  return ()

