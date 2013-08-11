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

(* TODO:
   1. Check for overflow in UInt32/UInt64 
*)

exception Truncated

let _marshal (x: int list) =
  let chars = List.map char_of_int (List.map (fun x -> x land 0xff) x) in
  let buf = String.make (List.length chars) '\000' in
  List.iteri (fun i c -> buf.[i] <- c) chars;
  buf
let _unmarshal (x: string) =
  let rec explode acc = function
    | -1 -> acc
    | n -> explode (x.[n] :: acc) (n - 1) in
 List.map int_of_char (explode [] (String.length x - 1))

let blit src srcoff dst dstoff len = 
    (* Printf.printf "blit src_len=%d srcoff=%d dst_len=%d dstoff=%d len=%d\n" (String.length src) srcoff (String.length dst) dstoff len;  *)
    String.blit src srcoff dst dstoff len

module UInt16 = struct
  type t = int

  let (||) = (lor)
  let (<<) = (lsl)
  let (>>) = (lsr)
  let (&&) = (land)

  let marshal (x: t) : string =
    _marshal [ x >> 8; x ]
  let marshal_at (buf: string) (off: int) (x: t) = 
    let raw = marshal x in
    blit raw 0 buf off 2;
    off + 2
  let unmarshal (x: Cstruct.t) : t =
    if Cstruct.len x < 2 then raise Truncated;
    let msb = Cstruct.get_uint8 x 0 in
    let lsb = Cstruct.get_uint8 x 1 in
    (msb lsl 8) || lsb

  let prettyprint = string_of_int
  let to_int x = x
  let of_int x = x
end

module UInt32 = struct
  type t = int32

  let (||) = Int32.logor
  let (lsl) = Int32.shift_left
  let (>>) = Int32.shift_right
  let (&&) = Int32.logand
  
  let marshal (x: t) : string = 
    _marshal (List.map Int32.to_int [ x >> 24; x >> 16; x >> 8; x ])
  let marshal_at (buf: string) (off: int) (x: t) = 
    let raw = marshal x in
    blit raw 0 buf off 4;
    off + 4
  let unmarshal (x: Cstruct.t) : t =
    if Cstruct.len x < 4 then raise Truncated;
    let a = Int32.of_int (Cstruct.get_uint8 x 0) in
    let b = Int32.of_int (Cstruct.get_uint8 x 1) in
    let c = Int32.of_int (Cstruct.get_uint8 x 2) in
    let d = Int32.of_int (Cstruct.get_uint8 x 3) in
    (a lsl 24) || (b lsl 16) || (c lsl 8) || d
	
  let prettyprint = string_of_int
  let to_int32 x = x
  let of_int32 x = x
end

module UInt64 = struct
  type t = int64

  let (||) = Int64.logor
  let (lsl) = Int64.shift_left
  let (>>) = Int64.shift_right
  let (&&) = Int64.logand

  let marshal (x: t) : string = 
    _marshal (List.map Int64.to_int [ x >> 56; x >> 48; x >> 40; x >> 32; x >> 24; x >> 16; x >> 8; x ])
  let unmarshal (x: string) : t = match List.map Int64.of_int (_unmarshal x) with
    | [ a; b; c; d; e; f; g; h ] -> (a lsl 56 ) || (b lsl 48) || (c lsl 40) || (d lsl 32) || (e lsl 24) || (f lsl 16) || (g lsl 8) || h
    | _ -> raise Truncated

  let prettyprint = Int64.to_string
  let to_int64 x = x
  let of_int64 x = x
end

module type ASYNC = sig
  type 'a t

  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module type CHANNEL = sig
  include ASYNC

  type fd

  val really_read: fd -> int -> Cstruct.t t
  val really_write: fd -> string -> unit t
end

module Make = functor(Channel: CHANNEL) -> struct
  open Channel

module ProtocolVersion = struct
  type t = { major: int; minor: int }

  exception Unmarshal_failure

  cstruct hdr {
    uint8_t rfb[4];
    uint8_t major[3];
    uint8_t dot;
    uint8_t minor[3];
    uint8_t newline
  } as big_endian

  let marshal_at (x: t) buf =
    set_hdr_rfb "RFB " 0 buf;
    set_hdr_major (Printf.sprintf "%03d" x.major) 0 buf;
    set_hdr_dot buf (int_of_char '.');
    set_hdr_minor (Printf.sprintf "%03d" x.minor) 0 buf;
    set_hdr_newline buf (int_of_char '\n')

  let marshal (x: t) =
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout sizeof_hdr)) in
    marshal_at x buf;
    Cstruct.to_string buf

  let unmarshal (s: Channel.fd) = 
    really_read s sizeof_hdr >>= fun x ->
    if copy_hdr_rfb x <> "RFB "
    then raise Unmarshal_failure;
    let major = int_of_string (copy_hdr_major x) in
    let minor = int_of_string (copy_hdr_minor x) in
    return { major = major; minor = minor }

  let prettyprint (x: t) = 
    Printf.sprintf "ProtocolVersion major = %d minor = %d" x.major x.minor
end

module Error = struct
  type t = string

  cstruct hdr {
    uint32_t length
  } as big_endian

  let sizeof (x: t) = sizeof_hdr + (String.length x)

  let marshal_at (x: t) buf =
    let x' = String.length x in
    set_hdr_length buf (Int32.of_int x');
    Cstruct.blit_from_string x 0 buf sizeof_hdr x'

  let marshal (x: t) =
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun x ->
    let len = get_hdr_length x in
    really_read s (Int32.to_int len)
end

(* 3.3 *)
module SecurityType = struct
  type t = Failed of string | NoSecurity | VNCAuth

  exception Unmarshal_failure

  cenum code {
    FAILED;
    NOSECURITY;
    VNCAUTH
  } as uint32_t

  cstruct hdr {
    uint32_t ty
  } as big_endian

  let sizeof (x: t) = match x with
    | Failed x -> sizeof_hdr + (Error.sizeof x)
    | NoSecurity | VNCAuth -> sizeof_hdr

  let marshal_at (x: t) buf = match x with
    | Failed x ->
      set_hdr_ty buf (code_to_int FAILED);
      Error.marshal_at x (Cstruct.shift buf sizeof_hdr)
    | NoSecurity ->
      set_hdr_ty buf (code_to_int NOSECURITY)
    | VNCAuth ->
      set_hdr_ty buf (code_to_int VNCAUTH)

  let marshal (x: t) =
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun x -> 
    match int_to_code (get_hdr_ty x) with
    | None -> return (`Error(Failure "unknown SecurityType"))
    | Some FAILED ->
      Error.unmarshal s >>= fun x ->
      return (`Ok (Failed (Cstruct.to_string x)))
    | Some NOSECURITY -> return (`Ok NoSecurity)
    | Some VNCAUTH -> return (`Ok VNCAuth)
end

module ClientInit = struct
  type t = bool (* shared-flag *)

  cstruct hdr {
    uint8_t shared
  } as big_endian

  let sizeof _ = sizeof_hdr

  let marshal_at (x: t) buf =
    set_hdr_shared buf (if x then 1 else 0)

  let marshal (x: t) =
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun x ->
    let shared = get_hdr_shared x in
    return (shared <> 0)
end

module PixelFormat = struct
  type bpp = BPP_8 | BPP_16 | BPP_32

  let int_of_bpp = function BPP_8 -> 8 | BPP_16 -> 16 | BPP_32 -> 32

  exception Illegal_bits_per_pixel of int

  let bpp_of_int = function
    | 8 -> BPP_8
    | 16 -> BPP_16
    | 32 -> BPP_32
    | x -> raise (Illegal_bits_per_pixel x)

  let string_of_bpp x = string_of_int (int_of_bpp x)

  type t = { bpp: bpp;
	     depth: int;
	     big_endian: bool;
	     true_colour: bool;
	     red_max_n: int;   (* red_max = 2 ** red_max_n - 1 *)
	     green_max_n: int; (* green_max = 2 ** green_max_n - 1 *)
	     blue_max_n: int;  (* blue_max = 2 ** blue_max_n - 1 *)
	     red_shift: int;
	     green_shift: int;
	     blue_shift: int }

  let to_string t =
    Printf.sprintf "{ bpp=%s; depth=%d; big_endian=%b; true_colour=%b; red_max_n=%d; green_max_n=%d; blue_max_n=%d; red_shift=%d; green_shift=%d; blue_shift=%d }"
    (string_of_bpp t.bpp) t.depth t.big_endian t.true_colour t.red_max_n t.green_max_n t.blue_max_n t.red_shift t.green_shift t.blue_shift

  let true_colour_default big_endian = {
    bpp = BPP_32; depth = 24; big_endian = big_endian;
    true_colour = true;
    red_max_n = 8; green_max_n = 8; blue_max_n = 8;
    red_shift = 16; green_shift = 8; blue_shift = 0;
  }

  let bytes_per_pixel t = int_of_bpp t.bpp / 8

  cstruct hdr {
    uint8_t bpp;
    uint8_t depth;
    uint8_t big_endian;
    uint8_t true_colour;
    uint16_t red_max;
    uint16_t green_max;
    uint16_t blue_max;
    uint8_t red_shift;
    uint8_t green_shift;
    uint8_t blue_shift;
    uint8_t padding[3]
  } as big_endian

  let sizeof _ = sizeof_hdr

  let marshal_at (x: t) buf =
    set_hdr_bpp buf (int_of_bpp x.bpp);
    set_hdr_depth buf x.depth;
    set_hdr_big_endian buf (if x.big_endian then 1 else 0);
    set_hdr_true_colour buf (if x.true_colour then 1 else 0);
    set_hdr_red_max buf (1 lsl x.red_max_n - 1);
    set_hdr_green_max buf (1 lsl x.green_max_n - 1);
    set_hdr_blue_max buf (1 lsl x.blue_max_n - 1);
    set_hdr_red_shift buf x.red_shift;
    set_hdr_green_shift buf x.green_shift;
    set_hdr_blue_shift buf x.blue_shift

  let marshal (x: t) = 
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf

  exception Illegal_colour_max

  let rec log2 = function
    | 0 -> raise Illegal_colour_max
    | 1 -> 0
    | n -> log2 (n / 2) + 1

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun buf ->
    let bpp = bpp_of_int (get_hdr_bpp buf) in
    let depth = get_hdr_depth buf in
    let big_endian = get_hdr_big_endian buf <> 0 in
    let true_colour = get_hdr_true_colour buf <> 0 in
    let red_max_n = log2 (get_hdr_red_max buf + 1) in
    let green_max_n = log2 (get_hdr_green_max buf + 1) in
    let blue_max_n = log2 (get_hdr_blue_max buf + 1) in
    let red_shift = get_hdr_red_shift buf in
    let green_shift = get_hdr_green_shift buf in
    let blue_shift = get_hdr_blue_shift buf in
    return { bpp; depth; big_endian; true_colour;
      red_max_n; green_max_n; blue_max_n;
      red_shift; green_shift; blue_shift }
end

module Pixel = struct
  open PixelFormat

  let encode pf r g b =
    if pf.true_colour then begin
      let r' = r lsr (8 - pf.red_max_n) in
      let g' = g lsr (8 - pf.green_max_n) in
      let b' = b lsr (8 - pf.blue_max_n) in
      (r' lsl pf.red_shift) lor (g' lsl pf.green_shift) lor (b' lsl pf.blue_shift)
    end else failwith "implement colour maps"

  let write pf buf ofs pixel =
    match pf.PixelFormat.bpp, pf.PixelFormat.big_endian with
    | BPP_8, _ ->
      buf.[ofs] <- char_of_int pixel
    | BPP_16, true ->
      buf.[ofs + 0] <- char_of_int (pixel lsr 8);
      buf.[ofs + 1] <- char_of_int (pixel land 0xff)
    | BPP_16, false ->
      buf.[ofs + 0] <- char_of_int (pixel land 0xff);
      buf.[ofs + 1] <- char_of_int (pixel lsr 8)
    | BPP_32, true ->
      buf.[ofs + 0] <- char_of_int (pixel lsr 16);
      buf.[ofs + 1] <- char_of_int ((pixel lsr 8) land 0xff);
      buf.[ofs + 2] <- char_of_int (pixel land 0xff);
      buf.[ofs + 3] <- char_of_int 0
    | BPP_32, false ->
      buf.[ofs + 0] <- char_of_int (pixel land 0xff);
      buf.[ofs + 1] <- char_of_int ((pixel lsr 8) land 0xff);
      buf.[ofs + 2] <- char_of_int (pixel lsr 16);
      buf.[ofs + 3] <- char_of_int 0
end

module ServerInit = struct
  type t = { width: int; height: int;
	     name: string;
	     pixelformat: PixelFormat.t }

  cstruct hdr {
    uint16_t width;
    uint16_t height;
    uint8_t pixelformat[16];
    uint32_t name_length
  } as big_endian

  let sizeof x = sizeof_hdr + (String.length x.name)

  let marshal_at (x: t) buf =
    set_hdr_width buf x.width;
    set_hdr_height buf x.height;
    PixelFormat.marshal_at x.pixelformat (Cstruct.shift buf 4);
    set_hdr_name_length buf (Int32.of_int (String.length x.name));
    Cstruct.blit_from_string x.name 0 buf sizeof_hdr (String.length x.name)

  let marshal (x: t) =
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf
end

module SetPixelFormat = struct
  type t = PixelFormat.t

  cstruct hdr {
    uint8_t padding[3]
  } as big_endian

  let sizeof _ = sizeof_hdr + PixelFormat.sizeof_hdr

  let marshal_at (x: t) buf =
    PixelFormat.marshal_at x (Cstruct.shift buf sizeof_hdr)

  let marshal (x: t) = 
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun _ ->
    PixelFormat.unmarshal s

  let prettyprint (x: t) = 
    Printf.sprintf "SetPixelFormat %s" (PixelFormat.to_string x) 
end

module Encoding = struct
  type t =
    | Raw
    | CopyRect
    | RRE
    | Hextile	
    | ZRLE
    | Cursor
    | DesktopSize

  let to_string = function
    | Raw         -> "Raw"
    | CopyRect    -> "CopyRect"
    | RRE         -> "RRE"
    | Hextile     -> "Hextile"
    | ZRLE        -> "ZRLE"
    | Cursor      -> "Cursor"
    | DesktopSize -> "DesktopSize"

  let to_int32 = function
    | Raw         -> 0l
    | CopyRect    -> 1l
    | RRE         -> 2l
    | Hextile     -> 5l
    | ZRLE        -> 16l
    | Cursor      -> -239l
    | DesktopSize -> -223l

  let of_int32 = function
    | 0l    -> Some Raw
    | 1l    -> Some CopyRect
    | 2l    -> Some RRE
    | 5l    -> Some Hextile
    | 16l   -> Some ZRLE
    | -239l -> Some Cursor
    | -223l -> Some DesktopSize
    | _     -> None
end

module SetEncodings = struct
  type t = Encoding.t list

  cstruct hdr {
    uint8_t padding;
    uint16_t nr_encodings
  } as big_endian

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun x ->
    let num = get_hdr_nr_encodings x in
    let rec loop acc n =
      if n > num
      then return (List.rev acc)
      else
        really_read s 4 >>= fun x ->
        match Encoding.of_int32 (Cstruct.BE.get_uint32 x 0) with
        | None -> loop acc (n + 1)
        | Some e -> loop (e :: acc) (n + 1) in
    loop [] 1

  let prettyprint (x: t) = 
    Printf.sprintf "SetEncodings (num=%d) [ %s ]" (List.length x) (String.concat "; " (List.map Encoding.to_string x))
end

module FramebufferUpdateRequest = struct
  type t = { incremental: bool;
	     x: int; y: int;
	     width: int; height: int }

  cstruct hdr {
    uint8_t incremental;
    uint16_t x;
    uint16_t y;
    uint16_t width;
    uint16_t height
  } as big_endian

  let unmarshal (s: Channel.fd) = 
    really_read s sizeof_hdr >>= fun buf ->
    let incremental = get_hdr_incremental buf <> 0 in
    let x = get_hdr_x buf in
    let y = get_hdr_y buf in
    let width = get_hdr_width buf in
    let height = get_hdr_height buf in
    return { incremental; x; y; width; height }

  let prettyprint (x: t) = 
    Printf.sprintf "FrameBufferUpdateRequest (incr=%b x=%d y=%d width=%d height=%d)" x.incremental x.x x.y x.width x.height
end

module FramebufferUpdate = struct
  module Raw = struct
    (* width * height * bpp *)
    type t = { buffer: string }
    let sizeof (x: t) = String.length x.buffer
    let marshal (x: t) = x.buffer
    let marshal_at (buf: string) (off: int) (x: t) = 
        let length = sizeof x in
        blit x.buffer 0 buf off length;
        off + length
    let prettyprint (x: t) = 
      "FrameBufferUpdate"
  end
  module CopyRect = struct
    type t = { x: int; y: int }
    let sizeof (x: t) = 2 + 2
    let marshal (x: t) = 
      UInt16.marshal x.x ^ (UInt16.marshal x.y)
    let marshal_at (buf: string) (off: int) (x: t) = 
      let off = UInt16.marshal_at buf off x.x in
      UInt16.marshal_at buf off x.y 
    let prettyprint (x: t) = 
      Printf.sprintf "{ x = %d; y = %d }" x.x x.y
  end
  module RRE = struct
    type t = {
      background: string;
      rectangles: rectangle list;
    } and rectangle = {
      foreground: string;
      x: int;
      y: int;
      w: int;
      h: int;
    }
    let sizeof (x: t) =
      let pixel = String.length x.background in
      pixel + 4 + (8 + pixel) * (List.length x.rectangles)
    let marshal (x: t) =
      UInt32.marshal (Int32.of_int (List.length x.rectangles)) ^ x.background ^
      (String.concat "" (List.map (fun r -> r.foreground ^ (UInt16.marshal r.x) ^ (UInt16.marshal r.y) ^ (UInt16.marshal r.w) ^ (UInt16.marshal r.h)) x.rectangles))
    let prettyprint (x: t) =
      Printf.sprintf "{ background = %s; rectangles = %d }" x.background (List.length x.rectangles) 
  end
  module Encoding = struct
    type t = 
      | Raw of Raw.t
      | CopyRect of CopyRect.t
      | RRE of RRE.t
      | DesktopSize
    let sizeof (x: t) = match x with
      | Raw x -> 4 + Raw.sizeof x
      | CopyRect x -> 4 + CopyRect.sizeof x
      | RRE x -> 4 + RRE.sizeof x
      | DesktopSize -> 4
    let marshal (x: t) = match x with
      | Raw x -> UInt32.marshal 0l ^ (Raw.marshal x)
      | CopyRect x -> UInt32.marshal 1l ^ (CopyRect.marshal x)
      | RRE x -> UInt32.marshal 2l ^ (RRE.marshal x)
      | DesktopSize -> UInt32.marshal (-223l)
    let marshal_at (buf: string) (off: int) (x: t) = match x with
      | Raw x -> 
        let off = UInt32.marshal_at buf off 0l in
        Raw.marshal_at buf off x
      | CopyRect x -> 
        let off = UInt32.marshal_at buf off 1l in
        CopyRect.marshal_at buf off x
      | RRE x -> failwith "unimplemented RRE.marshal_at"
      | DesktopSize -> 
        UInt32.marshal_at buf off (-223l)
    let prettyprint = function
      | Raw _ -> "Raw"
      | CopyRect x -> "CopyRect " ^ (CopyRect.prettyprint x)
      | RRE x -> "RRE " ^ (RRE.prettyprint x)
      | DesktopSize -> "DesktopSize"
  end
  type t = { x: int; y: int; w: int; h: int; encoding: Encoding.t }
  let sizeof (xs: t list) = 
    let one (one: t) = 2 + 2 + 2 + 2 + (Encoding.sizeof one.encoding) in
    2 (* \000\000 *) + 2 (* length *) + (List.fold_left (+) 0 (List.map one xs))
  let marshal_at (buf: string) (off: int) (xs: t list) = 
    let off = UInt16.marshal_at buf off 0 in
    let off = UInt16.marshal_at buf off (List.length xs) in
    let update (buf: string) (off: int) (one: t) = 
        let off = UInt16.marshal_at buf off one.x in
        let off = UInt16.marshal_at buf off one.y in
        let off = UInt16.marshal_at buf off one.w in
        let off = UInt16.marshal_at buf off one.h in
        Encoding.marshal_at buf off one.encoding in
    List.fold_left (fun off x -> update buf off x) off xs
  let marshal (xs: t list) = 
    let update (one: t) = 
      let x = UInt16.marshal one.x and y = UInt16.marshal one.y in
      let w = UInt16.marshal one.w and h = UInt16.marshal one.h in
      x ^ y ^ w ^ h ^ (Encoding.marshal one.encoding) in
    let length = UInt16.marshal (List.length xs) in
    "\000\000" ^ length ^ (String.concat "" (List.map update xs))
  let prettyprint (t: t) =
    Printf.sprintf "Rectangle {x=%d y=%d w=%d h=%d encoding=%s}"
      t.x t.y t.w t.h (Encoding.prettyprint t.encoding)
end

module SetColourMapEntries = struct
  type t = { first_colour: int; 
	     map: (int * int * int) list }

  cstruct hdr {
    uint8_t padding;
    uint16_t first_colour;
    uint16_t nr_colours
  } as big_endian

  cstruct colour {
    uint16_t r;
    uint16_t g;
    uint16_t b
  } as big_endian

  let sizeof x = sizeof_hdr + (List.length x.map * sizeof_colour)

  let marshal_at (x: t) buf =
    set_hdr_first_colour buf x.first_colour;
    set_hdr_nr_colours buf (List.length x.map);
    let (_: Cstruct.t) = List.fold_left (fun buf (r, g, b) ->
      set_colour_r buf r;
      set_colour_g buf g;
      set_colour_b buf b;
      Cstruct.shift buf sizeof_colour
    ) buf x.map in
    ()

  let marshal (x: t) = 
    let buf = Cstruct.of_bigarray (Bigarray.(Array1.create char c_layout (sizeof x))) in
    marshal_at x buf;
    Cstruct.to_string buf
end

module KeyEvent = struct
  type t = { down: bool; key: UInt32.t }

  cstruct hdr {
    uint8_t down;
    uint16_t padding;
    uint32_t key
  } as big_endian

  let unmarshal (s: Channel.fd) =
    really_read s sizeof_hdr >>= fun buf ->
    let down = get_hdr_down buf <> 0 in
    let key = get_hdr_key buf in
    return { down; key }

  let prettyprint (x: t) = 
    Printf.sprintf "KeyEvent { down = %b; key = %s }"
      x.down (Int32.to_string x.key)
end

module PointerEvent = struct
  type t = { mask: int; x: int; y: int }

  let unmarshal (s: Channel.fd) =
    really_read s 5 >>= fun buf ->
    return { mask = Cstruct.get_uint8 buf 0;
      x = UInt16.unmarshal (Cstruct.sub buf 1 2);
      y = UInt16.unmarshal (Cstruct.sub buf 3 2);
    }
  let prettyprint (x: t) = 
    Printf.sprintf "PointerEvent { mask = %d; x = %d; y = %d }"
      x.mask x.x x.y
end

module ClientCutText = struct
  type t = string

  let unmarshal (s: Channel.fd) =
    really_read s 7 >>= fun buf -> 
    let length = UInt32.unmarshal (Cstruct.sub buf 3 4) in
    really_read s (Int32.to_int length) >>= fun buf ->
    return (Cstruct.to_string buf)
  let prettyprint (x: t) = 
    Printf.sprintf "ClientCutText { %s }" x
end

module Request = struct
  type t = 
    | SetPixelFormat of SetPixelFormat.t
    | SetEncodings of SetEncodings.t	
    | FrameBufferUpdateRequest of FramebufferUpdateRequest.t
    | KeyEvent of KeyEvent.t
    | PointerEvent of PointerEvent.t
    | ClientCutText of ClientCutText.t

  let prettyprint = function
    | SetPixelFormat x -> SetPixelFormat.prettyprint x
    | SetEncodings x -> SetEncodings.prettyprint x
    | FrameBufferUpdateRequest x -> FramebufferUpdateRequest.prettyprint x
    | KeyEvent x -> KeyEvent.prettyprint x
    | PointerEvent x -> PointerEvent.prettyprint x
    | ClientCutText x -> ClientCutText.prettyprint x

  let unmarshal (s: Channel.fd) =
    really_read s 1 >>= fun x ->
    match Cstruct.get_uint8 x 0 with
    | 0 ->
        SetPixelFormat.unmarshal s >>= fun x ->
	return (SetPixelFormat x)
    | 2 ->
        SetEncodings.unmarshal s >>= fun x ->
	return (SetEncodings x)
    | 3 ->
        FramebufferUpdateRequest.unmarshal s >>= fun x ->
	return (FrameBufferUpdateRequest x)
    | 4 ->
        KeyEvent.unmarshal s >>= fun x ->
	return (KeyEvent x)
    | 5 ->
        PointerEvent.unmarshal s >>= fun x ->
	return (PointerEvent x)
    | 6 ->
        ClientCutText.unmarshal s >>= fun x ->
	return (ClientCutText x)
    | x ->
	failwith (Printf.sprintf "Unknown message type: %d" x)
end


let white = (255, 255, 255)
let black = (0, 0, 0)

let handshake name pixelformat w h (s: Channel.fd) =
  let ver = { ProtocolVersion.major = 3; minor = 3 } in
  really_write s (ProtocolVersion.marshal ver) >>= fun () ->
  ProtocolVersion.unmarshal s >>= fun ver' ->
  print_endline (ProtocolVersion.prettyprint ver');
  really_write s (SecurityType.marshal SecurityType.NoSecurity) >>= fun () ->
  ClientInit.unmarshal s >>= fun ci ->
  if ci then print_endline "Client requests a shared display"
  else print_endline "Client requests a non-shared display";
  let si = { ServerInit.name; pixelformat;
	     width = w; height = h } in
  really_write s (ServerInit.marshal si)
end
