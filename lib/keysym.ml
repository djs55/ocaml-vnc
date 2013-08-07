
(* X11 keysyms are 29 bit integer values which represent
   characters associated with keyboard keys "e.g., via
   the visible engraving". *)

type t = int


and tty_function_key =
| Backspace
| Tab
| Linefeed
| Clear
| Return
| Pause
| Scroll_lock
| Sys_Req
| Escape
| Delete

let of_int = function
| 0xff08 -> Backspace
| 0xff09 -> Tab
| 0xff0a -> Linefeed
| 0xff0b -> Clear
| 0xff0d -> Return
| 0xff13 -> Pause
| 0xff14 -> Scroll_lock
| 0xff15 -> Sys_Req
| 0xff1b -> Escape
| 0xffff -> Delete



