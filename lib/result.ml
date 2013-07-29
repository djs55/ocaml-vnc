
let ('a, 'b) result =
  | `Ok of 'a
  | `Error of 'b

let (>>=) x f = match x with
  | `Ok y -> f y
  | `Error z -> x

