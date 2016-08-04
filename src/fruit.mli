open Core.Std

type t = 
  | Apple
  | Banana
  | Kiwi
  [@@deriving sexp]

include Stringable.S with type t := t
