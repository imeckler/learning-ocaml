open Core.Std

module T = struct
  type t = 
    | Apple
    | Banana
    | Kiwi
    [@@deriving sexp]
end
include Sexpable.To_stringable(T)
include T
