(* types.ml *)

type throw = S of int | D of int | T of int
type checkout = throw list
type checkouts = checkout list
