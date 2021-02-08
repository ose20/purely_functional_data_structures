(* 二分探索木 *)
module type SET =
  sig
    type elt = int
    type t
    val empty : t
    val insert : elt -> t -> t
    val mem : elt -> t -> bool
    val top : t -> elt
  end

module Set : SET =
  struct
    type elt = int
    type t = Lf | Br of int * t * t
    let empty = Lf
    let rec mem x = function
      | Lf -> false
      | Br(y, left, right) -> 
          if x = y then true
          else if x < y then mem x left
          else mem x right
    
    let rec insert x = function
      | Lf -> Br(x, Lf, Lf)
      | Br(y, left, right) as t ->
          if x = y then t
          else if x < y then Br(y, insert x left, right)
          else Br(y, left, insert x right)

    let top = function
      | Lf -> 0
      | Br(x, left, right) -> x
  end