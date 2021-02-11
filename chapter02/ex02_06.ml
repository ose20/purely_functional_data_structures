(* 練習問題2.6 *)
(* FINITEMAP をシグネチャにもつ、有限マップを作成せよ *)
module type ORDERD =
  sig
    type t
    (* compare i j
    ** i > j なら正、i = j なら 0、i < j なら負の値
    *)
    val compare : t -> t -> int
  end

module type FINITEMAP =
  sig
    type key
    type 'a t

    exception Not_Found
    
    val empty : 'a t
    val bind : key -> 'a -> 'a t -> 'a t
    val lookup : key -> 'a t -> 'a (* 見つからない時は NotFound 例外を投げる *)
  end

module UnbalancedMap (Order : ORDERD)
  : FINITEMAP with type key = Order.t
=
  struct
    type key = Order.t
    type 'a tree = Lf | Br of key * 'a * 'a tree * 'a tree
    type 'a t = 'a tree

    exception Not_Found

    let empty = Lf
    
    let rec bind key elt = function
      | Lf -> Br(key, elt, Lf, Lf)
      | Br(key', elt', left, right) ->
          match Order.compare key key' with
          | 0 -> Br(key', elt, left, right)
          | r when r < 0 -> Br(key', elt', bind key elt left, right)
          | _ -> Br(key, elt', left, bind key elt right)

    let rec lookup key = function
      | Lf -> raise Not_Found
      | Br(key', elt', left, right) ->
          match Order.compare key key' with
          | 0 -> elt'
          | r when r < 0 -> lookup key left
          | _ -> lookup key right
  end


module Mymap = UnbalancedMap (
  struct
    type t = int
    let compare i j = i - j
  end
)