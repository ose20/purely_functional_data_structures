(* 練習問題2.3 *)
(* 二分探索木に対する insert について、
** 既に存在する要素が渡された時はそのまま返すようにする。
*)

type 'a t = Lf | Br of 'a * 'a t * 'a t

exception Same_value
let insert elt t =
  let rec aux elt = function
    | Lf -> Br(elt, Lf, Lf)
    | Br(x, left, right) ->
        if x = elt then raise Same_value
        else if elt < x then Br(elt, aux elt left, right)
        else Br(elt, left, aux elt right)
  in
  try aux elt t with Same_value -> t