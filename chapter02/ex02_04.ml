(* 練習問題2.4 *)
(* 二分探索木において、不要なコピーをせず、比較回数が *)
(* d+1 回以下であるような insert を作る。 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

exception Same_value
let insert elt tree =
  let rec aux elt cand = function
    | Lf ->
        begin
          match cand with
          | None -> Br(elt, Lf, Lf)
          | Some x ->
              if elt = x then raise Same_value
              else Br(elt, Lf, Lf)
        end
    | Br(x, left, right) ->
        if elt < x then Br(x, aux elt cand left, right)
        else Br(x, left, aux elt (Some x) right)
  in
  try aux elt None tree with Same_value -> tree