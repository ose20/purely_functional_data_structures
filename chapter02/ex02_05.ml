(* 練習問題2.5 *)
(* 単一のオブジェクトの中でもノードを共有する方法を使う *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

(* a *)
(* すべてのノードに x が格納された深さ d の完全二分木 complete(x, d) *)
(* 時間計算量は O(d) *)
let complete x d =
  let rec aux x d =
    if d = 0 then Lf
    else
      let subt = aux x (d - 1) in
      Br(x, subt, subt)
  in aux x d

(* 同じ問題を末尾再帰で書く *)
let complete' x d =
  let rec aux depth result =
    if depth > d then result
    else
      if depth = 0 then aux (depth + 1) Lf 
      else aux (depth + 1) (Br(x, result, result)) 
  in aux 0 Lf

let time f =
  let start = Sys.time () in
  let _ = f () in
  let end_ = Sys.time () in
  (end_ -. start)

(* complete の方が実行時間は若干短い（定数倍）だけど
** 末尾最近じゃないので d = 10^6 で Stack overflow が起きる
*)