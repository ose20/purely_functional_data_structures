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

(* complete の方が実行時間は若干短いだけど
** 末尾再帰じゃないので d = 10^6 で Stack overflow が起きる
*)

(* b *)
(* a を拡張して、任意のサイズの平衡木を作るようにする
** 与えられたノードに対し、2 つの部分木のサイズの差は高々 1 とする 
** O(log n) でできるようにしたい
*)
let create_tree elt size =
  (* サイズ m とサイズ m+1 の木のペアを作る *)
  (* 右の木のサイズを優先して大きくする *)
  let rec create2 = function
    | 0 -> (Lf, Br(elt, Lf, Lf))
    | m ->
        begin
          match m mod 2 with
          | 0 -> let (x, y) = create2 (m / 2 - 1)
              in (Br(elt, y, x), Br(elt, y, y))
          | _ -> let (x, y) = create2 ((m - 1) / 2)
              in (Br(elt, y, x), Br(elt, y, y))
        end
  in
  match size with
  | 0 -> Lf
  | _ ->
      begin
        match size mod 2 with
        | 0 ->
            let k = size / 2 in
            let (x, y) = create2 (k - 1) in
            Br(elt, y, x)
        | _ ->
            let k = (size - 1) / 2 in
            let (x, _) = create2 k in
            Br(elt, x, x)
      end