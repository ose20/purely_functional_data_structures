(* 練習問題2.2 *)
(* d を二分探索木の深さとして、比較を高々
** d+1 回しかしない mem 関数
** 探索中 < が最後に false になったものが唯一の解の候補
** なので、それを保持して最後にチェックする。
*)

type 'a t = Lf | Br of 'a * 'a t * 'a t

let mem x tree =
  let rec iter x cand = function
    | Lf ->
        begin
          match cand with
          | None -> false
          | Some c -> x = c
        end
    | Br(y, left, right) ->
        if y < x then iter x cand left
        else iter x (Some y) right
  in iter x None tree

