(* 練習問題2.1 *)
(* リスト xs を受け取り、xs のすべての設備じを長さの降順
** に並べたリストを返す関数
*)
let rec suffixes = function
  | [] -> [[]]
  | x::rest as xs -> xs :: suffixes rest

let check1 = suffixes [1;2;3;4]
let check2 = suffixes []
let check3 = suffixes [1]