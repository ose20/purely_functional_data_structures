(* 練習問題3.1 *)

(* 証明で使う型 *)
type 'a t = Lf | Br of 'a * 'a t * 'a t

(** 左偏ヒープを t、size(t) = n とする。命題を定式化すると
  * rank(t) <= log (n + 1)
  * であり、さらにこれを同値変形して
  * 2 ^ rank(t) - 1 <= n  ・・・(あ)
  * を得る。これを t の構造に関する帰納法により証明する。
  * ・t = Lf の場合
  *   n = 0、rank(t) = 0 なので（あ）は成り立つ。
  * ・t = Br(x, left, right) の場合
  *   n = 1 + size(left) + size(right)
  *      （帰納法の仮定より）
  *     >= 1 + (2 ^ rank(left)) - 1 + (2 ^ rank(right)) - 1
  *     = (2 ^ rank(left)) + (2 ^ rank(right)) -1
  *       （左偏ヒープの定義より rank(left) >= rank(right) なので）
  *     >= 2 * (2 ^ (rank(right))) - 1
  *       （左偏ヒープの定義より、rank(t) = rank(right) + 1 なので）
  *     = 2 ^ rank(t) - 1
  * 以上により（あ）が証明された。
  *)