structure MyRandom =
struct
  (* Word32 モジュールを利用した乱数生成器 *)
  open Word32

  (* 内部状態（seed）＝ 初期値 *)
  val seed = ref (fromInt 123456789)

  (* 線形合同法の定数 
     multiplier = 1103515245
     increment  = 12345
     modulus    = 2^31 (ただし、mod 演算は下位 31 ビットのマスクで実現)
  *)
  val mult = fromInt 1103515245
  val incr = fromInt 12345
  val mask = fromInt 2147483647 (* 0x7FFFFFFF *)

  (* 任意の整数をシードとして設定する関数 *)
  fun srand (n: int) = seed := fromInt n

  (* 次の乱数（Word32.word 型、mod 2^31）を生成 *)
  fun next () =
    let
      val current = !seed
      (* 乗算と加算は Word32 の算術演算子で高速に実行される *)
      val newVal = mult * current + incr
      (* mod 2^31 は下位 31 ビットの抽出で実現 *)
      val newSeed = Word32.andb (newVal, mask)
    in
      seed := newSeed;
      newSeed
    end

  (* 0～1 の実数を返す関数 *)
  fun randReal () =
    Real.fromInt (Word32.toInt (next ())) / 2147483648.0
end

val randReal = MyRandom.randReal
