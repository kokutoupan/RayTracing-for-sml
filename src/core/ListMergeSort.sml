structure ListMergeSort =
struct

  (* リストをほぼ半分の2つのサブリストに分割するヘルパー関数 *)
  fun split list =
    let
      fun loop ([], left, right) = (rev left, rev right)
        | loop ([x], left, right) =
            (rev (x :: left), rev right)
        | loop (x :: y :: xs, left, right) =
            loop (xs, x :: left, y :: right)
    in
      loop (list, [], [])
    end

  (* 2つのソート済みリストをマージする関数 *)
  (* cmp(a,b) が true なら a が b より「小さいか等しい」と判断する *)
  fun merge cmp ([], ys) = ys
    | merge cmp (xs, []) = xs
    | merge cmp (x :: xs', y :: ys') =
        if cmp (x, y) then x :: merge cmp (xs', y :: ys')
        else y :: merge cmp (x :: xs', ys')

  (* リストをソートする主要関数 *)
  fun sort cmp lst =
    case lst of
      [] => []
    | [_] => lst (* 要素が1つのリストは既にソート済み *)
    | _ =>
        let val (left_half, right_half) = split lst
        in merge cmp (sort cmp left_half, sort cmp right_half)
        end
end
