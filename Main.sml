structure Main = struct 
  val image_width = 256;
  val image_height = 256;

  fun render filename =
    let
      val out = TextIO.openOut filename

      (* ヘッダーの出力 *)
      val _ = TextIO.output (out, "P3\n")
      val _ = TextIO.output (out, Int.toString image_width ^ " " ^ Int.toString image_height ^ "\n255\n")

      (* メインループ：ピクセル毎にRGB値を計算して出力 *)
      val _ =
        List.app (fn j =>
          let 
            val _ = print("\rScanlines remaining:" ^ Int.toString (image_height - j) )
          in
            List.app (fn i =>
              let
                val r = Real.fromInt i / Real.fromInt (image_width - 1)
                val g = Real.fromInt j / Real.fromInt (image_height - 1)
                val b = 0.0

                val _ = Color.write_color out (Color.create(r, g, b))
              in
                ()
              end
            ) (List.tabulate (image_width, fn x => x))
          end
        ) (List.tabulate (image_height, fn y => y))

      val _ = TextIO.closeOut out
    in
      ()
    end
    
end;
