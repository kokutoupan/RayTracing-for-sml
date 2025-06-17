structure AABB = struct
  type t = {x:Interval.t,y:Interval.t,z:Interval.t }

  val pad_delta = 0.0001;

  fun pading x =
    if Interval.size x > pad_delta then x
    else Interval.expand x pad_delta

  fun create (a:Interval.t) (b:Interval.t) (c:Interval.t) = 
    {
      x = pading a,
      y = pading b,
      z = pading c
    }

  fun createV (a:Vec3.t) (b:Vec3.t)=
    let 
      val {x=a0,y=a1,z=a2} = a
      val {x=b0,y=b1,z=b2} = b
    in
      {
        x = pading (if a0 <= b0 then (a0,b0) else (b0,a0)),
        y = pading (if a1 <= b1 then (a1,b1) else (b1,a1)),
        z = pading (if a2 <= b2 then (a2,b2) else (b2,a2))
      }
    end;

  fun createBB (a:t) (b:t) =
    {
      x = Interval.createIN (#x a) (#x b),
      y = Interval.createIN (#y a) (#y b),
      z = Interval.createIN (#z a) (#z b)
    }

  fun axis_interval (aabb:t) (n:int) =
    if n = 0 then (#x aabb) else if n = 1 then (#y aabb) else (#z aabb)

fun is_hit (aabb: {x:real*real, y:real*real, z:real*real}) (ray: Ray.t) (initial_t_min: real, initial_t_max: real) : bool =
    let
        val {x=(ax_min, ax_max), y=(ay_min, ay_max), z=(az_min, az_max)} = aabb
        
        (* Rayの起点と方向ベクトルを一度だけ取得 *)
        val ray_orig_x = #x (#orig ray)
        val ray_orig_y = #y (#orig ray)
        val ray_orig_z = #z (#orig ray)

        val ray_dir_x = #x (#dir ray)
        val ray_dir_y = #y (#dir ray)
        val ray_dir_z = #z (#dir ray)

        (* 各軸に対する交差判定を行う内部関数
         * 元の is_hit_on_axis_wrapper に相当し、最適化を含む
         * 引数:
         * axis_min_val, axis_max_val: スラブの最小値と最大値
         * dir_one_axis: レイの方向ベクトルの該当軸成分
         * orig_one_axis: レイの起点ベクトルの該当軸成分
         * current_t_min_input: 現在の t_min
         * current_t_max_input: 現在の t_max
         * 返り値: (new_t_min, new_t_max, isValid)
         *)
        fun process_one_axis (axis_min_val: real, axis_max_val: real,
                              dir_one_axis: real, orig_one_axis: real,
                              current_t_min_input: real, current_t_max_input: real)
                            : (real * real * bool) =
            if Real.==(dir_one_axis, 0.0) then (* レイが軸に平行な場合 *)
                (* レイの起点がスラブ内にあるかチェック *)
                if orig_one_axis >= axis_min_val andalso orig_one_axis <= axis_max_val then
                    (current_t_min_input, current_t_max_input, true) (* 区間は変わらずヒット *)
                else
                    (current_t_min_input, current_t_max_input, false) (* スラブ外ならミス *)
            else
                let
                    val invDir = 1.0 / dir_one_axis
                    (* スラブの2つの面との交差パラメータを計算 *)
                    val t0 = (axis_min_val - orig_one_axis) * invDir
                    val t1 = (axis_max_val - orig_one_axis) * invDir

                    (* invDir (つまり dir_one_axis) の符号に応じて、
                     * t0 と t1 のどちらがスラブへの入口(entry)でどちらが出口(exit)かを判断。
                     * これにより、Real.min/Real.max の呼び出しを1回の条件分岐に置き換えられる。
                     *)
                    val (slab_entry_t, slab_exit_t) =
                        if invDir < 0.0 then (t1, t0)
                        else (t0, t1)

                    (* 現在の [t_min, t_max] 区間とスラブの交差区間を更新 *)
                    val new_t_min = Real.max(slab_entry_t, current_t_min_input)
                    val new_t_max = Real.min(slab_exit_t, current_t_max_input)
                in
                    (new_t_min, new_t_max, new_t_min <= new_t_max) (* 交差区間が有効か *)
                end

        (* X軸の処理 *)
        val (t_min_after_x, t_max_after_x, valid_x) =
            process_one_axis (ax_min, ax_max, ray_dir_x, ray_orig_x, initial_t_min, initial_t_max)

        (* Y軸の処理: X軸が有効だった場合のみ *)
        val (t_min_after_y, t_max_after_y, valid_y) =
            if not valid_x then
                (t_min_after_x, t_max_after_x, false) (* X軸でミスなら、その時点の値を引き継ぎつつ Y もミス *)
            else
                process_one_axis (ay_min, ay_max, ray_dir_y, ray_orig_y, t_min_after_x, t_max_after_x)

        (* Z軸の処理: Y軸が有効だった場合のみ *)
        val (_, _, valid_z) = (* 最終的な t_min, t_max の値は bool 結果には不要 *)
            if not valid_y then
                (t_min_after_y, t_max_after_y, false) (* Y軸でミスなら、その時点の値を引き継ぎつつ Z もミス *)
            else
                process_one_axis (az_min, az_max, ray_dir_z, ray_orig_z, t_min_after_y, t_max_after_y)
    in
        valid_z (* valid_x, valid_y, valid_z が全て true の場合に限り true となる *)
    end

  fun longest_axis (aabb:t) =
  let
    val {x=x,y=y,z=z} = aabb
    val ax = Interval.size x
    val ay = Interval.size y
    val az = Interval.size z
  in
    if ax >= ay andalso ax >= az then 0 else if ay >= ax andalso ay >= az then 1
    else 2
  end;

  fun box_min_cmp axis ((aabb1:t),(aabb2:t)) =
  let 
    val (minX,_) = axis_interval aabb1 axis
    val (minY,_) = axis_interval aabb2 axis
  in
    minX <= minY
  end

  fun box_center_cmp axis ((aabb1:t),(aabb2:t)) =
  let 
    val a1 = axis_interval aabb1 axis
    val a2 = axis_interval aabb2 axis

    val a1c = Interval.center a1
    val a2c = Interval.center a2
  in
    a1c <= a2c
  end



  fun print_aabb (aabb:t) =
  let
    val {x=x,y=y,z=z} = aabb
    val _ = Interval.print_interval x
    val _ = Interval.print_interval y
    val _ = Interval.print_interval z
    val _ = print("\n")
  in
    ()
  end


end;
