structure AABB = struct
  type t = {x:Interval.t,y:Interval.t,z:Interval.t }

  fun create (a:Interval.t) (b:Interval.t) (c:Interval.t) = 
    {
      x = a,
      y = b,
      z = c
    }

  fun createV (a:Vec3.t) (b:Vec3.t)=
    let 
      val {x=a0,y=a1,z=a2} = a
      val {x=b0,y=b1,z=b2} = b
    in
      {
        x = if a0 <= b0 then (a0,b0) else (b0,a0),
        y = if a1 <= b1 then (a1,b1) else (b1,a1),
        z = if a2 <= b2 then (a2,b2) else (b2,a2)
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
    val {x=x_interval, y=y_interval, z=z_interval} = aabb (* x_interval は (ax_min, ax_max) *)
    
    val ray_orig_vec = #orig ray
    val ray_dir_vec  = #dir ray

    (* is_hit_on_axis が (axis_interval, dir_comp, orig_comp, current_tmin, current_tmax) を受け取り、
       (new_tmin, new_tmax, isValid) を返すと仮定。
       ここでの is_hit_on_axis は、あなたが修正したであろう関数の名前と定義に合わせてください。
       以下の例では、前回の回答の process_axis のような関数を is_hit_on_axis と呼んでいます。
    *)
    fun is_hit_on_axis_wrapper ( (axis_min_val, axis_max_val): Interval.t, 
                                 dir_one_axis: real, 
                                 orig_one_axis: real,
                                 current_tm_outer: real, 
                                 current_tx_outer: real) : (real * real * bool) =
        if Real.==(dir_one_axis, 0.0) then (* レイが軸に平行な場合 *)
            if orig_one_axis >= axis_min_val andalso orig_one_axis <= axis_max_val then
                (current_tm_outer, current_tx_outer, true) (* レイの起点がスラブ内なら区間は変わらずヒット *)
            else
                (current_tm_outer, current_tx_outer, false) (* スラブ外ならミス *)
        else
            let
                val t0 = (axis_min_val - orig_one_axis) / dir_one_axis
                val t1 = (axis_max_val - orig_one_axis) / dir_one_axis

                val entry_t = Real.min(t0,t1)
                val exit_t  = Real.max(t0,t1)

                val new_tm = Real.max(entry_t, current_tm_outer) (* 正しい更新ロジック *)
                val new_tx = Real.min(exit_t, current_tx_outer) (* 正しい更新ロジック *)
            in
                (new_tm, new_tx, new_tm <= new_tx)
            end;

    (* X軸の処理 *)
    val (tm_after_x, tx_after_x, valid_x) = 
        is_hit_on_axis_wrapper (x_interval, #x ray_dir_vec,#x ray_orig_vec, initial_t_min, initial_t_max);

    (* Y軸の処理: X軸が有効だった場合のみ *)
    val (tm_after_y, tx_after_y, valid_y) = 
        if not valid_x then 
            (tm_after_x, tx_after_x, false) (* X軸でミスなら、その時点の値を引き継ぎつつ valid_y は false *)
        else 
            is_hit_on_axis_wrapper (y_interval, #y ray_dir_vec, #y ray_orig_vec, tm_after_x, tx_after_x);

    (* Z軸の処理: Y軸が有効だった場合のみ *)
    val (_, _, valid_z) = 
        if not valid_y then 
            (tm_after_y, tx_after_y, false) (* Y軸でミスなら、その時点の値を引き継ぎつつ valid_z は false *)
        else 
            is_hit_on_axis_wrapper (z_interval, #z ray_dir_vec, #z ray_orig_vec, tm_after_y, tx_after_y);
  in
    valid_z (* valid_x, valid_y, valid_z が全て true の場合に限り true となる *)
  end;

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

  fun box_x_compare ((aabb1:t),(aabb2:t)) =
  let
    val a1x = axis_interval aabb1 0
    val a2x = axis_interval aabb2 0
    val ax = Interval.size a1x
    val ay = Interval.size a2x
  in
    if ax < ay then false else true
  end

  fun box_y_compare ((aabb1:t),(aabb2:t)) =
  let
    val a1y = axis_interval aabb1 1
    val a2y = axis_interval aabb2 1
    val ax = Interval.size a1y
    val ay = Interval.size a2y
  in
    if ax < ay then false else true
  end

  fun box_z_compare ((aabb1:t),(aabb2:t)) =
  let
    val a1z = axis_interval aabb1 2
    val a2z = axis_interval aabb2 2
    val ax = Interval.size a1z
    val ay = Interval.size a2z
  in
    if ax < ay then false else true
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
