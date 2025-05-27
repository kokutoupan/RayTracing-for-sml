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

  fun is_hit (aabb:t) (ray:Ray.t) ((t_min,t_max):Interval.t) =
    let 
      val {x=x,y=y,z=z} = aabb
      val ray_orig = #orig ray
      val ray_dir = #dir ray

      fun is_hit_on_axis ((min,max):Interval.t, dir_one_axis,orig_one_axis) =
        let
          val t0 = (min - orig_one_axis) / dir_one_axis
          val t1 = (max - orig_one_axis) / dir_one_axis

          val (t_min,t_max) = 
            if t0 < t1 
            then (Common.max t0 t_max, Common.min t1 t_min) 
            else (Common.max t1 t_max, Common.min t0 t_min)
        in
          if t_min > t_max then false else true
        end
    in
      if is_hit_on_axis (x, #x ray_dir, #x ray_orig) andalso is_hit_on_axis (y, #y
      ray_dir, #y ray_orig) andalso is_hit_on_axis (z, #z ray_dir, #z ray_orig) then
        true else false
    end;
end;
