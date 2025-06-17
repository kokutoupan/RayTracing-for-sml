structure Vec3 = struct
  type t = {x: real, y: real, z: real}


  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}

  val zero = {x = 0.0, y = 0.0, z = 0.0}

  fun neg (v:t) = 
    {
      x = #x v * (~1.0),
      y = #y v * (~1.0),
      z = #z v * (~1.0)
    }

  fun add (v1:t) (v2:t) =
    { x = #x v1 + #x v2,
      y = #y v1 + #y v2,
      z = #z v1 + #z v2 }

  fun sub (v1:t) (v2:t) =
    { x = #x v1 - #x v2,
      y = #y v1 - #y v2,
      z = #z v1 - #z v2 }

  fun scale (v:t) (s:real) =
    {
      x = #x v * s,
      y = #y v * s,
      z = #z v * s
    }

  fun scaleV (v:t) (s:t) =
    {
      x = #x v * #x s,
      y = #y v * #y s,
      z = #z v * #z s
    }

  fun divide (v:t) (d:real) =
    {
      x = #x v / d,
      y = #y v / d,
      z = #z v / d
    }

  fun length (v:t) =
    Math.sqrt (#x v * #x v + #y v * #y v + #z v * #z v)

  fun length_sq (v:t) =
    (#x v * #x v + #y v * #y v + #z v * #z v)

  fun unit_vector (v:t) =
    scale v (1.0 / length v)


  fun dot (v1:t) (v2:t) =
    #x v1 * #x v2 + #y v1 * #y v2 + #z v1 * #z v2

  fun cross (v1:t) (v2:t) =
    {
      x = #y v1 * #z v2 - #z v1 * #y v2,
      y = #z v1 * #x v2 - #x v1 * #z v2,
      z = #x v1 * #y v2 - #y v1 * #x v2
    }

  fun random_vector ()=
  let 
    val x = (randReal ())*2.0 - 1.0
    val y = (randReal ())*2.0 - 1.0
    val z = (randReal ())*2.0 - 1.0
  in
    create(x,y,z)
  end;

  fun random_unit_vector ()=
  let 
    val p = random_vector ()
    val l = length_sq p
  in
    if(1.0e~160 < l andalso l <= 1.0) then divide p (Math.sqrt l) else
      random_unit_vector ()
  end;

  fun reflect (v:t) (n:t) = 
    sub v (scale n (2.0 * dot v n))

  fun refract (uv:t) (n:t) (eta_ratio:real) =
  let
    val cos_theta = Common.realMin (dot (neg uv) n) 1.0
    val r_out_perp = scale (add uv (scale n cos_theta)) eta_ratio
    val r_out_parallel = scale n (Math.sqrt (Real.abs(1.0 - length_sq
    r_out_perp)) * ~1.0)
  in
    add r_out_perp r_out_parallel
  end;


end;

structure Color = struct
  type t = Vec3.t
  
  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}

  val white = {x = 1.0, y = 1.0, z = 1.0};
  val black = {x = 0.0, y = 0.0, z = 0.0};


  val random_color = Vec3.random_vector


  fun write_color (out:TextIO.outstream) (color:t) =
  let 
    val intensity = Interval.clamp (0.0,1.0)
    val x = intensity (#x color)
    val y = intensity (#y color)
    val z = intensity (#z color)
    val r = Math.sqrt x
    val g = Math.sqrt y
    val b = Math.sqrt z


    val ir = Real.toInt IEEEReal.TO_NEAREST (intensity r * 255.999)
    val ig = Real.toInt IEEEReal.TO_NEAREST (intensity g * 255.999)
    val ib = Real.toInt IEEEReal.TO_NEAREST (intensity b * 255.999)

    val line = Int.toString ir ^ " " ^ Int.toString ig ^ " " ^ Int.toString ib ^ "\n"    
  in
    TextIO.output (out, line)
  end
end;
