structure Vec3 = struct
  type t = {x: real, y: real, z: real}

  val rng = Random.rand (21, 2107)

  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}

  val zero = {x = 0.0, y = 0.0, z = 0.0}

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

  fun random_vector rng=
  let 
    val x = Random.randReal rng
    val y = Random.randReal rng
    val z = Random.randReal rng
  in
    create(x,y,z)
  end;

  fun random_unit_vector rng=
  let 
    val p = random_vector rng
    val l = length_sq p
  in
    if(1.0e~160 < l andalso l <= 1.0) then divide p (Math.sqrt l) else
      random_unit_vector rng
  end;    

end;

structure Color = struct
  type t = Vec3.t
  
  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}
  fun write_color (out:TextIO.outstream) (color:t) =
  let 
    val r = #x color
    val g = #y color
    val b = #z color

    val intensity = Interval.clamp (0.0,1.0)

    val ir = Real.toInt IEEEReal.TO_NEAREST (intensity r * 255.999)
    val ig = Real.toInt IEEEReal.TO_NEAREST (intensity g * 255.999)
    val ib = Real.toInt IEEEReal.TO_NEAREST (intensity b * 255.999)

    val line = Int.toString ir ^ " " ^ Int.toString ig ^ " " ^ Int.toString ib ^ "\n"    
  in
    TextIO.output (out, line)
  end
end;
