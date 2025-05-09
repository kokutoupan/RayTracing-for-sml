structure Vec3 = struct
  type t = {x: real, y: real, z: real}

  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}

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

  fun divide (v:t) (d:real) =
    {
      x = #x v / d,
      y = #y v / d,
      z = #z v / d
    }

  fun length (v:t) =
    Math.sqrt (#x v * #x v + #y v * #y v + #z v * #z v)

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
end;

structure Color = struct
  type t = Vec3.t
  
  fun create (x:real, y:real, z:real) = {x = x, y = y, z = z}
  fun write_color (out:TextIO.outstream) (color:t) =
  let 
    val r = #x color
    val g = #y color
    val b = #z color


    val ir = Real.toInt IEEEReal.TO_NEAREST (r * 255.999)
    val ig = Real.toInt IEEEReal.TO_NEAREST (g * 255.999)
    val ib = Real.toInt IEEEReal.TO_NEAREST (b * 255.999)

    val line = Int.toString ir ^ " " ^ Int.toString ig ^ " " ^ Int.toString ib ^ "\n"    
  in
    TextIO.output (out, line)
  end
end;
