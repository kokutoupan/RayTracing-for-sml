structure Texture = struct
  type t = Type.texture_t;

  fun checker_texture (inv_scale:real) (u:real)(v:real) (pos: Vec3.t) =
  let
    val x = Real.toInt IEEEReal.TO_NEAREST ((#x pos) * inv_scale)
    val y = Real.toInt IEEEReal.TO_NEAREST ((#y pos) * inv_scale)
    val z = Real.toInt IEEEReal.TO_NEAREST ((#z pos) * inv_scale)
   
    (*
    val x = Real.toInt IEEEReal.TO_NEAREST (u * inv_scale)
    val y = Real.toInt IEEEReal.TO_NEAREST (v * inv_scale)
    *)
  in
    if( ((x + y + z) mod 2 ) = 0 ) then Color.white else Color.black
  end;
end;

