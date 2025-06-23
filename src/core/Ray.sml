structure Ray =
struct
  type t = {orig: Vec3.t, dir: Vec3.t}

  fun create (orig: Vec3.t) (dir: Vec3.t) =
    {orig = orig, dir = (Vec3.unit_vector dir)}

  fun at (ray: t) (ti: real) =
    Vec3.add (#orig ray) (Vec3.scale (#dir ray) ti)

end;
