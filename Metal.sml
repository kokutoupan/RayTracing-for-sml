structure Metal = struct
  type t = { albedo: Color.t ,fuzz:real}

  fun create (color: Color.t) fuzz =
    Type.MetalT { albedo = color ,fuzz = fuzz}

  fun scatter (Type.MetalT {albedo,fuzz}) (ray: Ray.t) (Type.Hit {p, normal, ...}) =
    let
      val reflect = Vec3.reflect (#dir ray) normal
      val reflectFuzz = Vec3.add (Vec3.unit_vector reflect) (Vec3.scale
      (Vec3.random_unit_vector Common.rng) fuzz)
       
      val scattered_ray = Ray.create p reflectFuzz
    in
      (scattered_ray, albedo)
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
