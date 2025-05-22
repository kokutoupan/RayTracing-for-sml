structure Lambertian = struct
  type t = { albedo: Color.t }

  fun create (color: Color.t) =
    Type.LambertianT { albedo = color }

  fun scatter ({albedo}:t) (_: Ray.t) (Type.Hit {p, normal, ...}) =
    let
      val target = Vec3.add normal (Vec3.random_unit_vector ())
      val scattered_ray = Ray.create p target
    in
      (scattered_ray, albedo)
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
