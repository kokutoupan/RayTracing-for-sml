structure Metal = struct
  type t = { albedo: Color.t }

  fun create (color: Color.t) =
    Type.MetalT { albedo = color }

  fun scatter (Type.MetalT {albedo}) (ray: Ray.t) (Type.Hit {p, normal, ...}) =
    let
      val reflect = Vec3.reflect (#dir ray) normal
      val scattered_ray = Ray.create p reflect
    in
      (scattered_ray, albedo)
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
