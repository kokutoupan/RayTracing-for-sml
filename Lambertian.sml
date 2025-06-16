structure Lambertian = struct
  type t = Type.lambertian_t

  fun create (color: Color.t) (tex: Type.texture_t option) =
    Type.LambertianT { albedo = color , tex = tex}

  fun scatter ({albedo, tex}:t) (_: Ray.t) (Type.Hit {p, normal,u,v, ...}) =
    let
      val target = Vec3.add normal (Vec3.random_unit_vector ())
      val scattered_ray = Ray.create p target
      val texture = tex
      val color = 
        case texture of
             NONE => albedo
           | SOME tex => Vec3.scaleV (tex u v p)  albedo
    in
      (scattered_ray, color)
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
