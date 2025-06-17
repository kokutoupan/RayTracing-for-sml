structure Lambertian = struct
  type t = Type.lambertian_t

  fun fromColor (color: Color.t)=
    Type.LambertianT { tex = Texture.solid_color color}

  fun fromTexture (texture:Texture.t)=
    Type.LambertianT { tex = texture}

  fun scatter ({tex}:t) (_: Ray.t) (Type.Hit {p, normal,u,v, ...}) =
    let
      val target = Vec3.add normal (Vec3.random_unit_vector ())
      val scattered_ray = Ray.create p target
    in
      SOME (scattered_ray, (tex u v p))
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
