structure Isotropic =
struct
  type t = Type.isotropic_t

  fun fromTexture (texture: Texture.t) = Type.IsotropicT {tex = texture}

  fun fromColor (color: Color.t) =
    Type.IsotropicT {tex = Texture.solid_color color}

  fun scatter ({tex}: t) (_: Ray.t) (Type.Hit {p, normal, t, u, v, ...}) =
        let
          val scattered_ray = Ray.create p (Vec3.random_unit_vector ())
          val col = (tex u v p)
        in
          SOME (scattered_ray, col)
        end
    | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"

end;
