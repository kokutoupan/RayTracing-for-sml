structure Isotropic =
struct
  type t = Type.isotropic_t

  fun scatter ({tex}: t) (_: Ray.t) (Type.Hit {p, normal, t, u, v, ...}) =
        let
          val scattered_ray = Ray.create p (Vec3.random_unit_vector ())
          val col = (tex u v p)
        in
          SOME (scattered_ray, col)
        end
    | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"


  fun fromTexture (texture: Texture.t) =
    let
      val matData = {tex = texture}
    in
      Type.Material
        {scatter = scatter matData, emit = fn _ => fn _ => Color.black}
    end

  fun fromColor (color: Color.t) =
    fromTexture (Texture.solid_color color)


end;
