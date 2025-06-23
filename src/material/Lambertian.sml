structure Lambertian =
struct
  type t = Type.lambertian_t

  fun scatter ({tex}: t) (_: Ray.t) (Type.Hit {p, normal, u, v, ...}) =
        let
          val target = Vec3.add normal (Vec3.random_unit_vector ())
          val scattered_ray = Ray.create p target
        in
          SOME (scattered_ray, (tex u v p))
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


end
