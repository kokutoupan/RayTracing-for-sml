structure DiffuseLight =
struct
  type t = Type.diffuseLight_t

  fun fromColor (albedo: Color.t) =
    Type.DiffuseLightT {tex = Texture.solid_color albedo}

  fun fromTexture (texture: Texture.t) = Type.DiffuseLightT {tex = texture}

  fun scatter (_: t) (_: Ray.t) (_) = NONE

  fun emit (light: t) (ray: Ray.t) (Type.Hit {p, u, v, ...}) : Color.t =
        ((#tex light) u v p)
    | emit _ _ Type.NoHit = raise Fail "emit not hit"

end;
