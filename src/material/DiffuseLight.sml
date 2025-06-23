structure DiffuseLight =
struct
  type t = Type.diffuseLight_t

  fun scatter (_: t) (_: Ray.t) (_) = NONE

  fun emit (light: t) (ray: Ray.t) (Type.Hit {p, u, v, ...}) : Color.t =
        ((#tex light) u v p)
    | emit _ _ Type.NoHit = raise Fail "emit not hit"


  fun fromTexture (texture: Texture.t) = 
  let 
    val matData = {tex = texture}
  in
    Type.Material {scatter = scatter matData , emit = emit matData }
  end

  fun fromColor (albedo: Color.t) =
    fromTexture (Texture.solid_color albedo)

end;
