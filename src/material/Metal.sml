structure Metal =
struct
  type t = Type.metal_t


  fun scatter ({albedo, fuzz, ...}: t) (ray: Ray.t) (Type.Hit {p, normal, ...}) =
        let
          val reflect = Vec3.reflect (#dir ray) normal
          val reflectFuzz = Vec3.add (Vec3.unit_vector reflect)
            (Vec3.scale (Vec3.random_unit_vector ()) fuzz)

          val scattered_ray = Ray.create p reflectFuzz
        in
          SOME (scattered_ray, albedo)
        end

    | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"

  fun create (color: Color.t) fuzz =
  let
    val matData = {albedo = color, fuzz = fuzz}
  in
    Type.Material {scatter = scatter matData , emit = fn _ => fn _ =>
    Color.black }
  end
end
