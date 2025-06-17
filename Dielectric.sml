structure Dielectric = struct
  type t = Type.dielectric_t

  fun create (ref_idx: real) =
    Type.DielectricT {ref_idx = ref_idx}

  fun scatter ({ref_idx,...}:t) (ray: Ray.t) 
    (Type.Hit {p,normal,t,front_face,...}) =
    let
      fun reflectance cos_theta ref_idx =
      let
        val r0 = ((1.0 - ref_idx) / (1.0 + ref_idx))
        val r1 = r0 * r0
      in
        r1 + (1.0 - r1) * Math.pow ((1.0 - cos_theta), 5.0)
      end


      val col = Color.create(1.0, 1.0, 1.0)
      val rif_ratio = if front_face then 1.0 / ref_idx else ref_idx
      val dir = #dir ray

      val cos_theta = Common.realMin (Vec3.dot (Vec3.neg dir) normal) 1.0
      val sin_theta = Math.sqrt (1.0 - cos_theta * cos_theta)

      val cannot_refract = rif_ratio * sin_theta > 1.0

      val r_dir = if cannot_refract orelse 
      reflectance cos_theta ref_idx > randReal ()
                      then
                        Vec3.reflect dir normal
                      else Vec3.refract dir normal rif_ratio

      val scattered_ray = Ray.create p r_dir

    in
      SOME (scattered_ray, col)
    end

  | scatter _ _ Type.NoHit = raise Fail "Cannot scatter from NoHit"
end
