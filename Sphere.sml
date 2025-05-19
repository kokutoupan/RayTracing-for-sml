structure Sphere = struct
  open Type;
  type t = sphere;

  fun create (center: Vec3.t) (radius: real) = {center = center, radius =
    radius}

  fun hit (sphere: t) (ray: Ray.t) (ray_t_min: real) (ray_t_max:
    real):hit_record =
  let
    val oc = Vec3.sub (#center sphere) (#orig ray);

    val a = Vec3.length_sq (#dir ray);
    val h = Vec3.dot (#dir ray) oc;
    val c =  Vec3.length_sq oc - (#radius sphere) * (#radius sphere);

    val discriminant = h*h - a*c
  in
    if (discriminant) < 0.0 then NoHit else
    let
      val sqrtd = Math.sqrt discriminant;
      val root = (h - sqrtd) / a;

      fun closest_hit (root: real) =
        if(root < ray_t_min orelse root > ray_t_max )then 
          NoHit
        else 
          Hit { p = Ray.at ray root, normal = Vec3.unit_vector (Vec3.sub
          (#center sphere) (Ray.at ray root)), t = root}

    in
        case (closest_hit root ) of
             NoHit => closest_hit ((h + sqrtd)/a)
           | Hit hit => Hit hit
    end
  end;
end;
