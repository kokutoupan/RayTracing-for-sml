structure Sphere = struct
  open Type;
  type t = sphere;

  fun create (center: Vec3.t) (radius: real) (mat:Type.material)= SphereT {center = center, radius =
    radius, mat = mat}

  fun hit (sphere: t) (ray: Ray.t) (t_ren:Interval.t):hit_record =
  let
    val oc = Vec3.sub (#center sphere) (#orig ray);

    val dir = (#dir ray);
    val a = Vec3.length_sq dir;
    val h = Vec3.dot dir oc;
    val c =  Vec3.length_sq oc - (#radius sphere) * (#radius sphere);

    val discriminant = h*h - a*c
  in
    if (discriminant) < 1.0e~8 then NoHit else
    let
      val sqrtd = Math.sqrt discriminant;
      val root = (h - sqrtd) / a;


      fun closest_hit (root: real) =
        if( not (Interval.surrounds t_ren root))then 
          NoHit
        else 
          let 
            val p = Ray.at ray root;
            val outward_normal = Vec3.divide (Vec3.sub p (#center sphere)) (#radius sphere);
            val (f,n) = Hittable.face_normal ray outward_normal
            val mat = (#mat sphere)
          in
            Hit { p = p, normal = n, t = root, front_face = f, mat=mat }
          end

    in
        case (closest_hit root ) of
             NoHit => closest_hit ((h + sqrtd)/a)
           | Hit hit => Hit hit
    end
  end;
end;
