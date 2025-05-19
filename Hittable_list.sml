structure Hittable_list = struct
  (*open Type;*)
  type t = Type.shape list;

  
  fun hit ([]: t) (_: Ray.t) (_: Interval.t) :Type.hit_record = Type.NoHit
    | hit ((h::hittable_list): t) (ray: Ray.t) ((ray_t_min, ray_t_max):Interval.t):(Type.hit_record) =
      let
        val hitrec = hit_shape h ray (ray_t_min,ray_t_max)
        val next_hitrec =
          case hitrec of
            Type.NoHit => hit hittable_list ray (ray_t_min,ray_t_max)
          | Type.Hit r => hit hittable_list ray (ray_t_min,(#t r))
      in
        case next_hitrec of
          Type.NoHit => hitrec
        | _     => next_hitrec
      end

  and hit_shape (Type.SphereT s) ray (t_min,t_max) :Type.hit_record =
        Sphere.hit s ray (t_min,t_max)
    | hit_shape (Type.Hittable_listT lst) ray (t_min,t_max):Type.hit_record =
        hit lst ray (t_min,t_max)

end; 
