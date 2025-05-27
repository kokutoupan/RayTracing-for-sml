structure Hittable_list = struct
  (*open Type;*)
  type t = Type.hittable_list;

  fun hlst_empty ()  = {lst=[], bbox = AABB.createV
    (Vec3.create (~0.0, ~0.0 ,~0.0)) (Vec3.create (~0.0,~0.0,~0.0))}

  fun hlst_add (hittable_list:t) (shape:Type.shape) =
    {
      lst = shape::(#lst hittable_list),
      bbox = AABB.createBB (#bbox hittable_list) (Type.bbox_of_shape shape)
    }

  fun hlst_add_list (hittable_list:t) (shapes:Type.shape list) =
  let
    val lst = foldl (fn (shape,hlst) => hlst_add hlst shape) hittable_list shapes
  in
    lst
  end

  fun hlst_create_list (shapes:Type.shape list) =
    hlst_add_list (hlst_empty ()) shapes

  
  fun hit ({lst=[],...}: t) (_: Ray.t) (_: Interval.t) :Type.hit_record = Type.NoHit
    | hit ({lst=lst,...}: t) (ray: Ray.t) ((ray_t_min, ray_t_max):Interval.t):(Type.hit_record) =
      let

        fun hit_list ([]:Type.shape list) (_: Ray.t) (_: Interval.t):Type.hit_record = Type.NoHit
          | hit_list ((h::hittable_list):Type.shape list) (ray: Ray.t) ((ray_t_min, ray_t_max):Interval.t):Type.hit_record =
          let
            val hitrec = hit_shape h ray (ray_t_min,ray_t_max)
            val next_hitrec =
              case hitrec of
                Type.NoHit => hit_list hittable_list ray (ray_t_min,ray_t_max)
              | Type.Hit r => hit_list hittable_list ray (ray_t_min,(#t r))
          in
            case next_hitrec of
                 Type.NoHit => hitrec
               | _ => next_hitrec
          end
      in
        hit_list lst ray (ray_t_min, ray_t_max)
      end

  and hit_shape (Type.SphereT s) ray (t_min,t_max) :Type.hit_record =
        Sphere.hit s ray (t_min,t_max)
    | hit_shape (Type.Hittable_listT lst) ray (t_min,t_max):Type.hit_record =
        hit lst ray (t_min,t_max)

end; 
