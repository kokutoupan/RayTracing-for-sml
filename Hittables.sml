structure Hittables = struct 
  type hlst_t = Type.hittable_list;
  type hbvh_t = Type.h_bvh;

  fun hlst_empty ()  = {lst=[], bbox = AABB.createV
    (Vec3.create (~0.0, ~0.0 ,~0.0)) (Vec3.create (~0.0,~0.0,~0.0))}

  fun hlst_add (hittable_list:hlst_t) (shape:Type.shape) =
    {
      lst = shape::(#lst hittable_list),
      bbox = AABB.createBB (#bbox hittable_list) (Type.bbox_of_shape shape)
    }

  fun hlst_add_list (hittable_list:hlst_t) (shapes:Type.shape list) =
  let
    val lst = foldl (fn (shape,hlst) => hlst_add hlst shape) hittable_list shapes
  in
    lst
  end

  fun hlst_create_list (shapes:Type.shape list) =
    hlst_add_list (hlst_empty ()) shapes

  
  fun hlst_hit ({lst=[],...}: hlst_t) (_: Ray.t) (_: Interval.t) :Type.hit_record = Type.NoHit
    | hlst_hit ({lst=lst,...}: hlst_t) (ray: Ray.t) ((ray_t_min, ray_t_max):Interval.t):(Type.hit_record) =
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

  and hbvh_hit (hbvh: hbvh_t) ray (t_min,t_max):Type.hit_record =
    if AABB.is_hit (#bbox hbvh) ray (t_min,t_max) then
      let 
        val lhs = #lhs hbvh
        val rhs = #rhs hbvh

        val lhs_hitrec = hit_shape lhs ray (t_min,t_max)

        val rhs_hitrec =
          case lhs_hitrec of
              Type.NoHit => hit_shape rhs ray (t_min,t_max)
            | Type.Hit r => hit_shape rhs ray (t_min,(#t r))

      in
        case rhs_hitrec of
             Type.NoHit => lhs_hitrec
           | _ => rhs_hitrec
      end
    else Type.NoHit

  and hit_shape (Type.NONE) _ _ = Type.NoHit
    | hit_shape (Type.SphereT s) ray (t_min,t_max) :Type.hit_record =
        Sphere.hit s ray (t_min,t_max)
    | hit_shape (Type.Hittable_listT lst) ray (t_min,t_max):Type.hit_record =
        hlst_hit lst ray (t_min,t_max)
    | hit_shape (Type.H_bvhT bvh) ray (t_min,t_max):Type.hit_record =
        hbvh_hit bvh ray (t_min,t_max)


  fun hbvh_create (lhs:Type.shape) (rhs:Type.shape) =
    {
      lhs = lhs,
      rhs = rhs,
      bbox = AABB.createBB (Type.bbox_of_shape lhs) (Type.bbox_of_shape rhs)
    }

  fun hbvh_build (lst:Type.shape list):Type.shape =
    if length lst <= 15 then
      let 
        val lst = hlst_create_list lst
      in
        Type.H_bvhT {
          lhs = Type.Hittable_listT lst,
          rhs = Type.NONE,
          bbox = (#bbox lst)
        }
      end 
    else
      let
        val hlst = hlst_create_list lst
        val select_axis = AABB.longest_axis (#bbox hlst)

        val comp = if select_axis = 0 then AABB.box_x_compare else if
        select_axis = 1 then AABB.box_y_compare else AABB.box_z_compare

        val bbox = (#bbox hlst)

        val sorted_lst = ListMergeSort.sort (fn (h1,h2) => comp
        (Type.bbox_of_shape h1,Type.bbox_of_shape h2)) lst
      in
        Type.H_bvhT {
          lhs = hbvh_build (List.drop(sorted_lst,(length lst) div 2)),
          rhs = hbvh_build (List.take(sorted_lst,(length lst) div 2)),
          bbox = bbox
        }
      end

end;
