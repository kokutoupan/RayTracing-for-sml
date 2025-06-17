structure Hittables = struct 
  type hlst_t = Type.hittable_list;
  type hbvh_t = Type.h_bvh;

  val Min_Leaf_size = 15

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

local  
  (* レイの方向ベクトルの特定の軸成分を取得するヘルパー関数 *)
  fun get_ray_dir_component (ray_dir: Vec3.t, axis: Type.split_axis) : real =
    case axis of
        Type.X_Axis => #x ray_dir
      | Type.Y_Axis => #y ray_dir
      | Type.Z_Axis => #z ray_dir
in
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


        val (first_child_to_test ,second_child_to_test) =
          case #axis_opt hbvh of
              NONE => (lhs,rhs)
            |SOME axis => 
                let 
                  val ray_dir_comp = get_ray_dir_component ((#dir ray), axis)
                in
                  if ray_dir_comp > 0.0
                  then (lhs,rhs)
                  else (rhs,lhs)
                end

        val hit_rec1 = hit_shape first_child_to_test ray (t_min, t_max) (* hit_shapeはshapeを扱う関数 *)
        val t_max_updated =
            case hit_rec1 of
                Type.NoHit => t_max
              | Type.Hit r => #t r
        val hit_rec2 = hit_shape second_child_to_test ray (t_min, t_max_updated)

      in
        case hit_rec2 of
             Type.NoHit => hit_rec1
           | _ => hit_rec2
      end
    else Type.NoHit

  and hit_shape (Type.NONE) _ _ = Type.NoHit
    | hit_shape (Type.SphereT s) ray (t_min,t_max) :Type.hit_record =
        Sphere.hit s ray (t_min,t_max)
    | hit_shape (Type.QuadT q) ray (t_min,t_max):Type.hit_record =
        Quad.hit q ray (t_min,t_max)
    | hit_shape (Type.Hittable_listT lst) ray (t_min,t_max):Type.hit_record =
        hlst_hit lst ray (t_min,t_max)
    | hit_shape (Type.H_bvhT bvh) ray (t_min,t_max):Type.hit_record =
        hbvh_hit bvh ray (t_min,t_max)
end

  fun hbvh_create (lhs:Type.shape) (rhs:Type.shape) =
    {
      lhs = lhs,
      rhs = rhs,
      bbox = AABB.createBB (Type.bbox_of_shape lhs) (Type.bbox_of_shape rhs)
    }

  fun hbvh_build (lst:Type.shape list):Type.shape =
    if length lst <= Min_Leaf_size then
      let 
        val lst = hlst_create_list lst
      in
        Type.H_bvhT {
          lhs = Type.Hittable_listT lst,
          rhs = Type.NONE,
          bbox = (#bbox lst),
          axis_opt = NONE
        }
      end 
    else
      let
        val hlst = hlst_create_list lst
        val select_axis = AABB.longest_axis (#bbox hlst)

        (*val comp = AABB.box_min_cmp select_axis*)
        val comp = AABB.box_center_cmp select_axis
        val bbox = (#bbox hlst)

        val _ = AABB.print_aabb (#bbox hlst)

        val sorted_lst = ListMergeSort.sort (fn (h1,h2) => comp
        (Type.bbox_of_shape h1,Type.bbox_of_shape h2)) lst
      in
        Type.H_bvhT {
          lhs = hbvh_build (List.drop(sorted_lst,(length lst) div 2)),
          rhs = hbvh_build (List.take(sorted_lst,(length lst) div 2)),
          bbox = bbox,
          axis_opt =SOME (if select_axis = 0 then Type.X_Axis 
                     else if select_axis = 1
                      then Type.Y_Axis
                      else Type.Z_Axis)
        }
      end

end;
