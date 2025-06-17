structure Quad = struct
  open Type;

  type t = Type.quad_t;

  local
    fun set_aabb (q:Vec3.t) (u:Vec3.t) (v:Vec3.t) = 
      AABB.createV q (Vec3.add (Vec3.add q u) v)


    fun is_interior (a:real) (b:real) =
    let 
      val unit_interval = (0.0,1.0)
    in
      if Interval.contains unit_interval a andalso Interval.contains
      unit_interval b then true else false
    end


  in

    fun create (q:Vec3.t) (u:Vec3.t) (v:Vec3.t) (mat:material) =
    let 
      val normal = Vec3.cross u v
      val w = Vec3.divide normal (Vec3.length_sq normal)
      val n = Vec3.unit_vector normal
    in
      QuadT {q=q,u=u,v=v,w=w, mat=mat, bbox=set_aabb q u v,
      normal=n,d=Vec3.dot q n}
    end


    fun hit (quad: t) (ray: Ray.t) (t_ren:Interval.t):hit_record =
    let 
      val demon = Vec3.dot (#normal quad) (#dir ray)
    in
      if Real.abs(demon) < 1e~8 then
        NoHit
      else
        let
          val t = ((#d quad) - (Vec3.dot (#normal quad) (#orig ray))) / demon
        in
          if not (Interval.contains t_ren t) then
            NoHit
          else
            let
              val intersection = Ray.at ray t;
              val planar_hitpt_vector  = Vec3.sub intersection (#q quad);
              val w = (#w quad);

              val alpha = Vec3.dot w (Vec3.cross planar_hitpt_vector (#v quad));
              val beta = Vec3.dot w (Vec3.cross (#u quad) planar_hitpt_vector)
            in
              if (not (is_interior alpha beta)) then NoHit else
                let 
                  val (f,n) = Hittable.face_normal ray (#normal quad)
                in
                  Hit {p=intersection, normal=n, t=t,u=alpha,v=beta,
                  front_face=f, mat=(#mat quad)}
                end
            end
        end
    end


  end;
end;
