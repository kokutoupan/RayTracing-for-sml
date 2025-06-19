structure Hittable = struct
  fun face_normal(ray : Ray.t) (outward_normal : Vec3.t) =
    let 
      val front = Vec3.dot (#dir ray) outward_normal < 0.0
      val normal = if front then outward_normal else Vec3.scale outward_normal ~1.0
    in
      (front,normal)
    end;
end;
