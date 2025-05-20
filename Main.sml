use "Common.sml";
use "Interval.sml";
use "Vec3.sml";
use "Type.sml";
use "Ray.sml";
use "Hittable.sml";
use "Lambertian.sml";
use "Metal.sml";
use "Sphere.sml";
use "Hittable_list.sml";
use "Camera.sml";

structure Main = struct 

  (*objects*)
  val mat_ground = Lambertian.create (Vec3.create(0.8, 0.8, 0.0));
  val mat_center = Lambertian.create (Vec3.create(0.1, 0.2, 0.5));
  val mat_left = Metal.create (Vec3.create(0.8, 0.6, 0.2));
  val mat_right = Metal.create (Vec3.create(0.8, 0.8, 0.8));

  val sphere_g = Sphere.create (Vec3.create(0.0, ~100.5, ~1.0))  100.0 mat_ground;
  val sphere_c = Sphere.create (Vec3.create(~0.0, ~0.0, ~1.2))  0.5 mat_center;
  val sphere_l = Sphere.create (Vec3.create(~1.0, ~0.0, ~1.0))  0.5 mat_left;
  val sphere_r = Sphere.create (Vec3.create(1.0, ~0.0, ~1.0))  0.5 mat_right;
  
  val wd_obj = Type.Hittable_listT [sphere_g, sphere_c, sphere_l, sphere_r];

  val output = "output.ppm";

  val render = Camera.render wd_obj
    
end;
