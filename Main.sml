use "Interval.sml";
use "Vec3.sml";
use "Type.sml";
use "Ray.sml";
use "Hittable.sml";
use "Sphere.sml";
use "Hittable_list.sml";
use "Camera.sml";

structure Main = struct 

  (*objects*)
  val sq = Sphere.create (Vec3.create(0.0, 0.0, ~2.0))  0.5;
  val sq2 = Sphere.create (Vec3.create(0.0, ~100.5, ~1.0))  100.0;
  val wd_obj = Type.Hittable_listT [sq,sq2];

  val output = "output.ppm";

  val render = Camera.render wd_obj
    
end;
