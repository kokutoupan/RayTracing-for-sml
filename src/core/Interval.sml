structure Interval =
struct
  type t = real * real;

  val universe = (~ Real.maxFinite, Real.maxFinite)

  fun create (min: real) (max: real) = (min, max);

  fun createIN ((a0, a1): t) ((b0, b1): t) =
    (Common.min a0 b0, Common.max a1 b1);

  fun size (i: t) =
    let val (min, max) = i
    in max - min
    end;

  fun contains (i: t) (x: real) =
    let val (min, max) = i
    in x >= min andalso x <= max
    end;

  fun surrounds (i: t) (x: real) =
    let val (min, max) = i
    in min < x andalso x < max
    end;

  fun clamp ((min, max): t) (x: real) =
    if x < min then min else if x > max then max else x

  fun center ((min, max): t) =
    (max - min) / 2.0 + min

  fun expand (min, max) delta =
    (min - delta / 2.0, max - delta / 2.0)

  fun move ((min, max): t) (delta: real) = (min + delta, max + delta)


  fun print_interval ((a0, a1): t) =
    let
      val _ = print
        (Int.toString (Real.toInt IEEEReal.TO_NEAREST (a0 * 100.0)) ^ ","
         ^ Int.toString (Real.toInt IEEEReal.TO_NEAREST (a1 * 100.0)) ^ " | ")
    in
      ()
    end

end;
