(*pp $DERIVING *)

open Defs

let nullsum =
  begin
    assert (Bounded.min_bound<nullsum> = N0);
    assert (Bounded.max_bound<nullsum> = N3);
  end

let poly0 =
  begin
    assert (Bounded.min_bound<poly0> = `T0);
    assert (Bounded.max_bound<poly0> = `T3);
  end

let tup4 =
  begin
    assert (Bounded.min_bound<tup4> = (min_int, min_int, false, ()));
    assert (Bounded.max_bound<tup4> = (max_int, max_int, true, ()));
  end

let t =
  begin
    assert (Bounded.min_bound<t> = min_int);
    assert (Bounded.max_bound<t> = max_int);
end
