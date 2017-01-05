module Point exposing (..)

type alias Point = { x : Int, y : Int}

add a b =
  {x = a.x + b.x, y = a.y + b.y}

sub a b =
  {x = a.x - b.x, y = a.y - b.y}

incr ix iy p =
  {x = p.x + ix, y = p.y + iy}
