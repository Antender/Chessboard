module Array2D exposing (get, set)

import Array exposing (Array)
import Maybe

get point array =
  Array.get point.y array |> Maybe.andThen (Array.get point.x)

set : {x : Int, y : Int} -> a -> Array (Array a) -> Array (Array a)
set point value array =
  case Array.get point.y array of
    Nothing -> array
    Just row ->
      Array.set point.y (Array.set point.x value row) array

