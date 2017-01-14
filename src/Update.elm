module Update exposing (init, update)

import Array exposing (Array)
import Array2D
import List
import Maybe exposing (..)

import Types exposing (..)
import Point exposing (Point)

init : {
  selection : Maybe Point,
  destinations : List Point,
  board : Array (Array (Maybe (Color,Piece)))
}
init =
  {
    selection = Nothing,
    destinations = [],
    board = Array.fromList [
      Array.fromList [
        Just (Black, Rook), Just (Black, Knight), Just (Black, Bishop), Just (Black, Queen),
        Just (Black, King), Just (Black, Bishop), Just (Black, Knight), Just (Black, Rook)],
      Array.repeat 8 (Just (Black, Pawn)),
      Array.repeat 8 Nothing,
      Array.repeat 8 Nothing,
      Array.repeat 8 Nothing,
      Array.repeat 8 Nothing,
      Array.repeat 8 (Just (White, Pawn)),
      Array.fromList [
        Just (White, Rook), Just (White, Knight), Just (White, Bishop), Just (White, Queen),
        Just (White, King), Just (White, Bishop), Just (White, Knight), Just (White, Rook)]
    ]
  }

update msg model =
  case msg of
    Clicked point ->
      case model.selection of
        Nothing ->
          case (Array2D.get point model.board |> Maybe.withDefault Nothing) of
            Nothing -> model
            _ -> { model | selection = Just point, destinations = findDestinations point model.board }
        Just selection ->
          if List.member point model.destinations then
            { model | selection = Nothing, destinations = [],
              board =
                Array2D.set
                  point
                  (Array2D.get selection model.board |> Maybe.withDefault Nothing)
                  (Array2D.set selection Nothing model.board)}
          else
            {model | selection = Nothing, destinations = []}

opposite color =
  case color of
    White -> Black
    Black -> White

canMoveToCell color board onEmpty onOpposite point =
  let cell =
    Array2D.get point board
  in
    case cell of
      Just cellContents ->
        case cellContents of
          Nothing -> onEmpty
          Just (piececolor, _) ->
            onOpposite (piececolor == opposite color)
      Nothing -> False

traverse board color path position directionX directionY =
  let cell =
    Array2D.get position board
  in
    case cell of
      Just cellContents ->
        case cellContents of
          Nothing ->
            traverse board color (position :: path) {
              position | x = position.x + directionX, y = position.y + directionY
            } directionX directionY
          Just (piececolor, _) ->
            if piececolor == opposite color then
              position :: path
            else
              path
      Nothing -> path

findDestinations selection board =
  let
    cellContents =
      Array2D.get selection board |> withDefault Nothing
    selectionColor =
      case cellContents of
        Nothing -> White
        Just (color, _) -> color
    filterPassable cellList onEmpty onOpposite =
      List.filter (canMoveToCell selectionColor board onEmpty onOpposite) cellList
    canMoveRelative point directions onEmpty onOpposite =
      filterPassable (List.map (\el ->
        let (x,y) = el in
          Point.incr x y point)
            directions) onEmpty onOpposite
    traverseBoard points =
      List.concat (
        List.map (\point ->
          let (x,y) = point in
            traverse board selectionColor [] (Point.incr x y selection) x y)
          points)
    traverseBishop =
      traverseBoard [(-1,-1),(-1,1),(1,-1),(1,1)]
    traverseRook =
      traverseBoard [(-1,0),(1,0),(0,-1),(0,1)]
  in
    case cellContents of
      Nothing -> []
      Just (color, Pawn) ->
        case color of
          White ->
            List.concat [canMoveRelative selection [(-1,-1),(1,-1)] False identity, canMoveRelative selection [(0,-1)] True (always False)]
          Black ->
            List.concat [canMoveRelative selection [(-1,1),(1,1)] False identity, canMoveRelative selection [(0,1)] True (always False)]
      Just (color, King) ->
        canMoveRelative selection [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)] False identity
      Just (color, Knight) ->
        canMoveRelative selection [(-2,1),(-2,-1),(2,1),(2,-1),(1,-2),(-1,-2),(1,2),(-1,2)] False identity
      Just (color, Bishop) ->
        traverseBishop
      Just (color, Rook) ->
        traverseRook
      Just (color, Queen) ->
        List.concat [traverseBishop, traverseRook]
