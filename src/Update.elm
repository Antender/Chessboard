module Update exposing (init, update)

import Array
import List
import Maybe exposing (..)

import Types exposing (..)

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
        Nothing -> {model | selection = Just point, destinations = findDestinations point model.board}
        _ -> {model | selection = Nothing, destinations = []}

opposite color =
  case color of
    White -> Black
    Black -> White

getCell board point =
  Array.get point.y board |> andThen (Array.get point.x)

canMoveToCell color board point =
  let cell =
    getCell board point
  in
    case cell of
      Just cellContents ->
        case cellContents of
          Nothing -> True
          Just (piececolor, _) ->
            if piececolor == opposite color then
              True
            else
              False
      Nothing -> False

traverse board color path position directionX directionY =
  let cell =
    getCell board position
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
  let cellContents =
    getCell board selection |> withDefault Nothing
  in
  let selectionColor =
    case cellContents of
      Nothing -> White
      Just (color, _) -> color
  in
  let filterPassable cellList =
    List.filter (canMoveToCell selectionColor board) cellList
  in
  let traverseBoard =
    traverse board selectionColor []
  in
  let traverseBishop =
    List.concat [
      traverseBoard {selection | x = selection.x - 1, y = selection.y - 1} -1 -1,
      traverseBoard {selection | x = selection.x - 1, y = selection.y + 1} -1  1,
      traverseBoard {selection | x = selection.x + 1, y = selection.y - 1}  1 -1,
      traverseBoard {selection | x = selection.x + 1, y = selection.y + 1}  1  1
    ]
  in
  let traverseRook =
    List.concat [
      traverseBoard {selection | x = selection.x - 1} -1  0,
      traverseBoard {selection | x = selection.x + 1}  1  0,
      traverseBoard {selection | y = selection.y - 1}  0 -1,
      traverseBoard {selection | y = selection.y + 1}  0  1
    ]
  in
    case cellContents of
      Nothing -> []
      Just (color, Pawn) ->
        case color of
          White ->
            filterPassable [
              {selection | x = selection.x - 1, y = selection.y - 1},
              {selection | x = selection.x + 1, y = selection.y - 1}
            ]
          Black ->
            filterPassable [
              {selection | x = selection.x - 1, y = selection.y + 1},
              {selection | x = selection.x + 1, y = selection.y + 1}
            ]
      Just (color, King) ->
        filterPassable [
          {selection | x = selection.x - 1, y = selection.y - 1},
          {selection | x = selection.x - 1, y = selection.y    },
          {selection | x = selection.x - 1, y = selection.y + 1},
          {selection | x = selection.x    , y = selection.y - 1},
          {selection | x = selection.x    , y = selection.y + 1},
          {selection | x = selection.x + 1, y = selection.y - 1},
          {selection | x = selection.x + 1, y = selection.y    },
          {selection | x = selection.x + 1, y = selection.y + 1}
        ]
      Just (color, Knight) ->
        filterPassable [
          {selection | x = selection.x - 2, y = selection.y + 1},
          {selection | x = selection.x - 2, y = selection.y - 1},
          {selection | x = selection.x + 2, y = selection.y + 1},
          {selection | x = selection.x + 2, y = selection.y - 1},
          {selection | x = selection.x + 1, y = selection.y - 2},
          {selection | x = selection.x - 1, y = selection.y - 2},
          {selection | x = selection.x + 1, y = selection.y + 2},
          {selection | x = selection.x - 1, y = selection.y + 2}
        ]
      Just (color, Bishop) ->
        traverseBishop
      Just (color, Rook) ->
        traverseRook
      Just (color, Queen) ->
        List.concat [traverseBishop, traverseRook]
