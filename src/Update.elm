module Update exposing (init, update)

import Array

import Types exposing (..)

init =
  {
    selection = Nothing,
    board = Array.fromList [
      Array.fromList [
        Just BlackRook, Just BlackKnight, Just BlackBishop, Just BlackQueen,
        Just BlackKing, Just BlackBishop, Just BlackKnight, Just BlackRook],
      (Array.repeat 8 (Just BlackPawn)),
      (Array.repeat 8 Nothing),
      (Array.repeat 8 Nothing),
      (Array.repeat 8 Nothing),
      (Array.repeat 8 Nothing),
      (Array.repeat 8 (Just WhitePawn)),
      Array.fromList [
        Just WhiteRook, Just WhiteKnight, Just WhiteBishop, Just WhiteQueen,
        Just WhiteKing, Just WhiteBishop, Just WhiteKnight, Just WhiteRook]
    ]
  }

update msg model =
  case msg of
    Clicked point ->
      case model.selection of
        Nothing -> {model | selection = Just point}
        _ -> {model | selection = Nothing}
