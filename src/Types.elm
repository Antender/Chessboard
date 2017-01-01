module Types exposing (..)

type alias Point = { x : Int, y : Int}

type Msg = Clicked Point

type Color = White | Black

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type alias Entity = (Color, Piece)
