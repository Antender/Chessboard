module Types exposing (..)

import Point exposing (Point)

type Msg = Clicked Point

type Color = White | Black

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type alias Entity = (Color, Piece)
