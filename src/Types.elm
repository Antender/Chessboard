module Types exposing (..)

type alias Point = (Int, Int)

type Msg = Clicked Point

type Piece =
  WhiteKing | BlackKing |
  WhiteQueen | BlackQueen |
  WhiteRook | BlackRook |
  WhiteBishop | BlackBishop |
  WhiteKnight | BlackKnight |
  WhitePawn | BlackPawn
