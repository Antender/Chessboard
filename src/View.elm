module View exposing (view)

import Html exposing (div, table, tbody, tr, td)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Basics.Extra exposing (..)
import Array

import Types exposing (..)

view model =
  let viewRowSelection =
    viewRow model.selection
  in
    table [
      style [
        "height" => "100vh",
        "border-collapse" => "collapse"]]
      [tbody [] (Array.toList (Array.indexedMap viewRowSelection model.board))]

viewRow selection rowindex row =
  tr [class "chessrow"] (Array.toList (Array.indexedMap (\colindex piece ->
    td [fieldcolor selection rowindex colindex, class "chesscolumn", onClick (Clicked (rowindex,colindex))] [
      pieceImage piece
    ]) row))

fieldcolor selection rowindex columnindex =
  let selected =
    case selection of
      Just (rowselection, columnselection) -> rowselection == rowindex && columnselection == columnindex
      _ -> False
  in
    style [
      "backgroundColor" =>
        if selected then
         "green"
        else
          if
            (rowindex + columnindex) % 2 == 0
          then
            "lightgrey"
          else
            "grey"
  ]

pieceImage cell =
  case cell of
    Nothing -> Html.text ""
    Just piece ->
      Html.img [
        src
          (case piece of
            WhiteKing   -> "chess_pieces/kwo.svg"
            BlackKing   -> "chess_pieces/kbr.svg"
            WhiteQueen  -> "chess_pieces/qwo.svg"
            BlackQueen  -> "chess_pieces/qbr.svg"
            WhiteRook   -> "chess_pieces/rwo.svg"
            BlackRook   -> "chess_pieces/rbr.svg"
            WhiteBishop -> "chess_pieces/bwo.svg"
            BlackBishop -> "chess_pieces/bbr.svg"
            WhiteKnight -> "chess_pieces/kwo.svg"
            BlackKnight -> "chess_pieces/kbr.svg"
            WhitePawn   -> "chess_pieces/pwo.svg"
            BlackPawn   -> "chess_pieces/pbr.svg")
          , class "chesspiece"] []
