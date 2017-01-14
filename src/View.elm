module View exposing (view)

import Html exposing (div, table, tbody, tr, td)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Basics.Extra exposing (..)
import Array

import Types exposing (..)

view model =
  let viewRowSelection =
    viewRow model.destinations model.selection
  in
    table [
      style [
        "height" => "100vh",
        "border-collapse" => "collapse",
        "table-layout" => "fixed"]]
      [tbody [] (Array.toList (Array.indexedMap viewRowSelection model.board))]

viewRow destinations selection rowindex row =
    tr [class "chessrow"] (Array.toList (Array.indexedMap (\colindex piece ->
      let selected =
        case selection of
          Just { x , y} -> x == colindex && y == rowindex
          _ -> False
        in
          td [fieldcolor destinations selection rowindex colindex selected, class "chesscolumn", onClick (Clicked { x = colindex, y = rowindex })] [
            pieceImage piece (
              style [
                "border" =>
                  ("thick solid " ++
                    if selected then
                      "green"
                    else
                      if List.member { x = colindex, y = rowindex} destinations then
                        "blue"
                      else
                        checkPattern rowindex colindex)
            ])
          ]) row))

fieldcolor destinations selection rowindex colindex selected =
  style [
    "backgroundColor" => checkPattern rowindex colindex
  ]

checkPattern row column =
  if (row + column) % 2 == 0 then
    "lightgrey"
  else
    "grey"

pieceImage entity style =
  case entity of
    Nothing -> Html.div [class "emptypiece", style] []
    Just piece ->
      Html.img [
        src
          (case piece of
            (White, King)   -> "chess_pieces/kwo.svg"
            (Black, King)   -> "chess_pieces/kbr.svg"
            (White, Queen)  -> "chess_pieces/qwo.svg"
            (Black, Queen)  -> "chess_pieces/qbr.svg"
            (White, Rook)   -> "chess_pieces/rwo.svg"
            (Black, Rook)   -> "chess_pieces/rbr.svg"
            (White, Bishop) -> "chess_pieces/bwo.svg"
            (Black, Bishop) -> "chess_pieces/bbr.svg"
            (White, Knight) -> "chess_pieces/nwo.svg"
            (Black, Knight) -> "chess_pieces/nbr.svg"
            (White, Pawn)   -> "chess_pieces/pwo.svg"
            (Black, Pawn)   -> "chess_pieces/pbr.svg")
          , class "chesspiece", style] []
