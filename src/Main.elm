module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Canvas
import Canvas.Settings
import Json.Decode as Decode
import Color
import Game
import Vector2 exposing (Vector2)
import Array

type alias Model =
    { game: Game.State
    }

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

init: () -> (Model, Cmd Msg)
init _ = 
    ( { game = Game.init }
    , Cmd.none
    )

type Msg = 
    ShiftLeft
    | ShiftRight
    | FlipX
    | FlipY
    | SoftDrop
    | HardDrop
    | NoAction

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        game = case msg of
            ShiftLeft ->
                Game.tryTranslate {x = -1, y = 0} model.game
            ShiftRight ->
                Game.tryTranslate {x = 1, y = 0} model.game
            FlipX ->
                Game.tryFlip Vector2.X model.game
            FlipY ->
                Game.tryFlip Vector2.Y model.game
            SoftDrop -> 
                Game.tryTranslate {x = 0, y = 1} model.game
            HardDrop ->
                Just <| Game.doHardDrop model.game
            NoAction ->
                Nothing
    in
      ({ model | game = game |> Maybe.withDefault model.game }, Cmd.none)

blockSize : Int
blockSize = 16
canvasSize: Vector2
canvasSize = Vector2.map ((*) blockSize) Game.boardSize

view: Model -> Html Msg
view model = 
    Html.div []
    [ canvasElement model
    ]

canvasElement : Model -> Html Msg
canvasElement model =
    Canvas.toHtml (Vector2.tuple canvasSize)
      [ style "border" "1px solid black"
      , style "display" "block"
      , style "width" <| String.fromInt (canvasSize.x * 2) ++ "px"
      , style "height" <| String.fromInt (canvasSize.y * 2) ++ "px"
      ]
      [ Canvas.shapes
        [ Canvas.Settings.fill Color.black ]
        [ Canvas.rect (0, 0) (toFloat canvasSize.x) (toFloat canvasSize.y) ] 
      , Canvas.shapes
        [ Canvas.Settings.fill Color.white ]
        (List.concat
            [ (renderBoard model.game.board)
            , (renderPiece model.game.piece)
            ])
      ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown handleKey

handleKey: Decode.Decoder Msg
handleKey =
    Decode.map toAction (Decode.field "code" Decode.string)

toAction: String -> Msg
toAction key =
    case key of
        "ArrowLeft" -> ShiftLeft
        "ArrowRight" -> ShiftRight
        "KeyX" -> FlipX
        "KeyZ" -> FlipY
        "ArrowDown" -> SoftDrop
        "ArrowUp" -> HardDrop
        _ -> NoAction

renderBoard : Game.Board -> List Canvas.Shape
renderBoard =
    Array.toList
    >> List.map Array.toList
    >> List.indexedMap renderRow
    >> List.concat

renderRow : Int -> List Int -> List Canvas.Shape
renderRow ypos =
    List.indexedMap (\(xpos) -> renderBlock (toFloat xpos) (toFloat ypos))
    >> List.filterMap identity

renderPiece : Game.Piece -> List Canvas.Shape
renderPiece =
    Game.getPieceBlocks
    >> List.map (\block -> renderBlock (toFloat block.x) (toFloat block.y) 1)
    >> List.filterMap identity

renderBlock: Float -> Float -> Int -> Maybe Canvas.Shape
renderBlock xpos ypos block =
    let
        size = toFloat blockSize
        x = xpos * size
        y = ypos * size
    in
        case block of
            0 -> Nothing
            _ -> Just <| Canvas.rect (x, y) size size