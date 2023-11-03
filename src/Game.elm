module Game exposing (..)

import Array exposing (Array)
import Vector2 exposing (Vector2)

type alias State =
    { board: Board
    , piece: Piece
    }

type alias BlockList = List Vector2
type alias Block = Int
type alias Board = Array (Array Block)
type alias Flip = { x: Bool, y: Bool }

type alias Piece =
    { index: Int
    , position: Vector2
    , flip: Flip
    }

boardSize: Vector2
boardSize = {x = 10, y = 20}

type alias PieceDataEntry = { blocks: BlockList }
pieceData : Array PieceDataEntry
pieceData =
    Array.fromList
    [ { blocks=[ {x=0, y=0}, {x=0, y=1}, {x=0, y=2}, {x=0, y=3} ] } -- I (Wall)
    , { blocks=[ {x=0, y=0}, {x=0, y=1}, {x=0, y=2}, {x=1, y=0} ] } -- Bar (Floor)
    , { blocks=[ {x=0, y=0}, {x=1, y=0}, {x=0, y=1}, {x=1, y=1} ] } -- O (Box)
    , { blocks=[ {x=0, y=0}, {x=1, y=0}, {x=2, y=0}, {x=1, y=1} ] } -- T (Bush)
    , { blocks=[ {x=0, y=0}, {x=0, y=1}, {x=0, y=2}, {x=1, y=1} ] } -- D (Tree)
    , { blocks=[ {x=1, y=0}, {x=0, y=1}, {x=1, y=1}, {x=1, y=2} ] } -- Tall Zig (Bolt)
    , { blocks=[ {x=1, y=0}, {x=0, y=1}, {x=1, y=1}, {x=2, y=0} ] } -- Wide Zig (Wind)
    , { blocks=[ {x=0, y=0}, {x=1, y=0}, {x=0, y=1}, {x=0, y=2} ] } -- Tall L (Chair)
    , { blocks=[ {x=0, y=0}, {x=1, y=0}, {x=2, y=0}, {x=0, y=1} ] } -- Wide L (Couch)
    ]

getPieceData: Int -> PieceDataEntry
getPieceData index =
    case Array.get (index - 1) pieceData of
        Just piece -> piece
        Nothing -> { blocks = [{x=0, y=0}] }


init : State
init = 
    { board = Array.repeat boardSize.y (Array.repeat boardSize.x 0)
    , piece = createPiece 1
    }

createPiece: Int -> Piece
createPiece index = 
    let
        size = pieceSize (getPieceData index).blocks
    in
        { index = index
        , position = {x = 5 - size.x // 2, y = 0}
        , flip = {x = False, y = False}
        }

tryFlip: Vector2.Axis -> State -> Maybe State
tryFlip axis state =
    let
        piece = state.piece
        newPiece = 
            { piece | flip =
                { x = xor (axis == Vector2.X) piece.flip.x
                , y = xor (axis == Vector2.Y) piece.flip.y
                }
            }
    in
        tryPiece newPiece state
            |> Maybe.map (\newState -> { newState | piece = newPiece })

tryTranslate: Vector2 -> State -> Maybe State
tryTranslate offset state =
    let
        piece = state.piece
        newPiece = { piece | position = Vector2.add state.piece.position offset }
    in
        tryPiece newPiece state
            |> Maybe.map (\newState -> { newState | piece = newPiece })

doHardDrop: State -> State
doHardDrop state =
    let
        piece = state.piece
        newPiece = { piece | position = Vector2.add state.piece.position {x = 0, y = 1} }
    in
        case tryPiece newPiece state of
            Just newState -> doHardDrop newState
            Nothing -> solidifyPiece state

tryPiece: Piece -> State -> Maybe State
tryPiece newPiece state =
    let
        newBlocks = getPieceBlocks newPiece
    in
        if doBlocksFit newBlocks state.board then
            Just { state | piece = newPiece }
        else
            Nothing

solidifyPiece: State -> State
solidifyPiece state =
    let
        blocks = getPieceBlocks state.piece
        newBoard = appendBlocks blocks state.piece.index state.board
        newPiece = createPiece 1
    in
        { state | board = newBoard, piece = newPiece }

getPieceBlocks: Piece -> BlockList
getPieceBlocks piece =
    getPieceData piece.index
        |> .blocks
        |> flipBlocks piece.flip
        |> translateBlocks piece.position

flipBlocks : Flip -> BlockList -> BlockList
flipBlocks flip blocks =
    let
        size = pieceSize blocks
    in
        blocks
        |> List.map (\{x, y} ->
            { x = if flip.x then size.x - x else x
            , y = if flip.y then size.y - y else y
            })

pieceSize : BlockList -> Vector2
pieceSize blocks =
    blocks
        |> List.foldl (\{x, y} size -> 
            { x = max size.x x, y = max size.y y })
        {x = 0, y = 0}

translateBlocks : Vector2 -> BlockList -> BlockList
translateBlocks shift blocks =
    blocks
    |> List.map (\{x, y} ->
        { x = shift.x + x
        , y = shift.y + y
        })

appendBlocks: BlockList -> Int -> Board -> Board
appendBlocks blocks color board =
    case blocks of
        [] -> board
        pos :: rest ->
            let
                updatedRow = Array.get pos.y board
                  |> Maybe.withDefault (Array.repeat boardSize.x 0)
                  |> Array.set pos.x color
                updatedBoard = board
                  |> Array.set pos.y updatedRow
            in
                appendBlocks rest color updatedBoard

doBlocksFit: BlockList -> Board -> Bool
doBlocksFit blocks board =
    blocks
    |> List.all (\pos -> isBoardFreeAt pos board)

isBoardFreeAt: Vector2 -> Board -> Bool
isBoardFreeAt {x, y} board = 
    let
        block = Array.get y board |> Maybe.andThen (Array.get x)
    in
        block == Just 0