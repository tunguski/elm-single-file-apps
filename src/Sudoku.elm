module Sudoku exposing (main)

{-| **Sudoku** — a playable 9x9 Sudoku. Click a cell (or arrow-key to it), type 1–9 to fill it,
toggle pencil-mark mode to jot candidates, and watch conflicts light up red. Puzzles come from a
small fixed bank; "New puzzle" cycles through them. The whole in-progress game — which puzzle,
your entries and pencil marks — lives in the file itself and round-trips on Ctrl+S. See [`App`](App).
-}

import App exposing (Env)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


main : Program () (App.State Model) (App.Event Msg)
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , encode = encode
        , decoder = decoder
        }



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { puzzleIndex : Int
    , entries : Dict Int Int
    , marks : Dict Int (Set Int)
    , selected : Maybe Int
    , pencil : Bool
    }


init : Env -> Model
init _ =
    { puzzleIndex = 0
    , entries = Dict.empty
    , marks = Dict.empty
    , selected = Nothing
    , pencil = False
    }



-- PUZZLE BANK ----------------------------------------------------------------


{-| A fixed bank of valid puzzles. '0' (or '.') marks a blank cell. Row-major, 81 chars each.
-}
bank : List String
bank =
    [ "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
    , "004300209005009001070060043006002087190007400050083000600000105003508690042910300"
    , "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
    , "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
    , "030050040008010500460000012070502080000603000040109030250000098001020600080060020"
    ]


bankSize : Int
bankSize =
    List.length bank


{-| The 81-char string for a puzzle index (wrapping, defensively).
-}
puzzleString : Int -> String
puzzleString idx =
    let
        i =
            modBy bankSize idx
    in
    bank
        |> List.drop i
        |> List.head
        |> Maybe.withDefault (String.repeat 81 "0")


charDigit : Char -> Int
charDigit c =
    case c of
        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        _ ->
            0


{-| The clue digits for a puzzle: cell index → digit, blanks omitted.
-}
givensFor : Int -> Dict Int Int
givensFor idx =
    puzzleString idx
        |> String.toList
        |> List.indexedMap (\i c -> ( i, charDigit c ))
        |> List.filter (\( _, d ) -> d /= 0)
        |> Dict.fromList



-- GEOMETRY -------------------------------------------------------------------


allIndices : List Int
allIndices =
    List.range 0 80


rowOf : Int -> Int
rowOf i =
    i // 9


colOf : Int -> Int
colOf i =
    modBy 9 i


boxOf : Int -> Int
boxOf i =
    (rowOf i // 3) * 3 + (colOf i // 3)


{-| Cells sharing a row, column, or 3×3 box with `i`, excluding `i` itself.
-}
peers : Int -> List Int
peers i =
    List.filter
        (\j ->
            j
                /= i
                && (rowOf j == rowOf i || colOf j == colOf i || boxOf j == boxOf i)
        )
        allIndices



-- STATE HELPERS --------------------------------------------------------------


isGiven : Model -> Int -> Bool
isGiven model i =
    Dict.member i (givensFor model.puzzleIndex)


{-| The effective digit in a cell (given clue, else user entry, else 0 = blank).
-}
digitAt : Dict Int Int -> Model -> Int -> Int
digitAt givens model i =
    case Dict.get i givens of
        Just d ->
            d

        Nothing ->
            Maybe.withDefault 0 (Dict.get i model.entries)


marksAt : Model -> Int -> Set Int
marksAt model i =
    Maybe.withDefault Set.empty (Dict.get i model.marks)


{-| Every cell that clashes with a same-digit peer.
-}
conflictSet : Dict Int Int -> Model -> Set Int
conflictSet givens model =
    let
        d i =
            digitAt givens model i
    in
    allIndices
        |> List.filter
            (\i ->
                let
                    di =
                        d i
                in
                di /= 0 && List.any (\j -> d j == di) (peers i)
            )
        |> Set.fromList


isSolved : Dict Int Int -> Model -> Bool
isSolved givens model =
    List.all (\i -> digitAt givens model i /= 0) allIndices
        && Set.isEmpty (conflictSet givens model)



-- UPDATE ---------------------------------------------------------------------


type Msg
    = SelectCell Int
    | KeyPress String
    | PadDigit Int
    | ClearCell
    | TogglePencil
    | NewPuzzle
    | ResetPuzzle


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCell i ->
            { model | selected = Just i }

        PadDigit d ->
            applyDigit d model

        ClearCell ->
            clearSelected model

        TogglePencil ->
            { model | pencil = not model.pencil }

        NewPuzzle ->
            { model
                | puzzleIndex = modBy bankSize (model.puzzleIndex + 1)
                , entries = Dict.empty
                , marks = Dict.empty
                , selected = Nothing
            }

        ResetPuzzle ->
            { model | entries = Dict.empty, marks = Dict.empty }

        KeyPress key ->
            handleKey key model


handleKey : String -> Model -> Model
handleKey key model =
    case key of
        "Backspace" ->
            clearSelected model

        "Delete" ->
            clearSelected model

        "0" ->
            clearSelected model

        "p" ->
            { model | pencil = not model.pencil }

        "P" ->
            { model | pencil = not model.pencil }

        "ArrowUp" ->
            moveSelection -9 model

        "ArrowDown" ->
            moveSelection 9 model

        "ArrowLeft" ->
            moveSelection -1 model

        "ArrowRight" ->
            moveSelection 1 model

        _ ->
            case String.toInt key of
                Just d ->
                    if d >= 1 && d <= 9 then
                        applyDigit d model

                    else
                        model

                Nothing ->
                    model


{-| Move the selection by a delta, clamped so it never leaves the 9×9 board and rows don't wrap.
-}
moveSelection : Int -> Model -> Model
moveSelection delta model =
    let
        cur =
            Maybe.withDefault 0 model.selected

        target =
            cur + delta

        ok =
            if target < 0 || target > 80 then
                False

            else if delta == -1 then
                colOf cur /= 0

            else if delta == 1 then
                colOf cur /= 8

            else
                True
    in
    case model.selected of
        Nothing ->
            { model | selected = Just 0 }

        Just _ ->
            if ok then
                { model | selected = Just target }

            else
                model


applyDigit : Int -> Model -> Model
applyDigit d model =
    case model.selected of
        Nothing ->
            model

        Just i ->
            if isGiven model i then
                model

            else if model.pencil then
                let
                    cur =
                        marksAt model i

                    next =
                        if Set.member d cur then
                            Set.remove d cur

                        else
                            Set.insert d cur
                in
                { model | marks = Dict.insert i next model.marks }

            else
                { model
                    | entries = Dict.insert i d model.entries
                    , marks = Dict.remove i model.marks
                }


clearSelected : Model -> Model
clearSelected model =
    case model.selected of
        Nothing ->
            model

        Just i ->
            if isGiven model i then
                model

            else
                { model
                    | entries = Dict.remove i model.entries
                    , marks = Dict.remove i model.marks
                }



-- SUBSCRIPTIONS --------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (D.map KeyPress (D.field "key" D.string))



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        givens =
            givensFor model.puzzleIndex

        conflicts =
            conflictSet givens model

        solved =
            isSolved givens model

        selDigit =
            case model.selected of
                Just i ->
                    digitAt givens model i

                Nothing ->
                    0
    in
    div [ class "app" ]
        [ div [ class "toolbar" ]
            [ span [ class "toolbar__title" ] [ text "Sudoku" ]
            , button
                [ class "btn"
                , classList [ ( "btn--primary", model.pencil ) ]
                , onClick TogglePencil
                ]
                [ text ("Pencil: " ++ onOff model.pencil) ]
            , button [ class "btn", onClick ClearCell ] [ text "Clear cell" ]
            , button [ class "btn btn--danger", onClick ResetPuzzle ] [ text "Reset" ]
            , button [ class "btn btn--primary", onClick NewPuzzle ] [ text "New puzzle" ]
            ]
        , div [ class "body sk-body" ]
            [ div [ class "sk-stage" ]
                [ div [ class "sk-grid" ]
                    (List.map (cellView givens model conflicts selDigit) allIndices)
                , div [ class "sk-side" ]
                    [ statusView solved model
                    , padView model
                    , helpView
                    ]
                ]
            ]
        ]


onOff : Bool -> String
onOff b =
    if b then
        "on"

    else
        "off"


cellView : Dict Int Int -> Model -> Set Int -> Int -> Int -> Html Msg
cellView givens model conflicts selDigit i =
    let
        d =
            digitAt givens model i

        given =
            Dict.member i givens

        isSel =
            model.selected == Just i

        isPeer =
            case model.selected of
                Just s ->
                    not isSel
                        && (rowOf s == rowOf i || colOf s == colOf i || boxOf s == boxOf i)

                Nothing ->
                    False

        isSame =
            selDigit /= 0 && d == selDigit && not isSel

        conflict =
            Set.member i conflicts

        r =
            rowOf i

        c =
            colOf i
    in
    div
        [ classList
            [ ( "sk-cell", True )
            , ( "sk-given", given )
            , ( "sk-sel", isSel )
            , ( "sk-peer", isPeer )
            , ( "sk-same", isSame )
            , ( "sk-conflict", conflict )
            , ( "sk-br", modBy 3 c == 2 && c /= 8 )
            , ( "sk-bb", modBy 3 r == 2 && r /= 8 )
            ]
        , onClick (SelectCell i)
        ]
        (if d /= 0 then
            [ span [ class "sk-digit" ] [ text (String.fromInt d) ] ]

         else
            [ marksView (marksAt model i) ]
        )


marksView : Set Int -> Html Msg
marksView marks =
    if Set.isEmpty marks then
        text ""

    else
        div [ class "sk-marks" ]
            (List.map
                (\n ->
                    span [ class "sk-mark" ]
                        [ text
                            (if Set.member n marks then
                                String.fromInt n

                             else
                                ""
                            )
                        ]
                )
                (List.range 1 9)
            )


statusView : Bool -> Model -> Html Msg
statusView solved model =
    if solved then
        div [ class "sk-status sk-status--solved" ] [ text "Solved! 🎉" ]

    else
        div [ class "sk-status" ]
            [ text ("Puzzle " ++ String.fromInt (model.puzzleIndex + 1) ++ " of " ++ String.fromInt bankSize) ]


padView : Model -> Html Msg
padView model =
    div [ class "sk-pad" ]
        (List.map
            (\n ->
                button
                    [ class "btn sk-key"
                    , onClick (PadDigit n)
                    ]
                    [ text (String.fromInt n) ]
            )
            (List.range 1 9)
            ++ [ button [ class "btn sk-key sk-key--wide", onClick ClearCell ] [ text "Clear" ] ]
        )


helpView : Html Msg
helpView =
    div [ class "sk-help muted" ]
        [ div []
            [ span [ class "kbd" ] [ text "1–9" ], text " fill" ]
        , div []
            [ span [ class "kbd" ] [ text "⌫" ], text " / ", span [ class "kbd" ] [ text "Del" ], text " clear" ]
        , div []
            [ span [ class "kbd" ] [ text "P" ], text " pencil mode" ]
        , div []
            [ span [ class "kbd" ] [ text "↑↓←→" ], text " move" ]
        ]



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "puzzleIndex", E.int model.puzzleIndex )
        , ( "entries", E.dict String.fromInt E.int model.entries )
        , ( "marks", E.dict String.fromInt (\s -> E.list E.int (Set.toList s)) model.marks )
        ]


decoder : Env -> D.Decoder Model
decoder _ =
    D.map3
        (\idx entries marks ->
            { puzzleIndex = modBy bankSize (max 0 idx)
            , entries = entries
            , marks = marks
            , selected = Nothing
            , pencil = False
            }
        )
        (D.oneOf [ D.field "puzzleIndex" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "entries" intKeyedDict, D.succeed Dict.empty ])
        (D.oneOf [ D.field "marks" intKeyedMarks, D.succeed Dict.empty ])


{-| Decode a JSON object with integer-as-string keys into a `Dict Int Int`.
-}
intKeyedDict : D.Decoder (Dict Int Int)
intKeyedDict =
    D.dict D.int
        |> D.map toIntKeys


intKeyedMarks : D.Decoder (Dict Int (Set Int))
intKeyedMarks =
    D.dict (D.list D.int)
        |> D.map (Dict.map (\_ xs -> Set.fromList xs))
        |> D.map toIntKeys


{-| Re-key a `Dict String v` to `Dict Int v`, dropping any non-integer keys.
-}
toIntKeys : Dict String v -> Dict Int v
toIntKeys d =
    Dict.foldl
        (\k v acc ->
            case String.toInt k of
                Just i ->
                    Dict.insert i v acc

                Nothing ->
                    acc
        )
        Dict.empty
        d
