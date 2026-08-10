module Sudoku exposing (main)

{-| **Sudoku** — a playable 9×9 Sudoku with an **infinite, seeded generator**. Puzzles are graded into
four difficulties (easy / medium / hard / hardcore) by how many clues are dug out while keeping a
**unique** solution; a difficulty `select` switches levels (easy by default). "New puzzle" makes a
fresh random one; **Puzzle of the Day** is a hard puzzle seeded from the date, so every player gets
the same board each day.

The in-progress game lives in the file (Ctrl+S). With a backend ([`server/SudokuServer.elm`]) each
solve is timed and POSTed, the puzzle is stored, and you can see this puzzle's ranking (everyone who
solved it, fastest first) and your own solved-puzzle history. Player identity is the file's UUID
([`App.Env`](App).newId). See [`App`](App).
-}

import App exposing (Env)
import Array exposing (Array)
import Browser.Events
import Char
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, input, option, select, span, text)
import Html.Attributes exposing (class, classList, href, placeholder, selected, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Random
import Set exposing (Set)
import Task
import Time


main : Program () (App.State Model) (App.Event Msg)
main =
    App.programEffect
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , encode = encode
        , decoder = decoder
        }


defaultServerUrl : String
defaultServerUrl =
    "https://sudoku.matsuo.pl"



-- MODEL ----------------------------------------------------------------------


type Difficulty
    = Easy
    | Medium
    | Hard
    | Hardcore


type alias Model =
    { givens : Dict Int Int -- the clue cells (index → digit); blanks omitted
    , solution : Dict Int Int -- the full solution (all 81 cells)
    , difficulty : Difficulty -- difficulty of the CURRENT puzzle
    , picked : Difficulty -- difficulty selected for the NEXT "New puzzle"
    , puzzleId : String -- stable id = the 81-char givens string
    , isDaily : Bool
    , entries : Dict Int Int
    , marks : Dict Int (Set Int)
    , selected : Maybe Int
    , pencil : Bool
    , today : String

    -- backend
    , playerId : String
    , serverUrl : String
    , startedAt : Maybe Int
    , solvedPosted : Bool
    , sync : Sync
    , ranking : List RankEntry -- this puzzle's ranking (fastest first)
    , solved : List SolvedEntry -- this player's solved puzzles
    }


type Sync
    = Idle
    | Syncing
    | Synced Int Int -- games count, total ms
    | Failed


type alias RankEntry =
    { player : String, elapsedMs : Int, solvedAt : Int }


type alias SolvedEntry =
    { puzzleId : String, difficulty : String, elapsedMs : Int, solvedAt : Int }


init : Env -> ( Model, Cmd Msg )
init env =
    let
        base =
            { givens = Dict.empty
            , solution = Dict.empty
            , difficulty = Easy
            , picked = Easy
            , puzzleId = ""
            , isDaily = False
            , entries = Dict.empty
            , marks = Dict.empty
            , selected = Nothing
            , pencil = False
            , today = env.today
            , playerId = env.newId
            , serverUrl = defaultServerUrl
            , startedAt = Nothing
            , solvedPosted = False
            , sync = Idle
            , ranking = []
            , solved = []
            }
    in
    ( loadPuzzle (generate Easy False (seedFrom env.now)) base, Cmd.none )



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


peers : Int -> List Int
peers i =
    List.filter
        (\j -> j /= i && (rowOf j == rowOf i || colOf j == colOf i || boxOf j == boxOf i))
        allIndices



-- GENERATOR ------------------------------------------------------------------


type alias Puzzle =
    { givens : Dict Int Int, solution : Dict Int Int, difficulty : Difficulty, isDaily : Bool }


{-| Generate a puzzle at `diff` from an integer seed (deterministic: same seed → same puzzle, which
is what makes the daily puzzle identical for everyone).
-}
generate : Difficulty -> Bool -> Int -> Puzzle
generate diff daily seedInt =
    let
        seed0 =
            Random.initialSeed seedInt
    in
    case fillGrid seed0 of
        Just ( full, seed1 ) ->
            let
                dug =
                    dig diff full seed1
            in
            { givens = gridToDict dug, solution = gridToDict full, difficulty = diff, isDaily = daily }

        Nothing ->
            { givens = Dict.empty, solution = Dict.empty, difficulty = diff, isDaily = daily }


emptyGrid : Array Int
emptyGrid =
    Array.repeat 81 0


{-| Digits already used in cell `i`'s row, column or box. -}
usedDigits : Array Int -> Int -> Set Int
usedDigits g i =
    let
        r =
            rowOf i

        c =
            colOf i

        b =
            boxOf i
    in
    List.foldl
        (\j acc ->
            if rowOf j == r || colOf j == c || boxOf j == b then
                case Array.get j g of
                    Just d ->
                        if d /= 0 then
                            Set.insert d acc

                        else
                            acc

                    Nothing ->
                        acc

            else
                acc
        )
        Set.empty
        allIndices


candidates : Array Int -> Int -> List Int
candidates g i =
    let
        used =
            usedDigits g i
    in
    List.filter (\d -> not (Set.member d used)) (List.range 1 9)


{-| A random completed grid: fill cells 0..80, trying each cell's candidates in a shuffled order and
backtracking. An empty grid always completes, so this never fails in practice.
-}
fillGrid : Random.Seed -> Maybe ( Array Int, Random.Seed )
fillGrid seed =
    fillFrom 0 emptyGrid seed


fillFrom : Int -> Array Int -> Random.Seed -> Maybe ( Array Int, Random.Seed )
fillFrom i g seed =
    if i >= 81 then
        Just ( g, seed )

    else
        let
            ( cands, seed2 ) =
                shuffle (candidates g i) seed
        in
        tryFill i g cands seed2


tryFill : Int -> Array Int -> List Int -> Random.Seed -> Maybe ( Array Int, Random.Seed )
tryFill i g cands seed =
    case cands of
        [] ->
            Nothing

        d :: rest ->
            case fillFrom (i + 1) (Array.set i d g) seed of
                Just result ->
                    Just result

                Nothing ->
                    tryFill i g rest seed


{-| Remove clues (in shuffled order) as long as the puzzle keeps exactly one solution, stopping when
the difficulty's target clue count is reached (hardcore digs until nothing more can be removed).
-}
dig : Difficulty -> Array Int -> Random.Seed -> Array Int
dig diff full seed =
    let
        ( order, _ ) =
            shuffle allIndices seed
    in
    digCells order (targetGivens diff) full


digCells : List Int -> Int -> Array Int -> Array Int
digCells order target g =
    case order of
        [] ->
            g

        i :: rest ->
            if givenCount g <= target then
                g

            else
                let
                    g2 =
                        Array.set i 0 g
                in
                if solveCount g2 == 1 then
                    digCells rest target g2

                else
                    digCells rest target g


targetGivens : Difficulty -> Int
targetGivens diff =
    case diff of
        Easy ->
            48

        Medium ->
            40

        Hard ->
            32

        Hardcore ->
            17


givenCount : Array Int -> Int
givenCount g =
    Array.foldl
        (\d acc ->
            if d /= 0 then
                acc + 1

            else
                acc
        )
        0
        g


{-| Number of solutions, capped at 2 (enough to decide uniqueness). Uses the minimum-remaining-values
heuristic (fill the most-constrained cell first) so it stays fast even on sparse grids.
-}
solveCount : Array Int -> Int
solveCount g =
    case mostConstrained g of
        Nothing ->
            1

        Just i ->
            countCands i g (candidates g i) 0


countCands : Int -> Array Int -> List Int -> Int -> Int
countCands i g cands acc =
    if acc >= 2 then
        acc

    else
        case cands of
            [] ->
                acc

            d :: rest ->
                countCands i g rest (acc + solveCount (Array.set i d g))


mostConstrained : Array Int -> Maybe Int
mostConstrained g =
    List.foldl
        (\i best ->
            if Array.get i g == Just 0 then
                let
                    n =
                        List.length (candidates g i)
                in
                case best of
                    Nothing ->
                        Just ( i, n )

                    Just ( _, bn ) ->
                        if n < bn then
                            Just ( i, n )

                        else
                            best

            else
                best
        )
        Nothing
        allIndices
        |> Maybe.map Tuple.first


{-| Deterministic shuffle: tag each element with a pseudo-random key and sort by it. -}
shuffle : List a -> Random.Seed -> ( List a, Random.Seed )
shuffle xs seed =
    let
        ( keys, seed2 ) =
            Random.step (Random.list (List.length xs) (Random.int 0 1000000000)) seed
    in
    ( List.map Tuple.second (List.sortBy Tuple.first (List.map2 Tuple.pair keys xs)), seed2 )


gridToDict : Array Int -> Dict Int Int
gridToDict g =
    Array.toIndexedList g
        |> List.filter (\( _, d ) -> d /= 0)
        |> Dict.fromList


{-| A positive, non-trivial seed derived from an integer (0 → a fixed fallback). -}
seedFrom : Int -> Int
seedFrom n =
    if n <= 0 then
        123457

    else
        n


{-| Deterministic seed for a date string (djb2), so the daily puzzle is stable per day. -}
seedForDate : String -> Int
seedForDate s =
    String.foldl (\ch acc -> modBy 2000000000 (acc * 33 + Char.toCode ch)) 5381 s + 1


loadPuzzle : Puzzle -> Model -> Model
loadPuzzle p model =
    { model
        | givens = p.givens
        , solution = p.solution
        , difficulty = p.difficulty
        , puzzleId = puzzleIdOf p.givens
        , isDaily = p.isDaily
        , entries = Dict.empty
        , marks = Dict.empty
        , selected = Nothing
        , startedAt = Nothing
        , solvedPosted = False
        , ranking = []
    }


puzzleIdOf : Dict Int Int -> String
puzzleIdOf givens =
    String.concat
        (List.map
            (\i ->
                case Dict.get i givens of
                    Just d ->
                        String.fromInt d

                    Nothing ->
                        "0"
            )
            allIndices
        )



-- STATE HELPERS --------------------------------------------------------------


isGiven : Model -> Int -> Bool
isGiven model i =
    Dict.member i model.givens


digitAt : Model -> Int -> Int
digitAt model i =
    case Dict.get i model.givens of
        Just d ->
            d

        Nothing ->
            Maybe.withDefault 0 (Dict.get i model.entries)


marksAt : Model -> Int -> Set Int
marksAt model i =
    Maybe.withDefault Set.empty (Dict.get i model.marks)


conflictSet : Model -> Set Int
conflictSet model =
    allIndices
        |> List.filter
            (\i ->
                let
                    di =
                        digitAt model i
                in
                di /= 0 && List.any (\j -> digitAt model j == di) (peers i)
            )
        |> Set.fromList


isSolved : Model -> Bool
isSolved model =
    List.all (\i -> digitAt model i /= 0) allIndices
        && Set.isEmpty (conflictSet model)


hasProgress : Model -> Bool
hasProgress model =
    not (Dict.isEmpty model.entries)



-- UPDATE ---------------------------------------------------------------------


type Msg
    = SelectCell Int
    | KeyPress String
    | PadDigit Int
    | ClearCell
    | TogglePencil
    | PickDifficulty String
    | NewPuzzle
    | DailyPuzzle
    | GenPuzzle Difficulty Bool Int
    | ResetPuzzle
    | StartedAt Int
    | SolvedAt Int
    | GotSync (Result Http.Error Summary)
    | GotRanking (Result Http.Error (List RankEntry))
    | GotSolved (Result Http.Error (List SolvedEntry))
    | SetServerUrl String
    | RefreshStats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCell i ->
            ( { model | selected = Just i }, Cmd.none )

        PadDigit d ->
            afterMove (applyDigit d model)

        ClearCell ->
            ( clearSelected model, Cmd.none )

        TogglePencil ->
            ( { model | pencil = not model.pencil }, Cmd.none )

        PickDifficulty s ->
            ( { model | picked = difficultyFromString s }, Cmd.none )

        NewPuzzle ->
            -- Fresh timestamp → fresh seed, so each new puzzle differs.
            ( model, requestNow (GenPuzzle model.picked False) )

        DailyPuzzle ->
            ( loadPuzzle (generate Hard True (seedForDate model.today)) model
            , Cmd.none
            )
                |> fetchRankingAfter

        GenPuzzle diff daily seed ->
            ( loadPuzzle (generate diff daily (seedFrom seed)) model, Cmd.none )
                |> fetchRankingAfter

        ResetPuzzle ->
            ( { model | entries = Dict.empty, marks = Dict.empty, startedAt = Nothing, solvedPosted = False }
            , Cmd.none
            )

        KeyPress key ->
            handleKey key model

        StartedAt t ->
            ( case model.startedAt of
                Just _ ->
                    model

                Nothing ->
                    { model | startedAt = Just t }
            , Cmd.none
            )

        SolvedAt t ->
            onSolved t model

        GotSync result ->
            ( { model | sync = syncFromResult result }, Cmd.none )

        GotRanking result ->
            ( { model | ranking = Result.withDefault model.ranking result }, Cmd.none )

        GotSolved result ->
            ( { model | solved = Result.withDefault model.solved result }, Cmd.none )

        SetServerUrl url ->
            ( { model | serverUrl = url }, Cmd.none )

        RefreshStats ->
            if backendReady model then
                ( { model | sync = Syncing }, Cmd.batch [ getStats model, getSolved model ] )

            else
                ( model, Cmd.none )


{-| After changing the puzzle, refresh this puzzle's ranking if a backend is configured. -}
fetchRankingAfter : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
fetchRankingAfter ( model, cmd ) =
    if backendReady model then
        ( model, Cmd.batch [ cmd, getRanking model ] )

    else
        ( model, cmd )


handleKey : String -> Model -> ( Model, Cmd Msg )
handleKey key model =
    case key of
        "Backspace" ->
            ( clearSelected model, Cmd.none )

        "Delete" ->
            ( clearSelected model, Cmd.none )

        "0" ->
            ( clearSelected model, Cmd.none )

        "p" ->
            ( { model | pencil = not model.pencil }, Cmd.none )

        "P" ->
            ( { model | pencil = not model.pencil }, Cmd.none )

        "ArrowUp" ->
            ( moveSelection -9 model, Cmd.none )

        "ArrowDown" ->
            ( moveSelection 9 model, Cmd.none )

        "ArrowLeft" ->
            ( moveSelection -1 model, Cmd.none )

        "ArrowRight" ->
            ( moveSelection 1 model, Cmd.none )

        _ ->
            case String.toInt key of
                Just d ->
                    if d >= 1 && d <= 9 then
                        afterMove (applyDigit d model)

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


afterMove : Model -> ( Model, Cmd Msg )
afterMove model =
    let
        solved =
            isSolved model

        startCmd =
            if model.startedAt == Nothing && hasProgress model then
                requestNow StartedAt

            else
                Cmd.none

        solveCmd =
            if solved && not model.solvedPosted then
                requestNow SolvedAt

            else
                Cmd.none
    in
    ( model, Cmd.batch [ startCmd, solveCmd ] )


onSolved : Int -> Model -> ( Model, Cmd Msg )
onSolved finishedAt model =
    let
        start =
            Maybe.withDefault finishedAt model.startedAt

        elapsed =
            max 0 (finishedAt - start)

        next =
            { model | solvedPosted = True, sync = Syncing }
    in
    ( next
    , Cmd.batch [ postGame next elapsed finishedAt, getRanking next, getSolved next ]
    )


requestNow : (Int -> Msg) -> Cmd Msg
requestNow toMsg =
    Task.perform (\posix -> toMsg (Time.posixToMillis posix)) Time.now


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
                { model | entries = Dict.insert i d model.entries, marks = Dict.remove i model.marks }


clearSelected : Model -> Model
clearSelected model =
    case model.selected of
        Nothing ->
            model

        Just i ->
            if isGiven model i then
                model

            else
                { model | entries = Dict.remove i model.entries, marks = Dict.remove i model.marks }



-- BACKEND --------------------------------------------------------------------


type alias Summary =
    { count : Int, totalMs : Int }


backendReady : Model -> Bool
backendReady model =
    String.trim model.serverUrl /= "" && String.trim model.playerId /= ""


apiBase : Model -> String
apiBase model =
    if String.endsWith "/" model.serverUrl then
        String.dropRight 1 model.serverUrl

    else
        model.serverUrl


postGame : Model -> Int -> Int -> Cmd Msg
postGame model elapsedMs solvedAt =
    if not (backendReady model) then
        Cmd.none

    else
        Http.post
            { url = apiBase model ++ "/games"
            , body =
                Http.jsonBody
                    (E.object
                        [ ( "playerId", E.string model.playerId )
                        , ( "puzzleId", E.string model.puzzleId )
                        , ( "givens", E.string model.puzzleId )
                        , ( "solution", E.string (solutionString model) )
                        , ( "difficulty", E.string (difficultyToString model.difficulty) )
                        , ( "elapsedMs", E.int elapsedMs )
                        , ( "solvedAt", E.int solvedAt )
                        ]
                    )
            , expect = Http.expectJson GotSync summaryDecoder
            }


getStats : Model -> Cmd Msg
getStats model =
    Http.get
        { url = apiBase model ++ "/games?playerId=" ++ model.playerId
        , expect = Http.expectJson GotSync summaryDecoder
        }


getRanking : Model -> Cmd Msg
getRanking model =
    if not (backendReady model) || model.puzzleId == "" then
        Cmd.none

    else
        Http.get
            { url = apiBase model ++ "/ranking?puzzleId=" ++ model.puzzleId
            , expect = Http.expectJson GotRanking (D.field "ranking" (D.list rankDecoder))
            }


getSolved : Model -> Cmd Msg
getSolved model =
    if not (backendReady model) then
        Cmd.none

    else
        Http.get
            { url = apiBase model ++ "/games?playerId=" ++ model.playerId
            , expect = Http.expectJson GotSolved (D.field "games" (D.list solvedDecoder))
            }


solutionString : Model -> String
solutionString model =
    String.concat
        (List.map
            (\i ->
                case Dict.get i model.solution of
                    Just d ->
                        String.fromInt d

                    Nothing ->
                        "0"
            )
            allIndices
        )


summaryDecoder : D.Decoder Summary
summaryDecoder =
    D.map2 Summary (D.field "count" D.int) (D.field "totalMs" D.int)


rankDecoder : D.Decoder RankEntry
rankDecoder =
    D.map3 RankEntry
        (D.field "player" D.string)
        (D.field "elapsedMs" D.int)
        (D.oneOf [ D.field "solvedAt" D.int, D.succeed 0 ])


solvedDecoder : D.Decoder SolvedEntry
solvedDecoder =
    D.map4 SolvedEntry
        (D.oneOf [ D.field "puzzleId" D.string, D.succeed "" ])
        (D.oneOf [ D.field "difficulty" D.string, D.succeed "" ])
        (D.field "elapsedMs" D.int)
        (D.oneOf [ D.field "solvedAt" D.int, D.succeed 0 ])


syncFromResult : Result Http.Error Summary -> Sync
syncFromResult result =
    case result of
        Ok s ->
            Synced s.count s.totalMs

        Err _ ->
            Failed



-- DIFFICULTY <-> STRING


difficultyToString : Difficulty -> String
difficultyToString d =
    case d of
        Easy ->
            "easy"

        Medium ->
            "medium"

        Hard ->
            "hard"

        Hardcore ->
            "hardcore"


difficultyLabel : Difficulty -> String
difficultyLabel d =
    case d of
        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"

        Hardcore ->
            "Hardcore"


difficultyFromString : String -> Difficulty
difficultyFromString s =
    case s of
        "medium" ->
            Medium

        "hard" ->
            Hard

        "hardcore" ->
            Hardcore

        _ ->
            Easy



-- SUBSCRIPTIONS --------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (D.map KeyPress (D.field "key" D.string))



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        conflicts =
            conflictSet model

        solved =
            isSolved model

        selDigit =
            case model.selected of
                Just i ->
                    digitAt model i

                Nothing ->
                    0
    in
    div [ class "app" ]
        [ div [ class "toolbar" ]
            [ span [ class "toolbar__title" ] [ text "Sudoku" ]
            , difficultySelect model
            , button [ class "btn btn--primary", onClick NewPuzzle ] [ text "New puzzle" ]
            , button [ class "btn", onClick DailyPuzzle ] [ text "Puzzle of the day" ]
            , button
                [ class "btn", classList [ ( "btn--primary", model.pencil ) ], onClick TogglePencil ]
                [ text ("Pencil: " ++ onOff model.pencil) ]
            , button [ class "btn", onClick ClearCell ] [ text "Clear cell" ]
            , button [ class "btn btn--danger", onClick ResetPuzzle ] [ text "Reset" ]
            ]
        , div [ class "body sk-body" ]
            [ div [ class "sk-stage" ]
                [ div [ class "sk-grid" ]
                    (List.map (cellView model conflicts selDigit) allIndices)
                , div [ class "sk-side" ]
                    [ statusView solved model
                    , padView
                    , syncView model
                    , rankingView model
                    , solvedView model
                    , helpView
                    ]
                ]
            ]
        ]


difficultySelect : Model -> Html Msg
difficultySelect model =
    select [ class "sk-diff", onInput PickDifficulty ]
        (List.map
            (\d ->
                option
                    [ value (difficultyToString d), selected (d == model.picked) ]
                    [ text (difficultyLabel d) ]
            )
            [ Easy, Medium, Hard, Hardcore ]
        )


onOff : Bool -> String
onOff b =
    if b then
        "on"

    else
        "off"


cellView : Model -> Set Int -> Int -> Int -> Html Msg
cellView model conflicts selDigit i =
    let
        d =
            digitAt model i

        given =
            isGiven model i

        isSel =
            model.selected == Just i

        isPeer =
            case model.selected of
                Just s ->
                    not isSel && (rowOf s == rowOf i || colOf s == colOf i || boxOf s == boxOf i)

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
    div [ class "sk-status-wrap" ]
        [ if solved then
            div [ class "sk-status sk-status--solved" ] [ text "Solved! 🎉" ]

          else
            div [ class "sk-status" ] [ text (difficultyLabel model.difficulty ++ " puzzle") ]
        , div [ class "sk-meta muted" ]
            [ if model.isDaily then
                span [ class "sk-daily-badge" ] [ text ("★ Daily · " ++ model.today) ]

              else
                text (String.fromInt (Dict.size model.givens) ++ " clues")
            ]
        ]


padView : Html Msg
padView =
    div [ class "sk-pad" ]
        (List.map
            (\n -> button [ class "btn sk-key", onClick (PadDigit n) ] [ text (String.fromInt n) ])
            (List.range 1 9)
            ++ [ button [ class "btn sk-key sk-key--wide", onClick ClearCell ] [ text "Clear" ] ]
        )


syncView : Model -> Html Msg
syncView model =
    div [ class "sk-sync" ]
        [ div [ class "sk-sync-status" ] [ syncBadge model.sync ]
        , input
            [ class "sk-sync-url", placeholder "backend URL", value model.serverUrl, onInput SetServerUrl ]
            []
        , div [ class "sk-sync-actions" ]
            [ button [ class "btn btn--ghost", onClick RefreshStats ] [ text "Refresh" ]
            , viewLeaderboardLink model
            ]
        ]


viewLeaderboardLink : Model -> Html Msg
viewLeaderboardLink model =
    if String.trim model.serverUrl == "" then
        text ""

    else
        a [ class "btn btn--ghost", href (apiBase model ++ "/leaderboard"), target "_blank" ]
            [ text "Leaderboard ↗" ]


syncBadge : Sync -> Html Msg
syncBadge sync =
    case sync of
        Idle ->
            span [ class "muted" ] [ text "Solve a puzzle to record your time." ]

        Syncing ->
            span [ class "muted" ] [ text "Syncing…" ]

        Synced count totalMs ->
            span [ class "sk-sync-ok" ]
                [ text ("✔ " ++ String.fromInt count ++ " solved · " ++ fmtDuration totalMs ++ " total") ]

        Failed ->
            span [ class "sk-sync-fail" ] [ text "⚠ Couldn't reach the backend (still saved locally)." ]


rankingView : Model -> Html Msg
rankingView model =
    if List.isEmpty model.ranking then
        text ""

    else
        div [ class "sk-panel" ]
            [ div [ class "sk-panel__title" ] [ text "This puzzle · fastest solvers" ]
            , div [ class "sk-rank-list" ]
                (List.indexedMap
                    (\n r ->
                        div [ classList [ ( "sk-rank-row", True ), ( "is-me", r.player == model.playerId ) ] ]
                            [ span [ class "sk-rank-pos" ] [ text (String.fromInt (n + 1)) ]
                            , span [ class "sk-rank-who kbd" ] [ text (String.left 8 r.player) ]
                            , span [ class "sk-rank-time" ] [ text (fmtDuration r.elapsedMs) ]
                            ]
                    )
                    model.ranking
                )
            ]


solvedView : Model -> Html Msg
solvedView model =
    if List.isEmpty model.solved then
        text ""

    else
        div [ class "sk-panel" ]
            [ div [ class "sk-panel__title" ]
                [ text ("Your solved puzzles (" ++ String.fromInt (List.length model.solved) ++ ")") ]
            , div [ class "sk-solved-list" ]
                (List.map
                    (\s ->
                        div [ class "sk-solved-row" ]
                            [ span [ class "sk-solved-diff" ] [ text (labelOr s.difficulty) ]
                            , span [ class "sk-solved-time" ] [ text (fmtDuration s.elapsedMs) ]
                            ]
                    )
                    (List.take 20 model.solved)
                )
            ]


labelOr : String -> String
labelOr s =
    if s == "" then
        "—"

    else
        String.toUpper (String.left 1 s) ++ String.dropLeft 1 s


helpView : Html Msg
helpView =
    div [ class "sk-help muted" ]
        [ div [] [ span [ class "kbd" ] [ text "1–9" ], text " fill" ]
        , div [] [ span [ class "kbd" ] [ text "⌫" ], text " clear · ", span [ class "kbd" ] [ text "P" ], text " pencil" ]
        , div [] [ span [ class "kbd" ] [ text "↑↓←→" ], text " move" ]
        ]


fmtDuration : Int -> String
fmtDuration ms =
    let
        totalSec =
            ms // 1000

        h =
            totalSec // 3600

        m =
            modBy 60 (totalSec // 60)

        s =
            modBy 60 totalSec

        pad n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    if h > 0 then
        String.fromInt h ++ ":" ++ pad m ++ ":" ++ pad s

    else
        String.fromInt m ++ ":" ++ pad s



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "givens", encodeCells model.givens )
        , ( "solution", encodeCells model.solution )
        , ( "difficulty", E.string (difficultyToString model.difficulty) )
        , ( "picked", E.string (difficultyToString model.picked) )
        , ( "puzzleId", E.string model.puzzleId )
        , ( "isDaily", E.bool model.isDaily )
        , ( "entries", E.dict String.fromInt E.int model.entries )
        , ( "marks", E.dict String.fromInt (\s -> E.list E.int (Set.toList s)) model.marks )
        , ( "startedAt", maybeInt model.startedAt )
        , ( "solvedPosted", E.bool model.solvedPosted )
        , ( "serverUrl", E.string model.serverUrl )
        , ( "playerId", E.string model.playerId )
        ]


encodeCells : Dict Int Int -> E.Value
encodeCells cells =
    E.dict String.fromInt E.int cells


maybeInt : Maybe Int -> E.Value
maybeInt m =
    case m of
        Just n ->
            E.int n

        Nothing ->
            E.null


{-| Applicative `andMap`, so the decoder can restore more fields than `mapN` allows. -}
andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap da df =
    D.map2 (\a f -> f a) da df


decoder : Env -> D.Decoder Model
decoder env =
    let
        ( baseModel, _ ) =
            init env

        build givens solution difficulty picked isDaily entries marks started posted serverUrl playerId =
            let
                withPuzzle =
                    if Dict.isEmpty givens then
                        baseModel

                    else
                        { baseModel
                            | givens = givens
                            , solution = solution
                            , difficulty = difficulty
                            , isDaily = isDaily
                            , puzzleId = puzzleIdOf givens
                        }
            in
            { withPuzzle
                | picked = picked
                , entries = entries
                , marks = marks
                , startedAt = started
                , solvedPosted = posted
                , serverUrl = serverUrl
                , playerId =
                    if playerId == "" then
                        baseModel.playerId

                    else
                        playerId
            }
    in
    D.succeed build
        |> andMap (D.oneOf [ D.field "givens" cellsDecoder, D.succeed Dict.empty ])
        |> andMap (D.oneOf [ D.field "solution" cellsDecoder, D.succeed Dict.empty ])
        |> andMap (D.oneOf [ D.field "difficulty" D.string, D.succeed "easy" ] |> D.map difficultyFromString)
        |> andMap (D.oneOf [ D.field "picked" D.string, D.succeed "easy" ] |> D.map difficultyFromString)
        |> andMap (D.oneOf [ D.field "isDaily" D.bool, D.succeed False ])
        |> andMap (D.oneOf [ D.field "entries" intKeyedDict, D.succeed Dict.empty ])
        |> andMap (D.oneOf [ D.field "marks" intKeyedMarks, D.succeed Dict.empty ])
        |> andMap (D.oneOf [ D.field "startedAt" (D.nullable D.int), D.succeed Nothing ])
        |> andMap (D.oneOf [ D.field "solvedPosted" D.bool, D.succeed False ])
        |> andMap (D.oneOf [ D.field "serverUrl" D.string, D.succeed defaultServerUrl ])
        |> andMap (D.oneOf [ D.field "playerId" D.string, D.succeed "" ])


cellsDecoder : D.Decoder (Dict Int Int)
cellsDecoder =
    D.dict D.int |> D.map toIntKeys


intKeyedDict : D.Decoder (Dict Int Int)
intKeyedDict =
    D.dict D.int |> D.map toIntKeys


intKeyedMarks : D.Decoder (Dict Int (Set Int))
intKeyedMarks =
    D.dict (D.list D.int)
        |> D.map (Dict.map (\_ xs -> Set.fromList xs))
        |> D.map toIntKeys


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
