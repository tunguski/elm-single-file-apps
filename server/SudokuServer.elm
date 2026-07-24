module SudokuServer exposing (main)

{-| **Sudoku backend** — a stateful HTTP server that keeps a per-user history of solved games and
serves a live leaderboard. Run it with the elm-lang compiler:

    ../../elm.sh server projects/elm-single-file-apps/server/SudokuServer.elm --port 8080

Users are identified by a UUID the Sudoku app mints when its file is first opened and stores in the
file itself (Ctrl+S). Each solve is POSTed here; the server accumulates games, total solve time and
the last-update time per user, all in memory.

Endpoints:

  - `POST /games`  body `{playerId, puzzleIndex, elapsedMs, solvedAt}` → records a solved game,
    returns that user's updated summary as JSON.
  - `GET  /games?playerId=…` → that user's summary + recent games as JSON.
  - `GET  /`  → an HTML leaderboard: the 10 users with the newest updates, each with the number of
    games passed and the total time spent solving them.
  - `OPTIONS *` → CORS preflight (so a `file://` Sudoku page, whose origin is `null`, may call it).

All API responses carry permissive CORS headers via `Server.cors`.

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Server exposing (Request, Response, cors, html, json, notFound, param, response, segments)



-- MODEL


type alias Game =
    { puzzleIndex : Int
    , elapsedMs : Int
    , solvedAt : Int -- client wall-clock epoch ms (the server handler is pure — no clock of its own)
    }


type alias User =
    { games : List Game -- newest first, capped
    , count : Int
    , totalMs : Int
    , lastUpdate : Int
    }


type alias Model =
    Dict String User


emptyUser : User
emptyUser =
    { games = [], count = 0, totalMs = 0, lastUpdate = 0 }


addGame : Game -> User -> User
addGame g u =
    { games = List.take 50 (g :: u.games)
    , count = u.count + 1
    , totalMs = u.totalMs + g.elapsedMs
    , lastUpdate = max u.lastUpdate g.solvedAt
    }


main : Server.Program Model
main =
    Server.program
        { init = Dict.empty
        , onRequest = onRequest
        , onTick = identity
        , tickMillis = 0
        }


onRequest : Request -> Model -> ( Model, Response )
onRequest req model =
    if req.method == "OPTIONS" then
        ( model, cors (response 204 "text/plain" "") )

    else
        case ( req.method, segments req ) of
            ( "POST", [ "games" ] ) ->
                postGame req model

            ( "GET", [ "games" ] ) ->
                ( model, cors (getGames req model) )

            ( "GET", [] ) ->
                ( model, cors (html (leaderboardPage model)) )

            ( "GET", [ "health" ] ) ->
                ( model, cors (Server.text "ok") )

            _ ->
                ( model, cors notFound )



-- POST /games


type alias Incoming =
    { playerId : String, game : Game }


incomingDecoder : D.Decoder Incoming
incomingDecoder =
    D.map4
        (\pid pi el sa ->
            { playerId = pid, game = { puzzleIndex = pi, elapsedMs = el, solvedAt = sa } }
        )
        (D.field "playerId" D.string)
        (D.field "puzzleIndex" D.int)
        (D.field "elapsedMs" D.int)
        (D.field "solvedAt" D.int)


postGame : Request -> Model -> ( Model, Response )
postGame req model =
    case D.decodeString incomingDecoder req.body of
        Ok { playerId, game } ->
            if String.trim playerId == "" then
                ( model, cors (errorJson 400 "missing playerId") )

            else
                let
                    updated =
                        addGame game (Maybe.withDefault emptyUser (Dict.get playerId model))
                in
                ( Dict.insert playerId updated model
                , cors (json (E.encode 0 (encodeUser playerId updated)))
                )

        Err _ ->
            ( model, cors (errorJson 400 "malformed JSON body") )


errorJson : Int -> String -> Response
errorJson status message =
    response status "application/json" (E.encode 0 (E.object [ ( "error", E.string message ) ]))



-- GET /games?playerId=…


getGames : Request -> Model -> Response
getGames req model =
    case param "playerId" req of
        Just pid ->
            json (E.encode 0 (encodeUser pid (Maybe.withDefault emptyUser (Dict.get pid model))))

        Nothing ->
            errorJson 400 "missing playerId query parameter"


encodeUser : String -> User -> E.Value
encodeUser pid u =
    E.object
        [ ( "playerId", E.string pid )
        , ( "count", E.int u.count )
        , ( "totalMs", E.int u.totalMs )
        , ( "lastUpdate", E.int u.lastUpdate )
        , ( "games", E.list encodeGame u.games )
        ]


encodeGame : Game -> E.Value
encodeGame g =
    E.object
        [ ( "puzzleIndex", E.int g.puzzleIndex )
        , ( "elapsedMs", E.int g.elapsedMs )
        , ( "solvedAt", E.int g.solvedAt )
        ]



-- GET / — the HTML leaderboard


leaderboardPage : Model -> String
leaderboardPage model =
    let
        ranked =
            Dict.toList model
                |> List.sortBy (\( _, u ) -> negate u.lastUpdate)
                |> List.take 10

        totalGames =
            List.sum (List.map (\( _, u ) -> u.count) (Dict.toList model))

        rowsHtml =
            if List.isEmpty ranked then
                "<tr><td colspan=\"5\" class=\"empty\">No games recorded yet — solve a puzzle in the Sudoku app.</td></tr>"

            else
                String.concat (List.indexedMap row ranked)
    in
    String.join "\n"
        [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        , "<meta http-equiv=\"refresh\" content=\"5\">"
        , "<title>Sudoku · leaderboard</title>"
        , "<style>" ++ pageCss ++ "</style></head><body>"
        , "<main class=\"wrap\">"
        , "<header><h1>🔢 Sudoku leaderboard</h1>"
        , "<p class=\"sub\">The 10 players with the most recent solves · "
            ++ String.fromInt (Dict.size model)
            ++ " players · "
            ++ String.fromInt totalGames
            ++ " games total · refreshes every 5s</p></header>"
        , "<table><thead><tr><th>#</th><th>Player</th><th class=\"num\">Games</th>"
        , "<th class=\"num\">Total solve time</th><th>Last update</th></tr></thead>"
        , "<tbody>" ++ rowsHtml ++ "</tbody></table>"
        , "</main></body></html>"
        ]


row : Int -> ( String, User ) -> String
row i ( pid, u ) =
    String.concat
        [ "<tr><td class=\"rank\">"
        , String.fromInt (i + 1)
        , "</td><td><code>"
        , shortId pid
        , "</code></td><td class=\"num\">"
        , String.fromInt u.count
        , "</td><td class=\"num\">"
        , fmtDuration u.totalMs
        , "</td><td>"
        , fmtUtc u.lastUpdate
        , "</td></tr>"
        ]


shortId : String -> String
shortId pid =
    String.left 8 pid



-- FORMATTING (pure — the handler has no clock, so timestamps come from the client)


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
        String.fromInt h ++ "h " ++ pad m ++ "m " ++ pad s ++ "s"

    else if m > 0 then
        String.fromInt m ++ "m " ++ pad s ++ "s"

    else
        String.fromInt s ++ "s"


{-| Format an epoch-ms instant as "YYYY-MM-DD HH:MM UTC" using pure integer date arithmetic
(Howard Hinnant's civil-from-days), so the server needs no time zone / clock support.
-}
fmtUtc : Int -> String
fmtUtc ms =
    if ms <= 0 then
        "—"

    else
        let
            days =
                ms // 86400000

            rem =
                modBy 86400000 ms

            hh =
                rem // 3600000

            mm =
                modBy 60 (rem // 60000)

            z =
                days + 719468

            era =
                (if z >= 0 then z else z - 146096) // 146097

            doe =
                z - era * 146097

            yoe =
                (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365

            y =
                yoe + era * 400

            doy =
                doe - (365 * yoe + yoe // 4 - yoe // 100)

            mp =
                (5 * doy + 2) // 153

            d =
                doy - (153 * mp + 2) // 5 + 1

            month =
                mp + (if mp < 10 then 3 else -9)

            year =
                y + (if month <= 2 then 1 else 0)

            pad n =
                String.padLeft 2 '0' (String.fromInt n)
        in
        String.fromInt year
            ++ "-"
            ++ pad month
            ++ "-"
            ++ pad d
            ++ " "
            ++ pad hh
            ++ ":"
            ++ pad mm
            ++ " UTC"


pageCss : String
pageCss =
    String.join ""
        [ ":root{color-scheme:light dark;--bg:#0f141b;--card:#171d26;--bd:#2a3340;--tx:#e6edf3;--mut:#93a0b1;--ac:#5b8cff}"
        , "*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--tx);"
        , "font:15px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif}"
        , ".wrap{max-width:760px;margin:0 auto;padding:48px 20px}"
        , "h1{font-size:26px;margin:0 0 6px}.sub{color:var(--mut);margin:0 0 26px;font-size:13.5px}"
        , "table{width:100%;border-collapse:collapse;background:var(--card);border:1px solid var(--bd);"
        , "border-radius:12px;overflow:hidden}"
        , "th,td{padding:11px 14px;text-align:left;border-bottom:1px solid var(--bd)}"
        , "th{font-size:12px;text-transform:uppercase;letter-spacing:.05em;color:var(--mut)}"
        , "tbody tr:last-child td{border-bottom:none}"
        , ".num{text-align:right;font-variant-numeric:tabular-nums}"
        , ".rank{color:var(--mut);width:2rem}code{color:var(--ac);font-family:ui-monospace,Menlo,Consolas,monospace}"
        , ".empty{color:var(--mut);text-align:center;padding:26px}"
        ]
