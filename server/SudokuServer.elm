module SudokuServer exposing (handle)

{-| **Sudoku backend** — an HTTP server that keeps a per-player history of solved games in an H2
database and serves a live leaderboard. Run it with the elm-lang compiler:

    DB_URL="jdbc:h2:file:./sudoku" \
      ../../elm.sh server projects/elm-single-file-apps/server/SudokuServer.elm --db "$DB_URL" --port 8080

(or bundle it — the bundled jar reads the same `DB_URL` environment variable; see server/Dockerfile).

Users are identified by a UUID the Sudoku app mints when its file is first opened and stores in the
file itself (Ctrl+S). Each solve is POSTed here and appended to the `games` table, so history now
survives restarts and redeploys.

Endpoints (all API responses carry permissive CORS headers via `Server.cors`, so a `file://` Sudoku
page — origin `null` — may call them):

  - `POST /games`  body `{playerId, puzzleIndex, elapsedMs, solvedAt}` → records a solve, returns that
    player's updated summary as JSON.
  - `GET  /games?playerId=…` → that player's summary + recent games as JSON.
  - `GET  /`  → an HTML leaderboard: the 10 players with the newest solves, each with the number of
    games passed and the total time spent solving them.
  - `GET  /health` → `ok` (no database access).
  - `OPTIONS *` → CORS preflight.

The handler is `Request -> Db Response` — a pure description of the SQL to run; the runner executes it
against the JDBC connection and never exposes it to this code. Parameters are bound (`?`), never
spliced, so player input can't be interpreted as SQL.

-}

import Db exposing (Db, RowDecoder, andMap, andThen, execute, int, intColumn, map, map2, queryWith, row, succeed, textColumn)
import Json.Decode as D
import Json.Encode as E
import Server exposing (Request, Response, cors, html, json, notFound, param, response, segments)



-- ROUTING


handle : Request -> Db Response
handle req =
    case ( req.method, segments req ) of
        ( "OPTIONS", _ ) ->
            succeed (cors (response 204 "text/plain" ""))

        ( "GET", [ "health" ] ) ->
            succeed (cors (response 200 "text/plain" "ok"))

        _ ->
            -- Every data route needs the table; creating it here (idempotent, cheap) means the server
            -- needs no separate migration step.
            execute createTableSql []
                |> andThen (\_ -> route req)


route : Request -> Db Response
route req =
    case ( req.method, segments req ) of
        ( "POST", [ "games" ] ) ->
            postGame req

        ( "GET", [ "games" ] ) ->
            getGames req

        ( "GET", [] ) ->
            leaderboard

        _ ->
            succeed (cors notFound)


createTableSql : String
createTableSql =
    "CREATE TABLE IF NOT EXISTS games "
        ++ "(player VARCHAR NOT NULL, puzzle INT NOT NULL, elapsed_ms BIGINT NOT NULL, solved_at BIGINT NOT NULL)"



-- POST /games


type alias Incoming =
    { playerId : String
    , puzzleIndex : Int
    , elapsedMs : Int
    , solvedAt : Int
    }


incomingDecoder : D.Decoder Incoming
incomingDecoder =
    D.map4 Incoming
        (D.field "playerId" D.string)
        (D.field "puzzleIndex" D.int)
        (D.field "elapsedMs" D.int)
        (D.field "solvedAt" D.int)


postGame : Request -> Db Response
postGame req =
    case D.decodeString incomingDecoder req.body of
        Ok g ->
            if String.trim g.playerId == "" then
                succeed (cors (errorJson 400 "missing playerId"))

            else
                execute
                    "INSERT INTO games (player, puzzle, elapsed_ms, solved_at) VALUES (?, ?, ?, ?)"
                    [ Db.text g.playerId, int g.puzzleIndex, int g.elapsedMs, int g.solvedAt ]
                    |> andThen (\_ -> summaryQuery g.playerId)
                    |> map (\s -> cors (json (encodeUser g.playerId s [])))

        Err _ ->
            succeed (cors (errorJson 400 "malformed JSON body"))



-- GET /games?playerId=…


getGames : Request -> Db Response
getGames req =
    case param "playerId" req of
        Just pid ->
            map2 (\s games -> cors (json (encodeUser pid s games)))
                (summaryQuery pid)
                (gamesQuery pid)

        Nothing ->
            succeed (cors (errorJson 400 "missing playerId query parameter"))



-- QUERIES


type alias Summary =
    { count : Int, totalMs : Int, lastUpdate : Int }


summaryRow : RowDecoder Summary
summaryRow =
    row Summary
        |> andMap intColumn
        |> andMap intColumn
        |> andMap intColumn


summaryQuery : String -> Db Summary
summaryQuery pid =
    queryWith
        -- CAST the SUM to BIGINT: H2's SUM(BIGINT) yields a DECIMAL, which the runner decodes as a
        -- real, not an int — the cast keeps the column an integer so `intColumn` matches it.
        "SELECT COUNT(*), CAST(COALESCE(SUM(elapsed_ms), 0) AS BIGINT), COALESCE(MAX(solved_at), 0) FROM games WHERE player = ?"
        [ Db.text pid ]
        summaryRow
        |> map (\res -> Maybe.withDefault (Summary 0 0 0) (List.head (Result.withDefault [] res)))


type alias Game =
    { puzzleIndex : Int, elapsedMs : Int, solvedAt : Int }


gameRow : RowDecoder Game
gameRow =
    row Game
        |> andMap intColumn
        |> andMap intColumn
        |> andMap intColumn


gamesQuery : String -> Db (List Game)
gamesQuery pid =
    queryWith
        "SELECT puzzle, elapsed_ms, solved_at FROM games WHERE player = ? ORDER BY solved_at DESC LIMIT 50"
        [ Db.text pid ]
        gameRow
        |> map (Result.withDefault [])



-- JSON


encodeUser : String -> Summary -> List Game -> String
encodeUser pid s games =
    E.encode 0
        (E.object
            [ ( "playerId", E.string pid )
            , ( "count", E.int s.count )
            , ( "totalMs", E.int s.totalMs )
            , ( "lastUpdate", E.int s.lastUpdate )
            , ( "games", E.list encodeGame games )
            ]
        )


encodeGame : Game -> E.Value
encodeGame g =
    E.object
        [ ( "puzzleIndex", E.int g.puzzleIndex )
        , ( "elapsedMs", E.int g.elapsedMs )
        , ( "solvedAt", E.int g.solvedAt )
        ]


errorJson : Int -> String -> Response
errorJson status message =
    response status "application/json" (E.encode 0 (E.object [ ( "error", E.string message ) ]))



-- GET / — the HTML leaderboard


type alias Rank =
    { player : String, count : Int, totalMs : Int, lastUpdate : Int }


rankRow : RowDecoder Rank
rankRow =
    row Rank
        |> andMap textColumn
        |> andMap intColumn
        |> andMap intColumn
        |> andMap intColumn


leaderboard : Db Response
leaderboard =
    map2 (\( players, games ) ranks -> cors (html (leaderboardPage players games ranks)))
        totalsQuery
        ranksQuery


totalsQuery : Db ( Int, Int )
totalsQuery =
    queryWith "SELECT COUNT(DISTINCT player), COUNT(*) FROM games"
        []
        (row Tuple.pair |> andMap intColumn |> andMap intColumn)
        |> map (\res -> Maybe.withDefault ( 0, 0 ) (List.head (Result.withDefault [] res)))


ranksQuery : Db (List Rank)
ranksQuery =
    queryWith
        ("SELECT player, COUNT(*), CAST(COALESCE(SUM(elapsed_ms), 0) AS BIGINT), COALESCE(MAX(solved_at), 0) "
            ++ "FROM games GROUP BY player ORDER BY MAX(solved_at) DESC LIMIT 10"
        )
        []
        rankRow
        |> map (Result.withDefault [])


leaderboardPage : Int -> Int -> List Rank -> String
leaderboardPage players totalGames ranks =
    let
        rowsHtml =
            if List.isEmpty ranks then
                "<tr><td colspan=\"5\" class=\"empty\">No games recorded yet — solve a puzzle in the Sudoku app.</td></tr>"

            else
                String.concat (List.indexedMap rankHtml ranks)
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
            ++ String.fromInt players
            ++ " players · "
            ++ String.fromInt totalGames
            ++ " games total · refreshes every 5s</p></header>"
        , "<table><thead><tr><th>#</th><th>Player</th><th class=\"num\">Games</th>"
        , "<th class=\"num\">Total solve time</th><th>Last update</th></tr></thead>"
        , "<tbody>" ++ rowsHtml ++ "</tbody></table>"
        , "</main></body></html>"
        ]


rankHtml : Int -> Rank -> String
rankHtml i r =
    String.concat
        [ "<tr><td class=\"rank\">"
        , String.fromInt (i + 1)
        , "</td><td><code>"
        , String.left 8 r.player
        , "</code></td><td class=\"num\">"
        , String.fromInt r.count
        , "</td><td class=\"num\">"
        , fmtDuration r.totalMs
        , "</td><td>"
        , fmtUtc r.lastUpdate
        , "</td></tr>"
        ]



-- FORMATTING (pure)


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
                (if z >= 0 then
                    z

                 else
                    z - 146096
                )
                    // 146097

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
                mp
                    + (if mp < 10 then
                        3

                       else
                        -9
                      )

            year =
                y
                    + (if month <= 2 then
                        1

                       else
                        0
                      )

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
