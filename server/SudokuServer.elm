module SudokuServer exposing (handle)

{-| **Sudoku backend** — an HTTP server that keeps a per-player history of solved games in an H2
database and serves a live leaderboard. Run it with the elm-lang compiler:

    DB_URL="jdbc:h2:file:./sudoku" \
      ../../elm.sh server projects/elm-single-file-apps/server/SudokuServer.elm --db "$DB_URL" --port 8080

(or bundle it — the bundled jar reads the same `DB_URL` environment variable; see server/Dockerfile).

Users are identified by a UUID the Sudoku app mints when its file is first opened and stores in the
file itself (Ctrl+S). The app generates its own puzzles (infinite, seeded, graded easy→hardcore, plus
a date-seeded daily puzzle); a puzzle's `id` is its 81-char givens string, so the same board — the
daily one especially — is the same id for everyone. Each solve is stored in `games` and the puzzle in
`puzzles`, so history + per-puzzle rankings survive restarts and redeploys.

Endpoints (all API responses carry permissive CORS headers via `Server.cors`, so a `file://` Sudoku
page — origin `null` — may call them):

  - `POST /games`  body `{playerId, puzzleId, givens, solution, difficulty, elapsedMs, solvedAt}` →
    stores the puzzle + the solve, returns that player's updated summary.
  - `GET  /games?playerId=…` → that player's summary + recent solves (each with puzzleId + difficulty).
  - `GET  /ranking?puzzleId=…` → everyone who solved that puzzle, best time first (for ordering).
  - `GET  /leaderboard`  → an HTML leaderboard: the 10 players with the newest solves, each with the
    number of games passed and the total time spent solving them.
  - `GET  /health` → `ok` (no database access).

`GET /` (and any other file path) is served from `STATIC_DIR` by the bundle runner *before* this
handler runs — in production that is the Sudoku game itself (server/Dockerfile copies the built
app as `index.html`). So the routes below never see `/`.
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
            -- Every data route ensures the schema first (idempotent + a lightweight migration for the
            -- older single-table version), so the server needs no separate migration step.
            migrate |> andThen (\_ -> route req)


route : Request -> Db Response
route req =
    case ( req.method, segments req ) of
        ( "POST", [ "games" ] ) ->
            postGame req

        ( "GET", [ "games" ] ) ->
            getGames req

        ( "GET", [ "ranking" ] ) ->
            ranking req

        ( "GET", [ "leaderboard" ] ) ->
            leaderboard

        _ ->
            succeed (cors notFound)


{-| Idempotent schema + migration. `games` gains `puzzle_id`/`difficulty` (ADD COLUMN IF NOT EXISTS
handles databases created by the older schema); `puzzles` stores each generated board. -}
migrate : Db ()
migrate =
    ddl
        [ "CREATE TABLE IF NOT EXISTS games (player VARCHAR NOT NULL, puzzle INT, elapsed_ms BIGINT NOT NULL, solved_at BIGINT NOT NULL)"
        , "ALTER TABLE games ADD COLUMN IF NOT EXISTS puzzle_id VARCHAR"
        , "ALTER TABLE games ADD COLUMN IF NOT EXISTS difficulty VARCHAR"
        , "CREATE TABLE IF NOT EXISTS puzzles (id VARCHAR PRIMARY KEY, givens VARCHAR, solution VARCHAR, difficulty VARCHAR, created TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
        ]


ddl : List String -> Db ()
ddl statements =
    case statements of
        [] ->
            succeed ()

        sql :: rest ->
            execute sql [] |> andThen (\_ -> ddl rest)



-- POST /games


type alias Incoming =
    { playerId : String
    , puzzleId : String
    , givens : String
    , solution : String
    , difficulty : String
    , elapsedMs : Int
    , solvedAt : Int
    }


incomingDecoder : D.Decoder Incoming
incomingDecoder =
    D.map7 Incoming
        (D.field "playerId" D.string)
        (D.field "puzzleId" D.string)
        (D.oneOf [ D.field "givens" D.string, D.succeed "" ])
        (D.oneOf [ D.field "solution" D.string, D.succeed "" ])
        (D.oneOf [ D.field "difficulty" D.string, D.succeed "" ])
        (D.field "elapsedMs" D.int)
        (D.field "solvedAt" D.int)


postGame : Request -> Db Response
postGame req =
    case D.decodeString incomingDecoder req.body of
        Ok g ->
            if String.trim g.playerId == "" || String.trim g.puzzleId == "" then
                succeed (cors (errorJson 400 "missing playerId or puzzleId"))

            else
                storePuzzle g
                    |> andThen (\_ -> insertGame g)
                    |> andThen (\_ -> summaryQuery g.playerId)
                    |> map (\s -> cors (json (encodeUser g.playerId s [])))

        Err _ ->
            succeed (cors (errorJson 400 "malformed JSON body"))


{-| Upsert the puzzle itself so we have its givens/solution/difficulty even before anyone else solves
it. `created` (defaulted) is left untouched on update, so it records first-seen. -}
storePuzzle : Incoming -> Db (Result String Int)
storePuzzle g =
    execute
        "MERGE INTO puzzles (id, givens, solution, difficulty) KEY (id) VALUES (?, ?, ?, ?)"
        [ Db.text g.puzzleId, Db.text g.givens, Db.text g.solution, Db.text g.difficulty ]


insertGame : Incoming -> Db (Result String Int)
insertGame g =
    execute
        "INSERT INTO games (player, puzzle, puzzle_id, difficulty, elapsed_ms, solved_at) VALUES (?, 0, ?, ?, ?, ?)"
        [ Db.text g.playerId, Db.text g.puzzleId, Db.text g.difficulty, int g.elapsedMs, int g.solvedAt ]



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
    { puzzleId : String, difficulty : String, elapsedMs : Int, solvedAt : Int }


gameRow : RowDecoder Game
gameRow =
    row Game
        |> andMap textColumn
        |> andMap textColumn
        |> andMap intColumn
        |> andMap intColumn


gamesQuery : String -> Db (List Game)
gamesQuery pid =
    queryWith
        "SELECT COALESCE(puzzle_id, ''), COALESCE(difficulty, ''), elapsed_ms, solved_at FROM games WHERE player = ? ORDER BY solved_at DESC LIMIT 50"
        [ Db.text pid ]
        gameRow
        |> map (Result.withDefault [])



-- GET /ranking?puzzleId=… — everyone who solved this puzzle, fastest first


ranking : Request -> Db Response
ranking req =
    case param "puzzleId" req of
        Just pid ->
            rankingQuery pid |> map (\rs -> cors (json (rankingJson rs)))

        Nothing ->
            succeed (cors (errorJson 400 "missing puzzleId query parameter"))


type alias RankEntry =
    { player : String, elapsedMs : Int, solvedAt : Int }


rankingQuery : String -> Db (List RankEntry)
rankingQuery pid =
    queryWith
        -- Each player's BEST time on this puzzle, fastest first. MIN(BIGINT) stays a BIGINT (unlike
        -- SUM), so no CAST is needed for `intColumn`.
        "SELECT player, MIN(elapsed_ms), MAX(solved_at) FROM games WHERE puzzle_id = ? GROUP BY player ORDER BY MIN(elapsed_ms) ASC LIMIT 100"
        [ Db.text pid ]
        (row RankEntry |> andMap textColumn |> andMap intColumn |> andMap intColumn)
        |> map (Result.withDefault [])


rankingJson : List RankEntry -> String
rankingJson rs =
    E.encode 0
        (E.object
            [ ( "ranking"
              , E.list
                    (\r ->
                        E.object
                            [ ( "player", E.string r.player )
                            , ( "elapsedMs", E.int r.elapsedMs )
                            , ( "solvedAt", E.int r.solvedAt )
                            ]
                    )
                    rs
              )
            ]
        )



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
        [ ( "puzzleId", E.string g.puzzleId )
        , ( "difficulty", E.string g.difficulty )
        , ( "elapsedMs", E.int g.elapsedMs )
        , ( "solvedAt", E.int g.solvedAt )
        ]


errorJson : Int -> String -> Response
errorJson status message =
    response status "application/json" (E.encode 0 (E.object [ ( "error", E.string message ) ]))



-- GET /leaderboard — the HTML leaderboard


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
            ++ " games total · refreshes every 5s · <a href=\"/\">← play Sudoku</a></p></header>"
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
        , "a{color:var(--ac);text-decoration:none}a:hover{text-decoration:underline}"
        , "table{width:100%;border-collapse:collapse;background:var(--card);border:1px solid var(--bd);"
        , "border-radius:12px;overflow:hidden}"
        , "th,td{padding:11px 14px;text-align:left;border-bottom:1px solid var(--bd)}"
        , "th{font-size:12px;text-transform:uppercase;letter-spacing:.05em;color:var(--mut)}"
        , "tbody tr:last-child td{border-bottom:none}"
        , ".num{text-align:right;font-variant-numeric:tabular-nums}"
        , ".rank{color:var(--mut);width:2rem}code{color:var(--ac);font-family:ui-monospace,Menlo,Consolas,monospace}"
        , ".empty{color:var(--mut);text-align:center;padding:26px}"
        ]
