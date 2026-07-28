module SpreadsheetServer exposing (handle)

{-| **Spreadsheet backend** — per-user spreadsheet on the server via the shared `Backend` library: login /
sessions, private/public visibility, per-element sharing, and request auditing (all standard document
routes). The bundled server also serves the Spreadsheet single-file app itself (STATIC_DIR), so
https://spreadsheet.matsuo.pl is both the app and its API.
-}

import Backend exposing (Actor)
import Db exposing (Db)
import Server exposing (Request, Response)


handle : Request -> Db Response
handle =
    Backend.dispatch "spreadsheet" router


router : Actor -> Request -> Db Response
router actor req =
    case Backend.documentRoutes "spreadsheet" actor req of
        Just done ->
            done

        Nothing ->
            Db.succeed (Backend.err 404 "no route")
