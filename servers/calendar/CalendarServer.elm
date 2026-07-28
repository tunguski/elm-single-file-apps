module CalendarServer exposing (handle)

{-| **Calendar backend** — per-user calendar on the server via the shared `Backend` library: login /
sessions, private/public visibility, per-element sharing, and request auditing (all standard document
routes). The bundled server also serves the Calendar single-file app itself (STATIC_DIR), so
https://calendar.matsuo.pl is both the app and its API.
-}

import Backend exposing (Actor)
import Db exposing (Db)
import Server exposing (Request, Response)


handle : Request -> Db Response
handle =
    Backend.dispatch "calendar" router


router : Actor -> Request -> Db Response
router actor req =
    case Backend.documentRoutes "calendar" actor req of
        Just done ->
            done

        Nothing ->
            Db.succeed (Backend.err 404 "no route")
