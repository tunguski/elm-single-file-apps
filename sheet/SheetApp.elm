module SheetApp exposing (main)

{-| **Spreadsheet** — the real [elm-spreadsheet](https://github.com/tunguski/elm-spreadsheet) engine
and grid as a self-saving single-file app.

The heavy lifting is entirely reused: [`SheetDoc.config`](SheetDoc) is the workspace document for a
single spreadsheet — its `codec` (JSON), `empty`/`activate` (create + recalc), `updateDoc` (pure) and
`viewDoc` (the full grid). We just drive that config through the shared [`App`](App) harness so the
sheet's cells live in the file itself and Ctrl+S writes them back.

The workspace *site* shell (managing many spreadsheets in localStorage) is intentionally dropped —
file-save is what replaces it here: one file _is_ one spreadsheet.

-}

import App exposing (Env)
import Dict
import Html exposing (Html)
import Json.Decode as D
import SheetDoc exposing (SheetDoc, SheetMsg)
import Workspace


main : Program () (App.State SheetDoc) (App.Event SheetMsg)
main =
    App.program
        { init = \_ -> SheetDoc.config.activate SheetDoc.config.empty
        , update = SheetDoc.config.updateDoc
        , view = view
        , subscriptions = \_ -> Sub.none
        , encode = SheetDoc.config.codec.encode
        , decoder = \_ -> D.map SheetDoc.config.activate SheetDoc.config.codec.decoder
        }


{-| A minimal editor environment: comments and cross-document references are workspace features that
don't apply to a standalone file, so they're switched off. With `commentsVisible = False`, the grid
never renders the comment marker (the one place the document view would want an icon font).
-}
editorEnv : Workspace.EditorEnv
editorEnv =
    { comments = Dict.empty
    , commentsVisible = False
    , commentCount = \_ -> 0
    , refError = Nothing
    , refWarnings = []
    }


view : SheetDoc -> Html SheetMsg
view doc =
    SheetDoc.config.viewDoc editorEnv doc
