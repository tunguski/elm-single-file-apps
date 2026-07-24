port module App exposing (Env, Spec, EffectSpec, State, Event, program, programEffect)

{-| The shared framework behind every single-file app in this showcase.

Each app is a self-contained HTML file that stores its own data _inside itself_. This module
is the seam between the pure Elm app and the tiny JS harness (`assets/harness.js`) that loads
that embedded data on boot and writes it back to disk on **Ctrl+S**.

The contract is two ports:

  - `load` (JS → Elm) — on boot the harness sends a small JSON envelope holding today's date and
    the previously-saved document (or `null` on a fresh, never-saved file).
  - `save` (Elm → JS) — after every change the app hands the harness its freshly-serialized state,
    which the harness mirrors into the page's `<script id="app-data">` block. Ctrl+S then just
    writes the page back out (in place via the File System Access API, or as a download).

An app therefore never touches ports directly: it provides a pure `Spec` (init/update/view plus a
JSON codec) and calls [`program`](#program).

-}

import Browser
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E



-- PORTS ----------------------------------------------------------------------


{-| Elm → JS: the current document, serialized. Sent after load and after every change so the
harness always holds the latest state to write on Ctrl+S.
-}
port save : String -> Cmd msg


{-| JS → Elm: the boot envelope (a JSON string). See [`Env`](#Env).
-}
port load : (String -> msg) -> Sub msg



-- SPEC -----------------------------------------------------------------------


{-| Boot-time facts the harness knows but Elm can't compute from scratch:

  - `today` — the local date "YYYY-MM-DD" (date-aware apps like the calendar avoid `Time` wiring).
  - `now` — the boot instant, epoch milliseconds (a reference clock without a `Time.now` round-trip).
  - `newId` — a freshly minted UUID, generated on every open. An app that wants a stable identity
    keeps its own copy in the saved document and adopts `newId` only when it has none yet — so the
    identity lives in the file and survives Ctrl+S, but a never-saved file gets a new one each open.

-}
type alias Env =
    { today : String
    , now : Int
    , newId : String
    }


{-| Everything one app must provide. All pure — the framework owns the effects.

  - `init` — the empty document, given the boot `Env`.
  - `update` / `view` — the usual, but `update` returns just a model (no `Cmd`; saving is automatic).
  - `subscriptions` — the app's own subs (keyboard, etc.); `always Sub.none` if it has none.
  - `encode` / `decoder` — how the document round-trips through the embedded data block.

-}
type alias Spec model msg =
    { init : Env -> model
    , update : msg -> model -> model
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    , encode : model -> E.Value
    , decoder : Env -> D.Decoder model
    }


{-| Like [`Spec`](#Spec) but effectful: `init`/`update` return `( model, Cmd msg )`, so the app can
run real effects (HTTP, `Time.now`, …) alongside the automatic save. Use [`programEffect`](#programEffect).
The app's `Cmd`s are batched with the framework's save command; the app never sees the ports.
-}
type alias EffectSpec model msg =
    { init : Env -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    , encode : model -> E.Value
    , decoder : Env -> D.Decoder model
    }



-- FRAMEWORK ------------------------------------------------------------------


{-| The framework's own model, wrapping the app's `model`. Named so apps can spell their `main`
signature: `Program () (App.State Model) (App.Event Msg)`.
-}
type alias State model =
    { model : model
    , loaded : Bool
    }


{-| The framework's own message, wrapping the app's `msg`.
-}
type Event msg
    = AppMsg msg
    | Load String


{-| Turn a pure app [`Spec`](#Spec) into a runnable program with the save/load harness wired in.
-}
program : Spec model msg -> Program () (State model) (Event msg)
program spec =
    build
        { init = \env -> ( spec.init env, Cmd.none )
        , update = \m model -> ( spec.update m model, Cmd.none )
        , view = spec.view
        , subscriptions = spec.subscriptions
        , encode = spec.encode
        , decoder = spec.decoder
        }


{-| Turn an effectful app [`EffectSpec`](#EffectSpec) into a runnable program.
-}
programEffect : EffectSpec model msg -> Program () (State model) (Event msg)
programEffect spec =
    build spec


defaultEnv : Env
defaultEnv =
    { today = "", now = 0, newId = "" }


build : EffectSpec model msg -> Program () (State model) (Event msg)
build spec =
    Browser.element
        { init =
            \_ ->
                let
                    ( model, cmd ) =
                        spec.init defaultEnv
                in
                ( { model = model, loaded = False }, Cmd.map AppMsg cmd )
        , update = wrapUpdate spec
        , view = \w -> Html.map AppMsg (spec.view w.model)
        , subscriptions =
            \w -> Sub.batch [ load Load, Sub.map AppMsg (spec.subscriptions w.model) ]
        }


wrapUpdate : EffectSpec model msg -> Event msg -> State model -> ( State model, Cmd (Event msg) )
wrapUpdate spec msg w =
    case msg of
        AppMsg m ->
            let
                ( next, cmd ) =
                    spec.update m w.model
            in
            ( { w | model = next }
            , Cmd.batch [ Cmd.map AppMsg cmd, saveCmd spec next ]
            )

        Load envJson ->
            let
                ( env, saved ) =
                    case D.decodeString envelopeDecoder envJson of
                        Ok pair ->
                            pair

                        Err _ ->
                            ( defaultEnv, Nothing )

                ( base, initCmd ) =
                    spec.init env

                model =
                    case saved of
                        Just value ->
                            case D.decodeValue (spec.decoder env) value of
                                Ok restored ->
                                    restored

                                Err _ ->
                                    base

                        Nothing ->
                            base
            in
            ( { model = model, loaded = True }
            , Cmd.batch [ Cmd.map AppMsg initCmd, saveCmd spec model ]
            )


saveCmd : EffectSpec model msg -> model -> Cmd (Event msg)
saveCmd spec model =
    save (E.encode 0 (spec.encode model))


envelopeDecoder : D.Decoder ( Env, Maybe D.Value )
envelopeDecoder =
    D.map4 (\today now newId saved -> ( { today = today, now = now, newId = newId }, saved ))
        (D.oneOf [ D.field "today" D.string, D.succeed "" ])
        (D.oneOf [ D.field "now" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "newId" D.string, D.succeed "" ])
        (D.oneOf [ D.field "saved" (D.nullable D.value), D.succeed Nothing ])
