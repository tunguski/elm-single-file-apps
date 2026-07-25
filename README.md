# elm-single-file-apps

A showcase of **single-file applications** written in [Elm](https://elm-lang.org) and compiled with
the [elm-lang](https://github.com/tunguski/elm-lang) compiler. Each app is one self-contained HTML
file that **stores its own data inside itself** — you edit, then press <kbd>Ctrl</kbd>+<kbd>S</kbd>
and the page writes a fresh copy of itself back to disk with your data embedded. No server, no
database, no `localStorage` — the file _is_ the document (the TiddlyWiki idea, in Elm).

**Live:** https://tunguski.github.io/elm-single-file-apps/

## The apps

| App | What it is |
| --- | --- |
| 🗒️ **Notes** | A two-pane note keeper with search. |
| ✅ **Todos** | A to-do list with filters, inline edit and a live remaining count. |
| 📚 **Wiki** | Interlinked plain-text pages with `[[wiki-links]]` that create pages on click. |
| 🔢 **Sudoku** | Playable Sudoku — keyboard entry, pencil marks, conflict highlighting, a puzzle bank. Optionally syncs solved games to the [Elm backend](#optional-backend--the-sudoku-history-server). |
| 📅 **Calendar** | A month view with events; all date math is pure (no `Time` effects). |
| 📊 **Spreadsheet** | The real [elm-spreadsheet](https://github.com/tunguski/elm-spreadsheet) engine (~120 formula functions) driven through the same harness. |

## How the self-saving works

Every app is built on a tiny shared harness ([`shared/App.elm`](shared/App.elm) +
[`assets/harness.js`](assets/harness.js)) connected by two ports:

1. **Load** — the built file carries a `<script id="app-data">` block (base64-encoded JSON). On boot
   the JS harness reads it and sends it into Elm as a small envelope `{ today, saved }`.
2. **Mirror** — after every change Elm hands its freshly-serialized state back out; the harness keeps
   it and mirrors it into the data block, so the live DOM always carries current data.
3. **Save** — on <kbd>Ctrl</kbd>+<kbd>S</kbd> the harness serializes the whole page (with an emptied
   mount, since the runtime appends on boot) and writes it back:
   - **In place** via the [File System Access API](https://developer.mozilla.org/en-US/docs/Web/API/File_System_Access_API)
     in Chrome & Edge — the same file is overwritten.
   - **As a download** everywhere else (Firefox, Safari, `file://`) — you save over the original.

   A small badge shows unsaved changes and confirms saves; closing with unsaved changes warns you.

Because everything is inlined, **every app already works fully offline** — open it straight from
disk. Served over HTTPS, each app also ships a web-app manifest + icon, so it's an **installable
PWA**. (A true offline service worker isn't used or needed: a single inlined file has nothing to
cache, and service workers don't run from `file://` anyway.)

An app never touches ports itself — it provides a pure `Spec` and calls `App.program`:

```elm
main : Program () (App.State Model) (App.Event Msg)
main =
    App.program
        { init = init            -- Env -> Model   (Env carries today's date)
        , update = update        -- Msg -> Model -> Model   (pure; saving is automatic)
        , view = view
        , subscriptions = \_ -> Sub.none
        , encode = encode        -- Model -> Json.Encode.Value
        , decoder = decoder      -- Env -> Json.Decode.Decoder Model
        }
```

## Building

Requires the elm-lang compiler in the parent repo (this project lives in `elm-lang/projects/`). From
this directory:

```bash
ELM=../../elm.sh ./build.sh            # build every app + the showcase into build/
ELM=../../elm.sh ./build.sh notes      # build a single app by id
```

Outputs land in `build/` as standalone `.html` files, plus `build/index.html` (the showcase
gallery, generated from the app registry in `build.sh`).

### Headless smoke test

Each build can be booted in [jsdom](https://github.com/jsdom/jsdom) to prove it mounts, its ports
work, and its data round-trips:

```bash
npm i jsdom
node tools/verify.mjs build/notes.html
node tools/verify.mjs build/notes.html --seed '{"notes":[{"id":1,"title":"Hi","body":"x"}],"nextId":2}'
```

The CI workflow ([`.github/workflows/pages.yml`](.github/workflows/pages.yml)) runs this over all six
apps and refuses to deploy if any fails to boot.

## Optional backend — the Sudoku history server

The Sudoku app can sync solved games to a small **HTTP server also written in Elm**
([`server/SudokuServer.elm`](server/SudokuServer.elm)), which keeps a per-player history and serves a
live leaderboard. This is the one hybrid piece: the apps stay offline-first, and sync is best-effort
on top.

**Identity lives in the file.** When a Sudoku file is first opened it mints a UUID
([`App.Env`](shared/App.elm)`.newId`, from `crypto.randomUUID`) and stores it in the document. Because
it's persisted, your identity survives **Ctrl+S** — but a file you never save gets a fresh UUID each
open (a new identity), exactly as intended.

**Run it:**

```bash
ELM=../../elm.sh ./server/run.sh          # serves on http://localhost:8080
# or: ../../elm.sh server server/SudokuServer.elm --port 8080
```

Open the Sudoku app, confirm the backend URL in its sync panel (defaults to `http://localhost:8080`),
and solve a puzzle — the app times the solve and POSTs it. Visit `http://localhost:8080/` for the
leaderboard: the 10 players with the newest solves, each with games passed and total solve time.

**Endpoints** (all API responses carry permissive CORS headers, so a `file://` page — whose origin is
`null` — can call them):

| Method | Path | Purpose |
| --- | --- | --- |
| `POST` | `/games` | record a solve `{playerId, puzzleIndex, elapsedMs, solvedAt}` |
| `GET`  | `/games?playerId=…` | that player's summary + recent games (JSON) |
| `GET`  | `/` | HTML leaderboard (auto-refreshes) |
| `OPTIONS` | `*` | CORS preflight |

State is in-memory (per server run). Restarting the server clears history — swap the `Dict` for a
persisted store (e.g. the compiler's `Db` library via `--db`) if you want durability.

**Local run** defaults to `http://localhost:8080`; the deployed client defaults to
`https://sudoku.matsuo.pl` (change it in the app's sync panel to point elsewhere).

### Deployment (`https://sudoku.matsuo.pl`)

The backend deploys as a container onto the same VPS as the other `*.matsuo.pl` apps (an
`nginx-proxy` + `acme-companion` setup: a container just declares `VIRTUAL_HOST` and gets routed with
an auto-issued TLS cert). Two moving parts:

1. **This repo** builds the image. [`.github/workflows/deploy-server.yml`](.github/workflows/deploy-server.yml)
   builds `elm.jar`, runs `elm bundle server server/SudokuServer.elm --port 8080` to produce a
   standalone `sudoku.jar`, and pushes `ghcr.io/tunguski/elm-single-file-apps/sudoku-server:latest`
   (via [`server/Dockerfile`](server/Dockerfile), a JRE + that jar).
2. **The `projects-aggregator` repo** runs it: a `sudoku` service in `containers/matsuo/docker-compose.yml`
   pulls that image with `VIRTUAL_HOST=sudoku.matsuo.pl` / `LETSENCRYPT_HOST=sudoku.matsuo.pl`.

**One-time setup:** point DNS `sudoku.matsuo.pl A → <vps-ip>`, ensure the GHCR image is public (or the
VPS is logged in to GHCR), then `podman compose up -d sudoku` (or the VPS's usual pull/redeploy) in
`containers/matsuo/`. nginx-proxy then routes it and acme-companion issues the cert on first request.

**Compiler features this uses.** Building this required two additions to the elm-lang compiler, both
in the parent repo:

- `Server.Response` gained a `headers` field plus `withHeaders` / `cors` helpers
  (`src/main/elm/lib/Server.elm` + `ServerRunner.java`) — needed to emit CORS headers.
- A latent bug was fixed in the JS runtime: `Json.Decode.int` clamped decoded integers to 32 bits
  (`j|0`), corrupting epoch-millisecond timestamps; it now uses `Math.trunc`
  (`src/main/resources/elm/js/dom.js`).

> ⚠️ The deploy workflow builds the client apps from `tunguski/elm-lang@master`; the **client apps**
> don't need these changes, but the **Sudoku↔server sync** does. Push those compiler changes to
> `elm-lang` before relying on the hosted build for the backend feature.

### Headless end-to-end test

With a server running, [`tools/e2e.mjs`](tools/e2e.mjs) boots the built Sudoku app in jsdom, seeds a
game one cell from solved, types the last digit, and asserts the backend recorded the solve with a
sane elapsed time:

```bash
ELM=../../elm.sh PORT=8097 ./server/run.sh &      # start a server
node tools/e2e.mjs build/sudoku.html http://localhost:8097
```

## Layout

```
shared/App.elm            the framework: ports + the App.program builder
src/*.elm                 the five simple apps (lean root project, no vendored deps)
src/*.css                 per-app styles (theme-aware, on top of assets/app.css)
sheet/SheetApp.elm        the spreadsheet, driving SheetDoc.config
sheet/elm.json            the spreadsheet's own project…
sheet/elm.vendored.json   …with elm-spreadsheet + elm-workspace as git source deps
server/SudokuServer.elm   the optional per-player history backend (an Elm HTTP server)
server/run.sh             starts the backend
assets/app.css            the shared design system (light/dark)
assets/harness.js         the load / mirror / Ctrl+S-save JS harness (+ UUID / now in the envelope)
assets/showcase.html      the gallery template ( <!--CARDS--> is filled from the registry )
tools/inject.pl           post-processor: inlines CSS + harness, adds the data block + PWA manifest
tools/verify.mjs          headless boot / round-trip / e2e test harnesses (jsdom)
build.sh                  compiles each app and post-processes it into a single file
```

The spreadsheet keeps its own project (`sheet/`) so its heavy vendored engine is bundled only into
the spreadsheet build — the compiler is whole-program, so mixing it into the root project would bloat
every app. That's why the five simple apps are ~190 KB while the full spreadsheet is ~820 KB.

## Adding an app

1. Write `src/YourApp.elm` exposing `main` via `App.program` (see any existing app).
2. Optionally add `src/yourid.css`.
3. Add one row to the `APPS` registry in `build.sh`: `id|Module|Title|#color|emoji|subtitle`.
4. `ELM=../../elm.sh ./build.sh yourid` and verify with `tools/verify.mjs`.

The showcase card and the PWA manifest/icon are generated from that registry row automatically.
