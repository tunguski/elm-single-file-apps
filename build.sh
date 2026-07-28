#!/usr/bin/env bash
#
# build.sh — compile each single-file app to a standalone, self-saving, installable HTML file.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed to
# `make` is absolute (computed after we cd into this script's own dir). Each app type-checks clean
# (no --no-check). Post-processing (tools/inject.pl) inlines the shared CSS + save harness, injects
# the embedded data block, and adds a PWA manifest + icon so the page is installable when hosted.
#
#   ELM=../../elm.sh ./build.sh            # build every app
#   ELM=../../elm.sh ./build.sh notes      # build one app by id
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
OUT="build"
P="$(pwd)"
mkdir -p "$OUT"

# App registry:  id | Module | Title | theme-colour | icon-emoji | one-line subtitle
APPS=(
  "notes|Notes|Notes|#3b6cf6|🗒️|A two-pane note keeper"
  "todos|Todos|Todos|#2f9e44|✅|Lists, done and pending"
  "wiki|Wiki|Wiki|#7c3aed|📚|Linked pages, [[wiki-style]]"
  "sudoku|Sudoku|Sudoku|#0891b2|🔢|Play, check and pencil-mark"
  "calendar|Calendar|Calendar|#e8590c|📅|Month view with events"
  "spreadsheet|SheetApp|Spreadsheet|#188a42|📊|The elm-spreadsheet engine"
)

build_app() {
  local id="$1" module="$2" title="$3" color="$4" emoji="$5"
  local out="$P/$OUT/$id.html"
  local extra="$P/src/$id.css"
  # The simple apps share the lean root project (shared/ + src/). The spreadsheet lives in its own
  # sheet/ subproject so its heavy vendored engine (elm-spreadsheet + elm-workspace, declared in
  # sheet/elm.vendored.json) is bundled ONLY into the spreadsheet build, not every app.
  local src proj
  if [ "$id" = "spreadsheet" ]; then
    src="$P/sheet/$module.elm"
    proj="$P/sheet/elm.json"
  else
    src="$P/src/$module.elm"
    proj="$P/elm.json"
  fi
  echo "── $id ($module) ──────────────────────────────"
  $ELM make "$src" --project="$proj" -o "$out"
  HTMLFILE="$out" TITLE="$title" THEME="$color" EMOJI="$emoji" \
    APPCSS="$P/assets/app.css" HARNESS="$P/assets/harness.js" \
    EXTRACSS="$extra" \
    perl "$P/tools/inject.pl"
  echo "   -> $OUT/$id.html"
}

# Generate build/index.html — the showcase gallery — from assets/showcase.html, injecting one card
# per registered app (icon tile in the app's theme colour, title, subtitle).
build_showcase() {
  local out="$P/$OUT/index.html"
  local cards=""
  for row in "${APPS[@]}"; do
    IFS='|' read -r id module title color emoji subtitle <<< "$row"
    cards+="<a class=\"card\" href=\"./$id.html\">"
    cards+="<span class=\"card__icon\" style=\"background:$color\">$emoji</span>"
    cards+="<span class=\"card__body\"><span class=\"card__title\">$title</span>"
    cards+="<span class=\"card__sub\">$subtitle</span></span></a>"
  done
  CARDS="$cards" perl -0777 -pe 's/<!--CARDS-->/$ENV{CARDS}/' "$P/assets/showcase.html" > "$out"
  echo "── showcase ───────────────────────────────────"
  echo "   -> $OUT/index.html"
}

want="${1:-}"
found=0
for row in "${APPS[@]}"; do
  IFS='|' read -r id module title color emoji subtitle <<< "$row"
  if [ -z "$want" ] || [ "$want" = "$id" ]; then
    build_app "$id" "$module" "$title" "$color" "$emoji"
    found=1
  fi
done

if [ "$found" = 0 ]; then
  echo "No app matched '$want'. Known ids: notes todos wiki sudoku calendar spreadsheet" >&2
  exit 1
fi

# The showcase always reflects the full registry, so (re)generate it on any build.
build_showcase

echo "Done."
