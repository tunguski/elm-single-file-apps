#!/usr/bin/env bash
#
# run.sh — start the Sudoku backend on http://localhost:$PORT (default 8080).
#
#   ELM=../../elm.sh ./server/run.sh          # from the project root
#   PORT=9000 ELM=../../elm.sh ./server/run.sh
#
# The client (the Sudoku app) defaults its backend URL to http://localhost:8080; change it in the
# app's sync panel to match if you use another port.
set -euo pipefail
cd "$(dirname "$0")/.."
ELM="${ELM:-../../elm.sh}"
PORT="${PORT:-8080}"
exec $ELM server "$(pwd)/server/SudokuServer.elm" --port "$PORT"
