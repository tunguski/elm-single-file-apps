// End-to-end: boot the Sudoku app in jsdom, seed a game that's one cell from solved, type the last
// digit, and confirm the app POSTs the solve to the running backend (which then reports the game).
import { JSDOM } from "jsdom";
import fs from "fs";

const file = process.argv[2];
const base = process.argv[3]; // server base URL
const html = fs.readFileSync(file, "utf8");

const puzzle =
  "530070000600195000098000060800060003400803001700020006060000280000419005000080079";
const solution =
  "534678912672195348198342567859761423426853791713924856961537284287419635345286179";

// Entries = every blank cell's solved digit, minus the last blank (the one we'll type).
const blanks = [];
for (let i = 0; i < 81; i++) if (puzzle[i] === "0") blanks.push(i);
const target = blanks[blanks.length - 1];
const missingDigit = solution[target];
const entries = {};
for (const i of blanks) if (i !== target) entries[i] = Number(solution[i]);

const playerId = "e2e-" + "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee";
const seed = {
  puzzleIndex: 0,
  entries,
  marks: {},
  playerId,
  serverUrl: base,
  startedAt: Date.now() - 65000, // ~65s ago → non-zero elapsed
  solvedPosted: false,
};
const seeded = html.replace(
  /(<script type="application\/x-elm-app-data" id="app-data">)(<\/script>)/,
  `$1${Buffer.from(JSON.stringify(seed), "utf8").toString("base64")}$2`
);

const dom = new JSDOM(seeded, { runScripts: "dangerously", pretendToBeVisual: true, url: "https://ex.com/sudoku.html" });
const { window } = dom;
window.requestAnimationFrame = (cb) => setTimeout(() => cb(Date.now()), 0);
// Give the Elm runtime a real fetch (Node's). Not a browser, so no CORS enforcement — that's fine;
// CORS is proven separately via curl. This checks the client actually issues the request.
window.fetch = (...args) => fetch(...args);

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

(async () => {
  await wait(350); // boot + load
  const cells = window.document.querySelectorAll(".sk-cell");
  if (cells.length !== 81) throw new Error("expected 81 cells, got " + cells.length);
  cells[target].click(); // select the empty target cell
  await wait(50);
  const ev = new window.KeyboardEvent("keydown", { key: String(missingDigit), bubbles: true });
  window.document.dispatchEvent(ev); // type the final digit -> solved -> POST
  await wait(700); // let Time.now + fetch round-trip complete

  const res = await fetch(base + "/games?playerId=" + encodeURIComponent(playerId));
  const summary = await res.json();
  console.log("server summary for player:", JSON.stringify(summary));
  const ok = summary.count >= 1 && summary.totalMs > 0 && summary.games && summary.games[0].puzzleIndex === 0;
  console.log(ok ? "E2E OK — solve was recorded by the backend" : "E2E FAIL");
  process.exit(ok ? 0 : 1);
})().catch((e) => {
  console.error("E2E ERROR:", e.message);
  process.exit(1);
});
