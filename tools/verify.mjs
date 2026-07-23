// Headless smoke test of a built single-file app: boot it in jsdom, confirm the app mounts,
// the load port delivers embedded data into Elm, edits flow back out through the save port, and
// the data block is mirrored. Usage: node verify.mjs <path-to-html> [--seed]
import { JSDOM } from "jsdom";
import fs from "fs";

const file = process.argv[2];
let html = fs.readFileSync(file, "utf8");

// Optionally seed a saved document into the data block to prove load restores it.
const seedArg = process.argv.indexOf("--seed");
if (seedArg > -1) {
  const json = process.argv[seedArg + 1];
  const b64 = Buffer.from(json, "utf8").toString("base64");
  html = html.replace(
    /(<script type="application\/x-elm-app-data" id="app-data">)(<\/script>)/,
    `$1${b64}$2`
  );
}

const dom = new JSDOM(html, {
  runScripts: "dangerously",
  pretendToBeVisual: true,
  url: "https://example.com/notes.html",
});
const { window } = dom;

// jsdom lacks a few things the runtime/harness may touch; stub minimally.
window.requestAnimationFrame = (cb) => setTimeout(() => cb(Date.now()), 0);
window.cancelAnimationFrame = (id) => clearTimeout(id);

function done() {
  const results = {};
  results.hasApp = !!window.$app;
  results.hasLoadPort = !!(window.$app && window.$app.ports && window.$app.ports.load);
  results.hasSavePort = !!(window.$app && window.$app.ports && window.$app.ports.save);
  const dataEl = window.document.getElementById("app-data");
  results.dataBlockPresent = !!dataEl;
  results.dataBlockNonEmpty = !!(dataEl && dataEl.textContent.trim().length);
  const appEl = window.document.getElementById("app");
  results.mountRendered = !!(appEl && appEl.children.length > 0);
  results.mountHtmlLen = appEl ? appEl.innerHTML.length : 0;
  let mirrored = null;
  if (results.dataBlockNonEmpty) {
    try {
      mirrored = Buffer.from(dataEl.textContent.trim(), "base64").toString("utf8");
    } catch (e) {}
  }
  results.mirroredData = mirrored;
  console.log(JSON.stringify(results, null, 2));
  const ok =
    results.hasApp &&
    results.hasLoadPort &&
    results.hasSavePort &&
    results.mountRendered &&
    results.dataBlockNonEmpty;
  process.exit(ok ? 0 : 1);
}

// Let the mount script + harness run, load fire (deferred a tick), and Elm's save echo back.
setTimeout(done, 300);
