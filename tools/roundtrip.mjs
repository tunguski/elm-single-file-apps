// Prove the full save->reopen loop: boot an app with seeded data, serialize the page EXACTLY as
// harness.serializePage does (clone, empty #app, keep mirrored data block, drop transient chrome),
// then boot that produced file FRESH with no seed and confirm the data is restored & re-mirrored.
import { JSDOM } from "jsdom";
import fs from "fs";

const file = process.argv[2];
const seed = process.argv[3];
const base = fs.readFileSync(file, "utf8");

function seedInto(html, json) {
  const b64 = Buffer.from(json, "utf8").toString("base64");
  return html.replace(
    /(<script type="application\/x-elm-app-data" id="app-data">)(<\/script>)/,
    `$1${b64}$2`
  );
}

function boot(html, url) {
  const dom = new JSDOM(html, { runScripts: "dangerously", pretendToBeVisual: true, url });
  dom.window.requestAnimationFrame = (cb) => setTimeout(() => cb(Date.now()), 0);
  dom.window.cancelAnimationFrame = (id) => clearTimeout(id);
  return dom;
}

function serializeLikeHarness(win) {
  // Mirror of harness.js serializePage().
  const clone = win.document.documentElement.cloneNode(true);
  const mount = clone.querySelector("#app");
  if (mount) mount.innerHTML = "";
  clone.querySelectorAll("#sfa-badge, [data-sfa-transient]").forEach((n) => n.remove());
  return "<!doctype html>\n" + clone.outerHTML + "\n";
}

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

(async () => {
  // 1. Boot the original with seeded data.
  const dom1 = boot(seedInto(base, seed), "https://example.com/app.html");
  await wait(300);
  const data1 = decode(dom1.window);
  const len1 = mountLen(dom1.window);

  // 2. Serialize as the harness would on Ctrl+S — this is "the saved file".
  const savedFile = serializeLikeHarness(dom1.window);

  // 3. Boot the saved file FRESH, no seed.
  const dom2 = boot(savedFile, "https://example.com/app.html");
  await wait(300);
  const data2 = decode(dom2.window);
  const len2 = mountLen(dom2.window);

  const ok =
    data2 != null &&
    equalJson(data1, data2) &&
    Math.abs(len1 - len2) < 5 && // same rendered content
    dom2.window.document.getElementById("app").children.length > 0;

  console.log(JSON.stringify({ data1, data2, len1, len2, ok }, null, 2));
  process.exit(ok ? 0 : 1);

  function decode(win) {
    const el = win.document.getElementById("app-data");
    const raw = (el && el.textContent.trim()) || "";
    if (!raw) return null;
    try { return Buffer.from(raw, "base64").toString("utf8"); } catch { return null; }
  }
  function mountLen(win) {
    const a = win.document.getElementById("app");
    return a ? a.innerHTML.length : 0;
  }
  function equalJson(a, b) {
    try { return JSON.stringify(JSON.parse(a)) === JSON.stringify(JSON.parse(b)); }
    catch { return a === b; }
  }
})();
