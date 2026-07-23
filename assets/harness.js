/* harness.js — the self-saving single-file harness.
 *
 * Injected (inlined) into every built app, AFTER the compiled Elm + its window.$start(...) mount,
 * so window.$app already exists. Responsibilities:
 *
 *   1. Load  — read the embedded <script id="app-data"> block and hand it to Elm through the
 *              `load` port as a small JSON envelope { today, saved }.
 *   2. Mirror — subscribe to Elm's `save` port; every change re-serializes the document, which we
 *              stash and mirror back into the data block so the live DOM always carries current data.
 *   3. Save  — on Ctrl+S, write the whole page back to disk: in place via the File System Access
 *              API when available (Chrome/Edge), otherwise as a download the user saves over.
 *
 * The document's data is stored base64-encoded inside the data block, which makes it immune to a
 * literal script-close tag appearing in user content and to any UTF-8/HTML-escaping surprises.
 */
(function () {
  "use strict";

  var DATA_ID = "app-data";
  var app = window.$app;

  // ---- base64 that survives UTF-8 -------------------------------------------------
  function b64encode(str) {
    return btoa(unescape(encodeURIComponent(str)));
  }
  function b64decode(b64) {
    return decodeURIComponent(escape(atob(b64)));
  }

  function dataEl() {
    return document.getElementById(DATA_ID);
  }

  // The serialized document as last written to DISK (baseline for the dirty indicator)...
  var savedToDisk = null;
  // ...and as Elm last reported it (may be ahead of disk = unsaved changes).
  var current = null;
  var baselined = false;
  var fileHandle = null;

  function readEmbedded() {
    var el = dataEl();
    if (!el) return null;
    var raw = (el.textContent || "").trim();
    if (!raw) return null;
    try {
      return b64decode(raw);
    } catch (e) {
      return null;
    }
  }

  // ---- 1. Load: push the embedded document into Elm -------------------------------
  function todayIso() {
    var d = new Date();
    function p(n) {
      return (n < 10 ? "0" : "") + n;
    }
    return d.getFullYear() + "-" + p(d.getMonth() + 1) + "-" + p(d.getDate());
  }

  function sendLoad() {
    if (!app || !app.ports || !app.ports.load) return;
    var savedJson = readEmbedded();
    savedToDisk = savedJson;
    var savedValue = null;
    if (savedJson) {
      try {
        savedValue = JSON.parse(savedJson);
      } catch (e) {
        savedValue = null;
      }
    }
    var envelope = JSON.stringify({ today: todayIso(), saved: savedValue });
    app.ports.load.send(envelope);
  }

  // ---- 2. Mirror: keep the data block current on every change ---------------------
  if (app && app.ports && app.ports.save) {
    app.ports.save.subscribe(function (json) {
      current = json;
      var el = dataEl();
      if (el) el.textContent = b64encode(json);
      // The first report after load is just the loaded state re-serialized — that's the baseline,
      // not a user edit, so it counts as "saved".
      if (!baselined) {
        baselined = true;
        savedToDisk = json;
      }
      updateBadge();
    });
  }

  // ---- 3. Save: write the whole page back out -------------------------------------
  function suggestedName() {
    try {
      var base = decodeURIComponent((location.pathname.split("/").pop() || ""));
      if (base && /\.html?$/i.test(base)) return base;
    } catch (e) {}
    return (document.title || "app").replace(/[^\w.-]+/g, "-") + ".html";
  }

  function serializePage() {
    // Clone the live document, then strip anything that must not persist:
    //   - the mount (#app) — the runtime APPENDS on boot, so a saved file must ship it empty,
    //   - transient chrome (the save badge, the debug overlay) — recreated on next boot.
    var clone = document.documentElement.cloneNode(true);
    var mount = clone.querySelector("#app");
    if (mount) mount.innerHTML = "";
    var transient = clone.querySelectorAll("#sfa-badge, [data-sfa-transient]");
    for (var i = 0; i < transient.length; i++) transient[i].remove();
    var dc = clone.querySelector("#" + DATA_ID);
    if (dc && current != null) dc.textContent = b64encode(current);
    return "<!doctype html>\n" + clone.outerHTML + "\n";
  }

  async function writeHandle(handle, text) {
    var writable = await handle.createWritable();
    await writable.write(text);
    await writable.close();
  }

  function downloadFile(text) {
    var blob = new Blob([text], { type: "text/html" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = suggestedName();
    document.body.appendChild(a);
    a.click();
    setTimeout(function () {
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }, 0);
  }

  async function saveFile() {
    var html = serializePage();
    // Prefer writing back to the same file in place (Chromium's File System Access API).
    try {
      if (fileHandle) {
        await writeHandle(fileHandle, html);
        afterSave("in place");
        return;
      }
      if (window.showSaveFilePicker) {
        fileHandle = await window.showSaveFilePicker({
          suggestedName: suggestedName(),
          types: [{ description: "Web page", accept: { "text/html": [".html"] } }],
        });
        await writeHandle(fileHandle, html);
        afterSave("in place");
        return;
      }
    } catch (err) {
      if (err && err.name === "AbortError") return; // user cancelled the picker — do nothing
      // Any other failure (e.g. file:// blocks the API) falls through to a plain download.
      fileHandle = null;
    }
    downloadFile(html);
    afterSave("downloaded");
  }

  function afterSave(how) {
    savedToDisk = current;
    updateBadge();
    flash("Saved (" + how + ")");
  }

  window.addEventListener(
    "keydown",
    function (e) {
      var k = (e.key || "").toLowerCase();
      if ((e.ctrlKey || e.metaKey) && k === "s") {
        e.preventDefault();
        saveFile();
      }
    },
    true
  );

  window.addEventListener("beforeunload", function (e) {
    if (current !== savedToDisk) {
      e.preventDefault();
      e.returnValue = "";
    }
  });

  // ---- status badge (a sibling of #app, so Elm never touches it) ------------------
  var badge = document.createElement("div");
  badge.id = "sfa-badge";
  badge.setAttribute("data-sfa-transient", "");
  document.body.appendChild(badge);

  var flashTimer = null;
  function flash(text) {
    badge.textContent = text;
    badge.className = "sfa-badge sfa-flash";
    if (flashTimer) clearTimeout(flashTimer);
    flashTimer = setTimeout(updateBadge, 1400);
  }

  function updateBadge() {
    var dirty = current !== savedToDisk;
    badge.textContent = dirty ? "Unsaved — press Ctrl+S" : "Saved";
    badge.className = "sfa-badge " + (dirty ? "sfa-dirty" : "sfa-clean");
  }

  updateBadge();
  // Defer the load one tick so Elm's port subscriptions are certainly wired.
  setTimeout(sendLoad, 0);
})();
