// Browser smoke test for PCAGO (headless Chrome via the DevTools protocol, no npm deps).
//
// Loads the app, imports the Monocytes sample data (read counts + sample annotation) through the
// real UI widgets, clicks through all tabs and reports
//   - whether Shiny disconnected (grey overlay "Disconnected from the server")
//   - outputs showing a real error (not a validation message).
//
// Usage:
//   docker run -d --name pcago-test --platform linux/amd64 -p 8000:8000 trichterhub/pcago-server:2.0-dev
//   node docker/browsertest.mjs http://127.0.0.1:8000 [path-to-chrome]
// Requires Node.js >= 22 and Google Chrome / Chromium.

import { spawn } from "node:child_process";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const appUrl = process.argv[2] ?? "http://127.0.0.1:8000";
const chromePath = process.argv[3] ?? "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const port = 9333;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const chrome = spawn(chromePath, [
  "--headless=new", `--remote-debugging-port=${port}`, "--no-first-run", "--no-default-browser-check",
  `--user-data-dir=${mkdtempSync(join(tmpdir(), "pcago-chrome-"))}`, "--window-size=1600,1200", "about:blank",
], { stdio: "ignore" });

let targets;
for (let i = 0; i < 50; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json(); break; } catch { await sleep(200); }
}
const page = targets.find((t) => t.type === "page");
const cdp = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((r) => (cdp.onopen = r));
let msgId = 0;
const pending = new Map();
const consoleErrors = [];
cdp.onmessage = (ev) => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
  if (m.method === "Runtime.exceptionThrown") consoleErrors.push(m.params.exceptionDetails.text);
};
const call = (method, params = {}) => new Promise((resolve) => {
  const id = ++msgId; pending.set(id, resolve); cdp.send(JSON.stringify({ id, method, params }));
});
const js = async (expr) => (await call("Runtime.evaluate", { expression: expr, returnByValue: true, awaitPromise: true })).result?.result?.value;

let failed = false;
const report = new Map();
const check = async (label) => {
  const state = await js(`(() => ({
    disconnected: !!document.getElementById('shiny-disconnected-overlay'),
    errors: [...document.querySelectorAll('.shiny-output-error')]
      .filter(e => !e.classList.contains('shiny-output-error-validation') && e.offsetParent !== null)
      .map(e => (e.id || e.closest('[id]')?.id || '?') + ': ' + e.textContent.trim().slice(0, 200)),
    busy: document.documentElement.classList.contains('shiny-busy'),
  }))()`);
  for (const e of state.errors) if (!report.has(e)) report.set(e, label);
  console.log(`[${label}] disconnected=${state.disconnected} visible-errors=${state.errors.length}`);
  if (state.disconnected) failed = true;
  return state;
};
const waitIdle = async (maxMs) => {
  const t0 = Date.now();
  await sleep(1500);
  while (Date.now() - t0 < maxMs) {
    if (!(await js("document.documentElement.classList.contains('shiny-busy')"))) return;
    await sleep(1000);
  }
};
// Set a Shiny input through its UI element (radio, select/selectize) and fire change events
const setInput = (id, value) => js(`(() => {
  const radio = document.querySelector('input[type=radio][name="${id}"][value="${value}"]');
  if (radio) { radio.click(); return 'radio'; }
  const el = document.getElementById('${id}');
  if (!el) return 'missing';
  if (el.selectize) { el.selectize.setValue('${value}'); return 'selectize'; }
  el.value = '${value}'; $(el).trigger('change'); return 'select';
})()`);
const shot = async (name) => {
  const r = await call("Page.captureScreenshot", { format: "png" });
  const out = join(tmpdir(), `pcago-${name}.png`);
  (await import("node:fs")).writeFileSync(out, Buffer.from(r.result.data, "base64"));
  console.log(`  screenshot: ${out}`);
};
const click = (id) => js(`(() => { const el = document.getElementById('${id}'); if (!el) return false; el.click(); return true; })()`);

await call("Runtime.enable");
await call("Page.enable");
await call("Page.navigate", { url: appUrl });
await sleep(5000);
await waitIdle(120000);
await check("start page");

await js(`document.querySelector('a[data-value="analyze"]').click()`);
await waitIdle(60000);
await check("analyze tab");

// "Load example data": imports the Monocytes read counts and then the sample annotation
await click("quickio.load");
await sleep(2000);
await click("quickio.load.yes"); // confirmation dialog (only shown if data is already loaded)
await sleep(5000);
await waitIdle(300000);
await sleep(5000);
await waitIdle(300000);
await check("example data loaded");
console.log("notifications:", await js(`[...document.querySelectorAll('.shiny-notification')].map(n => n.textContent.trim().replace(/^×/, '').slice(0, 120)).join(' | ')`));
await shot("after-example-data");

// Visit every tab so that its outputs are computed
const tabs = await js(`[...document.querySelectorAll('a[data-toggle="tab"], a[data-bs-toggle="tab"]')].map((a, i) => i)`);
for (const i of tabs ?? []) {
  const name = await js(`(() => { const a = [...document.querySelectorAll('a[data-toggle="tab"], a[data-bs-toggle="tab"]')][${i}]; a.click(); return a.textContent.trim() || a.getAttribute('data-value'); })()`);
  await waitIdle(240000);
  await check(`tab: ${name}`);
  if (/^(PCA samples plot|Samples plot|Scree plot)$/.test(name)) {
    const shot = await call("Page.captureScreenshot", { format: "png" });
    const out = join(tmpdir(), `pcago-${name.replace(/\W+/g, "_")}.png`);
    (await import("node:fs")).writeFileSync(out, Buffer.from(shot.result.data, "base64"));
    console.log(`  screenshot: ${out}`);
  }
  if (failed) break;
}

await js(`document.querySelector('a[data-value="analyze"]').click()`);
await sleep(1000);
await js(`[...document.querySelectorAll('a')].find(a => a.textContent.trim() === 'Samples plot' && a.offsetParent !== null)?.click()`);
await waitIdle(240000);
await check("analyze: samples plot");
await shot("samples-plot");

await call("Page.captureScreenshot", { format: "png" }).then((r) => {
  const out = join(tmpdir(), "pcago-browsertest.png");
  import("node:fs").then((fs) => fs.writeFileSync(out, Buffer.from(r.result.data, "base64")));
  console.log(`screenshot: ${out}`);
});
await sleep(500);

console.log(`\nreal output errors: ${report.size}`);
for (const [e, where] of report) console.log(`  [${where}] ${e}`);
if (consoleErrors.length) console.log(`JS exceptions: ${consoleErrors.length}\n  ${consoleErrors.slice(0, 5).join("\n  ")}`);
chrome.kill();
if (failed) { console.log("FAIL: Shiny disconnected"); process.exit(1); }
console.log(report.size ? "DONE with output errors (see above)" : "PASS");
process.exit(0);
