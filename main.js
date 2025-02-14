const params = new URL(window.location.href);

let useWebGL = true;
if (params.searchParams.has("webgl"))
  useWebGL = params.searchParams.get("webgl") == "true";

const isPhone = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
const isSafari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
if ((isPhone || isSafari) && !params.searchParams.has("webgl")) {
  useWebGL = false;
  MAXRES = 3;
  window.debugInfo.innerHTML +=
    "detected phone/Safari, falling back to Canvas with MAXRES=3. Reload with ?webgl=true to still try webgl.<br>";
}

const canvas = window.canvas;
const offscreen = canvas.transferControlToOffscreen();
const worker = new Worker("canvasWorker.js");
worker.postMessage({ canvas: offscreen, useWebGL }, [offscreen]);

let MAXRES = 2;

let errors = [];
const error = (s) => {
  clearCache();
  errors.push(s);
  console.error(s);
  window.error.innerText = "invalid term: " + errors.toReversed().join(", ");
};
const clearErrors = () => {
  errors = [];
  window.error.innerText = "";
};

/* canvas */

const drawAt = (x, y, color) => {
  worker.postMessage({
    drawAt: [
      color == "White" ? "white" : color == "Black" ? "black" : "#cccccc",
      x[0],
      y[0],
      x[1] - x[0],
      y[1] - y[0],
    ],
  });
};

const drawScreen = (ctxs, colors) => {
  ctxs = ctxs.map((ctx) => [
    ctx.x[0],
    ctx.y[0],
    ctx.x[1] - ctx.x[0],
    ctx.y[1] - ctx.y[0],
  ]);
  colors = colors.map((color) =>
    color == "White" ? "white" : color == "Black" ? "black" : "#cccccc",
  );

  worker.postMessage({ drawScreen: [colors, ctxs] });
};

const clearScreen = () => {
  worker.postMessage({ clear: true });
};

const decodeBase64 = (enc) => {
  const dec = atob(enc);
  let bits = "";
  for (let i = 0; i < dec.length; i++) {
    const byte = dec.charCodeAt(i);
    for (let j = 7; j >= 0; j--) {
      const bit = (byte >> j) & 1;
      bits += bit === 1 ? "1" : "0";
    }
  }
  return bits;
};

const encodeBase64 = (t) => {
  const bin = (_t) => {
    if (_t === null) return [];
    switch (_t.type) {
      case "abs":
        return [...[0, 0], ...bin(_t.body)];
      case "app":
        return [...[0, 1], ...bin(_t.left), ...bin(_t.right)];
      case "idx":
        let s = [];
        for (let i = 0; i < _t.idx + 1; i++) s.push(1);
        s.push(0);
        return s;
      case "def":
        error("unexpected def");
        return [];
    }
  };

  const bits = bin(t);
  while (bits.length % 8 !== 0) bits.push(0);

  let res = "";
  for (let i = 0; i < bits.length; i += 8) {
    let byte = 0;
    for (let j = 0; j < 8; j++) if (bits[i + j]) byte |= 1 << (7 - j);
    res += String.fromCharCode(byte);
  }
  return btoa(res);
};

let chunk = 10;
let ctxs = [];
let colors = [];
const render = (x1, x2, y1, y2, color) => {
  if (chunk == 0) {
    drawScreen(ctxs, colors);
    chunk = (1 / (x2 - x1)) * +window.resolutionConfig.value;
    ctxs = [];
    colors = [];
  }

  ctxs.push({ x: [x1, x2], y: [y1, y2] });
  colors.push(color);
  chunk--;
  // drawAt([x1, x2], [y1, y2], color);
};

const flush = () => {
  drawScreen(ctxs, colors);
  console.log("DONE!");
};

function helpSyntax() {
  alert(
    "The syntax uses standard notations for de Bruijn indexed lambda calculus. The de Bruijn indices start at 0. You can use `\\`, `λ`, or `[..]` for abstractions. Applications use parenthesis and assume left associativity by default. All terms can also be replaced by a string of binary lambda calculus (BLC) - useful if you're not comfortable with de Bruijn indices (e.g. by using John Tromp's `lam` compiler).\nYou can define substitutions (like in the presets) using `=`.\n\nHave fun!",
  );
}
