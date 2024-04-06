let errors = [];
const error = (s) => {
  errors.push(s);
  console.error(s);
  window.error.innerText = "invalid term: " + errors.toReversed().join(", ");
};
const clearErrors = () => {
  errors = [];
  window.error.innerText = "";
};

/* canvas */

const WHITE = 0;
const BLACK = 1;
const UNKNOWN = 2;

const canvas = window.canvas;

const root = { x: [0, canvas.width], y: [0, canvas.height] };

const offscreen = canvas.transferControlToOffscreen();
const worker = new Worker("worker.js");
worker.postMessage({ canvas: offscreen }, [offscreen]);

const drawAt = (x, y, color) => {
  worker.postMessage([
    color == WHITE
      ? [1, 1, 1, 1]
      : color == BLACK
        ? [0, 0, 0, 1]
        : [0.1, 0.1, 0.1, 0.3],
    x[0],
    y[0],
    x[1] - x[0],
    y[1] - y[0],
  ]);
};

const drawTopLeft = (ctx, color) => {
  const newX = [ctx.x[0], ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2];
  const newY = [ctx.y[0], ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawTopRight = (ctx, color) => {
  const newX = [ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2, ctx.x[1]];
  const newY = [ctx.y[0], ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomLeft = (ctx, color) => {
  const newX = [ctx.x[0], ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2];
  const newY = [ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2, ctx.y[1]];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomRight = (ctx, color) => {
  const newX = [ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2, ctx.x[1]];
  const newY = [ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2, ctx.y[1]];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

/* lambda calculus */

// ---
// `return null` and `if (foo === null) return null` are the monads of JavaScript!!
// ---

const allHashes = {};
const hash = (s) => {
  let h = 0;
  for (let i = 0; i < s.length; i++) {
    const chr = s.charCodeAt(i);
    h = (h << 5) - h + chr;
    h |= 0;
  }
  while (h in allHashes && allHashes[h] !== s) h += 1;
  allHashes[h] = s;
  return h;
};

const abs = (body) => {
  if (body === null) return null;
  const t = { type: "abs", body };
  t.hash = hash("abs" + body.hash);
  return t;
};

const app = (left) => (right) => {
  if (left === null || right === null) return null;
  const t = { type: "app", left, right };
  t.hash = hash("app" + left.hash + right.hash);
  return t;
};

const idx = (idx) => {
  if (idx === null) return null;
  const t = { type: "idx", idx };
  t.hash = hash("idx" + idx);
  return t;
};

const def = (name) => {
  const t = { type: "def", name };
  t.hash = hash("def" + name);
  return t;
};

const show = (t) => {
  if (t === null) return "";
  switch (t.type) {
    case "abs":
      // return `\\${show(t.body)}`;
      return `[${show(t.body)}]`;
    case "app":
      return `(${show(t.left)} ${show(t.right)})`;
    case "idx":
      return `${t.idx}`;
    case "def":
      return t.name;
  }
};

const isOpen = (t) => {
  const go = (t, d) => {
    if (t === null) return true;
    switch (t.type) {
      case "abs":
        return go(t.body, d + 1);
      case "app":
        return go(t.left, d) || go(t.right, d);
      case "idx":
        return t.idx >= d;
      case "def":
        return false;
    }
  };
  return go(t, 0);
};

const parseLam = (str) => {
  if (!str) {
    error("in parseLam");
    return [{}, ""];
  }
  const head = str[0];
  const tail = str.slice(1);
  switch (head) {
    case "λ":
    case "\\":
    case "[": // bruijn
      const [body, _tail] = parseLam(tail);
      return [abs(body), head == "[" ? _tail.slice(1) : _tail];
    case "(":
      const [left, tail1] = parseLam(tail);
      const [right, tail2] = parseLam(tail1.slice(1));
      return [app(left)(right), tail2.slice(1)];
    case ")":
    case "]":
      error("in parseLam");
      return [];
    default:
      if (str[0] >= "a" && str[0] <= "z") {
        // substitution
        let name = "";
        while (str && str[0] >= "a" && str[0] <= "z") {
          name += str[0];
          str = str.slice(1);
        }
        return [def(name), str];
      } else {
        // de Bruijn index
        let num = "";
        while (str && str[0] >= "0" && str[0] <= "9") {
          num += str[0];
          str = str.slice(1);
        }
        return [idx(parseInt(num)), str];
      }
  }
};

const parseBLC = (str) => {
  if (!str) {
    error("in parseBLC");
    return [{}, ""];
  }
  if (str.slice(0, 2) === "00") {
    const [body, tail] = parseBLC(str.slice(2));
    return [abs(body), tail];
  }
  if (str.slice(0, 2) === "01") {
    const [left, tail1] = parseBLC(str.slice(2));
    const [right, tail2] = parseBLC(tail1);
    return [app(left)(right), tail2];
  }
  const cnt = str.slice(1).indexOf("0");
  return [idx(cnt), str.slice(cnt + 2)];
};

const parseTerm = (str) => {
  const t = /^[01]+$/.test(str) ? parseBLC(str)[0] : parseLam(str)[0];
  if (isOpen(t)) {
    error("is open");
    return null;
  } else {
    return t;
  }
};

const substDef = (i, t, n) => {
  switch (t.type) {
    case "idx":
      return t;
    case "app":
      return app(substDef(i, t.left, n))(substDef(i, t.right, n));
    case "abs":
      return abs(substDef(i + 1, t.body, n));
    case "def":
      return t.name === n ? idx(i) : t;
  }
};

const resolveTerm = (_t, defs) => {
  if (_t === null) return null;
  let final = _t;
  let len = Object.keys(defs).length;
  for (let i = len; i > 0; i--) {
    final = abs(final);
  }
  let d = len;
  Object.entries(defs).forEach(([n, t]) => {
    final = app(substDef(--d - len, final, n))(t);
  });
  return final;
};

const parse = (str) => {
  const defs = {};
  let t;
  str
    .trim()
    .split(/\r?\n/)
    .every((line) => {
      if (line.startsWith("--") || line.length == 0) return true;
      if (!line.includes("=")) {
        t = resolveTerm(parseTerm(line), defs);
        return false;
      }
      [n, _t] = line.split("=");
      defs[n.trim()] = resolveTerm(parseTerm(_t.trim()), defs);
      return true;
    });
  return t;
};

/* lambda screen */

// [[1]]=w, [[0]]=b, other=g
const toColor = (t) => {
  if (t.type === "abs" && t.body.type === "abs" && t.body.body.type === "idx")
    return t.body.body.idx === 1
      ? WHITE
      : t.body.body.idx === 0
        ? BLACK
        : UNKNOWN;
  return UNKNOWN;
};

// [((((0 tl) tr) bl) br)]
const seemsScreeny = (t) =>
  t.type === "abs" &&
  t.body.type === "app" &&
  t.body.left.type === "app" &&
  t.body.left.left.type === "app" &&
  t.body.left.left.left.type === "app" &&
  t.body.left.left.left.left.type === "idx" &&
  t.body.left.left.left.left.idx === 0;

const clearScreen = () => {
  worker.postMessage("clear");
};

/* beta reduction */

let MAX = 0;
let depth = 0;
let canceled = false;
const cancelReduction = () => {
  if (depth++ > MAX && !canceled) {
    MAX **= 1.4;
    if (
      !confirm(
        `This takes awfully long (${depth} steps!). The reduction potentially won't converge to a valid screen (or at all!). Do you want to continue?\nWarning: This might crash your browser!`,
      )
    ) {
      canceled = true;
      return true;
    }
  }
  return canceled;
};

const incCache = {};
const inc = (i, t) => {
  if (cancelReduction() || t === null) {
    error("in inc");
    return null;
  }

  if (t.hash in incCache && i in incCache[t.hash]) return incCache[t.hash][i];

  let newT;
  switch (t.type) {
    case "idx":
      newT = idx(i <= t.idx ? t.idx + 1 : t.idx);
      break;
    case "app":
      newT = app(inc(i, t.left))(inc(i, t.right));
      break;
    case "abs":
      newT = abs(inc(i + 1, t.body));
      break;
    case "def":
      error("unexpected def");
      return null;
  }

  if (!(t.hash in incCache)) incCache[t.hash] = {};
  incCache[t.hash][i] = newT;
  return newT;
};

const substCache = {};
const subst = (i, t, s) => {
  if (cancelReduction() || t === null) {
    error("in subst");
    return null;
  }

  const h = hash("" + t.hash + s.hash);
  if (h in substCache && i in substCache[h]) return substCache[h][i];

  let newT;
  switch (t.type) {
    case "idx":
      newT = i == t.idx ? s : idx(t.idx > i ? t.idx - 1 : t.idx);
      break;
    case "app":
      newT = app(subst(i, t.left, s))(subst(i, t.right, s));
      break;
    case "abs":
      newT = abs(subst(i + 1, t.body, inc(0, s)));
      break;
    case "def":
      error("unexpected def");
      return null;
  }

  if (!(h in substCache)) substCache[h] = {};
  substCache[h][i] = newT;
  return newT;
};

// guaranteed normal form
// only use if sure that t is not a (potentially diverging) screen
const gnf = (t) => {
  if (cancelReduction() || t === null) {
    error("in gnf");
    return null;
  }
  switch (t.type) {
    case "app":
      const _left = gnf(t.left);
      if (_left === null) return null;
      return _left.type === "abs"
        ? gnf(subst(0, _left.body, t.right))
        : app(_left)(gnf(t.right));
    case "abs":
      return abs(gnf(t.body));
    case "def":
      error("unexpected def");
      return null;
    default:
      return t;
  }
};

// weak head normal form
const whnfCache = {};
const whnf = (t) => {
  if (cancelReduction() || t === null) {
    error("in whnf");
    return null;
  }

  if (t.hash in whnfCache) return whnfCache[t.hash];

  let newT;
  switch (t.type) {
    case "app":
      const _left = whnf(t.left);
      if (_left === null) return null;
      newT =
        _left.type === "abs"
          ? whnf(subst(0, _left.body, t.right))
          : app(_left)(t.right);
      break;
    case "def":
      error("unexpected def");
      return null;
    default:
      newT = t;
      break;
  }

  whnfCache[t.hash] = newT;
  return newT;
};

// screen normal form
// one of [((((0 tl) tr) bl) br)], [[0]], [[1]]
const snfCache = {};
const snf = (_t) => {
  if (_t !== null && _t.hash in snfCache) return snfCache[_t.hash];

  let t = whnf(_t);
  if (t === null || t.type !== "abs") {
    error("not a screen/pixel");
    return null;
  }

  t = abs(whnf(t.body));
  if (t.body.type === "abs") return gnf(t); // not a screen, probably a pixel

  while (t !== null && !seemsScreeny(t)) {
    // TODO: a bit unsure about this
    switch (t.type) {
      case "app":
        const _left = whnf(t.left);
        t =
          _left.type === "abs"
            ? subst(0, _left.body, t.right)
            : app(_left)(whnf(t.right));
        break;
      case "abs":
        t = abs(whnf(t.body));
        break;
      case "def":
        error("unexpected def");
        return null;
      default:
        error("type");
        return null;
    }
  }

  snfCache[_t.hash] = t;
  return t;
};

const reduce = (_t) => {
  const stack = [{ ctx: root, t: _t }];
  for (let i = 0; stack.length > 0 && i < 4 ** 10; i++) {
    // console.log(i, stack.length);
    let [{ ctx, t }] = stack.splice(
      Math.floor(Math.random() * stack.length),
      1,
    );
    // let { ctx, t } = stack.shift();
    // let { ctx, t } = stack.pop();
    if (toColor(t) !== UNKNOWN) continue;

    // could loop in gnf, therefore limit depth
    MAX = 10000000;
    depth = 0;
    canceled = false;
    try {
      t = snf(t);
    } catch (e) {
      error(e);
      return null;
    }
    if (t === null) {
      error("in reduce");
      return null;
    }

    if (seemsScreeny(t)) {
      const tl = t.body.left.left.left.right;
      stack.push({ ctx: drawTopLeft(ctx, toColor(tl)), t: tl });
      const tr = t.body.left.left.right;
      stack.push({ ctx: drawTopRight(ctx, toColor(tr)), t: tr });
      const bl = t.body.left.right;
      stack.push({ ctx: drawBottomLeft(ctx, toColor(bl)), t: bl });
      const br = t.body.right;
      stack.push({ ctx: drawBottomRight(ctx, toColor(br)), t: br });
    } else {
      // TODO: could we risk gnfing here?
      drawAt(ctx.x, ctx.y, toColor(t));
    }
  }
};

/* interface */

window.reductionMode.addEventListener("change", () => {
  const state = window.reductionMode.value;
  if (state === "auto") {
    window.slider.disabled = true;
    window.slider.style.opacity = 0;
  } else if (state === "slider") {
    window.slider.disabled = false;
    window.slider.style.opacity = 100;
  } else if (state === "click") {
    window.slider.disabled = true;
    window.slider.style.opacity = 0;
  }
});

window.examples.addEventListener("change", () => {
  clearScreen();
  window.term.value = window.examples.value;
});

window.render.addEventListener("click", () => {
  clearScreen();
  clearErrors();
  reduce(
    app(parse(window.term.value))(parse("\\((((0 \\\\1) \\\\1) \\\\1) \\\\1)")),
  );
});
