let MAXRES = 2;

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

const UNKNOWN = -1;
const COLORED = 1;

const drawAt = (worker, x, y, color) => {
  worker.postMessage([
    color == UNKNOWN ? [0x80, 0x80, 0x80] : color,
    x[0],
    y[0],
    x[1] - x[0],
    y[1] - y[0],
  ]);
};

const drawTopLeft = (worker, ctx, color) => {
  const newX = [ctx.x[0], ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2];
  const newY = [ctx.y[0], ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2];
  drawAt(worker, newX, newY, color);
  return { x: newX, y: newY };
};

const drawTopRight = (worker, ctx, color) => {
  const newX = [ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2, ctx.x[1]];
  const newY = [ctx.y[0], ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2];
  drawAt(worker, newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomLeft = (worker, ctx, color) => {
  const newX = [ctx.x[0], ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2];
  const newY = [ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2, ctx.y[1]];
  drawAt(worker, newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomRight = (worker, ctx, color) => {
  const newX = [ctx.x[0] + (ctx.x[1] - ctx.x[0]) / 2, ctx.x[1]];
  const newY = [ctx.y[0] + (ctx.y[1] - ctx.y[0]) / 2, ctx.y[1]];
  drawAt(worker, newX, newY, color);
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
  while (h in allHashes && allHashes[h] !== s) {
    console.warn("hash collision");
    h += 1;
  }
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
      const [body, _tail] = parseLam(tail.trim());
      return [abs(body), head == "[" ? _tail.trim().slice(1) : _tail.trim()];
    case "(":
      const [left, tail1] = parseLam(tail);
      const [right, tail2] = parseLam(tail1.trim());
      return [app(left)(right), tail2.trim().slice(1)];
    case ")":
    case "]":
      error("in parseLam");
      return [];
    default:
      if (head == " ") return parseLam(tail);
      if (head >= "a" && head <= "z") {
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

// helper function
const rgbToTerm = (b, c) => {
  const s = [...c.toString(b)];
  let t = s.slice(1).reduce((a, n) => app(a)(idx(+n)), idx(+s[0]));
  for (let i = 0; i < b; i++) t = abs(t);
  return t;
};

// λ^n(x_n .. x_0)
const toColor = (t) => {
  let base = 0;
  while (t.type == "abs") {
    t = t.body;
    base++;
  }

  let n = 0;
  let i = 0;
  while (1) {
    if (t.type == "abs") return UNKNOWN;

    if (t.type == "app") {
      if (t.right.type != "idx") return UNKNOWN;
      n += t.right.idx * base ** i++;
      t = t.left;
      continue;
    }
    if (t.type != "idx") return UNKNOWN;
    n += t.idx * base ** i++;
    break;
  }

  return [(n & 0xff0000) >> 16, (n & 0x00ff00) >> 8, n & 0x0000ff];
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

const clearScreen = (worker) => {
  worker.postMessage("clear");
};

/* beta reduction */

let MAX = 0;
let depth = 0;
let canceled = false;
const cancelReduction = () => {
  if (depth++ > MAX && !canceled) {
    MAX **= 1.3;
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

const reduceLoop = (worker, root, _t) => {
  const stack = [{ ctx: root, t: _t }];
  for (let i = 0; stack.length > 0; i++) {
    // console.log(i, stack.length);
    // let [{ ctx, t }] = stack.splice(
    //   Math.floor(Math.random() * stack.length),
    //   1,
    // );
    let { ctx, t } = stack.shift();
    // let { ctx, t } = stack.pop();

    // TODO: priority queue on context size
    if (toColor(t) !== UNKNOWN) continue;

    // could loop in gnf, therefore limit depth
    MAX = 10000000;
    depth = 0;
    canceled = false;
    try {
      t = snf(t);
    } catch (e) {
      if (e.message == "too much recursion")
        error(
          "your term most probably has some logical error (e.g. by not converging to a screen); if not, lmk",
        );
      error(e);
      return null;
    }
    if (t === null) {
      error("in reduceLoop");
      return null;
    }

    // smaller resolutions apparently crash the browser tab lol
    if (ctx.x[1] - ctx.x[0] < MAXRES) continue;

    if (seemsScreeny(t)) {
      const tl = t.body.left.left.left.right;
      stack.push({ ctx: drawTopLeft(worker, ctx, toColor(tl)), t: tl });
      const tr = t.body.left.left.right;
      stack.push({ ctx: drawTopRight(worker, ctx, toColor(tr)), t: tr });
      const bl = t.body.left.right;
      stack.push({ ctx: drawBottomLeft(worker, ctx, toColor(bl)), t: bl });
      const br = t.body.right;
      stack.push({ ctx: drawBottomRight(worker, ctx, toColor(br)), t: br });
    } else {
      // TODO: could we risk gnfing here?
      drawAt(worker, ctx.x, ctx.y, toColor(t));
    }
  }
};

function helpSyntax() {
  alert(
    "The syntax uses standard notations for de Bruijn indexed lambda calculus. The de Bruijn indices start at 0. You can use `\\`, `λ`, or `[..]` for abstractions. Applications use parenthesis and currently do *not* assume left associativity by default. All terms can also be replaced by a string of binary lambda calculus (BLC) - useful if you're not comfortable with de Bruijn indices (e.g. by using John Tromp's `lam` compiler).\nYou can define substitutions (like in the presets) using `=`.\n\nHave fun!",
  );
}
