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
const screen = canvas.getContext("2d");

const root = { x: [0, canvas.width], y: [0, canvas.height] };

const drawAt = (x, y, color) => {
  screen.fillStyle =
    color == WHITE ? "white" : color == BLACK ? "black" : "grey";
  screen.fillRect(x[0], y[0], x[1], y[1]);
};

const drawTopLeft = (ctx, color) => {
  const newX = [ctx.x[0], ctx.x[1] / 2];
  const newY = [ctx.y[0], ctx.y[1] / 2];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawTopRight = (ctx, color) => {
  const newX = [ctx.x[1] / 2, ctx.x[1]];
  const newY = [ctx.y[0], ctx.y[1] / 2];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomLeft = (ctx, color) => {
  const newX = [ctx.x[0], ctx.x[1] / 2];
  const newY = [ctx.y[1] / 2, ctx.y[1]];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

const drawBottomRight = (ctx, color) => {
  const newX = [ctx.x[1] / 2, ctx.x[1]];
  const newY = [ctx.y[1] / 2, ctx.y[1]];
  drawAt(newX, newY, color);
  return { x: newX, y: newY };
};

/* lambda calculus */

// ---
// `return null` and `if (foo === null) return null` are the monads of JavaScript!!
// ---

const abs = (body) => {
  if (body === null) return null;
  return { type: "abs", body };
};

const app = (left) => (right) => {
  if (left === null || right === null) return null;
  return { type: "app", left, right };
};

const idx = (idx) => {
  if (idx === null) return null;
  return { type: "idx", idx };
};

const def = (name) => {
  return { type: "def", name };
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
      const [body, _tail] = parseLam(tail);
      return [abs(body), _tail];
    case "[": // bruijn
      const [bbody, _btail] = parseLam(tail);
      return [abs(bbody), _btail.slice(1)];
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
  screen.clearRect(0, 0, canvas.width, canvas.height);
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

const inc = (i, t) => {
  if (cancelReduction() || t === null) {
    error("in inc");
    return null;
  }
  switch (t.type) {
    case "idx":
      return idx(i <= t.idx ? t.idx + 1 : t.idx);
    case "app":
      return app(inc(i, t.left))(inc(i, t.right));
    case "abs":
      return abs(inc(i + 1, t.body));
    case "def":
      error("unexpected def");
      return null;
  }
};

const subst = (i, t, s) => {
  if (cancelReduction() || t === null) {
    error("in subst");
    return null;
  }
  switch (t.type) {
    case "idx":
      return i == t.idx ? s : idx(t.idx > i ? t.idx - 1 : t.idx);
    case "app":
      return app(subst(i, t.left, s))(subst(i, t.right, s));
    case "abs":
      return abs(subst(i + 1, t.body, inc(0, s)));
    case "def":
      error("unexpected def");
      return null;
  }
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
const whnf = (t) => {
  if (cancelReduction() || t === null) {
    error("in whnf");
    return null;
  }
  switch (t.type) {
    case "app":
      const _left = whnf(t.left);
      if (_left === null) return null;
      return _left.type === "abs"
        ? whnf(subst(0, _left.body, t.right))
        : app(_left)(t.right);
    case "def":
      error("unexpected def");
      return null;
    default:
      return t;
  }
};

// screen normal form
// one of [((((0 tl) tr) bl) br)], [[0]], [[1]]
const snf = (_t) => {
  let t = whnf(_t);
  if (t === null || t.type !== "abs") {
    error("not a screen/pixel");
    return null;
  }
  t = abs(whnf(t.body));
  if (t.body.type === "abs") return gnf(t); // not a screen, probably a pixel

  while (t !== null && !seemsScreeny(t)) {
    if (cancelReduction()) {
      error("in snf");
      return null;
    }
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
  return t;
};

const reduce = (_t) => {
  console.log(show(_t));
  const stack = [{ ctx: root, t: _t }];
  for (let i = 0; stack.length > 0 && i < 1024; i++) {
    let { ctx, t } = stack.pop();
    if (toColor(t) !== UNKNOWN) continue;

    // could loop in gnf, therefore limit depth
    MAX = 16384;
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
  clearErrors();
  reduce(
    app(parse(window.term.value))(parse("\\((((0 \\\\1) \\\\1) \\\\1) \\\\1)")),
  );
});
