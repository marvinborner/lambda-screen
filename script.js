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

const namedAbs = (name, body) => {
  if (body === null) return null;
  return { type: "named-abs", name, body };
};

const higherOrderAbs = (f) => {
  if (f === null) return null;
  return { type: "higher-order-abs", f };
};

const higherOrderApp = (a) => {
  if (a === null) return () => null;
  else if (a.type == "higher-order-abs") return a.f;
  else return app(a);
};

const show = (term) => {
  if (term === null) return "";
  switch (term.type) {
    case "abs":
      return `\\${show(term.body)}`;
    case "app":
      return `(${show(term.left)} ${show(term.right)})`;
    case "idx":
      return `${term.idx}`;
  }
};

const parse = (str) => {
  if (!str) return [{}, ""];
  const head = str[0];
  const tail = str.slice(1);
  switch (head) {
    case "\\":
      const [body, _tail] = parse(tail);
      return [abs(body), _tail];
    case "(":
      const [left, tail1] = parse(tail);
      const [right, tail2] = parse(tail1.slice(1));
      return [app(left)(right), tail2.slice(1)];
    case ")":
      alert("fatal");
      return [];
    default:
      let num = "";
      while (str && str[0] >= "0" && str[0] <= "9") {
        num += str[0];
        str = str.slice(1);
      }
      return [idx(parseInt(num)), str];
  }
};

/* lambda screen */

// [[1]]=w, [[0]]=b, other=g
const toColor = (term) => {
  if (
    term.type === "abs" &&
    term.body.type === "abs" &&
    term.body.body.type === "idx"
  )
    return term.body.body.idx === 1
      ? WHITE
      : term.body.body.idx === 0
        ? BLACK
        : UNKNOWN;
  return UNKNOWN;
};

// [((((0 tl) tr) bl) br)]
const seemsScreeny = (term) => {
  if (
    term.type === "abs" &&
    term.body.type === "app" &&
    term.body.left.type === "app" &&
    term.body.left.left.type === "app" &&
    term.body.left.left.left.type === "app" &&
    term.body.left.left.left.left.type === "idx" &&
    term.body.left.left.left.left.idx === 0
  )
    return true;
  return false;
};

const draw = (ctx, term) => {
  if (!("type" in term)) return;
  drawTopLeft(ctx, toColor(term.body.left.left.left.right));
  drawTopRight(ctx, toColor(term.body.left.left.right));
  drawBottomLeft(ctx, toColor(term.body.left.right));
  drawBottomRight(ctx, toColor(term.body.right));
};

const clearScreen = () => {
  screen.clearRect(0, 0, canvas.width, canvas.height);
};

/* beta reduction */

let MAX = 0;
let depth = 0;
let canceled = false;
const cancelReduction = () => {
  if (depth++ > MAX && !canceled) {
    MAX **= 1.5;
    if (
      !confirm(
        `This takes awfully long (${depth} steps!). The reduction potentially won't converge. Do you want to continue?\nWarning: This might crash your browser!`,
      )
    ) {
      canceled = true;
      return true;
    }
  }
  return canceled;
};

const toTerm = () => {
  depth = 0;
  const go = (env) => (t) => {
    if (cancelReduction()) return null;
    if (t === null) return null;
    switch (t.type) {
      case "app":
        return app(go(env)(t.left))(go(env)(t.right));
      case "named-abs":
        return abs(go([t.name, ...env])(t.body));
      case "idx":
        return idx(t.idx in env ? env[t.idx] : t.idx);
      default:
        alert("fatal error");
        return null;
    }
  };
  return go([]);
};

const toNamedTerm = () => {
  depth = 0;
  const go = (d) => (t) => {
    if (cancelReduction()) return null;
    if (t === null) return null;
    switch (t.type) {
      case "app":
        return app(go(d)(t.left))(go(d)(t.right));
      case "higher-order-abs":
        return namedAbs(d, go(d + 1)(t.f(idx(d))));
      case "idx":
        return idx(t.idx);
      default:
        alert("fatal error");
        return null;
    }
  };
  return go(0);
};

const toHigherOrder = () => {
  depth = 0;
  const go = (env) => (t) => {
    if (cancelReduction()) return null;
    if (t === null) return null;
    switch (t.type) {
      case "app":
        return higherOrderApp(go(env)(t.left))(go(env)(t.right));
      case "abs":
        return higherOrderAbs((x) => go([x, ...env])(t.body));
      case "idx":
        if (t.idx in env) return env[t.idx];
        else return idx(t.idx);
      default:
        alert("fatal error");
        return null;
    }
  };
  return go([]);
};

const reduce = (t) => {
  MAX = 16384;
  canceled = false;
  try {
    return toTerm()(toNamedTerm()(toHigherOrder()(t)));
  } catch (e) {
    console.error(e);
    alert(e);
    return null;
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
  draw(
    root,
    reduce(
      parse(`(${window.term.value} \\((((0 \\\\1) \\\\1) \\\\1) \\\\1)`)[0],
    ),
  );
});
