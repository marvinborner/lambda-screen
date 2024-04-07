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

const canvas = window.canvas;

const root = { x: [0, canvas.width], y: [0, canvas.height] };

const offscreen = canvas.transferControlToOffscreen();
const worker = new Worker("canvasWorker.js");
worker.postMessage({ canvas: offscreen }, [offscreen]);

const drawAt = (x, y, color) => {
  worker.postMessage([
    color == WHITE
      ? [1, 1, 1, 1]
      : color == BLACK
        ? [0, 0, 0, 1]
        : [0.1, 0.1, 0.1, 0.4],
    // color == WHITE
    //   ? "white"
    //   : color == BLACK
    //     ? "black"
    //     : "grey",
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
  }
  return t;
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

const clearScreen = () => {
  worker.postMessage("clear");
};

/* beta reduction */

const loadBalance = (msg) => {
  const { ctx, t } = msg.data;
  // console.log(t);

  if (seemsScreeny(t)) {
    const tl = t.body.left.left.left.right;
    tlWorker.postMessage({ ctx: drawTopLeft(ctx, toColor(tl)), t: tl });
    const tr = t.body.left.left.right;
    trWorker.postMessage({ ctx: drawTopRight(ctx, toColor(tr)), t: tr });
    const bl = t.body.left.right;
    blWorker.postMessage({ ctx: drawBottomLeft(ctx, toColor(bl)), t: bl });
    const br = t.body.right;
    brWorker.postMessage({ ctx: drawBottomRight(ctx, toColor(br)), t: br });
  } else {
    // TODO: could we risk gnfing here?
    drawAt(ctx.x, ctx.y, toColor(t));
  }
};

const tlWorker = new Worker("reductionWorker.js");
tlWorker.addEventListener("message", loadBalance);
const trWorker = new Worker("reductionWorker.js");
trWorker.addEventListener("message", loadBalance);
const blWorker = new Worker("reductionWorker.js");
blWorker.addEventListener("message", loadBalance);
const brWorker = new Worker("reductionWorker.js");
brWorker.addEventListener("message", loadBalance);

const reduce = (_t) => {
  tlWorker.postMessage({ ctx: root, t: _t });
};

/* interface */

// window.reductionMode.addEventListener("change", () => {
//   const state = window.reductionMode.value;
//   if (state === "auto") {
//     window.slider.disabled = true;
//     window.slider.style.opacity = 0;
//   } else if (state === "slider") {
//     window.slider.disabled = false;
//     window.slider.style.opacity = 100;
//   } else if (state === "click") {
//     window.slider.disabled = true;
//     window.slider.style.opacity = 0;
//   }
// });

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
