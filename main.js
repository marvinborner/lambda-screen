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

/* caching */

let doCache = true;

const allHashes = {};
const incCache = {};
const substCache = {};
const whnfCache = {};
const snfCache = {};
const caches = [allHashes, incCache, substCache, whnfCache, snfCache];

const clearCache = () => {
  caches.forEach((cache) =>
    Object.keys(cache).forEach((key) => {
      delete cache[key];
    }),
  );
};

/* canvas */

const WHITE = 0;
const BLACK = 1;
const UNKNOWN = 2;

const drawAt = (worker, x, y, color) => {
  worker.postMessage({
    drawAt: [
      color == WHITE ? "white" : color == BLACK ? "black" : "#cccccc",
      x[0],
      y[0],
      x[1] - x[0],
      y[1] - y[0],
    ],
  });
};

const drawScreen = (worker, ctxs, colors) => {
  ctxs = ctxs.map((ctx) => [
    ctx.x[0],
    ctx.y[0],
    ctx.x[1] - ctx.x[0],
    ctx.y[1] - ctx.y[0],
  ]);
  colors = colors.map((color) =>
    color == WHITE ? "white" : color == BLACK ? "black" : "#cccccc",
  );

  worker.postMessage({ drawScreen: [colors, ctxs] });
};

/* lambda calculus */

// ---
// `return null` and `if (foo === null) return null` are the monads of JavaScript!!
// ---

const hash = (s) => {
  let h = 0;
  for (let i = 0; i < s.length; i++) {
    const chr = s.charCodeAt(i);
    h = (h << 5) - h + chr;
    h |= 0;
  }
  while (h in allHashes && allHashes[h] !== s) {
    console.warn("hash collision", allHashes[h], s);
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

const num = (n) => {
  const t = { type: "num", n };
  t.hash = hash("num" + n);
  return t;
};

const ope = (name, op) => {
  const t = { type: "ope", name, ...op };
  t.hash = hash("ope" + name);
  return t;
};

const operators = [
  { name: "add", arity: 2, args: [] },
  { name: "sub", arity: 2, args: [] },
  { name: "mul", arity: 2, args: [] },
  { name: "div", arity: 2, args: [] },
  { name: "pow", arity: 2, args: [] },
  { name: "max", arity: 2, args: [] },
  { name: "min", arity: 2, args: [] },
  { name: "mod", arity: 2, args: [] },
  { name: "eq", arity: 2, args: [] },
  { name: "gt", arity: 2, args: [] },
  { name: "ge", arity: 2, args: [] },
  { name: "lt", arity: 2, args: [] },
  { name: "le", arity: 2, args: [] },
  { name: "prime", arity: 1, args: [] },
  { name: "sqrt", arity: 1, args: [] },
  { name: "inc", arity: 1, args: [] },
  { name: "dec", arity: 1, args: [] },
  { name: "log", arity: 1, args: [] },
];

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
        error("unexpected def " + show(t));
        return [];
      case "num":
        return ""; // TODO?
      case "ope":
        return ""; // TODO?
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

const size = (t) => {
  if (t === null) return 0;
  switch (t.type) {
    case "abs":
      return 2 + size(t.body);
    case "app":
      return 2 + size(t.left) + size(t.right);
    case "idx":
      return t.idx + 2;
    case "def":
      error("unexpected def " + show(t));
      return 0;
    case "num":
      return 0; // TODO?
    case "ope":
      return 0; // TODO?
  }
};

const show = (t) => {
  if (t === null) return "";
  switch (t.type) {
    case "abs":
      return `\\${show(t.body)}`;
    // return `[${show(t.body)}]`;
    case "app":
      return `(${show(t.left)} ${show(t.right)})`;
    case "idx":
      return `${t.idx}`;
    case "def":
      return t.name;
    case "num":
      return `<${t.n}>`;
    case "ope":
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
      case "num":
        return false;
      case "ope":
        return false;
    }
  };
  return go(t, 0);
};

const parseLam = (str) => {
  // default to left-associative application
  const folded = (s) => {
    const init = parseLam(s);
    if (!init[1] || ")]".includes(init[1][0])) return init;

    const go = (acc, rst) => {
      const parsed = parseLam(rst);
      const chain = app(acc)(parsed[0]);
      if (!parsed[1] || ")]".includes(parsed[1][0])) return [chain, parsed[1]];
      return go(chain, parsed[1]);
    };
    return go(init[0], init[1]);
  };

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
      return [
        abs(body),
        head == "[" ? _tail.trim().slice(1).trim() : _tail.trim(),
      ];
    case "(":
      const [chain, _tail1] = folded(tail.trim());
      return [chain, _tail1.trim().slice(1).trim()];
    case ")":
    case "]":
      error("in parseLam");
      return [];
    case " ":
      return folded(tail);
    case "<": // integer
      let n = "";
      str = tail;
      while (str && str[0] >= "0" && str[0] <= "9") {
        n += str[0];
        str = str.slice(1);
      }
      str = str.trim().slice(1); // final >
      return [num(parseInt(n)), str.trim()];
    default:
      if (head >= "a" && head <= "z") {
        // substitution
        let subst = "";
        while (str && str[0] >= "a" && str[0] <= "z") {
          subst += str[0];
          str = str.slice(1);
        }
        return [
          operators.some(({ name }) => name == subst)
            ? ope(
                subst,
                operators.find(({ name }) => name == subst),
              )
            : def(subst),
          str.trim(),
        ];
      } else {
        // de Bruijn index
        let n = "";
        while (str && str[0] >= "0" && str[0] <= "9") {
          n += str[0];
          str = str.slice(1);
        }
        return [idx(parseInt(n)), str.trim()];
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
    error("is open " + show(t));
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
    case "num":
      return t;
    case "ope":
      return t;
  }
};

const resolveTerm = (_t, defs) => {
  if (_t === null) return null;

  let final = _t;
  for (let def of defs.reverse())
    final = app(abs(substDef(0, final, def[0])))(def[1]);
  return final;
};

const parse = (str) => {
  const defs = [];
  let t;
  str
    .trim()
    .split(/\r?\n/)
    .every((line) => {
      if (line.startsWith("--") || line.length === 0) return true;
      if (!line.includes("=")) {
        t = parseTerm(line);
        return false;
      }
      [n, _t] = line.split("=");
      defs.push([n.trim(), parseTerm(_t.trim())]);
      return true;
    });
  t = resolveTerm(t, defs);
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

// [((((0 tl) tr) bl) br) ...]
// (or more, as long as n is perfect square)
const seemsScreeny = (t) => {
  if (t.type !== "abs") return false;
  t = t.body;
  let d = 0;
  while ((d++, t.type === "app")) t = t.left;
  return t.type === "idx" && t.idx === 0 ? d - 1 : false;
};

const getSubScreens = (t) => {
  if (t.type !== "abs") return false;
  t = t.body;
  let ts = [];
  while (t.type === "app" && ts.unshift(t)) t = t.left;
  return ts;
};

const clearScreen = (worker) => {
  worker.postMessage({ clear: true });
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
      clearCache();
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

  const h = hash("" + i + t.hash);
  if (doCache && h in incCache) return incCache[h];

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
    case "num":
      newT = t;
      break;
    case "ope":
      newT = t;
      break;
    case "def":
      error("unexpected def " + show(t));
      return null;
  }

  incCache[h] = newT;
  return newT;
};

const subst = (i, t, s) => {
  if (cancelReduction() || t === null) {
    error("in subst");
    return null;
  }

  const h = hash("" + i + t.hash + s.hash);
  if (doCache && h in substCache) return substCache[h];

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
      error("unexpected def " + show(t));
      return null;
    case "num":
      newT = t; // TODO: in THEORY we could handle nums as Church here!
      break;
    case "ope":
      newT = t;
      newT.args = newT.args.map((arg) => subst(i, arg, s));
      break;
  }

  substCache[h] = newT;
  return newT;
};

// guaranteed normal form
// only use if sure that t is not a (potentially diverging) screen
// TODO: this assumes laziness LOL (OR DOES IT)
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
      error("unexpected def " + show(t));
      return null;
    default:
      return t;
  }
};

const isPrime = (num) => {
  for (let i = 2, s = Math.sqrt(num); i <= s; i++) {
    if (num % i === 0) return false;
  }
  console.log(num, num > 1);
  return num > 1;
};

const evalOp = (obj) => {
  if (
    obj.arity !== 0 ||
    (obj.args.every((arg) => arg.type != "num") && obj.name != "log")
  )
    return obj;

  switch (obj.name) {
    case "add":
      return num(obj.args[0].n + obj.args[1].n);
    case "sub":
      return num(obj.args[0].n - obj.args[1].n);
    case "mul":
      return num(obj.args[0].n * obj.args[1].n);
    case "div":
      return num(Math.floor(obj.args[0].n / obj.args[1].n));
    case "pow":
      return num(Math.pow(obj.args[0].n, obj.args[1].n));
    case "max":
      return num(Math.max(obj.args[0].n, obj.args[1].n));
    case "min":
      return num(Math.min(obj.args[0].n, obj.args[1].n));
    case "mod":
      return num(obj.args[0].n % obj.args[1].n);
    case "eq":
      return abs(abs(idx(obj.args[0].n == obj.args[1].n ? 1 : 0)));
    case "gt":
      return abs(abs(idx(obj.args[0].n > obj.args[1].n ? 1 : 0)));
    case "ge":
      return abs(abs(idx(obj.args[0].n >= obj.args[1].n ? 1 : 0)));
    case "lt":
      return abs(abs(idx(obj.args[0].n < obj.args[1].n ? 1 : 0)));
    case "le":
      return abs(abs(idx(obj.args[0].n <= obj.args[1].n ? 1 : 0)));
    case "prime":
      return abs(abs(idx(isPrime(obj.args[0].n) ? 1 : 0)));
    case "sqrt":
      return num(Math.floor(Math.sqrt(obj.args[0].n)));
    case "inc":
      return num(obj.args[0].n + 1);
    case "dec":
      return num(obj.args[0].n - 1);
    case "log":
      console.log(obj.args[0]);
      return obj.args[0];
  }
  return obj;
};

const applyOp = (obj, arg) => {
  let consumed = false;
  if (obj.arity !== 0) {
    obj.args.push(arg);
    obj.hash = hash("opapp" + obj.hash + arg.hash);
    obj.arity--;
    consumed = true;
  }

  obj.args = obj.args.map((arg) => evalOp(arg));
  obj = evalOp(obj);

  return consumed ? obj : app(obj)(arg);
};

// weak head normal form
const whnf = (t) => {
  if (cancelReduction() || t === null) {
    error("in whnf");
    return null;
  }

  if (doCache && t.hash in whnfCache) return whnfCache[t.hash];

  let newT;
  switch (t.type) {
    case "app":
      const _left = whnf(t.left);
      if (_left === null) return null;
      if (_left.type === "abs") newT = whnf(subst(0, _left.body, t.right));
      else if (_left.type === "ope") {
        newT = applyOp(structuredClone(_left), whnf(t.right));
      } else newT = app(_left)(t.right);
      break;
    case "def":
      error("unexpected def " + show(t));
      return null;
    default:
      newT = t;
      break;
  }

  whnfCache[t.hash] = newT;
  return newT;
};

// screen normal form
// one of [((((0 tl) tr) bl) br) ...], [[0]], [[1]]
// TODO: Is this form of caching fundamentally wrong? (incongruences after subst or idx shifts!?)
//       Does this only work accidentally because of WHNF, deliberate symmetry and closed terms or sth?
const snf = (_t) => {
  if (doCache && _t !== null && _t.hash in snfCache) return snfCache[_t.hash];

  let t = whnf(_t);
  if (t === null || t.type !== "abs") {
    console.log(t);
    error("not a screen/pixel: " + show(_t) + " \n\nWHNF of\n\n" + show(t));
    return null;
  }

  t = abs(whnf(t.body));
  if (t.body.type === "abs") return gnf(t); // not a screen, probably a pixel

  // yes `=== false` is relevant here
  while (t !== null && seemsScreeny(t) === false) {
    switch (t.type) {
      case "app":
        const _left = whnf(t.left);
        if (_left.type === "abs") t = subst(0, _left.body, t.right);
        else if (_left.type === "ope") {
          _left.args.map((arg) => whnf(arg));
          t = applyOp(structuredClone(_left), whnf(t.right));
        } else t = app(_left)(whnf(t.right));
        break;
      case "abs":
        t = abs(whnf(t.body));
        break;
      case "def":
        error("unexpected def " + show(t));
        return null;
      case "num":
        break;
      case "ope":
        break;
      default:
        error("type");
        return null;
    }
  }

  snfCache[_t.hash] = t;
  return t;
};

const reduceLoop = (conf, _t) => {
  const { worker, root, logger, scheduler, caching } = conf;
  let cnt = 0;
  logger(`Term size: ${size(_t)} bit (BLC)<br>`);
  const stack = [{ ctx: root, t: _t }];
  doCache = caching;
  for (cnt = 0; stack.length > 0 && !canceled; cnt++) {
    let { ctx, t } = scheduler(stack);

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

    let n;
    if ((n = seemsScreeny(t)) && n > 3 && Math.sqrt(n) % 1 === 0) {
      const subScreens = getSubScreens(t);
      console.assert(n == subScreens.length);

      const splitSize = Math.sqrt(n);
      const ctxWidth = (ctx.x[1] - ctx.x[0]) / splitSize;
      const ctxHeight = (ctx.y[1] - ctx.y[0]) / splitSize;

      const ctxs = [];
      const colors = [];

      let x0 = ctx.x[0];
      let y0 = ctx.y[0];

      for (let i = 0; i < n; i++) {
        const current = subScreens[i];
        const subCtx = { x: [x0, x0 + ctxWidth], y: [y0, y0 + ctxHeight] };
        ctxs.push(subCtx);
        stack.push({ ctx: subCtx, t: current.right });
        colors.push(toColor(current.right));

        if ((i + 1) % splitSize == 0) {
          x0 = ctx.x[0];
          y0 += ctxHeight;
        } else x0 += ctxWidth;
      }

      drawScreen(worker, ctxs, colors);
    } else {
      // TODO: could we risk gnfing here?
      drawAt(worker, ctx.x, ctx.y, toColor(t));
    }
  }

  return cnt;
};

function helpSyntax() {
  alert(
    "The syntax uses standard notations for de Bruijn indexed lambda calculus. The de Bruijn indices start at 0. You can use `\\`, `λ`, or `[..]` for abstractions. Applications use parenthesis and assume left associativity by default. All terms can also be replaced by a string of binary lambda calculus (BLC) - useful if you're not comfortable with de Bruijn indices (e.g. by using John Tromp's `lam` compiler).\nYou can define substitutions (like in the presets) using `=`.\n\nHave fun!",
  );
}
