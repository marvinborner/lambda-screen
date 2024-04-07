// TODO: reduction could be made faster using higher-order reduction (see my infinite-apply implementation)

importScripts("common.js");

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

let running = false;
const stack = [];
const reduce = () => {
  running = true;

  for (let i = 0; stack.length > 0; i++) {
    // console.log(i, stack.length);
    let [{ ctx, t }] = stack.splice(
      Math.floor(Math.random() * stack.length),
      1,
    );
    // let { ctx, t } = stack.shift();
    // let { ctx, t } = stack.pop();

    // TODO: priority queue on context size
    if (toColor(t) !== UNKNOWN) continue;

    // could loop in gnf, therefore limit depth
    MAX = 1000000;
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

    // smaller resolutions apparently crash the browser tab lol
    if (ctx.x[1] - ctx.x[0] < 3) continue;

    postMessage({ ctx, t });
  }

  running = false;
};

self.onmessage = (msg) => {
  stack.push(msg.data);
  if (!running) reduce();
};
