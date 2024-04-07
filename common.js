/* lambda calculus */

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

/* lambda screen */

const WHITE = 0;
const BLACK = 1;
const UNKNOWN = 2;

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
