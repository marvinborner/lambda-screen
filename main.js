const resolution = () => +window.resolutionConfig.value;

const app = new PIXI.Application();
app
  .init({
    width: resolution(),
    height: resolution(),
    preferene: "webgl",
  })
  .then(() => {
    document.querySelector("main div#canvas").appendChild(app.canvas);
  });

const clearScreen = () => {
  // worker.postMessage({ type: "clear" });
};

function decodeBase64(enc) {
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
}

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

const colors = [0xffffff, 0x000000, 0xa0a0a0];

const vertexShader = `
in vec2 aVertexPosition;
in vec2 aOffset;
in vec4 aColor;

uniform mat3 uProjectionMatrix;

varying vec4 vColor;

void main() {
    vec3 pos = uProjectionMatrix * vec3(aVertexPosition + aOffset, 1.0);
    gl_Position = vec4(pos.xy, 0.0, 1.0);
    vColor = aColor;
}
`;

const fragmentShader = `
    varying vec4 vColor;
    void main() {
        gl_FragColor = vColor;
    }
`;

const shader = PIXI.Shader.from({
  gl: { vertex: vertexShader, fragment: fragmentShader },
});

const CHUNK_AMOUNT = 5;
let cache = {};
const render = (x1, x2, y1, y2, color, cont) => {
  const width = x2 - x1;
  const height = y2 - y1;
  console.assert(width == height);

  let offsets, colors, count;
  if (width in cache) {
    const elem = cache[width];
    offsets = elem.offsets;
    colors = elem.colors;
    count = elem.count;
  } else {
    offsets = new Float32Array(CHUNK_AMOUNT * 4);
    colors = new Float32Array(CHUNK_AMOUNT * 4);
    count = { n: 0 };
    cache[width] = { offsets, colors, count };
  }

  offsets[count.n * 4] = x1;
  offsets[count.n * 4 + 1] = y1;
  offsets[count.n * 4 + 2] = x1;
  offsets[count.n * 4 + 3] = y1;

  colors[count.n * 4] = color == "Black";
  colors[count.n * 4 + 1] = color == "Grey";
  colors[count.n * 4 + 2] = color == "White";
  colors[count.n * 4 + 3] = 1.0; // Alpha

  if (count.n++ == CHUNK_AMOUNT) {
    const geometry = new PIXI.Geometry();
    geometry.addAttribute(
      "aVertexPosition",
      [
        0,
        0, // Top-left
        width,
        0, // Top-right
        width,
        height, // Bottom-right
        0,
        height, // Bottom-left
      ],
      2,
    );
    geometry.addIndex([0, 1, 2, 0, 2, 3]);
    geometry.addAttribute("aOffset", offsets, 4, false, true);
    geometry.addAttribute("aColor", colors, 4, false, true);
    const mesh = new PIXI.Mesh({ geometry, shader });
    mesh.drawMode = PIXI.DRAW_MODES.TRIANGLES;
    // mesh.alpha = 1;
    // mesh.zIndex = 100;
    // mesh.blendMode = "overlay";
    app.stage.addChild(mesh);

    console.log(app.renderer.gl.getError());
    delete cache[x2 - x1];
    // count.n = 0;
  }
};

const flush = () => {
  console.log(cache);
  // cache = {};
  // drawScreen(ctxs, colors);
  console.log("DONE!");
};

function helpSyntax() {
  alert(
    "The syntax uses standard notations for de Bruijn indexed lambda calculus. The de Bruijn indices start at 0. You can use `\\`, `λ`, or `[..]` for abstractions. Applications use parenthesis and assume left associativity by default. All terms can also be replaced by a string of binary lambda calculus (BLC) - useful if you're not comfortable with de Bruijn indices (e.g. by using John Tromp's `lam` compiler).\nYou can define substitutions (like in the presets) using `=`.\n\nHave fun!",
  );
}
