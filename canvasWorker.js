let canvas, gl;
let snapshotMode = false;

const WHITE = 0;
const BLACK = 1;
const UNKNOWN = 2;

const colorFromCode = (color) => {
  if (color === WHITE || color === "white") return [1, 1, 1, 1];
  if (color === BLACK || color === "black") return [0, 0, 0, 1];
  return [0.1, 0.1, 0.1, 0.3];
};

const canvasColorFromCode = (color) => {
  if (color === WHITE || color === "white") return "white";
  if (color === BLACK || color === "black") return "black";
  return "#cccccc";
};

const createShader = (gl, type, source) => {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  const success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
  if (success) return shader;

  console.error(gl.getShaderInfoLog(shader));
  gl.deleteShader(shader);
};

const createProgram = (gl, vertexShader, fragmentShader) => {
  const program = gl.createProgram();
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);
  gl.linkProgram(program);
  const success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (success) return program;

  console.error(gl.getProgramInfoLog(program));
  gl.deleteProgram(program);
};

const initGL = () => {
  const vertexShaderSource = `
    attribute vec2 a_position;
    uniform vec2 u_resolution;
    void main() {
      vec2 inverted = vec2(a_position.x, u_resolution.y - a_position.y); // !! :)
      vec2 zeroToOne = inverted / u_resolution;
      vec2 zeroToTwo = zeroToOne * 2.0;
      vec2 clipSpace = zeroToTwo - 1.0;
      gl_Position = vec4(clipSpace, 0, 1);
    }`;
  const fragmentShaderSource = `
    precision mediump float;
    uniform vec4 u_color;
    void main() {
      gl_FragColor = u_color;
    }`;

  const vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
  const fragmentShader = createShader(
    gl,
    gl.FRAGMENT_SHADER,
    fragmentShaderSource,
  );

  const program = createProgram(gl, vertexShader, fragmentShader);
  const positionAttributeLocation = gl.getAttribLocation(program, "a_position");
  const colorUniformLocation = gl.getUniformLocation(program, "u_color");
  const resolutionUniformLocation = gl.getUniformLocation(
    program,
    "u_resolution",
  );

  const positionBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
  gl.useProgram(program);
  gl.uniform2f(resolutionUniformLocation, gl.canvas.width, gl.canvas.height);
  gl.enableVertexAttribArray(positionAttributeLocation);
  gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);

  return (positions, color) => {
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STREAM_DRAW);
    gl.uniform4f(colorUniformLocation, ...color);
    gl.drawArrays(gl.TRIANGLES, 0, positions.length / 2);
  };
};

let useWebGL = true;

let draw;
const batchPositions = (rects, colors) => {
  const counts = [0, 0, 0];
  for (let i = 0; i < colors.length; i++) counts[colors[i]]++;

  const positions = counts.map((count) => new Float32Array(count * 12));
  const offsets = [0, 0, 0];

  for (let i = 0; i < colors.length; i++) {
    const color = colors[i];
    const j = i * 4;
    const x = rects[j];
    const y = rects[j + 1];
    const width = rects[j + 2];
    const height = rects[j + 3];
    const x1 = x + width;
    const y1 = y + height;
    const out = positions[color];
    let k = offsets[color];

    out[k++] = x;
    out[k++] = y;
    out[k++] = x1;
    out[k++] = y;
    out[k++] = x;
    out[k++] = y1;
    out[k++] = x;
    out[k++] = y1;
    out[k++] = x1;
    out[k++] = y;
    out[k++] = x1;
    out[k++] = y1;
    offsets[color] = k;
  }

  return positions;
};

const drawRects = (colors, rects) => {
  if (colors.length === 0) return;

  if (useWebGL) {
    const positions = batchPositions(rects, colors);
    for (let color = 0; color < positions.length; color++)
      if (positions[color].length > 0)
        draw(positions[color], colorFromCode(color));
  } else {
    for (let i = 0; i < colors.length; i++) {
      const j = i * 4;
      const x = rects[j];
      const y = rects[j + 1];
      const width = rects[j + 2];
      const height = rects[j + 3];
      if (width < 3 || height < 3) continue;
      gl.fillStyle = canvasColorFromCode(colors[i]);
      gl.fillRect(x, y, width, height);
    }
  }
};

const initCanvas = (_canvas, _useWebGL) => {
  canvas = _canvas;
  useWebGL = _useWebGL;
  if (useWebGL) {
    try {
      gl = canvas.getContext("webgl", {
        preserveDrawingBuffer: true,
        antialias: false,
      });
      if (!gl)
        gl = canvas.getContext("experimental-webgl", {
          preserveDrawingBuffer: true,
          antialias: false,
        });
    } catch (e) {
    } finally {
      if (!gl) {
        console.error("WebGL not supported, using canvas instead.");
        useWebGL = false;
        gl = canvas.getContext("2d");
        return;
      }
    }
    console.log("using WebGL");
    gl.viewport(0, 0, canvas.width, canvas.height);
    draw = initGL();
  } else {
    useWebGL = false;
    console.log("using canvas");
    gl = canvas.getContext("2d");
  }
};

const flush = () => {
  if (useWebGL && gl) {
    if (snapshotMode) gl.finish();
    else gl.flush();
  }
  if (snapshotMode && canvas.transferToImageBitmap) {
    const bitmap = canvas.transferToImageBitmap();
    self.postMessage({ snapshot: bitmap }, [bitmap]);
  } else {
    self.postMessage({ flushed: true });
  }
};

self.onmessage = (msg) => {
  if ("clear" in msg.data) {
    if (useWebGL) {
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
    } else {
      gl.clearRect(0, 0, canvas.width, canvas.height);
    }
  } else if ("resize" in msg.data) {
    const { width, height } = msg.data.resize;
    canvas.width = width;
    canvas.height = height;
    gl.canvas.width = width;
    gl.canvas.height = height;

    if (useWebGL) {
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      draw = initGL();
    } else {
      gl.clearRect(0, 0, canvas.width, canvas.height);
    }
  } else if ("snapshotCanvas" in msg.data) {
    const { width, height, useWebGL } = msg.data.snapshotCanvas;
    snapshotMode = true;
    initCanvas(new OffscreenCanvas(width, height), useWebGL);
  } else if ("canvas" in msg.data) {
    snapshotMode = false;
    initCanvas(msg.data.canvas, msg.data.useWebGL);
  } else if ("drawRects" in msg.data) {
    const { colors, rects } = msg.data.drawRects;
    drawRects(colors, rects);
  } else if ("drawAt" in msg.data) {
    const [color, x, y, width, height] = msg.data.drawAt;
    const colorCode =
      color == "white" ? WHITE : color == "black" ? BLACK : UNKNOWN;
    drawRects(
      new Uint8Array([colorCode]),
      new Float32Array([x, y, width, height]),
    );
  } else if ("drawScreen" in msg.data) {
    const [colors, xys] = msg.data.drawScreen;
    const rects = new Float32Array(xys.length * 4);
    const colorBytes = new Uint8Array(colors.length);
    for (let i = 0; i < xys.length; i++) {
      const [x, y, width, height] = xys[i];
      const j = i * 4;
      rects[j] = x;
      rects[j + 1] = y;
      rects[j + 2] = width;
      rects[j + 3] = height;
      colorBytes[i] =
        colors[i] == "white" ? WHITE : colors[i] == "black" ? BLACK : UNKNOWN;
    }
    drawRects(colorBytes, rects);
  } else if ("flush" in msg.data) {
    flush();
  } else {
    console.warn(msg.data);
  }
};
