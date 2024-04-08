let canvas, gl;

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
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.uniform4f(colorUniformLocation, ...color);
    gl.drawArrays(gl.TRIANGLE_FAN, 0, 4);
  };
};

let useWebGL = true;

let draw;
self.onmessage = (msg) => {
  if (msg.data == "clear") {
    if (useWebGL) {
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
    } else {
      gl.clearRect(0, 0, canvas.width, canvas.height);
    }
  } else if ("canvas" in msg.data) {
    canvas = msg.data.canvas;
    useWebGL = msg.data.useWebGL;
    if (useWebGL) {
      console.log("using WebGL");
      gl = canvas.getContext("webgl", { preserveDrawingBuffer: true });
      if (!gl) return self.onmessage({ canvas, useWebGL: false });
      gl.viewport(0, 0, canvas.width, canvas.height);
      draw = initGL();
    } else {
      console.log("using canvas");
      gl = canvas.getContext("2d");
    }
  } else if (useWebGL) {
    const [color, x, y, width, height] = msg.data;
    let colorArr =
      color == "white"
        ? [1, 1, 1, 1]
        : color == "black"
          ? [0, 0, 0, 1]
          : [0.1, 0.1, 0.1, 0.3];
    draw([x, y + height, x + width, y + height, x + width, y, x, y], colorArr);
  } else {
    const [color, x, y, width, height] = msg.data;
    if (width < 4 || height < 4) return;
    gl.fillStyle = color;
    gl.fillRect(x, y, width, height);
  }
};
