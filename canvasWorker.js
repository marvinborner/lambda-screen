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
  } else if ("canvas" in msg.data) {
    canvas = msg.data.canvas;
    useWebGL = msg.data.useWebGL;
    if (useWebGL) {
      console.log("using WebGL");
      // i hate this
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
      gl.viewport(0, 0, canvas.width, canvas.height);
      draw = initGL();
    } else {
      useWebGL = false;
      console.log("using canvas");
      gl = canvas.getContext("2d");
    }
  } else if ("drawAt" in msg.data) {
    const [color, x, y, width, height] = msg.data.drawAt;
    if (useWebGL) {
      let colorArr =
        color == "white"
          ? [1, 1, 1, 1]
          : color == "black"
            ? [0, 0, 0, 1]
            : [0.1, 0.1, 0.1, 0.3];
      draw(
        [x, y + height, x + width, y + height, x + width, y, x, y],
        colorArr,
      );
    } else {
      if (width < 3 || height < 3) return;
      gl.fillStyle = color;
      gl.fillRect(x, y, width, height);
    }
  } else if ("drawScreen" in msg.data) {
    const [colors, xys] = msg.data.drawScreen;
    if (useWebGL) {
      let colorArrs = colors.map((color) =>
        color == "white"
          ? [1, 1, 1, 1]
          : color == "black"
            ? [0, 0, 0, 1]
            : [0.1, 0.1, 0.1, 0.3],
      );
      // @JsDevs WHERE ZIP FUNCTION
      let i = 0;
      xys.forEach(([x, y, width, height]) =>
        draw(
          [x, y + height, x + width, y + height, x + width, y, x, y],
          colorArrs[i++],
        ),
      );
    } else {
      let i = 0;
      xys.forEach(([x, y, width, height]) => {
        if (width < 3 || height < 3) return;
        gl.fillStyle = colors[i++];
        gl.fillRect(x, y, width, height);
      });
    }
  } else {
    console.warn(msg.data);
  }
};
