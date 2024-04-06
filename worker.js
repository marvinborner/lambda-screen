let canvas, screen;

self.onmessage = (msg) => {
  if (msg.data == "clear") {
    screen.clearRect(0, 0, canvas.width, canvas.height);
  } else if ("canvas" in msg.data) {
    canvas = msg.data.canvas;
    screen = canvas.getContext("2d");
  } else {
    [color, x, y, width, height] = msg.data;
    screen.fillStyle = color;
    screen.fillRect(x, y, width, height);
  }
};
