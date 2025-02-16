import {
  Application,
  Assets,
  Sprite,
  Graphics,
} from "https://cdn.jsdelivr.net/npm/pixi.js@8.8.0/+esm";

let app;

async function init({ size, resolution, offscreen }) {
  app = new Application();
  await app.init({
    width: size,
    height: size,
    resolution,
    canvas: offscreen,
  });
}

function addSquare({ x: [x1, x2], y: [y1, y2], color }) {
  const graphics = new Graphics();
  console.log(graphics);
  graphics.rect(x1, y1, x2 - x1, y2 - y1);
  graphics.fill(0xf0ff00);
  app.stage.addChild(graphics);
}

self.onmessage = async (msg) => {
  if (!("type" in msg.data)) console.error("invalid message", msg.data);

  if (msg.data.type == "init") init(msg.data);
  else if (msg.data.type == "addSquare") addSquare(msg.data);
  else console.error("invalid type", msg.data.type);
};
