import { Socket } from "phoenix";
import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const playerIdKey = "playerId";

const envApiBaseUrl = process.env.ELM_APP_API_BASE_URL;

const serverBaseUrl = envApiBaseUrl || "192.168.1.14:4000";

const useHttps = !envApiBaseUrl;

// const socketProtocol = useHttps ? "wss" : "ws";
const socketProtocol = "ws"

const socket = new Socket(`${socketProtocol}://${serverBaseUrl}/socket`, {});
socket.connect();

let gameChannel;

let playerId = localStorage.getItem(playerIdKey);

const uuidv4 = () => 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
  .replace(/[xy]/g, c => {
    const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  }
)

if (!playerId) {
  playerId = uuidv4();
  localStorage.setItem(playerIdKey, playerId)
}

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    playerId: playerId,
  }
});

const attachGameChannelListeners = (gc) => {
  // gc.on("players:updated", ({ players }) => {
  //   app.ports.playersUpdated.send(players);
  // });

  gc.on("game:updated", (gameState) => {
    app.ports.gameUpdated.send(gameState);
  });
};

app.ports.joinGame.subscribe(([gameId, playerId]) => {
  gameChannel = socket.channel(`game:${gameId}`, playerId);

  attachGameChannelListeners(gameChannel);

  gameChannel.join().receive("ok", (response) => {
    app.ports.gameUpdated.send(response);
  });
});

app.ports.requestJump.subscribe((playerId) => {
  gameChannel.push("player_jump", {player_id: playerId});
});

app.ports.requestMove.subscribe(({playerId, x, y}) => {
  gameChannel.push("player_move", {player_id: playerId, x: x / 1, y: y / 1});
});

app.ports.requestRotate.subscribe(({playerId, angle}) => {
  gameChannel.push("player_rotate", {player_id: playerId, angle: angle});
});

// app.ports.startGame.subscribe((data) => {
//   gameChannel.push("start_game", data);
// });

window.onbeforeunload = () => {
  if (gameChannel) {
    gameChannel.leave();
  }
};

// prevent zoom on doubletap
let lastTouchEnd = 0;
document.addEventListener(
  "touchend",
  (event) => {
    const now = new Date().getTime();
    if (now - lastTouchEnd <= 300) {
      event.preventDefault();
    }
    lastTouchEnd = now;
  },
  false
);

registerServiceWorker();
