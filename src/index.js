import { Socket } from "phoenix";
import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const playerIdKey = "playerId";

const envApiBaseUrl = process.env.ELM_APP_API_BASE_URL;

const serverBaseUrl = envApiBaseUrl || "settle-it.gigalixirapp.com";

const useHttps = !envApiBaseUrl;

const socketProtocol = useHttps ? "wss" : "ws";

const socket = new Socket(`${socketProtocol}://${serverBaseUrl}/socket`, {});
socket.connect();

let gameChannel;

let playerId = localStorage.getItem(playerIdKey);

const uuidv4 = () => 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
  .replace(/[xy]/g, c => {
    const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  }
);

const generateGameCode = () => {
  let result = "";
  const characters = "abcdefghijklmnopqrstuvwxyz";
  const charactersLength = characters.length;
  for ( let i = 0; i < 4; i++ ) {
     result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

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
  gc.on("game:updated", (gameState) => {
    app.ports.gameUpdated.send(gameState);
  });

  gc.on("bodies:updated", (bodiesState) => {
    app.ports.bodiesUpdated.send(bodiesState);
  });
};

app.ports.joinGame.subscribe(({gameId, playerId, topic}) => {
  const joinPayload = topic ? {player_id: playerId, topic: topic} : {player_id: playerId};
  const gameCode = (gameId || generateGameCode()).toLowerCase();

  gameChannel = socket.channel(`game:${gameCode}`, joinPayload);

  attachGameChannelListeners(gameChannel);

  gameChannel.join().receive("ok", () => {});
});

app.ports.startGame.subscribe(() => {
  gameChannel.push("start_game", {});
})

app.ports.createTeam.subscribe(({playerId, name}) => {
  gameChannel.push("create_team", {player_id: playerId, name: name});
});

app.ports.deleteTeam.subscribe((teamId) => {
  gameChannel.push("delete_team", {team_id: teamId});
});

app.ports.joinTeam.subscribe(({playerId, teamId}) => {
  gameChannel.push("player_join_team", {player_id: playerId, team_id: teamId});
});

app.ports.updatePlayerName.subscribe(({playerId, name}) => {
  gameChannel.push("player_update_name", {player_id: playerId, name: name});
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

app.ports.requestShoot.subscribe(({playerId, position, linvel}) => {
  gameChannel.push("player_shoot", { player_id: playerId, position, linvel});
});

window.onbeforeunload = () => {
  if (gameChannel) {
    gameChannel.leave();
  }
};

// prevent zoom on doubletap
let lastTouchEnd = 0;
let touchTapDelta = 500;
document.addEventListener(
  "touchend",
  (event) => {
    const now = new Date().getTime();
    if (now - lastTouchEnd <= touchTapDelta) {
      event.preventDefault();
    }
    lastTouchEnd = now;
  },
  false
);

registerServiceWorker();
