import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

Elm.Main.init({
  node: document.getElementById("root"),
});

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
