//Hot reload fix
document.body.innerHTML = "";
require("./css/main.scss");
require("./output/Main").main();

var enabled = false;
function enableNoSleep() {
  if (!enabled) {
    enabled = true;
    var NoSleep = require("nosleep.js");
    var noSleep = new NoSleep();
    noSleep.enable();
    document.removeEventListener("touchstart", enableNoSleep, false);
    document.removeEventListener("click", enableNoSleep, false);
  }
}
document.addEventListener("click", enableNoSleep, false);
document.addEventListener("touchstart", enableNoSleep, false);
