//Hot reload fix
if (document.getElementById("coyote") !== null) {
  document.getElementById("coyote").innerHTML = "";
}

function enableNoSleep() {
  if (!enabled) {
    var NoSleep = require("nosleep.js");
    var noSleep = new NoSleep();
    noSleep.enable();
    document.removeEventListener("touchstart", enableNoSleep, false);
    document.removeEventListener("click", enableNoSleep, false);
  }
}
document.addEventListener("click", enableNoSleep, false);
document.addEventListener("touchstart", enableNoSleep, false);

require("./css/main.scss");
require("./output/Main").main();
