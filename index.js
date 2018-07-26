//Hot reload fix
if (document.getElementById("coyote") !== null) {
  document.getElementById("coyote").innerHTML = "";
}
require("./css/main.scss");
require("./output/Main").main();
