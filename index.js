//Hot reload fix
if (document.getElementById("coyote") !== null) {
  document.getElementById("coyote").innerHTML = "";
}
require("./css/main.scss");
require("./output/Main").main();

function enableFullScreen() {
  var element = document.getElementById("coyote");
  var requestMethod =
    element.requestFullScreen ||
    element.webkitRequestFullScreen ||
    element.mozRequestFullScreen ||
    element.msRequestFullScreen;
  if (requestMethod) {
    console.log(requestMethod);
    requestMethod.call(element);
  }
  document.removeEventListener("click", enableFullScreen, false);
}
document.addEventListener("click", enableFullScreen, false);
