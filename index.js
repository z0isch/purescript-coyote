if (process.env.NODE_ENV === "production") {
  function enableFullScreen() {
    var element = document.getElementById("coyote");
    var requestMethod =
      element.requestFullScreen ||
      element.webkitRequestFullScreen ||
      element.mozRequestFullScreen ||
      element.msRequestFullScreen;
    if (requestMethod) {
      requestMethod.call(element);
    }
  }
  document.addEventListener("click", enableFullScreen, false);
} else {
  if (document.getElementById("coyote") !== null) {
    document.getElementById("coyote").innerHTML = "";
  }
}

require("./css/main.scss");
require("./output/Main").main();
