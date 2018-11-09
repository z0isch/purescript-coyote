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
}

require("./css/main.scss");

function main() {
  if (process.env.NODE_ENV === "development") require("./output/Main").main();
  //We're building the optimized version here
  else require("./output");
}

if (module.hot) {
  module.hot.dispose(function() {});

  module.hot.accept(function() {
    document.getElementById("coyote").innerHTML = "";
    main();
  });
}
main();
