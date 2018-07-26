//Hot reload fix
document.body.innerHTML = "";
require("./css/main.scss");

var NoSleep = require("nosleep.js");
var noSleep = new NoSleep();
noSleep.enable();

require("./output/Main").main();
