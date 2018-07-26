//Hot reload fix
document.body.innerHTML = "";
require("./css/main.scss");
require("./output/Main").main();
var NoSleep = require("nosleep.js");
var noSleep = new NoSleep();
noSleep.enable();
