var NoSleep = require("nosleep.js");
var noSleep = new NoSleep();

exports._enable = function() {
  noSleep.enable();
};

exports._disable = function() {
  noSleep.disable();
};
