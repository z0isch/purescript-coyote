"use strict";

var zingtouch = require("zingtouch");

exports._bind = function(el, type, f) {
  return function() {
    var region = zingtouch.Region(el);

    region.bind(el, "swipe", function(e) {
      console.log(e);
      f(e)();
    });
  };
};

exports._unbind = function(el) {
  return function() {
    var region = zingtouch.Region(el);
    region.unbind(el);
  };
};
