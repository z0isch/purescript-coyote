"use strict";

var QRCode = require("qrcode");

exports._insert = function(el, text) {
  return function() {
    el.innerHTML = "";
    var newCanvas = document.createElement("canvas");
    el.appendChild(newCanvas);
    QRCode.toCanvas(newCanvas, text);
  };
};
