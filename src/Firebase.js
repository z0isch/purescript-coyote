"use strict";

var firebase = require("firebase");

firebase.initializeApp({
  apiKey: "AIzaSyD5jPEuT-cp8at22Xe_uqo2VDhFezFA6jM",
  authDomain: "coyote-game.firebaseapp.com",
  databaseURL: "https://coyote-game.firebaseio.com",
  projectId: "coyote-game",
  storageBucket: "coyote-game.appspot.com",
  messagingSenderId: "61318787330"
});

exports.foo = function(f) {
  return function() {
    firebase
      .database()
      .ref("users/t")
      .on("value", function(s) {
        f(s.val())();
      });
  };
};
