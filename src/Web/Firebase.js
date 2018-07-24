"use strict";

var firebase = require("firebase/app");
require("firebase/database");

if (!firebase.apps.length) {
  firebase.initializeApp({
    apiKey: "AIzaSyD5jPEuT-cp8at22Xe_uqo2VDhFezFA6jM",
    authDomain: "coyote-game.firebaseapp.com",
    databaseURL: "https://coyote-game.firebaseio.com",
    projectId: "coyote-game",
    storageBucket: "coyote-game.appspot.com",
    messagingSenderId: "61318787330"
  });
}

exports._write = function(id, json) {
  return function() {
    firebase
      .database()
      .ref("game/" + id)
      .set(btoa(JSON.stringify(json)));
  };
};

exports._get = function(id) {
  return new Promise(function(resolve, reject) {
    firebase
      .database()
      .ref("game/" + id)
      .once("value", function(s) {
        resolve(JSON.parse(atob(s.val())));
      });
  });
};

exports._subscribe = function(id, f) {
  return function() {
    firebase
      .database()
      .ref("game/" + id)
      .on("value", function(s) {
        if (s.val() !== null) {
          f(JSON.parse(atob(s.val())))();
        }
      });
  };
};
