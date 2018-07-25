"use strict";

var firebase = require("firebase/app");
require("firebase/database");

//Hot reload fix
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

function gameRef(id) {
  return firebase.database().ref("game/" + id);
}
exports._new = function(id, json) {
  return function() {
    gameRef(id).set(btoa(JSON.stringify(json)));
  };
};

exports._update = function(id, json, left, right) {
  return function() {
    return new Promise(function(resolve, reject) {
      gameRef(id).transaction(function(curr) {
        if (curr === null) {
          //Not sure why this makes any sense
          return btoa(JSON.stringify(json));
        } else {
          var currVal = JSON.parse(atob(curr));
          if (currVal.stateHash === json.stateHash) {
            resolve(right());
            json.stateHash = require("uuid").v4();
            return btoa(JSON.stringify(json));
          } else {
            resolve(left(currVal));
            return;
          }
        }
      });
    });
  };
};

exports._get = function(id) {
  return function() {
    return new Promise(function(resolve, reject) {
      firebase;
      gameRef(id).once("value", function(s) {
        resolve(JSON.parse(atob(s.val())));
      });
    });
  };
};

exports._subscribe = function(id, f) {
  return function() {
    gameRef(id).on("value", function(s) {
      if (s.val() !== null) {
        f(JSON.parse(atob(s.val())))();
      }
    });
  };
};
