"use strict";

var firebase = require("firebase/app");
require("firebase/database");

//Shim for btoa and atob for testing
var Buffer = require("buffer").Buffer;

if (typeof btoa === "undefined") {
  global.btoa = function(str) {
    return new Buffer(str, "binary").toString("base64");
  };
}

if (typeof atob === "undefined") {
  global.atob = function(b64Encoded) {
    return new Buffer(b64Encoded, "base64").toString("binary");
  };
}

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
  gameRef(id).set(btoa(JSON.stringify(json)));
};

exports._update = function(left, right, id, stateHash, f) {
  return function(onError, onSuccess) {
    gameRef(id).transaction(
      function(curr) {
        if (curr === null) {
          //Not sure why this makes any sense
          return null;
        } else {
          var currVal = JSON.parse(atob(curr));
          if (currVal.stateHash === stateHash) {
            var newVal = f(currVal)();
            newVal.stateHash++;
            return btoa(JSON.stringify(newVal));
          } else {
            return;
          }
        }
      },
      function(error, committed, snapshot) {
        var currVal = JSON.parse(atob(snapshot.val()));
        if (error) {
          onError(error);
        } else if (!committed) {
          onSuccess(left(currVal));
        } else {
          onSuccess(right(currVal));
        }
      }
    );

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports._get = function(id) {
  return function(onError, onSuccess) {
    gameRef(id).once("value", function(s) {
      onSuccess(JSON.parse(atob(s.val())));
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports._subscribe = function(id, f) {
  gameRef(id).on("value", function(s) {
    if (s.val() !== null) {
      f(JSON.parse(atob(s.val())))();
    }
  });
};
