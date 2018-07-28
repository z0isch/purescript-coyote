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

exports._update = function(id, f, left, right) {
  return function() {
    return new Promise(function(resolve, reject) {
      gameRef(id).transaction(function(curr) {
        if (curr === null) {
          //Not sure why this makes any sense
          return null;
        } else {
          var currVal = JSON.parse(atob(curr));
          var newVal = f(currVal)();
          if (currVal.stateHash === newVal.stateHash) {
            newVal.stateHash = currVal.stateHash + 1;
            resolve(right());
            return btoa(JSON.stringify(newVal));
          } else {
            resolve(left());
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

exports.test1 = {
  stateHash: 191,
  state: {
    previousRounds: [
      { total: 9, hands: [{ b: [5], a: 0 }, { b: [4], a: 1 }] },
      { total: 25, hands: [{ b: [15], a: 0 }, { b: [10], a: 1 }] },
      { total: 2, hands: [{ b: [2], a: 0 }, { b: ["Night"], a: 1 }] },
      { total: 7, hands: [{ b: [3], a: 0 }, { b: [4], a: 1 }] },
      { total: 15, hands: [{ b: [10], a: 0 }, { b: [5], a: 1 }] },
      { total: 15, hands: [{ b: [10], a: 0 }, { b: [5], a: 1 }] },
      { total: 15, hands: [{ b: [15], a: 0 }, { b: ["Night"], a: 1 }] },
      { total: 14, hands: [{ b: [10], a: 0 }, { b: [4], a: 1 }] },
      { total: 11, hands: [{ b: [1], a: 0 }, { b: [10], a: 1 }] },
      { total: 16, hands: [{ b: [15], a: 0 }, { b: [1], a: 1 }] },
      { total: 2, hands: [{ b: ["X2"], a: 0 }, { b: [1], a: 1 }] },
      { total: -20, hands: [{ b: ["MaxNeg"], a: 0 }, { b: [20], a: 1 }] },
      { total: 0, hands: [{ b: ["Night"], a: 0 }, { b: ["Max0"], a: 1 }] },
      { total: 10, hands: [{ b: [10], a: 0 }, { b: ["Night"], a: 1 }] },
      { total: -6, hands: [{ b: [-10], a: 0 }, { b: [4], a: 1 }] },
      { total: 11, hands: [{ b: [10], a: 0 }, { b: [1], a: 1 }] },
      { total: 7, hands: [{ b: [2], a: 0 }, { b: [5], a: 1 }] },
      { total: 0, hands: [{ b: ["Max0"], a: 0 }, { b: ["X2"], a: 1 }] },
      { total: 0, hands: [{ b: [5], a: 0 }, { b: [-5], a: 1 }] },
      { total: -2, hands: [{ b: [3], a: 0 }, { b: [-5], a: 1 }] },
      { total: 20, hands: [{ b: [5], a: 0 }, { b: [15], a: 1 }] },
      { total: 20, hands: [{ b: ["Night"], a: 0 }, { b: [20], a: 1 }] },
      { total: 5, hands: [{ b: [15], a: 0 }, { b: [-10], a: 1 }] },
      { total: 17, hands: [{ b: [15], a: 0 }, { b: [2], a: 1 }] },
      { total: -10, hands: [{ b: [10], a: 0 }, { b: ["MaxNeg"], a: 1 }] },
      { total: 5, hands: [{ b: [10], a: 0 }, { b: [-5], a: 1 }] },
      { total: 15, hands: [{ b: [5], a: 0 }, { b: [10], a: 1 }] },
      { total: 2, hands: [{ b: [2], a: 0 }, { b: ["Night"], a: 1 }] },
      { total: 30, hands: [{ b: [15], a: 0 }, { b: ["X2"], a: 1 }] },
      { total: 7, hands: [{ b: [2], a: 0 }, { b: [5], a: 1 }] },
      { total: 1, hands: [{ b: [1], a: 0 }, { b: [], a: 1 }] },
      { total: 6, hands: [{ b: [4], a: 0 }, { b: [2], a: 1 }] },
      { total: 6, hands: [{ b: [2], a: 0 }, { b: [4], a: 1 }] },
      { total: 13, hands: [{ b: [3], a: 0 }, { b: [10], a: 1 }] },
      { total: 20, hands: [{ b: ["Night"], a: 0 }, { b: [20], a: 1 }] },
      { total: 4, hands: [{ b: [2], a: 0 }, { b: [2], a: 1 }] },
      { total: 5, hands: [{ b: [1], a: 0 }, { b: [4], a: 1 }] },
      { total: 13, hands: [{ b: [10], a: 0 }, { b: [3], a: 1 }] },
      { total: 9, hands: [{ b: [5], a: 0 }, { b: [4], a: 1 }] },
      { total: 16, hands: [{ b: [1], a: 0 }, { b: [15], a: 1 }] },
      { total: 4, hands: [{ b: [2], a: 0 }, { b: ["X2"], a: 1 }] },
      { total: -5, hands: [{ b: [-10], a: 0 }, { b: [5], a: 1 }] },
      { total: 7, hands: [{ b: [4], a: 0 }, { b: [3], a: 1 }] },
      { total: 18, hands: [{ b: [15], a: 0 }, { b: [3], a: 1 }] },
      { total: 0, hands: [{ b: ["Max0"], a: 0 }, { b: [5], a: 1 }] },
      { total: -4, hands: [{ b: [1], a: 0 }, { b: [-5], a: 1 }] },
      { total: 22, hands: [{ b: [2], a: 0 }, { b: [20], a: 1 }] },
      { total: 11, hands: [{ b: [1], a: 0 }, { b: [10], a: 1 }] },
      { total: 9, hands: [{ b: [5], a: 0 }, { b: [4], a: 1 }] },
      {
        total: -10,
        hands: [{ b: [10], a: 0 }, { b: ["MaxNeg", "Question"], a: 1 }]
      },
      { total: -2, hands: [{ b: [3], a: 0 }, { b: [-5], a: 1 }] },
      { total: 10, hands: [{ b: [10], a: 0 }, { b: ["Night"], a: 1 }] },
      { total: 10, hands: [{ b: ["X2"], a: 0 }, { b: [5], a: 1 }] },
      { total: -3, hands: [{ b: [-5], a: 0 }, { b: [2], a: 1 }] },
      { total: 4, hands: [{ b: [1], a: 0 }, { b: [3], a: 1 }] },
      {
        total: -3,
        hands: [{ b: ["MaxNeg", "Question"], a: 0 }, { b: [3], a: 1 }]
      },
      { total: 0, hands: [{ b: [10], a: 0 }, { b: [-10], a: 1 }] },
      { total: 9, hands: [{ b: [4], a: 0 }, { b: [5], a: 1 }] },
      { total: 13, hands: [{ b: [10], a: 0 }, { b: [3], a: 1 }] },
      { total: 2, hands: [{ b: ["Night"], a: 0 }, { b: [2], a: 1 }] },
      { total: 3, hands: [{ b: [1], a: 0 }, { b: [2], a: 1 }] },
      { total: 8, hands: [{ b: [5], a: 0 }, { b: [3], a: 1 }] },
      { total: -5, hands: [{ b: ["Night"], a: 0 }, { b: [-5], a: 1 }] }
    ],
    players: [{ b: { hand: [2] }, a: 0 }, { b: { hand: [] }, a: 1 }],
    discardPile: [],
    deck: [
      2,
      15,
      5,
      3,
      3,
      4,
      2,
      1,
      15,
      1,
      3,
      1,
      2,
      -5,
      "X2",
      "Max0",
      10,
      1,
      4,
      10,
      -10,
      5,
      4,
      3,
      "Question",
      20,
      5,
      4,
      -5,
      10,
      5,
      "Night",
      "MaxNeg"
    ]
  },
  playerMap: [
    { b: 0, a: "0fa248b3-bb42-4afb-ae87-4766bc7ae788" },
    { b: 1, a: "f5648862-51fa-4ace-914b-a83a7cd3f7f9" }
  ]
};
