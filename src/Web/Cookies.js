"use strict";
// module Web.Cookies

// https://learn.javascript.ru/cookie
exports._setCookie = function(name) {
  return function(value) {
    return function(options) {
      return function() {
        options = options || {};

        var expires = options.expires;

        if (typeof expires == "number" && expires) {
          var d = new Date();
          d.setTime(d.getTime() + expires * 1000);
          expires = options.expires = d;
        }
        if (expires && expires.toUTCString) {
          options.expires = expires.toUTCString();
        }

        value = encodeURIComponent(value);

        var updatedCookie = name + "=" + value;

        for (var propName in options) {
          updatedCookie += "; " + propName;
          var propValue = options[propName];
          if (propValue !== true) {
            updatedCookie += "=" + propValue;
          }
        }

        document.cookie = updatedCookie;
      };
    };
  };
};

exports._getCookie = function(name) {
  return function() {
    var matches = document.cookie.match(
      new RegExp(
        "(?:^|; )" +
          name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, "\\$1") +
          "=([^;]*)"
      )
    );
    var data = matches ? decodeURIComponent(matches[1]) : undefined;

    if (data == undefined || data == null) {
      return [];
    } else {
      return [data];
    }
  };
};

exports.deleteCookie = function(name) {
  return function() {
    exports._setCookie(name)("")({ expires: -1 })();
  };
};
