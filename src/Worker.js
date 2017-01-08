"use strict";

// The SW will be shutdown when not in use to save memory,
// be aware that any global state is likely to disappear

exports.onInstall = function (callback) {
  return function () {
    self.addEventListener("install", function (event) {
      callback(event)();
    });
  };
}
