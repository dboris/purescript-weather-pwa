"use strict";

exports.register_ = function (scriptURL, errback, callback) {
  if ("serviceWorker" in navigator) {
    navigator.serviceWorker.register(scriptURL)
      .then(function (registration) {
        callback(registration)();
      });
  }
  else {
    errback(new Error("serviceWorker API not supported"))();
  }
};
