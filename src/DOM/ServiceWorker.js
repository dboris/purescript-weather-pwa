"use strict";

exports.register_ = function (scriptURL, errback, callback) {
  return function () {
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
};

exports.onInstall_ = function (callback) {
  return function () {
    self.addEventListener("install", function (event) { callback(event)(); });
  };
};

exports.onActivate_ = function (callback) {
  return function () {
    self.addEventListener("activate", function (event) { callback(event)(); });
  };
};


// Extends the lifetime of the event. Prevents the browser from terminating
// the service worker before asynchronous operations within the event have completed.
exports.waitUntil_ = function (event, f) {
  return function () {
    event.waitUntil(new Promise(function (resolve, reject) {
      try {
        f();
        resolve();
      }
      catch (err) {
        reject();
      }
    }));
  };
};

exports.claimClients_ = function () {
  self.clients.claim();
};
