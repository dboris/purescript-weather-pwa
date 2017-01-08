"use strict";

// The SW will be shutdown when not in use to save memory,
// be aware that any global state is likely to disappear

// TODO export to PS
function fetchCached(fetchEvent) {
  var request = fetchEvent.request;
  console.log("[ServiceWorker] Fetch", request.url);
  fetchEvent.respondWith(
    caches.match(request)
      .then(function (response) {
        return response || fetch(request);
      })
  );
}

self.addEventListener("fetch", fetchCached);

exports.onInstall = function (callback) {
  return function () {
    self.addEventListener("install", function (event) {
      callback(event)();
    });
  };
}

exports.onFetch = function (callback) {
  return function () {
    self.addEventListener("fetch", function (event) {
      callback(event)();
    });
  };
}
