"use strict";

exports.openCache_ = function (cacheName, errback, callback) {
  return function () {
    try {
      caches.open(cacheName)
        .then(function (cache) {
          callback(cache)();
        });
    }
    catch (err) {
      errback(err)();
    }
  };
};

exports.hasCache_ = function (cacheName, errback, callback) {
  return function () {
    caches.has(cacheName).then(callback());
  };
};

exports.deleteCache_ = function (cacheName, errback, callback) {
  return function () {
    caches.delete(cacheName).then(callback());
  };
};

exports.add_ = function (cache, url, errback, callback) {
  return function () {
    cache.add(url)
      .then(callback())
      .catch(function (err) {
        errback(err)();
      });
  };
};

exports.addAll_ = function (cache, urls, errback, callback) {
  return function () {
    cache.addAll(urls)
      .then(callback())
      .catch(function (err) {
        errback(err)();
      });
  };
};

exports.match_ = function (cache, request, errback, callback) {
  return function () {
    cache.match(request)
      .then(callback())
      .catch(function (err) {
        errback(err)();
      });
  };
};

exports.matchCaches_ = function (request, errback, callback) {
  return function () {
    caches.match(request)
      .then(callback())
      .catch(function (err) {
        errback(err)();
      });
  };
};

exports.deleteOldCaches_ = function (cacheNames, errback, callback) {
  return function () {
    caches.keys()
      .then(function (keyList) {
        Promise.all(
          keyList.map(function (key) {
            if (!cacheNames.includes(key)) {
              console.log("[ServiceWorker] Removing cache", key);
              return caches.delete(key);
            }
          }))
        .then(callback())
        .catch(function (err) {
            errback(err)();
        });
      });
  };
};
