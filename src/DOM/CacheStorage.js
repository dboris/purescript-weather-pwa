"use strict";

exports.openCache_ = function (cacheName, errback, callback) {
  return function () {
    try {
      caches.open(cacheName).then(function (cache) {
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
    caches.has(cacheName).then(callback);
  };
};

exports.deleteCache_ = function (cacheName, errback, callback) {
  return function () {
    caches.delete(cacheName).then(callback);
  };
};

exports.add_ = function (cache, url, errback, callback) {
  return function () {
    cache.add(url)
      .then(function () { callback()() })
      .catch(function (err) { errback(err)() });
  };
};

exports.addAll_ = function (cache, urls, errback, callback) {
  return function () {
    cache.addAll(urls).then(callback);
  };
};
