// module Standalone.Router.Dispatch

var cb = function() { console.error("callback not yet set");};
var oldUrl = null;


exports.routeChanged = function(cb_) {
  return function() { // Eff
    // TODO - hash changes
    cb = cb_;
  }
}

// Ensure callback is called after document load.
window.onload = function() {

  newUrl = document.location.pathname + document.location.search;
  cb(null)(newUrl)();
  oldUrl = newUrl;
}

exports.pushState = function(newUrl) {
  return function() { // Eff
    history.pushState({}, "", newUrl);
    cb(oldUrl)(newUrl)();
    oldUrl = newUrl;
  }
}
