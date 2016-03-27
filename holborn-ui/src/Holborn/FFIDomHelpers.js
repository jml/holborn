"use strict";
// module Holborn.DomHelpers

exports.scroll = function(x) {
  return function(y) {
    return function() { // Eff
      window.scroll(x, y);
    }
  }
}
