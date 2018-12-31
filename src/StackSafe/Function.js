"use strict";

exports.identityImpl = [];

exports.fromFunction = function (f) {
  return [f];
};

exports.composeFunc = function(f) {
  return function(g) {
    return [f, g];
  };
};

exports.toFunction = function (fs) {
  return function (x) {
    var stack = fs.slice(),
      r = x, i, current, length;

    for (;;) {
      if (stack.length === 0) {
        break;
      }

      current = stack.pop();

      if (typeof current === "function") {
        r = current(r);
        continue;
      }

      length = current.length;
      for (i = 0; i < length; i++) {
        stack.push(current[i]);
      }
    }

    return r;
  };
};
