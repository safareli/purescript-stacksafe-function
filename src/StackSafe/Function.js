"use strict";

exports.identityImpl = [];

exports.fromFunction = function (f) {
  return [f];
};

exports.composeFunc = function(f) {
  return function(g) {
    return [g, f];
  };
};

exports.toFunction = function (fs) {
  return function (x) {
    var stack = fs.slice(),
      queue = [], r = x,
      i, current, length;

    for (;;) {
      if (stack.length === 0) {
        break;
      }

      current = stack.pop();

      if (typeof current === "function") {
        queue.push(current);
        continue;
      }

      length = current.length;
      for (i = 0; i < length; i++) {
        stack.push(current[i]);
      }
    }

    for (i = queue.length - 1; i >= 0; i--) {
      r = queue[i](r);
    }

    return r;
  };
};
