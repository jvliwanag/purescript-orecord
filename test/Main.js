exports.hasKey = function (key) {
  return function (r) {
    return r.hasOwnProperty(key);
  };
};

exports.unsafeGet = function (key) {
  return function (r) {
    return r[key];
  };
};
