exports.toORecordImpl = function (unMaybe) {
  return function (optKeys) {
    return function (r) {
      var o = Object.assign({}, r);
      for (var i = 0; i < optKeys.length; i++) {
        var key = optKeys[i];
        var maybeValue = r[key];
        var value = unMaybe(maybeValue);

        if (typeof value !== "undefined") {
          o[key] = value;
        } else {
          delete o[key];
        }
      }
      return o;
    };
  };
};

exports.fromORecord = function (r) {
  return r;
};

exports.undefined = undefined;
