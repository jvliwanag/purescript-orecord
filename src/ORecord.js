exports.allRequiredImpl = function (keys) {
  return function (r) {
    var o = {};
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i];
      o[key] = r[key];
    }
    return o;
  };
};

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

exports.fromORecordImpl = function (nothing) {
  return function (just) {
    return function (optKeys) {
      return function (o) {
        var r = Object.assign({}, o);
        for (var i = 0; i < optKeys.length; i++) {
          var key = optKeys[i];
          var undefOrValue = o[key];

          if (typeof undefOrValue !== "undefined") {
            r[key] = just(undefOrValue);
          } else {
            r[key] = nothing;
          }
        }
        return r;
      };
    };
  };
};

exports.undefined = undefined;
