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

exports.allOptionalImpl = function (keys) {
  return function (nothing) {
    return function (just) {
      return function (r) {
        var o = {};
        for (var i = 0; i < keys.length; i++) {
          var key = keys[i];
          var v = r[key];

          if (typeof v !== "undefined") {
            o[key] = just(v);
          } else {
            o[key] = nothing;
          }
        }
        return o;
      };
    };
  };
};

exports.getRequiredImpl = function (key) {
  return function (r) {
    return r[key];
  };
};

exports.getOptionalImpl = function (key) {
  return function (nothing) {
    return function (just) {
      return function (r) {
        var v = r[key];
        if (typeof v !== "undefined") {
          return just(v);
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.getOptionalImpl = function (nothing) {
  return function (just) {
    return function (key) {
      return function (r) {
        var v = r[key];
        if (typeof v !== "undefined") {
          return just(v);
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.setImpl = function (key) {
  return function (a) {
    return function (r) {
      var o = Object.assign({}, r);
      o[key] = a;
      return o;
    };
  };
};

exports.deleteImpl = function (key) {
  return function (a) {
    return function (r) {
      var o = Object.assign({}, r);
      delete o[key];
    };
  };
};

exports.undefined = undefined;
