(() => {
  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map114 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map114($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applyFirst = function(dictApply) {
    var apply1 = apply(dictApply);
    var map32 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map32($$const)(a2))(b2);
      };
    };
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map32 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map32($$const(identity2))(a2))(b2);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map32 = map(dictApply.Functor0());
    return function(f) {
      return function(a2) {
        return function(b2) {
          return apply1(map32(f)(a2))(b2);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure16(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind16 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind16(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Control.Lazy/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var defer = function(dict) {
    return dict.defer;
  };
  var fix = function(dictLazy) {
    var defer1 = defer(dictLazy);
    return function(f) {
      var $lazy_go = $runtime_lazy("go", "Control.Lazy", function() {
        return defer1(function(v) {
          return f($lazy_go(25));
        });
      });
      var go2 = $lazy_go(25);
      return go2;
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a2) {
      return sub1(zero2)(a2);
    };
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showCharImpl = function(c) {
    var code3 = c.charCodeAt(0);
    if (code3 < 32 || code3 === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code3.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty8;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i2 = 0, l = xs.length; i2 < l; i2++) {
        ss[i2] = f(xs[i2]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show15 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key2 = reflectSymbol2($$Proxy.value);
              var focus3 = unsafeGet(key2)(record);
              return " " + (key2 + (": " + (show15(focus3) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show15 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key2 = reflectSymbol2($$Proxy.value);
            var focus3 = unsafeGet(key2)(record);
            return " " + (key2 + (": " + (show15(focus3) + " ")));
          };
        }
      };
    };
  };

  // output/Data.Generic.Rep/index.js
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value14) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value14);
  };
  var replicatePolyfill = function(count, value14) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value14;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head5, tail2) {
      this.head = head5;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head5) {
      return function(tail2) {
        return new Cons3(head5, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr5, xs) {
      return listToArray(foldr5(curryCons)(emptyList)(xs));
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty8, next, xs) {
    return xs.length === 0 ? empty8({}) : next(xs[0])(xs.slice(1));
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i2 = 0, l = xs.length; i2 < l; i2++) {
      if (f(xs[i2]))
        return just(i2);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length)
      return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var sortByImpl = function() {
    function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to);
      i2 = from3;
      j = mid;
      k = from3;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare4(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare4, fromOrdering, xs) {
      var out;
      if (xs.length < 2)
        return xs;
      out = xs.slice(0);
      mergeFromTo(compare4, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  }();
  var zipWithImpl = function(f, xs, ys) {
    var l = xs.length < ys.length ? xs.length : ys.length;
    var result = new Array(l);
    for (var i2 = 0; i2 < l; i2++) {
      result[i2] = f(xs[i2])(ys[i2]);
    }
    return result;
  };
  var unsafeIndexImpl = function(xs, n) {
    return xs[n];
  };

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind11 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind11(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind11 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind11(f)(function(f$prime) {
          return bind11(a2)(function(a$prime) {
            return pure16(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a2) {
      return function(f) {
        return f(a2);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();
  var monadEither = {
    Applicative0: function() {
      return applicativeEither;
    },
    Bind1: function() {
      return bindEither;
    }
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy2("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy2("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref3) {
    return function() {
      return ref3.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref3) {
      return function() {
        var t = f(ref3.value);
        ref3.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref3) {
      return function() {
        ref3.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_2 = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go2(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map4(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var pure_ = function(a2) {
    return function() {
      return a2;
    };
  };
  var bind_ = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };
  function forST(lo) {
    return function(hi) {
      return function(f) {
        return function() {
          for (var i2 = lo; i2 < hi; i2++) {
            f(i2)();
          }
        };
      };
    };
  }

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy3("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  var sortByImpl2 = function() {
    function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to);
      i2 = from3;
      j = mid;
      k = from3;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare4(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare4, fromOrdering, xs) {
      if (xs.length < 2)
        return xs;
      mergeFromTo(compare4, fromOrdering, xs, xs.slice(0), 0, xs.length);
      return xs;
    };
  }();
  var pushImpl = function(a2, xs) {
    return xs.push(a2);
  };

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a2) {
      return function() {
        return fn(a2);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a2) {
      return function(b2) {
        return function() {
          return fn(a2, b2);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var bind2 = /* @__PURE__ */ bind(bindST);
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var run2 = function(st) {
    return bind2(st)(unsafeFreeze)();
  };
  var push = /* @__PURE__ */ runSTFn2(pushImpl);

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x) {
    return x;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond7 = applySecond(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond7(f($454));
        })(pure16(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_15 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_15(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty3;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append9 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append9(f(x))(acc);
          };
        })(mempty3);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var find = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(p2) {
      var go2 = function(v) {
        return function(v1) {
          if (v instanceof Nothing && p2(v1)) {
            return new Just(v1);
          }
          ;
          return v;
        };
      };
      return foldl22(go2)(Nothing.value);
    };
  };
  var any = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
    };
  };
  var elem = function(dictFoldable) {
    var any1 = any(dictFoldable)(heytingAlgebraBoolean);
    return function(dictEq) {
      var $462 = eq(dictEq);
      return function($463) {
        return any1($462($463));
      };
    };
  };

  // output/Data.Function.Uncurried/foreign.js
  var mkFn5 = function(fn) {
    return function(a2, b2, c, d, e) {
      return fn(a2)(b2)(c)(d)(e);
    };
  };
  var runFn2 = function(fn) {
    return function(a2) {
      return function(b2) {
        return fn(a2, b2);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return fn(a2, b2, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map32) {
        return function(pure16) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure16([]);
                  case 1:
                    return map32(array1)(f(array[bot]));
                  case 2:
                    return apply2(map32(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map32(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply2(map32(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value14 = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var unsafeIndex = function() {
    return runFn2(unsafeIndexImpl);
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ function() {
    return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var sortBy = function(comp) {
    return runFn3(sortByImpl)(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 897, column 38 - line 900, column 11): " + [v.constructor.name]);
    });
  };
  var sort = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(xs) {
      return sortBy(compare4)(xs);
    };
  };
  var intersperse = function(a2) {
    return function(arr) {
      var v = length(arr);
      if (v < 2) {
        return arr;
      }
      ;
      if (otherwise) {
        return run2(function() {
          var unsafeGetElem = function(idx) {
            return unsafeIndex1(arr)(idx);
          };
          return function __do2() {
            var out = newSTArray();
            push(unsafeGetElem(0))(out)();
            forST(1)(v)(function(idx) {
              return function __do3() {
                push(a2)(out)();
                return $$void3(push(unsafeGetElem(idx))(out))();
              };
            })();
            return out;
          };
        }());
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 623, column 21 - line 633, column 17): " + [v.constructor.name]);
    };
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    var eq2 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq2(v)(x);
      });
    };
  };
  var notElem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a2) {
      return function(arr) {
        return isNothing(elemIndex1(a2)(arr));
      };
    };
  };
  var elem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a2) {
      return function(arr) {
        return isJust(elemIndex1(a2)(arr));
      };
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var some = function(dictAlternative) {
    var apply1 = apply(dictAlternative.Applicative0().Apply0());
    var map32 = map(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer6 = defer(dictLazy);
      return function(v) {
        return apply1(map32(cons)(v))(defer6(function(v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many = function(dictAlternative) {
    var alt9 = alt(dictAlternative.Plus1().Alt0());
    var pure16 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt9(some(dictAlternative)(dictLazy)(v))(pure16([]));
      };
    };
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty3);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var monoidList = /* @__PURE__ */ function() {
    return {
      mempty: Nil.value,
      Semigroup0: function() {
        return semigroupList;
      }
    };
  }();
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.List/index.js
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var uncons2 = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map5(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons2(xs));
    });
  };
  var singleton3 = function(a2) {
    return new Cons(a2, Nil.value);
  };
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var manyRec = function(dictMonadRec) {
    var bind16 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt9 = alt(Alt0);
      var map114 = map(Alt0.Functor0());
      var pure16 = pure(dictAlternative.Applicative0());
      return function(p2) {
        var go2 = function(acc) {
          return bind16(alt9(map114(Loop.create)(p2))(pure16(new Done(unit))))(function(aa) {
            return pure16(bimap2(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse2(acc);
            })(aa));
          });
        };
        return tailRecM5(go2)(Nil.value);
      };
    };
  };
  var some2 = function(dictAlternative) {
    var apply2 = apply(dictAlternative.Applicative0().Apply0());
    var map114 = map(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer6 = defer(dictLazy);
      return function(v) {
        return apply2(map114(Cons.create)(v))(defer6(function(v1) {
          return many2(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many2 = function(dictAlternative) {
    var alt9 = alt(dictAlternative.Plus1().Alt0());
    var pure16 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt9(some2(dictAlternative)(dictLazy)(v))(pure16(Nil.value));
      };
    };
  };
  var length2 = /* @__PURE__ */ foldl3(function(acc) {
    return function(v) {
      return acc + 1 | 0;
    };
  })(0);
  var index = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v instanceof Nil) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Cons && v1 === 0) {
          $tco_done = true;
          return new Just(v.value0);
        }
        ;
        if (v instanceof Cons) {
          $tco_var_v = v.value1;
          $copy_v1 = v1 - 1 | 0;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 281, column 1 - line 281, column 44): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error5) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error5) {
        setTimeout(function() {
          throw error5;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error5) {
        return left(error5);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error5) {
        k(left(error5))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size4 !== 0) {
          size4--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error5) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step5 = aff;
      var fail3 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step5 = bhead(step5);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util.left(e);
                step5 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step5)) {
                status = RETURN;
                fail3 = step5;
                step5 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step5 = util.fromRight(step5);
              }
              break;
            case CONTINUE:
              switch (step5.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step5._2;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step5 = util.right(step5._1);
                  } else {
                    status = STEP_BIND;
                    step5 = step5._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step5 = runSync(util.left, util.right, step5._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step5 = runAsync(util.left, step5._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step5 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util.left(step5._1);
                  step5 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step5, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step5, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step5, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step5, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step5._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step5._1) {
                    tmp.run();
                  }
                  step5 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step5 = sequential3(util, supervisor, step5._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step5 = interrupt || fail3 || step5;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step5 = attempt._2(util.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step5 = util.fromRight(step5);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util.fromRight(step5);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step5 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step5, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step5 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step5 = attempt._1.failed(util.fromLeft(fail3))(attempt._2);
                    } else {
                      step5 = attempt._1.completed(util.fromRight(step5))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step5, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step5 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step5 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step5));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util.fromLeft(fail3);
                }, 0);
              } else if (util.isLeft(step5) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step5);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step5)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error5, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error5);
              status = COMPLETED;
              step5 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error5);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step5(error5)), attempts, interrupt);
                }
                status = RETURN;
                step5 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error5);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step5 = null;
                fail3 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error5, par2, cb2) {
        var step5 = par2;
        var head5 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step5.tag) {
              case FORKED:
                if (step5._3 === EMPTY) {
                  tmp = fibers[step5._1];
                  kills2[count++] = tmp.kill(error5, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head5 === null) {
                  break loop;
                }
                step5 = head5._2;
                if (tail2 === null) {
                  head5 = null;
                } else {
                  head5 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step5 = step5._2;
                break;
              case APPLY:
              case ALT:
                if (head5) {
                  tail2 = new Aff2(CONS, head5, tail2);
                }
                head5 = step5;
                step5 = step5._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head5, tail2) {
        var fail3, step5, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail3 = result;
          step5 = null;
        } else {
          step5 = result;
          fail3 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head5 === null) {
              cb(fail3 || step5)();
              return;
            }
            if (head5._3 !== EMPTY) {
              return;
            }
            switch (head5.tag) {
              case MAP:
                if (fail3 === null) {
                  head5._3 = util.right(head5._1(util.fromRight(step5)));
                  step5 = head5._3;
                } else {
                  head5._3 = fail3;
                }
                break;
              case APPLY:
                lhs = head5._1._3;
                rhs = head5._2._3;
                if (fail3) {
                  head5._3 = fail3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail3 === lhs ? head5._2 : head5._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail3, null, null);
                      } else {
                        join3(fail3, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step5 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head5._3 = step5;
                }
                break;
              case ALT:
                lhs = head5._1._3;
                rhs = head5._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail3 = step5 === lhs ? rhs : lhs;
                  step5 = null;
                  head5._3 = fail3;
                } else {
                  head5._3 = step5;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step5 === lhs ? head5._2 : head5._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step5, null, null);
                      } else {
                        join3(step5, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head5 = null;
            } else {
              head5 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step5 = par;
        var head5 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step5.tag) {
                  case MAP:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(MAP, step5._1, EMPTY, EMPTY);
                    step5 = step5._2;
                    break;
                  case APPLY:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(APPLY, EMPTY, step5._2, EMPTY);
                    step5 = step5._1;
                    break;
                  case ALT:
                    if (head5) {
                      tail2 = new Aff2(CONS, head5, tail2);
                    }
                    head5 = new Aff2(ALT, EMPTY, step5._2, EMPTY);
                    step5 = step5._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step5;
                    step5 = new Aff2(FORKED, fid, new Aff2(CONS, head5, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step5)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head5 === null) {
                  break loop;
                }
                if (head5._1 === EMPTY) {
                  head5._1 = step5;
                  status = CONTINUE;
                  step5 = head5._2;
                  head5._2 = EMPTY;
                } else {
                  head5._2 = step5;
                  step5 = head5;
                  if (tail2 === null) {
                    head5 = null;
                  } else {
                    head5 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step5;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error5, cb2) {
        interrupt = util.left(error5);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error5, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map32 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map32(Right.create)(a2))(function($52) {
        return pure16(Left.create($52));
      });
    };
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Writer.Class/index.js
  var tell = function(dict) {
    return dict.tell;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map6 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map114 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map114(map6(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind11 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind11(v)(either(function($187) {
            return pure16(Left.create($187));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };

  // output/Control.Monad.Writer.Trans/index.js
  var WriterT = function(x) {
    return x;
  };
  var runWriterT = function(v) {
    return v;
  };
  var mapWriterT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorWriterT = function(dictFunctor) {
    var map32 = map(dictFunctor);
    return {
      map: function(f) {
        return mapWriterT(map32(function(v) {
          return new Tuple(f(v.value0), v.value1);
        }));
      }
    };
  };
  var applyWriterT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    return function(dictApply) {
      var apply2 = apply(dictApply);
      var Functor0 = dictApply.Functor0();
      var map32 = map(Functor0);
      var functorWriterT1 = functorWriterT(Functor0);
      return {
        apply: function(v) {
          return function(v1) {
            var k = function(v3) {
              return function(v4) {
                return new Tuple(v3.value0(v4.value0), append9(v3.value1)(v4.value1));
              };
            };
            return apply2(map32(k)(v))(v1);
          };
        },
        Functor0: function() {
          return functorWriterT1;
        }
      };
    };
  };
  var bindWriterT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    var applyWriterT1 = applyWriterT(dictSemigroup);
    return function(dictBind) {
      var bind11 = bind(dictBind);
      var Apply0 = dictBind.Apply0();
      var map32 = map(Apply0.Functor0());
      var applyWriterT2 = applyWriterT1(Apply0);
      return {
        bind: function(v) {
          return function(k) {
            return bind11(v)(function(v1) {
              var v2 = k(v1.value0);
              return map32(function(v3) {
                return new Tuple(v3.value0, append9(v1.value1)(v3.value1));
              })(v2);
            });
          };
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var applicativeWriterT = function(dictMonoid) {
    var mempty3 = mempty(dictMonoid);
    var applyWriterT1 = applyWriterT(dictMonoid.Semigroup0());
    return function(dictApplicative) {
      var pure16 = pure(dictApplicative);
      var applyWriterT2 = applyWriterT1(dictApplicative.Apply0());
      return {
        pure: function(a2) {
          return pure16(new Tuple(a2, mempty3));
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var monadWriterT = function(dictMonoid) {
    var applicativeWriterT1 = applicativeWriterT(dictMonoid);
    var bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
    return function(dictMonad) {
      var applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
      var bindWriterT2 = bindWriterT1(dictMonad.Bind1());
      return {
        Applicative0: function() {
          return applicativeWriterT2;
        },
        Bind1: function() {
          return bindWriterT2;
        }
      };
    };
  };
  var monadTellWriterT = function(dictMonoid) {
    var Semigroup0 = dictMonoid.Semigroup0();
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function(dictMonad) {
      var monadWriterT22 = monadWriterT1(dictMonad);
      return {
        tell: function() {
          var $252 = pure(dictMonad.Applicative0());
          var $253 = Tuple.create(unit);
          return function($254) {
            return WriterT($252($253($254)));
          };
        }(),
        Semigroup0: function() {
          return Semigroup0;
        },
        Monad1: function() {
          return monadWriterT22;
        }
      };
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_8 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_15 = traverse_8(dictFoldable);
        return function(f) {
          var $51 = traverse_15(function($53) {
            return parallel4(f($53));
          });
          return function($52) {
            return sequential3($51($52));
          };
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictApplicative) {
      var parTraverse_2 = parTraverse_1(dictApplicative);
      return function(dictFoldable) {
        return parTraverse_2(dictFoldable)(identity4);
      };
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var map7 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map1 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy4("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Apply0: function() {
      return applyAff;
    },
    Apply1: function() {
      return applyParAff;
    }
  };
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var applicativeParAff = {
    pure: function($76) {
      return parallel2(pure22($76));
    },
    Apply0: function() {
      return applyParAff;
    }
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($77) {
    return Canceler($$const(liftEffect2($77)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map7(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void4(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map7(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($83) {
        return liftEffect2(k($83));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void4(runAff(k)(aff));
    };
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));

  // output/Data.Lazy/foreign.js
  var defer2 = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l) {
    return l();
  };

  // output/Control.Monad.State.Trans/index.js
  var runStateT = function(v) {
    return v;
  };
  var monadTransStateT = {
    lift: function(dictMonad) {
      var bind11 = bind(dictMonad.Bind1());
      var pure16 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind11(m)(function(x) {
            return pure16(new Tuple(x, s));
          });
        };
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map32 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map32(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind11 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind11(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure16 = pure(dictMonad.Applicative0());
    return {
      pure: function(a2) {
        return function(s) {
          return pure16(new Tuple(a2, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadTellStateT = function(dictMonadTell) {
    var Monad1 = dictMonadTell.Monad1();
    var Semigroup0 = dictMonadTell.Semigroup0();
    var monadStateT1 = monadStateT(Monad1);
    return {
      tell: function() {
        var $201 = lift3(Monad1);
        var $202 = tell(dictMonadTell);
        return function($203) {
          return $201($202($203));
        };
      }(),
      Semigroup0: function() {
        return Semigroup0;
      },
      Monad1: function() {
        return monadStateT1;
      }
    };
  };

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map8 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map8(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map9 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map9(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
    } else {
      return nothing;
    }
  }
  function focus(elt) {
    return function() {
      return elt.focus();
    };
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var toElement = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom22);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var load2 = "load";
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";

  // output/Halogen.Aff.Util/index.js
  var bind3 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map10 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind3(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map10(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind3(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure3)(body2);
    });
  });

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.Map.Internal/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  }();
  var Split = /* @__PURE__ */ function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  }();
  var SplitLast = /* @__PURE__ */ function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  }();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 680, column 5 - line 684, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + function() {
          var $277 = l.value0 > r.value0;
          if ($277) {
            return l.value0;
          }
          ;
          return r.value0;
        }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 686, column 5 - line 690, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 678, column 32 - line 690, column 68): " + [l.constructor.name]);
  };
  var singleton4 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height8 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 735, column 12 - line 737, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height8(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height8(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton4(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 695, column 40 - line 716, column 34): " + [l.constructor.name]);
    };
  }();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy5("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(771)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(774)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 769, column 5 - line 777, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 765, column 34 - line 777, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(764);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy5("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(757)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 754, column 37 - line 758, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(753);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 742, column 25 - line 746, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var lookup = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare4(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 281, column 7 - line 284, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 278, column 8 - line 284, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var insert = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton4(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare4(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 469, column 7 - line 472, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 466, column 8 - line 472, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(170)(m$prime.value4, f(m$prime.value3)($lazy_go(170)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 167, column 26 - line 170, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(167);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(176)(f($lazy_go(176)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 173, column 26 - line 176, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(173);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append12 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty3;
          }
          ;
          if (v instanceof Node) {
            return append12(go2(v.value4))(append12(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 179, column 10 - line 182, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare4(k)(v.value2);
          if (v1 instanceof LT) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
          }
          ;
          if (v1 instanceof GT) {
            return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
          }
          ;
          if (v1 instanceof EQ) {
            return unsafeJoinNodes(v.value4, v.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 496, column 7 - line 499, column 43): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 493, column 8 - line 499, column 43): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var alter = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare4, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 512, column 3 - line 516, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.Slot/index.js
  var foreachSlot = function(dictApplicative) {
    var traverse_8 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_8(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty3 = empty2;

  // output/Data.String.Common/foreign.js
  var toLower = function(s) {
    return s.toLowerCase();
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step2 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map11 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap3 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map11(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map11(map12(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap3(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key2, obj) {
    return obj[key2];
  }
  function unsafeHasAny(key2, obj) {
    return obj.hasOwnProperty(key2);
  }
  function unsafeSetAny(key2, val, obj) {
    obj[key2] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function setScrollLeft(scrollLeft2) {
    return function(node) {
      return function() {
        node.scrollLeft = scrollLeft2;
      };
    };
  }

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy6("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step2(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy6("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy6("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step2(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy6("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step2(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy6("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;
  var pow = function(n) {
    return function(p2) {
      return Math.pow(n, p2);
    };
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Data.List.NonEmpty/index.js
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons$prime = function(x) {
    return function(xs) {
      return new NonEmpty(x, xs);
    };
  };
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a2) {
    return a2.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton6 = function(c) {
    return c;
  };
  var _toChar = function(just) {
    return function(nothing) {
      return function(s) {
        return s.length === 1 ? just(s) : nothing;
      };
    };
  };
  var length5 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length)
        return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var uncons3 = function(v) {
    if (v === "") {
      return Nothing.value;
    }
    ;
    return new Just({
      head: charAt(0)(v),
      tail: drop2(1)(v)
    });
  };
  var toChar = /* @__PURE__ */ function() {
    return _toChar(Just.create)(Nothing.value);
  }();
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length5(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure16 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure16(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object/index.js
  var lookup2 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key2, el) {
    var v = hasAttribute(nullImpl, key2, el);
    if (v) {
      return removeAttribute(nullImpl, key2, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key2, el));
    if (v1 === "string") {
      return unsafeSetAny(key2, "", el);
    }
    ;
    if (key2 === "rowSpan") {
      return unsafeSetAny(key2, 1, el);
    }
    ;
    if (key2 === "colSpan") {
      return unsafeSetAny(key2, 1, el);
    }
    ;
    return unsafeSetAny(key2, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromInt = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup2("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref3 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref3)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref3), events);
            addEventListener2(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy7("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text5 = function($29) {
    return HTML(Text.create($29));
  };
  var ref = function(f) {
    return new Ref(function($30) {
      return f(function(v) {
        if (v instanceof Created) {
          return new Just(v.value0);
        }
        ;
        if (v instanceof Removed) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Halogen.HTML.Core (line 109, column 21 - line 111, column 23): " + [v.constructor.name]);
      }($30));
    });
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInt = {
    toPropValue: propFromInt
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure16(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons2(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply2(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure16 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure16(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity5);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons4 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc2 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null3 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl5 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons4(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl5(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons5 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null3(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append3 = link;
  var semigroupCatList = {
    append: append3
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append3(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy8 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append4(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons5(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc3(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy8("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure4 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure4($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map114 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map114(Done.create)(pure16(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map114(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM5(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var bind4 = /* @__PURE__ */ bind(bindEffect);
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void5(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_2(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind4(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var getRef = function(p2) {
    return liftF(new GetRef(p2, identity6));
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize2(value0) {
      this.value0 = value0;
    }
    ;
    Initialize2.create = function(value0) {
      return new Initialize2(value0);
    };
    return Initialize2;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy9 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy9("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map13 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map13(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var h1 = /* @__PURE__ */ element2("h1");
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var input2 = function(props) {
    return element2("input")(props)([]);
  };
  var li = /* @__PURE__ */ element2("li");
  var li_ = /* @__PURE__ */ li([]);
  var pre = /* @__PURE__ */ element2("pre");
  var pre_ = /* @__PURE__ */ pre([]);
  var span3 = /* @__PURE__ */ element2("span");
  var table = /* @__PURE__ */ element2("table");
  var table_ = /* @__PURE__ */ table([]);
  var td = /* @__PURE__ */ element2("td");
  var td_ = /* @__PURE__ */ td([]);
  var textarea = function(es) {
    return element2("textarea")(es)([]);
  };
  var th = /* @__PURE__ */ element2("th");
  var th_ = /* @__PURE__ */ th([]);
  var tr = /* @__PURE__ */ element2("tr");
  var tr_ = /* @__PURE__ */ tr([]);
  var ul = /* @__PURE__ */ element2("ul");
  var ul_ = /* @__PURE__ */ ul([]);
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var button = /* @__PURE__ */ element2("button");
  var br = function(props) {
    return element2("br")(props)([]);
  };
  var br_ = /* @__PURE__ */ br([]);

  // output/Control.Monad.Except/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap2(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key2, value14) {
    return value14 == null ? f : s(value14[key2]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail3 = fail(dictMonad);
    var pure16 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail3(new TypeMismatch("object", typeOf(value14))), pure16, k, value14);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.KeyboardEvent.EventTypes/index.js
  var keydown = "keydown";

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map14 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var keyHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map14(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var onKeyDown = /* @__PURE__ */ function() {
    var $23 = handler2(keydown);
    return function($24) {
      return $23(keyHandler($24));
    };
  }();
  var onLoad = /* @__PURE__ */ handler2(load2);
  var addForeignPropHandler = function(key2) {
    return function(prop4) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop4))(unsafeToForeign(a2));
          };
          return handler$prime(key2)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input)("value")(readString2);

  // output/Halogen.HTML.Properties/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var ref2 = /* @__PURE__ */ function() {
    var go2 = function(p2) {
      return function(mel) {
        return new Just(new RefUpdate(p2, mel));
      };
    };
    return function($29) {
      return ref(go2($29));
    };
  }();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop3 = /* @__PURE__ */ prop2(isPropInt);
  var rows4 = /* @__PURE__ */ prop3("rows");
  var type_18 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var id2 = /* @__PURE__ */ prop22("id");
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap3($37));
    };
  }();

  // output/Halogen.Query/index.js
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var getHTMLElementRef = /* @__PURE__ */ function() {
    var $24 = map(functorHalogenM)(function(v) {
      return bindFlipped5(fromElement)(v);
    });
    return function($25) {
      return $24(getRef($25));
    };
  }();

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork2 = function(dict) {
    return dict.fork;
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_8 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_8(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty3)();
            var childrenOut = $$new(empty3)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh3 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component,
              state: component.initialState(input3),
              refs: empty2,
              children: empty3,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh3,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup4 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork2(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure6 = /* @__PURE__ */ pure(applicativeAff);
  var map16 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map17 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map22 = /* @__PURE__ */ map(functorMaybe);
  var insert3 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert1 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup1 = /* @__PURE__ */ lookup(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref3) {
      return function __do2() {
        var v = read(ref3)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped6(lookup4(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref3) {
    return function(au) {
      return bind12(liftEffect4(read(ref3)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref3));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind12(liftEffect4(f))(function(result) {
          return bind12(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure6(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref3) {
      return bind12(liftEffect4(read(ref3)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render2) {
    return function(ref3) {
      return function(q2) {
        return bind12(liftEffect4(read(ref3)))(function(v) {
          return evalM(render2)(ref3)(v["component"]["eval"](new Query(map16(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render2) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref3) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref3)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render2)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map17(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref3) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind12(liftEffect4(read(ref3)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure6(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers,
                    state: v3.value1
                  })(ref3)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render2(v2.lifecycleHandlers)(ref3)))(function() {
                      return pure6(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind12(fresh(SubscriptionId)(ref3))(function(sid) {
                return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render2)(ref3)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref3)))(function(v2) {
                    return discard1(liftEffect4(modify_2(map22(insert3(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure6(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref3)))(function() {
                return pure6(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref3)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind12(liftEffect4(read(ref3)))(function(v2) {
                return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render2)(ref3);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref3))(function(fid) {
                return bind12(liftEffect4(read(ref3)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do2() {
                      modify_2($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render2)(ref3)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_2(insert1(fid)(fiber))(v2.forks))))(function() {
                        return pure6(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind12(liftEffect4(read(ref3)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref3)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref3)))(function(v2) {
                return pure6(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render2) {
    return function(ref3) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_2)(ref3)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers,
              refs: alter2($$const(v.value1))(v.value0)(st.refs)
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref3)))(function(v1) {
            return evalM(render2)(ref3)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind5 = /* @__PURE__ */ bind(bindEffect);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork2(monadForkAff);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard4(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var map18 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void6 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref3) {
    return function __do2() {
      var queue = read(ref3)();
      write(Nothing.value)(ref3)();
      return for_2(queue)(function() {
        var $59 = traverse_5(fork4);
        return function($60) {
          return handleAff($59(reverse2($60)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped7(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped7(traverse_33(function() {
        var $61 = killFiber(error("finalized"));
        return function($62) {
          return handleAff($61($62));
        };
      }()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render2)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped7(unDriverStateX(function() {
                    var $63 = render2(lchs);
                    return function($64) {
                      return $63(function(v) {
                        return v.selfRef;
                      }($64));
                    };
                  }()))(read($$var2))();
                  bindFlipped7(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot) {
                  return function __do2() {
                    var childrenIn = map18(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure12(unit))(handler3);
                              return function($66) {
                                return $65(slot.output($66));
                              };
                            }())();
                            return handleAff(evalM(render2)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure12(unit))(handler3);
                          return function($68) {
                            return $67(slot.output($68));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map18(function($69) {
                      return isJust(slot.get($69));
                    })(read(childrenOutRef))();
                    when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot.set($$var2))(childrenOutRef)();
                    return bind5(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure7(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render2 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map18(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render2)(v.selfRef);
                return function($72) {
                  return $70($$void6($71($72)));
                };
              }();
              var childHandler = function() {
                var $73 = queueOrRun(v.pendingQueries);
                return function($74) {
                  return $73(handler3(Action.create($74)));
                };
              }();
              var rendering = renderSpec2.render(function($75) {
                return handleAff(handler3($75));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_2)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers,
                  rendering: new Just(rendering),
                  children: children2
                };
              }))();
              return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $76 = traverse_5(fork4);
                    return function($77) {
                      return handleAff($76(reverse2($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null)(mmore);
                  if ($52) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_2(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref3) {
            return function(q2) {
              return bind13(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render2)(ref3)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind13(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create3();
              var dsx = bindFlipped7(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect5($78($79));
                };
              }())(i2)(component))();
              return unDriverStateX(function(st) {
                return pure7({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map19 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map19(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map19(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy10 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map20 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void7(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void7(appendChild(v)(v2.value0));
        }
        ;
        return pure8(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document2) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap4)(spec);
          var $lazy_patch = $runtime_lazy10("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step2(st.value0, slot.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy10("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step5 = buildThunk2(slot.value0);
                return mkStep(new Step(extract2(step5), new Just(step5), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy10("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render2 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render2;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document2
        };
      };
    };
  };
  var renderSpec = function(document2) {
    return function(container) {
      var render2 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document2);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void7(appendChild(node)(toNode(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step2(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render2,
        renderChild: identity7,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map20(toDocument)(bindFlipped8(document)(windowImpl))))(function(document2) {
          return runUI(renderSpec(document2)(element3))(component)(i2);
        });
      };
    };
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate2 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsArgument = function(dictShow) {
    var show7 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show7(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate2(" ")(append6([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShow = function(dictGeneric) {
    var from3 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x) {
        return genericShow$prime1(from3(x));
      };
    };
  };

  // output/Parsing/index.js
  var $runtime_lazy11 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var show2 = /* @__PURE__ */ show(showString);
  var unwrap5 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ParseState2(value0, value1, value22);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift3 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var lazyParserT = {
    defer: function(f) {
      var m = defer2(f);
      return function(state1, more, lift1, $$throw2, done) {
        var v = force(m);
        return v(state1, more, lift1, $$throw2, done);
      };
    }
  };
  var genericPosition_ = {
    to: function(x) {
      return x;
    },
    from: function(x) {
      return x;
    }
  };
  var genericShow2 = /* @__PURE__ */ genericShow(genericPosition_)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "column";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "index";
    }
  })(/* @__PURE__ */ showRecordFieldsConsNil({
    reflectSymbol: function() {
      return "line";
    }
  })(showInt))(showInt))(showInt))))({
    reflectSymbol: function() {
      return "Position";
    }
  }));
  var showPosition = {
    show: function(x) {
      return genericShow2(x);
    }
  };
  var show1 = /* @__PURE__ */ show(showPosition);
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                return done(state22, f(a2));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state22, f) {
              return more(function(v3) {
                return v1(state22, more, lift1, $$throw2, function(state3, a2) {
                  return more(function(v4) {
                    return done(state3, f(a2));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                var v3 = next(a2);
                return v3(state22, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped9 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a2) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a2);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadRecParserT = {
    tailRecM: function(next) {
      return function(initArg) {
        return function(state1, more, lift1, $$throw2, done) {
          var $lazy_loop = $runtime_lazy11("loop", "Parsing", function() {
            return function(state22, arg, gas) {
              var v = next(arg);
              return v(state22, more, lift1, $$throw2, function(state3, step5) {
                if (step5 instanceof Loop) {
                  var $206 = gas === 0;
                  if ($206) {
                    return more(function(v1) {
                      return $lazy_loop(277)(state3, step5.value0, 30);
                    });
                  }
                  ;
                  return $lazy_loop(279)(state3, step5.value0, gas - 1 | 0);
                }
                ;
                if (step5 instanceof Done) {
                  return done(state3, step5.value0);
                }
                ;
                throw new Error("Failed pattern match at Parsing (line 273, column 39 - line 281, column 43): " + [step5.constructor.name]);
              });
            };
          });
          var loop2 = $lazy_loop(270);
          return loop2(state1, initArg, 30);
        };
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError3 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k) {
    return function(state1, v, v1, v2, done) {
      var v3 = k(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var showParseError = {
    show: function(v) {
      return "(ParseError " + (show2(v.value0) + (" " + (show1(v.value1) + ")")));
    }
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map32 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go2 = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step5) {
            var v1 = step5(unit);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift3) {
              $tco_done = true;
              return map32(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure16(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM5(go2)(function(v1) {
          return v(state1, More.create, Lift3.create, function(state22, err) {
            return new Stop(state22, new Left(err));
          }, function(state22, res) {
            return new Stop(state22, new Right(res));
          });
        });
      };
    };
  };
  var position2 = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map32 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p2) {
        var initialState2 = new ParseState(s, initialPos, false);
        return map32(fst)(runParserT$prime1(initialState2)(p2));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $281 = runParserT1(s);
    return function($282) {
      return unwrap5($281($282));
    };
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError3(new ParseError(message2, pos));
    };
  };
  var fail2 = function(message2) {
    return bindFlipped9(failWithPosition(message2))(position2);
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail2("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };

  // output/Parsing.Combinators/index.js
  var alt5 = /* @__PURE__ */ alt(altParserT);
  var defer4 = /* @__PURE__ */ defer(lazyParserT);
  var voidLeft4 = /* @__PURE__ */ voidLeft(functorParserT);
  var pure9 = /* @__PURE__ */ pure(applicativeParserT);
  var applySecond2 = /* @__PURE__ */ applySecond(applyParserT);
  var tailRecM4 = /* @__PURE__ */ tailRecM(monadRecParserT);
  var bind6 = /* @__PURE__ */ bind(bindParserT);
  var map21 = /* @__PURE__ */ map(functorParserT);
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var applyFirst2 = /* @__PURE__ */ applyFirst(applyParserT);
  var empty7 = /* @__PURE__ */ empty(plusParserT);
  var withLazyErrorMessage = function(p2) {
    return function(msg) {
      return alt5(p2)(defer4(function(v) {
        return fail2("Expected " + msg(unit));
      }));
    };
  };
  var withErrorMessage = function(p2) {
    return function(msg) {
      return alt5(p2)(fail2("Expected " + msg));
    };
  };
  var $$try3 = function(v) {
    return function(v1, more, lift4, $$throw2, done) {
      return v(v1, more, lift4, function(v2, err) {
        return $$throw2(new ParseState(v2.value0, v2.value1, v1.value2), err);
      }, done);
    };
  };
  var skipMany1 = function(p2) {
    var go2 = function(v) {
      return alt5(voidLeft4(p2)(new Loop(unit)))(pure9(new Done(unit)));
    };
    return applySecond2(p2)(tailRecM4(go2)(unit));
  };
  var skipMany = function(p2) {
    return alt5(skipMany1(p2))(pure9(unit));
  };
  var sepBy1 = function(p2) {
    return function(sep) {
      return bind6(p2)(function(a2) {
        return bind6(manyRec2(applySecond2(sep)(p2)))(function(as) {
          return pure9(cons$prime(a2)(as));
        });
      });
    };
  };
  var sepBy = function(p2) {
    return function(sep) {
      return alt5(map21(toList)(sepBy1(p2)(sep)))(pure9(Nil.value));
    };
  };
  var option2 = function(a2) {
    return function(p2) {
      return alt5(p2)(pure9(a2));
    };
  };
  var notFollowedBy = function(p2) {
    return $$try3(alt5(applySecond2($$try3(p2))(fail2("Negated parser succeeded")))(pure9(unit)));
  };
  var choice = function(dictFoldable) {
    var go2 = function(p1) {
      return function(v) {
        if (v instanceof Nothing) {
          return new Just(p1);
        }
        ;
        if (v instanceof Just) {
          return new Just(alt5(p1)(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Combinators (line 358, column 11 - line 360, column 32): " + [v.constructor.name]);
      };
    };
    var $95 = fromMaybe(empty7);
    var $96 = foldr(dictFoldable)(go2)(Nothing.value);
    return function($97) {
      return $95($96($97));
    };
  };
  var between = function(open) {
    return function(close2) {
      return function(p2) {
        return applyFirst2(applySecond2(open)(p2))(close2);
      };
    };
  };
  var asErrorMessage = /* @__PURE__ */ flip(withErrorMessage);

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head5, tail2) {
      this.head = head5;
      this.tail = tail2;
    };
    function finalCell(head5) {
      return new ConsCell(head5, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply2, map32, f) {
      var buildFrom = function(x, ys) {
        return apply2(map32(consList)(f(x)))(ys);
      };
      var go2 = function(acc, currentLen, xs) {
        if (currentLen === 0) {
          return acc;
        } else {
          var last3 = xs[currentLen - 1];
          return new Cont(function() {
            var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
            return built;
          });
        }
      };
      return function(array) {
        var acc = map32(finalCell)(f(array[array.length - 1]));
        var result = go2(acc, array.length - 1, array);
        while (result instanceof Cont) {
          result = result.fn();
        }
        return map32(listToArray)(result);
      };
    };
  }();

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index4) {
            return function(str) {
              var length9 = str.length;
              if (index4 < 0 || index4 >= length9)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i2 = index4; ; --i2) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i2 === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index4)(str);
            };
          };
        };
      };
    };
  };
  var _fromCodePointArray = function(singleton9) {
    return hasFromCodePoint ? function(cps) {
      if (cps.length < 1e4) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton9).join("");
    } : function(cps) {
      return cps.map(singleton9).join("");
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy12 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map23 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons6 = function(s) {
    var v = length5(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map23(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons6(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length5(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum2($77));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons6(s);
        if (v instanceof Just) {
          var $66 = n === 0;
          if ($66) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy12("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Parsing.String/index.js
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show12 = /* @__PURE__ */ show(showString);
  var show22 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum3(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum3(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var updatePosString = function($copy_pos) {
    return function($copy_before) {
      return function($copy_after) {
        var $tco_var_pos = $copy_pos;
        var $tco_var_before = $copy_before;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(pos, before, after) {
          var v = uncons6(before);
          if (v instanceof Nothing) {
            $tco_done = true;
            return pos;
          }
          ;
          if (v instanceof Just) {
            var newPos = function() {
              if ($$null2(v.value0.tail)) {
                return updatePosSingle(pos)(v.value0.head)(after);
              }
              ;
              if (otherwise) {
                return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): " + []);
            }();
            $tco_var_pos = newPos;
            $tco_var_before = v.value0.tail;
            $copy_after = after;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
        }
        ;
        return $tco_result;
      };
    };
  };
  var satisfyCodePoint = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons6(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var $76 = f(v3.value0.head);
                if ($76) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 136, column 7 - line 143, column 73): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons6(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum3(v3.value0.head);
                var $85 = cp < 0 || cp > 65535;
                if ($85) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust5(toEnum2(cp));
                var $86 = f(ch);
                if ($86) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var eof = /* @__PURE__ */ mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw2) {
          return function(done) {
            var $133 = $$null2(v.value0);
            if ($133) {
              return done(new ParseState(v.value0, v.value1, true), unit);
            }
            ;
            return $$throw2(v, new ParseError("Expected EOF", v.value1));
          };
        };
      };
    };
  });
  var consumeWith = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = f(v.value0);
              if (v3 instanceof Left) {
                return $$throw2(v, new ParseError(v3.value0, v.value1));
              }
              ;
              if (v3 instanceof Right) {
                return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var string = function(str) {
    return consumeWith(function(input3) {
      var v = stripPrefix(str)(input3);
      if (v instanceof Just) {
        return new Right({
          value: str,
          consumed: str,
          remainder: v.value0
        });
      }
      ;
      return new Left("Expected " + show12(str));
    });
  };
  var $$char = function(c) {
    return withErrorMessage(satisfy(function(v) {
      return v === c;
    }))(show22(c));
  };

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

  // output/Data.CodePoint.Unicode.Internal/index.js
  var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqInt);
  var NUMCAT_LU = /* @__PURE__ */ function() {
    function NUMCAT_LU2() {
    }
    ;
    NUMCAT_LU2.value = new NUMCAT_LU2();
    return NUMCAT_LU2;
  }();
  var NUMCAT_LL = /* @__PURE__ */ function() {
    function NUMCAT_LL2() {
    }
    ;
    NUMCAT_LL2.value = new NUMCAT_LL2();
    return NUMCAT_LL2;
  }();
  var NUMCAT_LT = /* @__PURE__ */ function() {
    function NUMCAT_LT2() {
    }
    ;
    NUMCAT_LT2.value = new NUMCAT_LT2();
    return NUMCAT_LT2;
  }();
  var NUMCAT_LM = /* @__PURE__ */ function() {
    function NUMCAT_LM2() {
    }
    ;
    NUMCAT_LM2.value = new NUMCAT_LM2();
    return NUMCAT_LM2;
  }();
  var NUMCAT_LO = /* @__PURE__ */ function() {
    function NUMCAT_LO2() {
    }
    ;
    NUMCAT_LO2.value = new NUMCAT_LO2();
    return NUMCAT_LO2;
  }();
  var NUMCAT_MN = /* @__PURE__ */ function() {
    function NUMCAT_MN2() {
    }
    ;
    NUMCAT_MN2.value = new NUMCAT_MN2();
    return NUMCAT_MN2;
  }();
  var NUMCAT_MC = /* @__PURE__ */ function() {
    function NUMCAT_MC2() {
    }
    ;
    NUMCAT_MC2.value = new NUMCAT_MC2();
    return NUMCAT_MC2;
  }();
  var NUMCAT_ME = /* @__PURE__ */ function() {
    function NUMCAT_ME2() {
    }
    ;
    NUMCAT_ME2.value = new NUMCAT_ME2();
    return NUMCAT_ME2;
  }();
  var NUMCAT_ND = /* @__PURE__ */ function() {
    function NUMCAT_ND2() {
    }
    ;
    NUMCAT_ND2.value = new NUMCAT_ND2();
    return NUMCAT_ND2;
  }();
  var NUMCAT_NL = /* @__PURE__ */ function() {
    function NUMCAT_NL2() {
    }
    ;
    NUMCAT_NL2.value = new NUMCAT_NL2();
    return NUMCAT_NL2;
  }();
  var NUMCAT_NO = /* @__PURE__ */ function() {
    function NUMCAT_NO2() {
    }
    ;
    NUMCAT_NO2.value = new NUMCAT_NO2();
    return NUMCAT_NO2;
  }();
  var NUMCAT_PC = /* @__PURE__ */ function() {
    function NUMCAT_PC2() {
    }
    ;
    NUMCAT_PC2.value = new NUMCAT_PC2();
    return NUMCAT_PC2;
  }();
  var NUMCAT_PD = /* @__PURE__ */ function() {
    function NUMCAT_PD2() {
    }
    ;
    NUMCAT_PD2.value = new NUMCAT_PD2();
    return NUMCAT_PD2;
  }();
  var NUMCAT_PS = /* @__PURE__ */ function() {
    function NUMCAT_PS2() {
    }
    ;
    NUMCAT_PS2.value = new NUMCAT_PS2();
    return NUMCAT_PS2;
  }();
  var NUMCAT_PE = /* @__PURE__ */ function() {
    function NUMCAT_PE2() {
    }
    ;
    NUMCAT_PE2.value = new NUMCAT_PE2();
    return NUMCAT_PE2;
  }();
  var NUMCAT_PI = /* @__PURE__ */ function() {
    function NUMCAT_PI2() {
    }
    ;
    NUMCAT_PI2.value = new NUMCAT_PI2();
    return NUMCAT_PI2;
  }();
  var NUMCAT_PF = /* @__PURE__ */ function() {
    function NUMCAT_PF2() {
    }
    ;
    NUMCAT_PF2.value = new NUMCAT_PF2();
    return NUMCAT_PF2;
  }();
  var NUMCAT_PO = /* @__PURE__ */ function() {
    function NUMCAT_PO2() {
    }
    ;
    NUMCAT_PO2.value = new NUMCAT_PO2();
    return NUMCAT_PO2;
  }();
  var NUMCAT_SM = /* @__PURE__ */ function() {
    function NUMCAT_SM2() {
    }
    ;
    NUMCAT_SM2.value = new NUMCAT_SM2();
    return NUMCAT_SM2;
  }();
  var NUMCAT_SC = /* @__PURE__ */ function() {
    function NUMCAT_SC2() {
    }
    ;
    NUMCAT_SC2.value = new NUMCAT_SC2();
    return NUMCAT_SC2;
  }();
  var NUMCAT_SK = /* @__PURE__ */ function() {
    function NUMCAT_SK2() {
    }
    ;
    NUMCAT_SK2.value = new NUMCAT_SK2();
    return NUMCAT_SK2;
  }();
  var NUMCAT_SO = /* @__PURE__ */ function() {
    function NUMCAT_SO2() {
    }
    ;
    NUMCAT_SO2.value = new NUMCAT_SO2();
    return NUMCAT_SO2;
  }();
  var NUMCAT_ZS = /* @__PURE__ */ function() {
    function NUMCAT_ZS2() {
    }
    ;
    NUMCAT_ZS2.value = new NUMCAT_ZS2();
    return NUMCAT_ZS2;
  }();
  var NUMCAT_ZL = /* @__PURE__ */ function() {
    function NUMCAT_ZL2() {
    }
    ;
    NUMCAT_ZL2.value = new NUMCAT_ZL2();
    return NUMCAT_ZL2;
  }();
  var NUMCAT_ZP = /* @__PURE__ */ function() {
    function NUMCAT_ZP2() {
    }
    ;
    NUMCAT_ZP2.value = new NUMCAT_ZP2();
    return NUMCAT_ZP2;
  }();
  var NUMCAT_CC = /* @__PURE__ */ function() {
    function NUMCAT_CC2() {
    }
    ;
    NUMCAT_CC2.value = new NUMCAT_CC2();
    return NUMCAT_CC2;
  }();
  var NUMCAT_CF = /* @__PURE__ */ function() {
    function NUMCAT_CF2() {
    }
    ;
    NUMCAT_CF2.value = new NUMCAT_CF2();
    return NUMCAT_CF2;
  }();
  var NUMCAT_CS = /* @__PURE__ */ function() {
    function NUMCAT_CS2() {
    }
    ;
    NUMCAT_CS2.value = new NUMCAT_CS2();
    return NUMCAT_CS2;
  }();
  var NUMCAT_CO = /* @__PURE__ */ function() {
    function NUMCAT_CO2() {
    }
    ;
    NUMCAT_CO2.value = new NUMCAT_CO2();
    return NUMCAT_CO2;
  }();
  var NUMCAT_CN = /* @__PURE__ */ function() {
    function NUMCAT_CN2() {
    }
    ;
    NUMCAT_CN2.value = new NUMCAT_CN2();
    return NUMCAT_CN2;
  }();
  var numSpaceBlocks = 7;
  var numLat1Blocks = 63;
  var numConvBlocks = 1332;
  var numBlocks = 3396;
  var gencatZS = 2;
  var rule1 = /* @__PURE__ */ function() {
    return {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var spacechars = [{
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }];
  var gencatZP = 67108864;
  var rule162 = /* @__PURE__ */ function() {
    return {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZL = 33554432;
  var rule161 = /* @__PURE__ */ function() {
    return {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSO = 8192;
  var rule13 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule170 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
    };
  }();
  var rule171 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
    };
  }();
  var gencatSM = 64;
  var rule6 = /* @__PURE__ */ function() {
    return {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSK = 1024;
  var rule10 = /* @__PURE__ */ function() {
    return {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSC = 8;
  var rule3 = /* @__PURE__ */ function() {
    return {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPS = 16;
  var rule4 = /* @__PURE__ */ function() {
    return {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPO = 4;
  var rule2 = /* @__PURE__ */ function() {
    return {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPI = 32768;
  var rule15 = /* @__PURE__ */ function() {
    return {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPF = 262144;
  var rule19 = /* @__PURE__ */ function() {
    return {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPE = 32;
  var rule5 = /* @__PURE__ */ function() {
    return {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPD = 128;
  var rule7 = /* @__PURE__ */ function() {
    return {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPC = 2048;
  var rule11 = /* @__PURE__ */ function() {
    return {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNO = 131072;
  var rule17 = /* @__PURE__ */ function() {
    return {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNL = 16777216;
  var rule128 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule168 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
    };
  }();
  var rule169 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
    };
  }();
  var gencatND = 256;
  var rule8 = /* @__PURE__ */ function() {
    return {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMN = 2097152;
  var rule92 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule93 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
    };
  }();
  var gencatME = 4194304;
  var rule119 = /* @__PURE__ */ function() {
    return {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMC = 8388608;
  var rule124 = /* @__PURE__ */ function() {
    return {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLU = 512;
  var nullrule = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule104 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
    };
  }();
  var rule107 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule115 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
    };
  }();
  var rule117 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
    };
  }();
  var rule118 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
    };
  }();
  var rule120 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
    };
  }();
  var rule122 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
    };
  }();
  var rule125 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
    };
  }();
  var rule127 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38864,
      titledist: 0
    };
  }();
  var rule137 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3008 | 0,
      titledist: 0
    };
  }();
  var rule142 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
    };
  }();
  var rule144 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule153 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
    };
  }();
  var rule156 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
    };
  }();
  var rule157 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
    };
  }();
  var rule158 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
    };
  }();
  var rule159 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
    };
  }();
  var rule160 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
    };
  }();
  var rule163 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
    };
  }();
  var rule164 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
    };
  }();
  var rule165 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
    };
  }();
  var rule166 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
    };
  }();
  var rule172 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
    };
  }();
  var rule173 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
    };
  }();
  var rule174 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
    };
  }();
  var rule177 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
    };
  }();
  var rule178 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
    };
  }();
  var rule179 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
    };
  }();
  var rule180 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
    };
  }();
  var rule181 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
    };
  }();
  var rule183 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
    };
  }();
  var rule184 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
    };
  }();
  var rule186 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42308 | 0,
      titledist: 0
    };
  }();
  var rule187 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42319 | 0,
      titledist: 0
    };
  }();
  var rule188 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42315 | 0,
      titledist: 0
    };
  }();
  var rule189 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42305 | 0,
      titledist: 0
    };
  }();
  var rule190 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42258 | 0,
      titledist: 0
    };
  }();
  var rule191 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42282 | 0,
      titledist: 0
    };
  }();
  var rule192 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42261 | 0,
      titledist: 0
    };
  }();
  var rule193 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 928,
      titledist: 0
    };
  }();
  var rule194 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -48 | 0,
      titledist: 0
    };
  }();
  var rule195 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42307 | 0,
      titledist: 0
    };
  }();
  var rule196 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35384 | 0,
      titledist: 0
    };
  }();
  var rule201 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
    };
  }();
  var rule203 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 34,
      titledist: 0
    };
  }();
  var rule22 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var rule24 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
    };
  }();
  var rule26 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
    };
  }();
  var rule29 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
    };
  }();
  var rule30 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
    };
  }();
  var rule31 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
    };
  }();
  var rule32 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
    };
  }();
  var rule33 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
    };
  }();
  var rule34 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
    };
  }();
  var rule35 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
    };
  }();
  var rule37 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
    };
  }();
  var rule38 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
    };
  }();
  var rule40 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
    };
  }();
  var rule42 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
    };
  }();
  var rule43 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
    };
  }();
  var rule44 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
    };
  }();
  var rule45 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
    };
  }();
  var rule47 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
    };
  }();
  var rule51 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
    };
  }();
  var rule52 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
    };
  }();
  var rule53 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
    };
  }();
  var rule54 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
    };
  }();
  var rule55 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
    };
  }();
  var rule56 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
    };
  }();
  var rule58 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
    };
  }();
  var rule59 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
    };
  }();
  var rule60 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
    };
  }();
  var rule9 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
    };
  }();
  var rule94 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 116,
      titledist: 0
    };
  }();
  var rule95 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
    };
  }();
  var rule96 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
    };
  }();
  var rule97 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
    };
  }();
  var rule98 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
    };
  }();
  var gencatLT = 524288;
  var rule151 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule154 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
    };
  }();
  var rule48 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var gencatLO = 16384;
  var rule14 = /* @__PURE__ */ function() {
    return {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLM = 1048576;
  var rule91 = /* @__PURE__ */ function() {
    return {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLL = 4096;
  var rule100 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
    };
  }();
  var rule101 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
    };
  }();
  var rule102 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
    };
  }();
  var rule103 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
    };
  }();
  var rule105 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
    };
  }();
  var rule106 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
    };
  }();
  var rule108 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
    };
  }();
  var rule109 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
    };
  }();
  var rule110 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
    };
  }();
  var rule111 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
    };
  }();
  var rule112 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
    };
  }();
  var rule113 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
    };
  }();
  var rule114 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -116 | 0,
      lowdist: 0,
      titledist: -116 | 0
    };
  }();
  var rule116 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
    };
  }();
  var rule12 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
    };
  }();
  var rule121 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
    };
  }();
  var rule123 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
    };
  }();
  var rule126 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3008,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule129 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6254 | 0,
      lowdist: 0,
      titledist: -6254 | 0
    };
  }();
  var rule130 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6253 | 0,
      lowdist: 0,
      titledist: -6253 | 0
    };
  }();
  var rule131 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6244 | 0,
      lowdist: 0,
      titledist: -6244 | 0
    };
  }();
  var rule132 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6242 | 0,
      lowdist: 0,
      titledist: -6242 | 0
    };
  }();
  var rule133 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6243 | 0,
      lowdist: 0,
      titledist: -6243 | 0
    };
  }();
  var rule134 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6236 | 0,
      lowdist: 0,
      titledist: -6236 | 0
    };
  }();
  var rule135 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6181 | 0,
      lowdist: 0,
      titledist: -6181 | 0
    };
  }();
  var rule136 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35266,
      lowdist: 0,
      titledist: 35266
    };
  }();
  var rule138 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
    };
  }();
  var rule139 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
    };
  }();
  var rule140 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35384,
      lowdist: 0,
      titledist: 35384
    };
  }();
  var rule141 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
    };
  }();
  var rule143 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
    };
  }();
  var rule145 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
    };
  }();
  var rule146 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
    };
  }();
  var rule147 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
    };
  }();
  var rule148 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
    };
  }();
  var rule149 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
    };
  }();
  var rule150 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
    };
  }();
  var rule152 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
    };
  }();
  var rule155 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
    };
  }();
  var rule167 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
    };
  }();
  var rule175 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
    };
  }();
  var rule176 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
    };
  }();
  var rule18 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
    };
  }();
  var rule182 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
    };
  }();
  var rule185 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 48,
      lowdist: 0,
      titledist: 48
    };
  }();
  var rule197 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -928 | 0,
      lowdist: 0,
      titledist: -928 | 0
    };
  }();
  var rule198 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38864 | 0,
      lowdist: 0,
      titledist: -38864 | 0
    };
  }();
  var rule20 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule202 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
    };
  }();
  var rule204 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -34 | 0,
      lowdist: 0,
      titledist: -34 | 0
    };
  }();
  var rule21 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
    };
  }();
  var rule23 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule25 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
    };
  }();
  var rule27 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
    };
  }();
  var rule28 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
    };
  }();
  var rule36 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
    };
  }();
  var rule39 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
    };
  }();
  var rule41 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
    };
  }();
  var rule46 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
    };
  }();
  var rule49 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule50 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
    };
  }();
  var rule57 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
    };
  }();
  var rule61 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
    };
  }();
  var rule62 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
    };
  }();
  var rule63 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
    };
  }();
  var rule64 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
    };
  }();
  var rule65 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
    };
  }();
  var rule66 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
    };
  }();
  var rule67 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
    };
  }();
  var rule68 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
    };
  }();
  var rule69 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42319,
      lowdist: 0,
      titledist: 42319
    };
  }();
  var rule70 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42315,
      lowdist: 0,
      titledist: 42315
    };
  }();
  var rule71 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
    };
  }();
  var rule72 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
    };
  }();
  var rule73 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42308,
      lowdist: 0,
      titledist: 42308
    };
  }();
  var rule74 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
    };
  }();
  var rule75 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
    };
  }();
  var rule76 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
    };
  }();
  var rule77 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42305,
      lowdist: 0,
      titledist: 42305
    };
  }();
  var rule78 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
    };
  }();
  var rule79 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
    };
  }();
  var rule80 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
    };
  }();
  var rule81 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
    };
  }();
  var rule82 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
    };
  }();
  var rule83 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42307,
      lowdist: 0,
      titledist: 42307
    };
  }();
  var rule84 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42282,
      lowdist: 0,
      titledist: 42282
    };
  }();
  var rule85 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
    };
  }();
  var rule86 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
    };
  }();
  var rule87 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
    };
  }();
  var rule88 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
    };
  }();
  var rule89 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42261,
      lowdist: 0,
      titledist: 42261
    };
  }();
  var rule90 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42258,
      lowdist: 0,
      titledist: 42258
    };
  }();
  var rule99 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
    };
  }();
  var gencatCS = 134217728;
  var rule199 = /* @__PURE__ */ function() {
    return {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCO = 268435456;
  var rule200 = /* @__PURE__ */ function() {
    return {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCF = 65536;
  var rule16 = /* @__PURE__ */ function() {
    return {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCC = 1;
  var rule0 = /* @__PURE__ */ function() {
    return {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var convchars = [{
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }];
  var bsearch = function(a2) {
    return function(array) {
      return function(size4) {
        return function(compare4) {
          var go2 = function($copy_i) {
            return function($copy_k) {
              var $tco_var_i = $copy_i;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(i2, k) {
                if (i2 > k || i2 >= length(array)) {
                  $tco_done = true;
                  return Nothing.value;
                }
                ;
                if (otherwise) {
                  var j = floor2(toNumber(i2 + k | 0) / 2);
                  var b2 = unsafeIndex2(array)(j);
                  var v = compare4(a2)(b2);
                  if (v instanceof EQ) {
                    $tco_done = true;
                    return new Just(b2);
                  }
                  ;
                  if (v instanceof GT) {
                    $tco_var_i = j + 1 | 0;
                    $copy_k = k;
                    return;
                  }
                  ;
                  $tco_var_i = i2;
                  $copy_k = j - 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i2.constructor.name, k.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_k);
              }
              ;
              return $tco_result;
            };
          };
          return go2(0)(size4);
        };
      };
    };
  };
  var blkCmp = function(v) {
    return function(v1) {
      if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
        return EQ.value;
      }
      ;
      if (v.start > v1.start) {
        return GT.value;
      }
      ;
      if (otherwise) {
        return LT.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getRule = function(blocks) {
    return function(unichar) {
      return function(size4) {
        var key2 = {
          start: unichar,
          length: 1,
          convRule: nullrule
        };
        var maybeCharBlock = bsearch(key2)(blocks)(size4)(blkCmp);
        if (maybeCharBlock instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (maybeCharBlock instanceof Just) {
          return new Just(maybeCharBlock.value0.convRule);
        }
        ;
        throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
      };
    };
  };
  var caseConv = function(f) {
    return function($$char2) {
      var maybeConversionRule = getRule(convchars)($$char2)(numConvBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return $$char2;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return $$char2 + f(maybeConversionRule.value0) | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5727, column 5 - line 5729, column 53): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uTowlower = /* @__PURE__ */ caseConv(function(v) {
    return v.lowdist;
  });
  var uTowupper = /* @__PURE__ */ caseConv(function(v) {
    return v.updist;
  });
  var checkAttrS = function(categories) {
    return function($$char2) {
      var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
  var allchars = [{
    start: 0,
    length: 32,
    convRule: rule0
  }, {
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 33,
    length: 3,
    convRule: rule2
  }, {
    start: 36,
    length: 1,
    convRule: rule3
  }, {
    start: 37,
    length: 3,
    convRule: rule2
  }, {
    start: 40,
    length: 1,
    convRule: rule4
  }, {
    start: 41,
    length: 1,
    convRule: rule5
  }, {
    start: 42,
    length: 1,
    convRule: rule2
  }, {
    start: 43,
    length: 1,
    convRule: rule6
  }, {
    start: 44,
    length: 1,
    convRule: rule2
  }, {
    start: 45,
    length: 1,
    convRule: rule7
  }, {
    start: 46,
    length: 2,
    convRule: rule2
  }, {
    start: 48,
    length: 10,
    convRule: rule8
  }, {
    start: 58,
    length: 2,
    convRule: rule2
  }, {
    start: 60,
    length: 3,
    convRule: rule6
  }, {
    start: 63,
    length: 2,
    convRule: rule2
  }, {
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 91,
    length: 1,
    convRule: rule4
  }, {
    start: 92,
    length: 1,
    convRule: rule2
  }, {
    start: 93,
    length: 1,
    convRule: rule5
  }, {
    start: 94,
    length: 1,
    convRule: rule10
  }, {
    start: 95,
    length: 1,
    convRule: rule11
  }, {
    start: 96,
    length: 1,
    convRule: rule10
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 123,
    length: 1,
    convRule: rule4
  }, {
    start: 124,
    length: 1,
    convRule: rule6
  }, {
    start: 125,
    length: 1,
    convRule: rule5
  }, {
    start: 126,
    length: 1,
    convRule: rule6
  }, {
    start: 127,
    length: 33,
    convRule: rule0
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 161,
    length: 1,
    convRule: rule2
  }, {
    start: 162,
    length: 4,
    convRule: rule3
  }, {
    start: 166,
    length: 1,
    convRule: rule13
  }, {
    start: 167,
    length: 1,
    convRule: rule2
  }, {
    start: 168,
    length: 1,
    convRule: rule10
  }, {
    start: 169,
    length: 1,
    convRule: rule13
  }, {
    start: 170,
    length: 1,
    convRule: rule14
  }, {
    start: 171,
    length: 1,
    convRule: rule15
  }, {
    start: 172,
    length: 1,
    convRule: rule6
  }, {
    start: 173,
    length: 1,
    convRule: rule16
  }, {
    start: 174,
    length: 1,
    convRule: rule13
  }, {
    start: 175,
    length: 1,
    convRule: rule10
  }, {
    start: 176,
    length: 1,
    convRule: rule13
  }, {
    start: 177,
    length: 1,
    convRule: rule6
  }, {
    start: 178,
    length: 2,
    convRule: rule17
  }, {
    start: 180,
    length: 1,
    convRule: rule10
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 182,
    length: 2,
    convRule: rule2
  }, {
    start: 184,
    length: 1,
    convRule: rule10
  }, {
    start: 185,
    length: 1,
    convRule: rule17
  }, {
    start: 186,
    length: 1,
    convRule: rule14
  }, {
    start: 187,
    length: 1,
    convRule: rule19
  }, {
    start: 188,
    length: 3,
    convRule: rule17
  }, {
    start: 191,
    length: 1,
    convRule: rule2
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 215,
    length: 1,
    convRule: rule6
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 223,
    length: 1,
    convRule: rule20
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 247,
    length: 1,
    convRule: rule6
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 312,
    length: 1,
    convRule: rule20
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 329,
    length: 1,
    convRule: rule20
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 397,
    length: 1,
    convRule: rule20
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 411,
    length: 1,
    convRule: rule20
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 426,
    length: 2,
    convRule: rule20
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 442,
    length: 1,
    convRule: rule20
  }, {
    start: 443,
    length: 1,
    convRule: rule14
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 446,
    length: 1,
    convRule: rule20
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 448,
    length: 4,
    convRule: rule14
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 496,
    length: 1,
    convRule: rule20
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 545,
    length: 1,
    convRule: rule20
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 564,
    length: 6,
    convRule: rule20
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 597,
    length: 1,
    convRule: rule20
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 600,
    length: 1,
    convRule: rule20
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 602,
    length: 1,
    convRule: rule20
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 605,
    length: 3,
    convRule: rule20
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 610,
    length: 1,
    convRule: rule20
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 612,
    length: 1,
    convRule: rule20
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 615,
    length: 1,
    convRule: rule20
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 621,
    length: 2,
    convRule: rule20
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 624,
    length: 1,
    convRule: rule20
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 627,
    length: 2,
    convRule: rule20
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 630,
    length: 7,
    convRule: rule20
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 638,
    length: 2,
    convRule: rule20
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 641,
    length: 1,
    convRule: rule20
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 644,
    length: 3,
    convRule: rule20
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 653,
    length: 5,
    convRule: rule20
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 659,
    length: 1,
    convRule: rule20
  }, {
    start: 660,
    length: 1,
    convRule: rule14
  }, {
    start: 661,
    length: 8,
    convRule: rule20
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 671,
    length: 17,
    convRule: rule20
  }, {
    start: 688,
    length: 18,
    convRule: rule91
  }, {
    start: 706,
    length: 4,
    convRule: rule10
  }, {
    start: 710,
    length: 12,
    convRule: rule91
  }, {
    start: 722,
    length: 14,
    convRule: rule10
  }, {
    start: 736,
    length: 5,
    convRule: rule91
  }, {
    start: 741,
    length: 7,
    convRule: rule10
  }, {
    start: 748,
    length: 1,
    convRule: rule91
  }, {
    start: 749,
    length: 1,
    convRule: rule10
  }, {
    start: 750,
    length: 1,
    convRule: rule91
  }, {
    start: 751,
    length: 17,
    convRule: rule10
  }, {
    start: 768,
    length: 69,
    convRule: rule92
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 838,
    length: 42,
    convRule: rule92
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 884,
    length: 1,
    convRule: rule91
  }, {
    start: 885,
    length: 1,
    convRule: rule10
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 890,
    length: 1,
    convRule: rule91
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 894,
    length: 1,
    convRule: rule2
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 900,
    length: 2,
    convRule: rule10
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 903,
    length: 1,
    convRule: rule2
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 912,
    length: 1,
    convRule: rule20
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 944,
    length: 1,
    convRule: rule20
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 978,
    length: 3,
    convRule: rule107
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1014,
    length: 1,
    convRule: rule6
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1020,
    length: 1,
    convRule: rule20
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1154,
    length: 1,
    convRule: rule13
  }, {
    start: 1155,
    length: 5,
    convRule: rule92
  }, {
    start: 1160,
    length: 2,
    convRule: rule119
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1369,
    length: 1,
    convRule: rule91
  }, {
    start: 1370,
    length: 6,
    convRule: rule2
  }, {
    start: 1376,
    length: 1,
    convRule: rule20
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 1415,
    length: 2,
    convRule: rule20
  }, {
    start: 1417,
    length: 1,
    convRule: rule2
  }, {
    start: 1418,
    length: 1,
    convRule: rule7
  }, {
    start: 1421,
    length: 2,
    convRule: rule13
  }, {
    start: 1423,
    length: 1,
    convRule: rule3
  }, {
    start: 1425,
    length: 45,
    convRule: rule92
  }, {
    start: 1470,
    length: 1,
    convRule: rule7
  }, {
    start: 1471,
    length: 1,
    convRule: rule92
  }, {
    start: 1472,
    length: 1,
    convRule: rule2
  }, {
    start: 1473,
    length: 2,
    convRule: rule92
  }, {
    start: 1475,
    length: 1,
    convRule: rule2
  }, {
    start: 1476,
    length: 2,
    convRule: rule92
  }, {
    start: 1478,
    length: 1,
    convRule: rule2
  }, {
    start: 1479,
    length: 1,
    convRule: rule92
  }, {
    start: 1488,
    length: 27,
    convRule: rule14
  }, {
    start: 1519,
    length: 4,
    convRule: rule14
  }, {
    start: 1523,
    length: 2,
    convRule: rule2
  }, {
    start: 1536,
    length: 6,
    convRule: rule16
  }, {
    start: 1542,
    length: 3,
    convRule: rule6
  }, {
    start: 1545,
    length: 2,
    convRule: rule2
  }, {
    start: 1547,
    length: 1,
    convRule: rule3
  }, {
    start: 1548,
    length: 2,
    convRule: rule2
  }, {
    start: 1550,
    length: 2,
    convRule: rule13
  }, {
    start: 1552,
    length: 11,
    convRule: rule92
  }, {
    start: 1563,
    length: 1,
    convRule: rule2
  }, {
    start: 1564,
    length: 1,
    convRule: rule16
  }, {
    start: 1566,
    length: 2,
    convRule: rule2
  }, {
    start: 1568,
    length: 32,
    convRule: rule14
  }, {
    start: 1600,
    length: 1,
    convRule: rule91
  }, {
    start: 1601,
    length: 10,
    convRule: rule14
  }, {
    start: 1611,
    length: 21,
    convRule: rule92
  }, {
    start: 1632,
    length: 10,
    convRule: rule8
  }, {
    start: 1642,
    length: 4,
    convRule: rule2
  }, {
    start: 1646,
    length: 2,
    convRule: rule14
  }, {
    start: 1648,
    length: 1,
    convRule: rule92
  }, {
    start: 1649,
    length: 99,
    convRule: rule14
  }, {
    start: 1748,
    length: 1,
    convRule: rule2
  }, {
    start: 1749,
    length: 1,
    convRule: rule14
  }, {
    start: 1750,
    length: 7,
    convRule: rule92
  }, {
    start: 1757,
    length: 1,
    convRule: rule16
  }, {
    start: 1758,
    length: 1,
    convRule: rule13
  }, {
    start: 1759,
    length: 6,
    convRule: rule92
  }, {
    start: 1765,
    length: 2,
    convRule: rule91
  }, {
    start: 1767,
    length: 2,
    convRule: rule92
  }, {
    start: 1769,
    length: 1,
    convRule: rule13
  }, {
    start: 1770,
    length: 4,
    convRule: rule92
  }, {
    start: 1774,
    length: 2,
    convRule: rule14
  }, {
    start: 1776,
    length: 10,
    convRule: rule8
  }, {
    start: 1786,
    length: 3,
    convRule: rule14
  }, {
    start: 1789,
    length: 2,
    convRule: rule13
  }, {
    start: 1791,
    length: 1,
    convRule: rule14
  }, {
    start: 1792,
    length: 14,
    convRule: rule2
  }, {
    start: 1807,
    length: 1,
    convRule: rule16
  }, {
    start: 1808,
    length: 1,
    convRule: rule14
  }, {
    start: 1809,
    length: 1,
    convRule: rule92
  }, {
    start: 1810,
    length: 30,
    convRule: rule14
  }, {
    start: 1840,
    length: 27,
    convRule: rule92
  }, {
    start: 1869,
    length: 89,
    convRule: rule14
  }, {
    start: 1958,
    length: 11,
    convRule: rule92
  }, {
    start: 1969,
    length: 1,
    convRule: rule14
  }, {
    start: 1984,
    length: 10,
    convRule: rule8
  }, {
    start: 1994,
    length: 33,
    convRule: rule14
  }, {
    start: 2027,
    length: 9,
    convRule: rule92
  }, {
    start: 2036,
    length: 2,
    convRule: rule91
  }, {
    start: 2038,
    length: 1,
    convRule: rule13
  }, {
    start: 2039,
    length: 3,
    convRule: rule2
  }, {
    start: 2042,
    length: 1,
    convRule: rule91
  }, {
    start: 2045,
    length: 1,
    convRule: rule92
  }, {
    start: 2046,
    length: 2,
    convRule: rule3
  }, {
    start: 2048,
    length: 22,
    convRule: rule14
  }, {
    start: 2070,
    length: 4,
    convRule: rule92
  }, {
    start: 2074,
    length: 1,
    convRule: rule91
  }, {
    start: 2075,
    length: 9,
    convRule: rule92
  }, {
    start: 2084,
    length: 1,
    convRule: rule91
  }, {
    start: 2085,
    length: 3,
    convRule: rule92
  }, {
    start: 2088,
    length: 1,
    convRule: rule91
  }, {
    start: 2089,
    length: 5,
    convRule: rule92
  }, {
    start: 2096,
    length: 15,
    convRule: rule2
  }, {
    start: 2112,
    length: 25,
    convRule: rule14
  }, {
    start: 2137,
    length: 3,
    convRule: rule92
  }, {
    start: 2142,
    length: 1,
    convRule: rule2
  }, {
    start: 2144,
    length: 11,
    convRule: rule14
  }, {
    start: 2208,
    length: 21,
    convRule: rule14
  }, {
    start: 2230,
    length: 18,
    convRule: rule14
  }, {
    start: 2259,
    length: 15,
    convRule: rule92
  }, {
    start: 2274,
    length: 1,
    convRule: rule16
  }, {
    start: 2275,
    length: 32,
    convRule: rule92
  }, {
    start: 2307,
    length: 1,
    convRule: rule124
  }, {
    start: 2308,
    length: 54,
    convRule: rule14
  }, {
    start: 2362,
    length: 1,
    convRule: rule92
  }, {
    start: 2363,
    length: 1,
    convRule: rule124
  }, {
    start: 2364,
    length: 1,
    convRule: rule92
  }, {
    start: 2365,
    length: 1,
    convRule: rule14
  }, {
    start: 2366,
    length: 3,
    convRule: rule124
  }, {
    start: 2369,
    length: 8,
    convRule: rule92
  }, {
    start: 2377,
    length: 4,
    convRule: rule124
  }, {
    start: 2381,
    length: 1,
    convRule: rule92
  }, {
    start: 2382,
    length: 2,
    convRule: rule124
  }, {
    start: 2384,
    length: 1,
    convRule: rule14
  }, {
    start: 2385,
    length: 7,
    convRule: rule92
  }, {
    start: 2392,
    length: 10,
    convRule: rule14
  }, {
    start: 2402,
    length: 2,
    convRule: rule92
  }, {
    start: 2404,
    length: 2,
    convRule: rule2
  }, {
    start: 2406,
    length: 10,
    convRule: rule8
  }, {
    start: 2416,
    length: 1,
    convRule: rule2
  }, {
    start: 2417,
    length: 1,
    convRule: rule91
  }, {
    start: 2418,
    length: 15,
    convRule: rule14
  }, {
    start: 2433,
    length: 1,
    convRule: rule92
  }, {
    start: 2434,
    length: 2,
    convRule: rule124
  }, {
    start: 2437,
    length: 8,
    convRule: rule14
  }, {
    start: 2447,
    length: 2,
    convRule: rule14
  }, {
    start: 2451,
    length: 22,
    convRule: rule14
  }, {
    start: 2474,
    length: 7,
    convRule: rule14
  }, {
    start: 2482,
    length: 1,
    convRule: rule14
  }, {
    start: 2486,
    length: 4,
    convRule: rule14
  }, {
    start: 2492,
    length: 1,
    convRule: rule92
  }, {
    start: 2493,
    length: 1,
    convRule: rule14
  }, {
    start: 2494,
    length: 3,
    convRule: rule124
  }, {
    start: 2497,
    length: 4,
    convRule: rule92
  }, {
    start: 2503,
    length: 2,
    convRule: rule124
  }, {
    start: 2507,
    length: 2,
    convRule: rule124
  }, {
    start: 2509,
    length: 1,
    convRule: rule92
  }, {
    start: 2510,
    length: 1,
    convRule: rule14
  }, {
    start: 2519,
    length: 1,
    convRule: rule124
  }, {
    start: 2524,
    length: 2,
    convRule: rule14
  }, {
    start: 2527,
    length: 3,
    convRule: rule14
  }, {
    start: 2530,
    length: 2,
    convRule: rule92
  }, {
    start: 2534,
    length: 10,
    convRule: rule8
  }, {
    start: 2544,
    length: 2,
    convRule: rule14
  }, {
    start: 2546,
    length: 2,
    convRule: rule3
  }, {
    start: 2548,
    length: 6,
    convRule: rule17
  }, {
    start: 2554,
    length: 1,
    convRule: rule13
  }, {
    start: 2555,
    length: 1,
    convRule: rule3
  }, {
    start: 2556,
    length: 1,
    convRule: rule14
  }, {
    start: 2557,
    length: 1,
    convRule: rule2
  }, {
    start: 2558,
    length: 1,
    convRule: rule92
  }, {
    start: 2561,
    length: 2,
    convRule: rule92
  }, {
    start: 2563,
    length: 1,
    convRule: rule124
  }, {
    start: 2565,
    length: 6,
    convRule: rule14
  }, {
    start: 2575,
    length: 2,
    convRule: rule14
  }, {
    start: 2579,
    length: 22,
    convRule: rule14
  }, {
    start: 2602,
    length: 7,
    convRule: rule14
  }, {
    start: 2610,
    length: 2,
    convRule: rule14
  }, {
    start: 2613,
    length: 2,
    convRule: rule14
  }, {
    start: 2616,
    length: 2,
    convRule: rule14
  }, {
    start: 2620,
    length: 1,
    convRule: rule92
  }, {
    start: 2622,
    length: 3,
    convRule: rule124
  }, {
    start: 2625,
    length: 2,
    convRule: rule92
  }, {
    start: 2631,
    length: 2,
    convRule: rule92
  }, {
    start: 2635,
    length: 3,
    convRule: rule92
  }, {
    start: 2641,
    length: 1,
    convRule: rule92
  }, {
    start: 2649,
    length: 4,
    convRule: rule14
  }, {
    start: 2654,
    length: 1,
    convRule: rule14
  }, {
    start: 2662,
    length: 10,
    convRule: rule8
  }, {
    start: 2672,
    length: 2,
    convRule: rule92
  }, {
    start: 2674,
    length: 3,
    convRule: rule14
  }, {
    start: 2677,
    length: 1,
    convRule: rule92
  }, {
    start: 2678,
    length: 1,
    convRule: rule2
  }, {
    start: 2689,
    length: 2,
    convRule: rule92
  }, {
    start: 2691,
    length: 1,
    convRule: rule124
  }, {
    start: 2693,
    length: 9,
    convRule: rule14
  }, {
    start: 2703,
    length: 3,
    convRule: rule14
  }, {
    start: 2707,
    length: 22,
    convRule: rule14
  }, {
    start: 2730,
    length: 7,
    convRule: rule14
  }, {
    start: 2738,
    length: 2,
    convRule: rule14
  }, {
    start: 2741,
    length: 5,
    convRule: rule14
  }, {
    start: 2748,
    length: 1,
    convRule: rule92
  }, {
    start: 2749,
    length: 1,
    convRule: rule14
  }, {
    start: 2750,
    length: 3,
    convRule: rule124
  }, {
    start: 2753,
    length: 5,
    convRule: rule92
  }, {
    start: 2759,
    length: 2,
    convRule: rule92
  }, {
    start: 2761,
    length: 1,
    convRule: rule124
  }, {
    start: 2763,
    length: 2,
    convRule: rule124
  }, {
    start: 2765,
    length: 1,
    convRule: rule92
  }, {
    start: 2768,
    length: 1,
    convRule: rule14
  }, {
    start: 2784,
    length: 2,
    convRule: rule14
  }, {
    start: 2786,
    length: 2,
    convRule: rule92
  }, {
    start: 2790,
    length: 10,
    convRule: rule8
  }, {
    start: 2800,
    length: 1,
    convRule: rule2
  }, {
    start: 2801,
    length: 1,
    convRule: rule3
  }, {
    start: 2809,
    length: 1,
    convRule: rule14
  }, {
    start: 2810,
    length: 6,
    convRule: rule92
  }, {
    start: 2817,
    length: 1,
    convRule: rule92
  }, {
    start: 2818,
    length: 2,
    convRule: rule124
  }, {
    start: 2821,
    length: 8,
    convRule: rule14
  }, {
    start: 2831,
    length: 2,
    convRule: rule14
  }, {
    start: 2835,
    length: 22,
    convRule: rule14
  }, {
    start: 2858,
    length: 7,
    convRule: rule14
  }, {
    start: 2866,
    length: 2,
    convRule: rule14
  }, {
    start: 2869,
    length: 5,
    convRule: rule14
  }, {
    start: 2876,
    length: 1,
    convRule: rule92
  }, {
    start: 2877,
    length: 1,
    convRule: rule14
  }, {
    start: 2878,
    length: 1,
    convRule: rule124
  }, {
    start: 2879,
    length: 1,
    convRule: rule92
  }, {
    start: 2880,
    length: 1,
    convRule: rule124
  }, {
    start: 2881,
    length: 4,
    convRule: rule92
  }, {
    start: 2887,
    length: 2,
    convRule: rule124
  }, {
    start: 2891,
    length: 2,
    convRule: rule124
  }, {
    start: 2893,
    length: 1,
    convRule: rule92
  }, {
    start: 2901,
    length: 2,
    convRule: rule92
  }, {
    start: 2903,
    length: 1,
    convRule: rule124
  }, {
    start: 2908,
    length: 2,
    convRule: rule14
  }, {
    start: 2911,
    length: 3,
    convRule: rule14
  }, {
    start: 2914,
    length: 2,
    convRule: rule92
  }, {
    start: 2918,
    length: 10,
    convRule: rule8
  }, {
    start: 2928,
    length: 1,
    convRule: rule13
  }, {
    start: 2929,
    length: 1,
    convRule: rule14
  }, {
    start: 2930,
    length: 6,
    convRule: rule17
  }, {
    start: 2946,
    length: 1,
    convRule: rule92
  }, {
    start: 2947,
    length: 1,
    convRule: rule14
  }, {
    start: 2949,
    length: 6,
    convRule: rule14
  }, {
    start: 2958,
    length: 3,
    convRule: rule14
  }, {
    start: 2962,
    length: 4,
    convRule: rule14
  }, {
    start: 2969,
    length: 2,
    convRule: rule14
  }, {
    start: 2972,
    length: 1,
    convRule: rule14
  }, {
    start: 2974,
    length: 2,
    convRule: rule14
  }, {
    start: 2979,
    length: 2,
    convRule: rule14
  }, {
    start: 2984,
    length: 3,
    convRule: rule14
  }, {
    start: 2990,
    length: 12,
    convRule: rule14
  }, {
    start: 3006,
    length: 2,
    convRule: rule124
  }, {
    start: 3008,
    length: 1,
    convRule: rule92
  }, {
    start: 3009,
    length: 2,
    convRule: rule124
  }, {
    start: 3014,
    length: 3,
    convRule: rule124
  }, {
    start: 3018,
    length: 3,
    convRule: rule124
  }, {
    start: 3021,
    length: 1,
    convRule: rule92
  }, {
    start: 3024,
    length: 1,
    convRule: rule14
  }, {
    start: 3031,
    length: 1,
    convRule: rule124
  }, {
    start: 3046,
    length: 10,
    convRule: rule8
  }, {
    start: 3056,
    length: 3,
    convRule: rule17
  }, {
    start: 3059,
    length: 6,
    convRule: rule13
  }, {
    start: 3065,
    length: 1,
    convRule: rule3
  }, {
    start: 3066,
    length: 1,
    convRule: rule13
  }, {
    start: 3072,
    length: 1,
    convRule: rule92
  }, {
    start: 3073,
    length: 3,
    convRule: rule124
  }, {
    start: 3076,
    length: 1,
    convRule: rule92
  }, {
    start: 3077,
    length: 8,
    convRule: rule14
  }, {
    start: 3086,
    length: 3,
    convRule: rule14
  }, {
    start: 3090,
    length: 23,
    convRule: rule14
  }, {
    start: 3114,
    length: 16,
    convRule: rule14
  }, {
    start: 3133,
    length: 1,
    convRule: rule14
  }, {
    start: 3134,
    length: 3,
    convRule: rule92
  }, {
    start: 3137,
    length: 4,
    convRule: rule124
  }, {
    start: 3142,
    length: 3,
    convRule: rule92
  }, {
    start: 3146,
    length: 4,
    convRule: rule92
  }, {
    start: 3157,
    length: 2,
    convRule: rule92
  }, {
    start: 3160,
    length: 3,
    convRule: rule14
  }, {
    start: 3168,
    length: 2,
    convRule: rule14
  }, {
    start: 3170,
    length: 2,
    convRule: rule92
  }, {
    start: 3174,
    length: 10,
    convRule: rule8
  }, {
    start: 3191,
    length: 1,
    convRule: rule2
  }, {
    start: 3192,
    length: 7,
    convRule: rule17
  }, {
    start: 3199,
    length: 1,
    convRule: rule13
  }, {
    start: 3200,
    length: 1,
    convRule: rule14
  }, {
    start: 3201,
    length: 1,
    convRule: rule92
  }, {
    start: 3202,
    length: 2,
    convRule: rule124
  }, {
    start: 3204,
    length: 1,
    convRule: rule2
  }, {
    start: 3205,
    length: 8,
    convRule: rule14
  }, {
    start: 3214,
    length: 3,
    convRule: rule14
  }, {
    start: 3218,
    length: 23,
    convRule: rule14
  }, {
    start: 3242,
    length: 10,
    convRule: rule14
  }, {
    start: 3253,
    length: 5,
    convRule: rule14
  }, {
    start: 3260,
    length: 1,
    convRule: rule92
  }, {
    start: 3261,
    length: 1,
    convRule: rule14
  }, {
    start: 3262,
    length: 1,
    convRule: rule124
  }, {
    start: 3263,
    length: 1,
    convRule: rule92
  }, {
    start: 3264,
    length: 5,
    convRule: rule124
  }, {
    start: 3270,
    length: 1,
    convRule: rule92
  }, {
    start: 3271,
    length: 2,
    convRule: rule124
  }, {
    start: 3274,
    length: 2,
    convRule: rule124
  }, {
    start: 3276,
    length: 2,
    convRule: rule92
  }, {
    start: 3285,
    length: 2,
    convRule: rule124
  }, {
    start: 3294,
    length: 1,
    convRule: rule14
  }, {
    start: 3296,
    length: 2,
    convRule: rule14
  }, {
    start: 3298,
    length: 2,
    convRule: rule92
  }, {
    start: 3302,
    length: 10,
    convRule: rule8
  }, {
    start: 3313,
    length: 2,
    convRule: rule14
  }, {
    start: 3328,
    length: 2,
    convRule: rule92
  }, {
    start: 3330,
    length: 2,
    convRule: rule124
  }, {
    start: 3332,
    length: 9,
    convRule: rule14
  }, {
    start: 3342,
    length: 3,
    convRule: rule14
  }, {
    start: 3346,
    length: 41,
    convRule: rule14
  }, {
    start: 3387,
    length: 2,
    convRule: rule92
  }, {
    start: 3389,
    length: 1,
    convRule: rule14
  }, {
    start: 3390,
    length: 3,
    convRule: rule124
  }, {
    start: 3393,
    length: 4,
    convRule: rule92
  }, {
    start: 3398,
    length: 3,
    convRule: rule124
  }, {
    start: 3402,
    length: 3,
    convRule: rule124
  }, {
    start: 3405,
    length: 1,
    convRule: rule92
  }, {
    start: 3406,
    length: 1,
    convRule: rule14
  }, {
    start: 3407,
    length: 1,
    convRule: rule13
  }, {
    start: 3412,
    length: 3,
    convRule: rule14
  }, {
    start: 3415,
    length: 1,
    convRule: rule124
  }, {
    start: 3416,
    length: 7,
    convRule: rule17
  }, {
    start: 3423,
    length: 3,
    convRule: rule14
  }, {
    start: 3426,
    length: 2,
    convRule: rule92
  }, {
    start: 3430,
    length: 10,
    convRule: rule8
  }, {
    start: 3440,
    length: 9,
    convRule: rule17
  }, {
    start: 3449,
    length: 1,
    convRule: rule13
  }, {
    start: 3450,
    length: 6,
    convRule: rule14
  }, {
    start: 3457,
    length: 1,
    convRule: rule92
  }, {
    start: 3458,
    length: 2,
    convRule: rule124
  }, {
    start: 3461,
    length: 18,
    convRule: rule14
  }, {
    start: 3482,
    length: 24,
    convRule: rule14
  }, {
    start: 3507,
    length: 9,
    convRule: rule14
  }, {
    start: 3517,
    length: 1,
    convRule: rule14
  }, {
    start: 3520,
    length: 7,
    convRule: rule14
  }, {
    start: 3530,
    length: 1,
    convRule: rule92
  }, {
    start: 3535,
    length: 3,
    convRule: rule124
  }, {
    start: 3538,
    length: 3,
    convRule: rule92
  }, {
    start: 3542,
    length: 1,
    convRule: rule92
  }, {
    start: 3544,
    length: 8,
    convRule: rule124
  }, {
    start: 3558,
    length: 10,
    convRule: rule8
  }, {
    start: 3570,
    length: 2,
    convRule: rule124
  }, {
    start: 3572,
    length: 1,
    convRule: rule2
  }, {
    start: 3585,
    length: 48,
    convRule: rule14
  }, {
    start: 3633,
    length: 1,
    convRule: rule92
  }, {
    start: 3634,
    length: 2,
    convRule: rule14
  }, {
    start: 3636,
    length: 7,
    convRule: rule92
  }, {
    start: 3647,
    length: 1,
    convRule: rule3
  }, {
    start: 3648,
    length: 6,
    convRule: rule14
  }, {
    start: 3654,
    length: 1,
    convRule: rule91
  }, {
    start: 3655,
    length: 8,
    convRule: rule92
  }, {
    start: 3663,
    length: 1,
    convRule: rule2
  }, {
    start: 3664,
    length: 10,
    convRule: rule8
  }, {
    start: 3674,
    length: 2,
    convRule: rule2
  }, {
    start: 3713,
    length: 2,
    convRule: rule14
  }, {
    start: 3716,
    length: 1,
    convRule: rule14
  }, {
    start: 3718,
    length: 5,
    convRule: rule14
  }, {
    start: 3724,
    length: 24,
    convRule: rule14
  }, {
    start: 3749,
    length: 1,
    convRule: rule14
  }, {
    start: 3751,
    length: 10,
    convRule: rule14
  }, {
    start: 3761,
    length: 1,
    convRule: rule92
  }, {
    start: 3762,
    length: 2,
    convRule: rule14
  }, {
    start: 3764,
    length: 9,
    convRule: rule92
  }, {
    start: 3773,
    length: 1,
    convRule: rule14
  }, {
    start: 3776,
    length: 5,
    convRule: rule14
  }, {
    start: 3782,
    length: 1,
    convRule: rule91
  }, {
    start: 3784,
    length: 6,
    convRule: rule92
  }, {
    start: 3792,
    length: 10,
    convRule: rule8
  }, {
    start: 3804,
    length: 4,
    convRule: rule14
  }, {
    start: 3840,
    length: 1,
    convRule: rule14
  }, {
    start: 3841,
    length: 3,
    convRule: rule13
  }, {
    start: 3844,
    length: 15,
    convRule: rule2
  }, {
    start: 3859,
    length: 1,
    convRule: rule13
  }, {
    start: 3860,
    length: 1,
    convRule: rule2
  }, {
    start: 3861,
    length: 3,
    convRule: rule13
  }, {
    start: 3864,
    length: 2,
    convRule: rule92
  }, {
    start: 3866,
    length: 6,
    convRule: rule13
  }, {
    start: 3872,
    length: 10,
    convRule: rule8
  }, {
    start: 3882,
    length: 10,
    convRule: rule17
  }, {
    start: 3892,
    length: 1,
    convRule: rule13
  }, {
    start: 3893,
    length: 1,
    convRule: rule92
  }, {
    start: 3894,
    length: 1,
    convRule: rule13
  }, {
    start: 3895,
    length: 1,
    convRule: rule92
  }, {
    start: 3896,
    length: 1,
    convRule: rule13
  }, {
    start: 3897,
    length: 1,
    convRule: rule92
  }, {
    start: 3898,
    length: 1,
    convRule: rule4
  }, {
    start: 3899,
    length: 1,
    convRule: rule5
  }, {
    start: 3900,
    length: 1,
    convRule: rule4
  }, {
    start: 3901,
    length: 1,
    convRule: rule5
  }, {
    start: 3902,
    length: 2,
    convRule: rule124
  }, {
    start: 3904,
    length: 8,
    convRule: rule14
  }, {
    start: 3913,
    length: 36,
    convRule: rule14
  }, {
    start: 3953,
    length: 14,
    convRule: rule92
  }, {
    start: 3967,
    length: 1,
    convRule: rule124
  }, {
    start: 3968,
    length: 5,
    convRule: rule92
  }, {
    start: 3973,
    length: 1,
    convRule: rule2
  }, {
    start: 3974,
    length: 2,
    convRule: rule92
  }, {
    start: 3976,
    length: 5,
    convRule: rule14
  }, {
    start: 3981,
    length: 11,
    convRule: rule92
  }, {
    start: 3993,
    length: 36,
    convRule: rule92
  }, {
    start: 4030,
    length: 8,
    convRule: rule13
  }, {
    start: 4038,
    length: 1,
    convRule: rule92
  }, {
    start: 4039,
    length: 6,
    convRule: rule13
  }, {
    start: 4046,
    length: 2,
    convRule: rule13
  }, {
    start: 4048,
    length: 5,
    convRule: rule2
  }, {
    start: 4053,
    length: 4,
    convRule: rule13
  }, {
    start: 4057,
    length: 2,
    convRule: rule2
  }, {
    start: 4096,
    length: 43,
    convRule: rule14
  }, {
    start: 4139,
    length: 2,
    convRule: rule124
  }, {
    start: 4141,
    length: 4,
    convRule: rule92
  }, {
    start: 4145,
    length: 1,
    convRule: rule124
  }, {
    start: 4146,
    length: 6,
    convRule: rule92
  }, {
    start: 4152,
    length: 1,
    convRule: rule124
  }, {
    start: 4153,
    length: 2,
    convRule: rule92
  }, {
    start: 4155,
    length: 2,
    convRule: rule124
  }, {
    start: 4157,
    length: 2,
    convRule: rule92
  }, {
    start: 4159,
    length: 1,
    convRule: rule14
  }, {
    start: 4160,
    length: 10,
    convRule: rule8
  }, {
    start: 4170,
    length: 6,
    convRule: rule2
  }, {
    start: 4176,
    length: 6,
    convRule: rule14
  }, {
    start: 4182,
    length: 2,
    convRule: rule124
  }, {
    start: 4184,
    length: 2,
    convRule: rule92
  }, {
    start: 4186,
    length: 4,
    convRule: rule14
  }, {
    start: 4190,
    length: 3,
    convRule: rule92
  }, {
    start: 4193,
    length: 1,
    convRule: rule14
  }, {
    start: 4194,
    length: 3,
    convRule: rule124
  }, {
    start: 4197,
    length: 2,
    convRule: rule14
  }, {
    start: 4199,
    length: 7,
    convRule: rule124
  }, {
    start: 4206,
    length: 3,
    convRule: rule14
  }, {
    start: 4209,
    length: 4,
    convRule: rule92
  }, {
    start: 4213,
    length: 13,
    convRule: rule14
  }, {
    start: 4226,
    length: 1,
    convRule: rule92
  }, {
    start: 4227,
    length: 2,
    convRule: rule124
  }, {
    start: 4229,
    length: 2,
    convRule: rule92
  }, {
    start: 4231,
    length: 6,
    convRule: rule124
  }, {
    start: 4237,
    length: 1,
    convRule: rule92
  }, {
    start: 4238,
    length: 1,
    convRule: rule14
  }, {
    start: 4239,
    length: 1,
    convRule: rule124
  }, {
    start: 4240,
    length: 10,
    convRule: rule8
  }, {
    start: 4250,
    length: 3,
    convRule: rule124
  }, {
    start: 4253,
    length: 1,
    convRule: rule92
  }, {
    start: 4254,
    length: 2,
    convRule: rule13
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4347,
    length: 1,
    convRule: rule2
  }, {
    start: 4348,
    length: 1,
    convRule: rule91
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 4352,
    length: 329,
    convRule: rule14
  }, {
    start: 4682,
    length: 4,
    convRule: rule14
  }, {
    start: 4688,
    length: 7,
    convRule: rule14
  }, {
    start: 4696,
    length: 1,
    convRule: rule14
  }, {
    start: 4698,
    length: 4,
    convRule: rule14
  }, {
    start: 4704,
    length: 41,
    convRule: rule14
  }, {
    start: 4746,
    length: 4,
    convRule: rule14
  }, {
    start: 4752,
    length: 33,
    convRule: rule14
  }, {
    start: 4786,
    length: 4,
    convRule: rule14
  }, {
    start: 4792,
    length: 7,
    convRule: rule14
  }, {
    start: 4800,
    length: 1,
    convRule: rule14
  }, {
    start: 4802,
    length: 4,
    convRule: rule14
  }, {
    start: 4808,
    length: 15,
    convRule: rule14
  }, {
    start: 4824,
    length: 57,
    convRule: rule14
  }, {
    start: 4882,
    length: 4,
    convRule: rule14
  }, {
    start: 4888,
    length: 67,
    convRule: rule14
  }, {
    start: 4957,
    length: 3,
    convRule: rule92
  }, {
    start: 4960,
    length: 9,
    convRule: rule2
  }, {
    start: 4969,
    length: 20,
    convRule: rule17
  }, {
    start: 4992,
    length: 16,
    convRule: rule14
  }, {
    start: 5008,
    length: 10,
    convRule: rule13
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 5120,
    length: 1,
    convRule: rule7
  }, {
    start: 5121,
    length: 620,
    convRule: rule14
  }, {
    start: 5741,
    length: 1,
    convRule: rule13
  }, {
    start: 5742,
    length: 1,
    convRule: rule2
  }, {
    start: 5743,
    length: 17,
    convRule: rule14
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 5761,
    length: 26,
    convRule: rule14
  }, {
    start: 5787,
    length: 1,
    convRule: rule4
  }, {
    start: 5788,
    length: 1,
    convRule: rule5
  }, {
    start: 5792,
    length: 75,
    convRule: rule14
  }, {
    start: 5867,
    length: 3,
    convRule: rule2
  }, {
    start: 5870,
    length: 3,
    convRule: rule128
  }, {
    start: 5873,
    length: 8,
    convRule: rule14
  }, {
    start: 5888,
    length: 13,
    convRule: rule14
  }, {
    start: 5902,
    length: 4,
    convRule: rule14
  }, {
    start: 5906,
    length: 3,
    convRule: rule92
  }, {
    start: 5920,
    length: 18,
    convRule: rule14
  }, {
    start: 5938,
    length: 3,
    convRule: rule92
  }, {
    start: 5941,
    length: 2,
    convRule: rule2
  }, {
    start: 5952,
    length: 18,
    convRule: rule14
  }, {
    start: 5970,
    length: 2,
    convRule: rule92
  }, {
    start: 5984,
    length: 13,
    convRule: rule14
  }, {
    start: 5998,
    length: 3,
    convRule: rule14
  }, {
    start: 6002,
    length: 2,
    convRule: rule92
  }, {
    start: 6016,
    length: 52,
    convRule: rule14
  }, {
    start: 6068,
    length: 2,
    convRule: rule92
  }, {
    start: 6070,
    length: 1,
    convRule: rule124
  }, {
    start: 6071,
    length: 7,
    convRule: rule92
  }, {
    start: 6078,
    length: 8,
    convRule: rule124
  }, {
    start: 6086,
    length: 1,
    convRule: rule92
  }, {
    start: 6087,
    length: 2,
    convRule: rule124
  }, {
    start: 6089,
    length: 11,
    convRule: rule92
  }, {
    start: 6100,
    length: 3,
    convRule: rule2
  }, {
    start: 6103,
    length: 1,
    convRule: rule91
  }, {
    start: 6104,
    length: 3,
    convRule: rule2
  }, {
    start: 6107,
    length: 1,
    convRule: rule3
  }, {
    start: 6108,
    length: 1,
    convRule: rule14
  }, {
    start: 6109,
    length: 1,
    convRule: rule92
  }, {
    start: 6112,
    length: 10,
    convRule: rule8
  }, {
    start: 6128,
    length: 10,
    convRule: rule17
  }, {
    start: 6144,
    length: 6,
    convRule: rule2
  }, {
    start: 6150,
    length: 1,
    convRule: rule7
  }, {
    start: 6151,
    length: 4,
    convRule: rule2
  }, {
    start: 6155,
    length: 3,
    convRule: rule92
  }, {
    start: 6158,
    length: 1,
    convRule: rule16
  }, {
    start: 6160,
    length: 10,
    convRule: rule8
  }, {
    start: 6176,
    length: 35,
    convRule: rule14
  }, {
    start: 6211,
    length: 1,
    convRule: rule91
  }, {
    start: 6212,
    length: 53,
    convRule: rule14
  }, {
    start: 6272,
    length: 5,
    convRule: rule14
  }, {
    start: 6277,
    length: 2,
    convRule: rule92
  }, {
    start: 6279,
    length: 34,
    convRule: rule14
  }, {
    start: 6313,
    length: 1,
    convRule: rule92
  }, {
    start: 6314,
    length: 1,
    convRule: rule14
  }, {
    start: 6320,
    length: 70,
    convRule: rule14
  }, {
    start: 6400,
    length: 31,
    convRule: rule14
  }, {
    start: 6432,
    length: 3,
    convRule: rule92
  }, {
    start: 6435,
    length: 4,
    convRule: rule124
  }, {
    start: 6439,
    length: 2,
    convRule: rule92
  }, {
    start: 6441,
    length: 3,
    convRule: rule124
  }, {
    start: 6448,
    length: 2,
    convRule: rule124
  }, {
    start: 6450,
    length: 1,
    convRule: rule92
  }, {
    start: 6451,
    length: 6,
    convRule: rule124
  }, {
    start: 6457,
    length: 3,
    convRule: rule92
  }, {
    start: 6464,
    length: 1,
    convRule: rule13
  }, {
    start: 6468,
    length: 2,
    convRule: rule2
  }, {
    start: 6470,
    length: 10,
    convRule: rule8
  }, {
    start: 6480,
    length: 30,
    convRule: rule14
  }, {
    start: 6512,
    length: 5,
    convRule: rule14
  }, {
    start: 6528,
    length: 44,
    convRule: rule14
  }, {
    start: 6576,
    length: 26,
    convRule: rule14
  }, {
    start: 6608,
    length: 10,
    convRule: rule8
  }, {
    start: 6618,
    length: 1,
    convRule: rule17
  }, {
    start: 6622,
    length: 34,
    convRule: rule13
  }, {
    start: 6656,
    length: 23,
    convRule: rule14
  }, {
    start: 6679,
    length: 2,
    convRule: rule92
  }, {
    start: 6681,
    length: 2,
    convRule: rule124
  }, {
    start: 6683,
    length: 1,
    convRule: rule92
  }, {
    start: 6686,
    length: 2,
    convRule: rule2
  }, {
    start: 6688,
    length: 53,
    convRule: rule14
  }, {
    start: 6741,
    length: 1,
    convRule: rule124
  }, {
    start: 6742,
    length: 1,
    convRule: rule92
  }, {
    start: 6743,
    length: 1,
    convRule: rule124
  }, {
    start: 6744,
    length: 7,
    convRule: rule92
  }, {
    start: 6752,
    length: 1,
    convRule: rule92
  }, {
    start: 6753,
    length: 1,
    convRule: rule124
  }, {
    start: 6754,
    length: 1,
    convRule: rule92
  }, {
    start: 6755,
    length: 2,
    convRule: rule124
  }, {
    start: 6757,
    length: 8,
    convRule: rule92
  }, {
    start: 6765,
    length: 6,
    convRule: rule124
  }, {
    start: 6771,
    length: 10,
    convRule: rule92
  }, {
    start: 6783,
    length: 1,
    convRule: rule92
  }, {
    start: 6784,
    length: 10,
    convRule: rule8
  }, {
    start: 6800,
    length: 10,
    convRule: rule8
  }, {
    start: 6816,
    length: 7,
    convRule: rule2
  }, {
    start: 6823,
    length: 1,
    convRule: rule91
  }, {
    start: 6824,
    length: 6,
    convRule: rule2
  }, {
    start: 6832,
    length: 14,
    convRule: rule92
  }, {
    start: 6846,
    length: 1,
    convRule: rule119
  }, {
    start: 6847,
    length: 2,
    convRule: rule92
  }, {
    start: 6912,
    length: 4,
    convRule: rule92
  }, {
    start: 6916,
    length: 1,
    convRule: rule124
  }, {
    start: 6917,
    length: 47,
    convRule: rule14
  }, {
    start: 6964,
    length: 1,
    convRule: rule92
  }, {
    start: 6965,
    length: 1,
    convRule: rule124
  }, {
    start: 6966,
    length: 5,
    convRule: rule92
  }, {
    start: 6971,
    length: 1,
    convRule: rule124
  }, {
    start: 6972,
    length: 1,
    convRule: rule92
  }, {
    start: 6973,
    length: 5,
    convRule: rule124
  }, {
    start: 6978,
    length: 1,
    convRule: rule92
  }, {
    start: 6979,
    length: 2,
    convRule: rule124
  }, {
    start: 6981,
    length: 7,
    convRule: rule14
  }, {
    start: 6992,
    length: 10,
    convRule: rule8
  }, {
    start: 7002,
    length: 7,
    convRule: rule2
  }, {
    start: 7009,
    length: 10,
    convRule: rule13
  }, {
    start: 7019,
    length: 9,
    convRule: rule92
  }, {
    start: 7028,
    length: 9,
    convRule: rule13
  }, {
    start: 7040,
    length: 2,
    convRule: rule92
  }, {
    start: 7042,
    length: 1,
    convRule: rule124
  }, {
    start: 7043,
    length: 30,
    convRule: rule14
  }, {
    start: 7073,
    length: 1,
    convRule: rule124
  }, {
    start: 7074,
    length: 4,
    convRule: rule92
  }, {
    start: 7078,
    length: 2,
    convRule: rule124
  }, {
    start: 7080,
    length: 2,
    convRule: rule92
  }, {
    start: 7082,
    length: 1,
    convRule: rule124
  }, {
    start: 7083,
    length: 3,
    convRule: rule92
  }, {
    start: 7086,
    length: 2,
    convRule: rule14
  }, {
    start: 7088,
    length: 10,
    convRule: rule8
  }, {
    start: 7098,
    length: 44,
    convRule: rule14
  }, {
    start: 7142,
    length: 1,
    convRule: rule92
  }, {
    start: 7143,
    length: 1,
    convRule: rule124
  }, {
    start: 7144,
    length: 2,
    convRule: rule92
  }, {
    start: 7146,
    length: 3,
    convRule: rule124
  }, {
    start: 7149,
    length: 1,
    convRule: rule92
  }, {
    start: 7150,
    length: 1,
    convRule: rule124
  }, {
    start: 7151,
    length: 3,
    convRule: rule92
  }, {
    start: 7154,
    length: 2,
    convRule: rule124
  }, {
    start: 7164,
    length: 4,
    convRule: rule2
  }, {
    start: 7168,
    length: 36,
    convRule: rule14
  }, {
    start: 7204,
    length: 8,
    convRule: rule124
  }, {
    start: 7212,
    length: 8,
    convRule: rule92
  }, {
    start: 7220,
    length: 2,
    convRule: rule124
  }, {
    start: 7222,
    length: 2,
    convRule: rule92
  }, {
    start: 7227,
    length: 5,
    convRule: rule2
  }, {
    start: 7232,
    length: 10,
    convRule: rule8
  }, {
    start: 7245,
    length: 3,
    convRule: rule14
  }, {
    start: 7248,
    length: 10,
    convRule: rule8
  }, {
    start: 7258,
    length: 30,
    convRule: rule14
  }, {
    start: 7288,
    length: 6,
    convRule: rule91
  }, {
    start: 7294,
    length: 2,
    convRule: rule2
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7360,
    length: 8,
    convRule: rule2
  }, {
    start: 7376,
    length: 3,
    convRule: rule92
  }, {
    start: 7379,
    length: 1,
    convRule: rule2
  }, {
    start: 7380,
    length: 13,
    convRule: rule92
  }, {
    start: 7393,
    length: 1,
    convRule: rule124
  }, {
    start: 7394,
    length: 7,
    convRule: rule92
  }, {
    start: 7401,
    length: 4,
    convRule: rule14
  }, {
    start: 7405,
    length: 1,
    convRule: rule92
  }, {
    start: 7406,
    length: 6,
    convRule: rule14
  }, {
    start: 7412,
    length: 1,
    convRule: rule92
  }, {
    start: 7413,
    length: 2,
    convRule: rule14
  }, {
    start: 7415,
    length: 1,
    convRule: rule124
  }, {
    start: 7416,
    length: 2,
    convRule: rule92
  }, {
    start: 7418,
    length: 1,
    convRule: rule14
  }, {
    start: 7424,
    length: 44,
    convRule: rule20
  }, {
    start: 7468,
    length: 63,
    convRule: rule91
  }, {
    start: 7531,
    length: 13,
    convRule: rule20
  }, {
    start: 7544,
    length: 1,
    convRule: rule91
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7546,
    length: 3,
    convRule: rule20
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7550,
    length: 16,
    convRule: rule20
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7567,
    length: 12,
    convRule: rule20
  }, {
    start: 7579,
    length: 37,
    convRule: rule91
  }, {
    start: 7616,
    length: 58,
    convRule: rule92
  }, {
    start: 7675,
    length: 5,
    convRule: rule92
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7830,
    length: 5,
    convRule: rule20
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7836,
    length: 2,
    convRule: rule20
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7839,
    length: 1,
    convRule: rule20
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8016,
    length: 1,
    convRule: rule20
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8018,
    length: 1,
    convRule: rule20
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8020,
    length: 1,
    convRule: rule20
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8022,
    length: 1,
    convRule: rule20
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8114,
    length: 1,
    convRule: rule20
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8116,
    length: 1,
    convRule: rule20
  }, {
    start: 8118,
    length: 2,
    convRule: rule20
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8125,
    length: 1,
    convRule: rule10
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8127,
    length: 3,
    convRule: rule10
  }, {
    start: 8130,
    length: 1,
    convRule: rule20
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8132,
    length: 1,
    convRule: rule20
  }, {
    start: 8134,
    length: 2,
    convRule: rule20
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8141,
    length: 3,
    convRule: rule10
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8146,
    length: 2,
    convRule: rule20
  }, {
    start: 8150,
    length: 2,
    convRule: rule20
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8157,
    length: 3,
    convRule: rule10
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8162,
    length: 3,
    convRule: rule20
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8166,
    length: 2,
    convRule: rule20
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8173,
    length: 3,
    convRule: rule10
  }, {
    start: 8178,
    length: 1,
    convRule: rule20
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8180,
    length: 1,
    convRule: rule20
  }, {
    start: 8182,
    length: 2,
    convRule: rule20
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8189,
    length: 2,
    convRule: rule10
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8203,
    length: 5,
    convRule: rule16
  }, {
    start: 8208,
    length: 6,
    convRule: rule7
  }, {
    start: 8214,
    length: 2,
    convRule: rule2
  }, {
    start: 8216,
    length: 1,
    convRule: rule15
  }, {
    start: 8217,
    length: 1,
    convRule: rule19
  }, {
    start: 8218,
    length: 1,
    convRule: rule4
  }, {
    start: 8219,
    length: 2,
    convRule: rule15
  }, {
    start: 8221,
    length: 1,
    convRule: rule19
  }, {
    start: 8222,
    length: 1,
    convRule: rule4
  }, {
    start: 8223,
    length: 1,
    convRule: rule15
  }, {
    start: 8224,
    length: 8,
    convRule: rule2
  }, {
    start: 8232,
    length: 1,
    convRule: rule161
  }, {
    start: 8233,
    length: 1,
    convRule: rule162
  }, {
    start: 8234,
    length: 5,
    convRule: rule16
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8240,
    length: 9,
    convRule: rule2
  }, {
    start: 8249,
    length: 1,
    convRule: rule15
  }, {
    start: 8250,
    length: 1,
    convRule: rule19
  }, {
    start: 8251,
    length: 4,
    convRule: rule2
  }, {
    start: 8255,
    length: 2,
    convRule: rule11
  }, {
    start: 8257,
    length: 3,
    convRule: rule2
  }, {
    start: 8260,
    length: 1,
    convRule: rule6
  }, {
    start: 8261,
    length: 1,
    convRule: rule4
  }, {
    start: 8262,
    length: 1,
    convRule: rule5
  }, {
    start: 8263,
    length: 11,
    convRule: rule2
  }, {
    start: 8274,
    length: 1,
    convRule: rule6
  }, {
    start: 8275,
    length: 1,
    convRule: rule2
  }, {
    start: 8276,
    length: 1,
    convRule: rule11
  }, {
    start: 8277,
    length: 10,
    convRule: rule2
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 8288,
    length: 5,
    convRule: rule16
  }, {
    start: 8294,
    length: 10,
    convRule: rule16
  }, {
    start: 8304,
    length: 1,
    convRule: rule17
  }, {
    start: 8305,
    length: 1,
    convRule: rule91
  }, {
    start: 8308,
    length: 6,
    convRule: rule17
  }, {
    start: 8314,
    length: 3,
    convRule: rule6
  }, {
    start: 8317,
    length: 1,
    convRule: rule4
  }, {
    start: 8318,
    length: 1,
    convRule: rule5
  }, {
    start: 8319,
    length: 1,
    convRule: rule91
  }, {
    start: 8320,
    length: 10,
    convRule: rule17
  }, {
    start: 8330,
    length: 3,
    convRule: rule6
  }, {
    start: 8333,
    length: 1,
    convRule: rule4
  }, {
    start: 8334,
    length: 1,
    convRule: rule5
  }, {
    start: 8336,
    length: 13,
    convRule: rule91
  }, {
    start: 8352,
    length: 32,
    convRule: rule3
  }, {
    start: 8400,
    length: 13,
    convRule: rule92
  }, {
    start: 8413,
    length: 4,
    convRule: rule119
  }, {
    start: 8417,
    length: 1,
    convRule: rule92
  }, {
    start: 8418,
    length: 3,
    convRule: rule119
  }, {
    start: 8421,
    length: 12,
    convRule: rule92
  }, {
    start: 8448,
    length: 2,
    convRule: rule13
  }, {
    start: 8450,
    length: 1,
    convRule: rule107
  }, {
    start: 8451,
    length: 4,
    convRule: rule13
  }, {
    start: 8455,
    length: 1,
    convRule: rule107
  }, {
    start: 8456,
    length: 2,
    convRule: rule13
  }, {
    start: 8458,
    length: 1,
    convRule: rule20
  }, {
    start: 8459,
    length: 3,
    convRule: rule107
  }, {
    start: 8462,
    length: 2,
    convRule: rule20
  }, {
    start: 8464,
    length: 3,
    convRule: rule107
  }, {
    start: 8467,
    length: 1,
    convRule: rule20
  }, {
    start: 8468,
    length: 1,
    convRule: rule13
  }, {
    start: 8469,
    length: 1,
    convRule: rule107
  }, {
    start: 8470,
    length: 2,
    convRule: rule13
  }, {
    start: 8472,
    length: 1,
    convRule: rule6
  }, {
    start: 8473,
    length: 5,
    convRule: rule107
  }, {
    start: 8478,
    length: 6,
    convRule: rule13
  }, {
    start: 8484,
    length: 1,
    convRule: rule107
  }, {
    start: 8485,
    length: 1,
    convRule: rule13
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8487,
    length: 1,
    convRule: rule13
  }, {
    start: 8488,
    length: 1,
    convRule: rule107
  }, {
    start: 8489,
    length: 1,
    convRule: rule13
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8492,
    length: 2,
    convRule: rule107
  }, {
    start: 8494,
    length: 1,
    convRule: rule13
  }, {
    start: 8495,
    length: 1,
    convRule: rule20
  }, {
    start: 8496,
    length: 2,
    convRule: rule107
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8499,
    length: 1,
    convRule: rule107
  }, {
    start: 8500,
    length: 1,
    convRule: rule20
  }, {
    start: 8501,
    length: 4,
    convRule: rule14
  }, {
    start: 8505,
    length: 1,
    convRule: rule20
  }, {
    start: 8506,
    length: 2,
    convRule: rule13
  }, {
    start: 8508,
    length: 2,
    convRule: rule20
  }, {
    start: 8510,
    length: 2,
    convRule: rule107
  }, {
    start: 8512,
    length: 5,
    convRule: rule6
  }, {
    start: 8517,
    length: 1,
    convRule: rule107
  }, {
    start: 8518,
    length: 4,
    convRule: rule20
  }, {
    start: 8522,
    length: 1,
    convRule: rule13
  }, {
    start: 8523,
    length: 1,
    convRule: rule6
  }, {
    start: 8524,
    length: 2,
    convRule: rule13
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8527,
    length: 1,
    convRule: rule13
  }, {
    start: 8528,
    length: 16,
    convRule: rule17
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8576,
    length: 3,
    convRule: rule128
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 8581,
    length: 4,
    convRule: rule128
  }, {
    start: 8585,
    length: 1,
    convRule: rule17
  }, {
    start: 8586,
    length: 2,
    convRule: rule13
  }, {
    start: 8592,
    length: 5,
    convRule: rule6
  }, {
    start: 8597,
    length: 5,
    convRule: rule13
  }, {
    start: 8602,
    length: 2,
    convRule: rule6
  }, {
    start: 8604,
    length: 4,
    convRule: rule13
  }, {
    start: 8608,
    length: 1,
    convRule: rule6
  }, {
    start: 8609,
    length: 2,
    convRule: rule13
  }, {
    start: 8611,
    length: 1,
    convRule: rule6
  }, {
    start: 8612,
    length: 2,
    convRule: rule13
  }, {
    start: 8614,
    length: 1,
    convRule: rule6
  }, {
    start: 8615,
    length: 7,
    convRule: rule13
  }, {
    start: 8622,
    length: 1,
    convRule: rule6
  }, {
    start: 8623,
    length: 31,
    convRule: rule13
  }, {
    start: 8654,
    length: 2,
    convRule: rule6
  }, {
    start: 8656,
    length: 2,
    convRule: rule13
  }, {
    start: 8658,
    length: 1,
    convRule: rule6
  }, {
    start: 8659,
    length: 1,
    convRule: rule13
  }, {
    start: 8660,
    length: 1,
    convRule: rule6
  }, {
    start: 8661,
    length: 31,
    convRule: rule13
  }, {
    start: 8692,
    length: 268,
    convRule: rule6
  }, {
    start: 8960,
    length: 8,
    convRule: rule13
  }, {
    start: 8968,
    length: 1,
    convRule: rule4
  }, {
    start: 8969,
    length: 1,
    convRule: rule5
  }, {
    start: 8970,
    length: 1,
    convRule: rule4
  }, {
    start: 8971,
    length: 1,
    convRule: rule5
  }, {
    start: 8972,
    length: 20,
    convRule: rule13
  }, {
    start: 8992,
    length: 2,
    convRule: rule6
  }, {
    start: 8994,
    length: 7,
    convRule: rule13
  }, {
    start: 9001,
    length: 1,
    convRule: rule4
  }, {
    start: 9002,
    length: 1,
    convRule: rule5
  }, {
    start: 9003,
    length: 81,
    convRule: rule13
  }, {
    start: 9084,
    length: 1,
    convRule: rule6
  }, {
    start: 9085,
    length: 30,
    convRule: rule13
  }, {
    start: 9115,
    length: 25,
    convRule: rule6
  }, {
    start: 9140,
    length: 40,
    convRule: rule13
  }, {
    start: 9180,
    length: 6,
    convRule: rule6
  }, {
    start: 9186,
    length: 69,
    convRule: rule13
  }, {
    start: 9280,
    length: 11,
    convRule: rule13
  }, {
    start: 9312,
    length: 60,
    convRule: rule17
  }, {
    start: 9372,
    length: 26,
    convRule: rule13
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 9450,
    length: 22,
    convRule: rule17
  }, {
    start: 9472,
    length: 183,
    convRule: rule13
  }, {
    start: 9655,
    length: 1,
    convRule: rule6
  }, {
    start: 9656,
    length: 9,
    convRule: rule13
  }, {
    start: 9665,
    length: 1,
    convRule: rule6
  }, {
    start: 9666,
    length: 54,
    convRule: rule13
  }, {
    start: 9720,
    length: 8,
    convRule: rule6
  }, {
    start: 9728,
    length: 111,
    convRule: rule13
  }, {
    start: 9839,
    length: 1,
    convRule: rule6
  }, {
    start: 9840,
    length: 248,
    convRule: rule13
  }, {
    start: 10088,
    length: 1,
    convRule: rule4
  }, {
    start: 10089,
    length: 1,
    convRule: rule5
  }, {
    start: 10090,
    length: 1,
    convRule: rule4
  }, {
    start: 10091,
    length: 1,
    convRule: rule5
  }, {
    start: 10092,
    length: 1,
    convRule: rule4
  }, {
    start: 10093,
    length: 1,
    convRule: rule5
  }, {
    start: 10094,
    length: 1,
    convRule: rule4
  }, {
    start: 10095,
    length: 1,
    convRule: rule5
  }, {
    start: 10096,
    length: 1,
    convRule: rule4
  }, {
    start: 10097,
    length: 1,
    convRule: rule5
  }, {
    start: 10098,
    length: 1,
    convRule: rule4
  }, {
    start: 10099,
    length: 1,
    convRule: rule5
  }, {
    start: 10100,
    length: 1,
    convRule: rule4
  }, {
    start: 10101,
    length: 1,
    convRule: rule5
  }, {
    start: 10102,
    length: 30,
    convRule: rule17
  }, {
    start: 10132,
    length: 44,
    convRule: rule13
  }, {
    start: 10176,
    length: 5,
    convRule: rule6
  }, {
    start: 10181,
    length: 1,
    convRule: rule4
  }, {
    start: 10182,
    length: 1,
    convRule: rule5
  }, {
    start: 10183,
    length: 31,
    convRule: rule6
  }, {
    start: 10214,
    length: 1,
    convRule: rule4
  }, {
    start: 10215,
    length: 1,
    convRule: rule5
  }, {
    start: 10216,
    length: 1,
    convRule: rule4
  }, {
    start: 10217,
    length: 1,
    convRule: rule5
  }, {
    start: 10218,
    length: 1,
    convRule: rule4
  }, {
    start: 10219,
    length: 1,
    convRule: rule5
  }, {
    start: 10220,
    length: 1,
    convRule: rule4
  }, {
    start: 10221,
    length: 1,
    convRule: rule5
  }, {
    start: 10222,
    length: 1,
    convRule: rule4
  }, {
    start: 10223,
    length: 1,
    convRule: rule5
  }, {
    start: 10224,
    length: 16,
    convRule: rule6
  }, {
    start: 10240,
    length: 256,
    convRule: rule13
  }, {
    start: 10496,
    length: 131,
    convRule: rule6
  }, {
    start: 10627,
    length: 1,
    convRule: rule4
  }, {
    start: 10628,
    length: 1,
    convRule: rule5
  }, {
    start: 10629,
    length: 1,
    convRule: rule4
  }, {
    start: 10630,
    length: 1,
    convRule: rule5
  }, {
    start: 10631,
    length: 1,
    convRule: rule4
  }, {
    start: 10632,
    length: 1,
    convRule: rule5
  }, {
    start: 10633,
    length: 1,
    convRule: rule4
  }, {
    start: 10634,
    length: 1,
    convRule: rule5
  }, {
    start: 10635,
    length: 1,
    convRule: rule4
  }, {
    start: 10636,
    length: 1,
    convRule: rule5
  }, {
    start: 10637,
    length: 1,
    convRule: rule4
  }, {
    start: 10638,
    length: 1,
    convRule: rule5
  }, {
    start: 10639,
    length: 1,
    convRule: rule4
  }, {
    start: 10640,
    length: 1,
    convRule: rule5
  }, {
    start: 10641,
    length: 1,
    convRule: rule4
  }, {
    start: 10642,
    length: 1,
    convRule: rule5
  }, {
    start: 10643,
    length: 1,
    convRule: rule4
  }, {
    start: 10644,
    length: 1,
    convRule: rule5
  }, {
    start: 10645,
    length: 1,
    convRule: rule4
  }, {
    start: 10646,
    length: 1,
    convRule: rule5
  }, {
    start: 10647,
    length: 1,
    convRule: rule4
  }, {
    start: 10648,
    length: 1,
    convRule: rule5
  }, {
    start: 10649,
    length: 63,
    convRule: rule6
  }, {
    start: 10712,
    length: 1,
    convRule: rule4
  }, {
    start: 10713,
    length: 1,
    convRule: rule5
  }, {
    start: 10714,
    length: 1,
    convRule: rule4
  }, {
    start: 10715,
    length: 1,
    convRule: rule5
  }, {
    start: 10716,
    length: 32,
    convRule: rule6
  }, {
    start: 10748,
    length: 1,
    convRule: rule4
  }, {
    start: 10749,
    length: 1,
    convRule: rule5
  }, {
    start: 10750,
    length: 258,
    convRule: rule6
  }, {
    start: 11008,
    length: 48,
    convRule: rule13
  }, {
    start: 11056,
    length: 21,
    convRule: rule6
  }, {
    start: 11077,
    length: 2,
    convRule: rule13
  }, {
    start: 11079,
    length: 6,
    convRule: rule6
  }, {
    start: 11085,
    length: 39,
    convRule: rule13
  }, {
    start: 11126,
    length: 32,
    convRule: rule13
  }, {
    start: 11159,
    length: 105,
    convRule: rule13
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11377,
    length: 1,
    convRule: rule20
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11380,
    length: 1,
    convRule: rule20
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11383,
    length: 5,
    convRule: rule20
  }, {
    start: 11388,
    length: 2,
    convRule: rule91
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11492,
    length: 1,
    convRule: rule20
  }, {
    start: 11493,
    length: 6,
    convRule: rule13
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11503,
    length: 3,
    convRule: rule92
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11513,
    length: 4,
    convRule: rule2
  }, {
    start: 11517,
    length: 1,
    convRule: rule17
  }, {
    start: 11518,
    length: 2,
    convRule: rule2
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 11568,
    length: 56,
    convRule: rule14
  }, {
    start: 11631,
    length: 1,
    convRule: rule91
  }, {
    start: 11632,
    length: 1,
    convRule: rule2
  }, {
    start: 11647,
    length: 1,
    convRule: rule92
  }, {
    start: 11648,
    length: 23,
    convRule: rule14
  }, {
    start: 11680,
    length: 7,
    convRule: rule14
  }, {
    start: 11688,
    length: 7,
    convRule: rule14
  }, {
    start: 11696,
    length: 7,
    convRule: rule14
  }, {
    start: 11704,
    length: 7,
    convRule: rule14
  }, {
    start: 11712,
    length: 7,
    convRule: rule14
  }, {
    start: 11720,
    length: 7,
    convRule: rule14
  }, {
    start: 11728,
    length: 7,
    convRule: rule14
  }, {
    start: 11736,
    length: 7,
    convRule: rule14
  }, {
    start: 11744,
    length: 32,
    convRule: rule92
  }, {
    start: 11776,
    length: 2,
    convRule: rule2
  }, {
    start: 11778,
    length: 1,
    convRule: rule15
  }, {
    start: 11779,
    length: 1,
    convRule: rule19
  }, {
    start: 11780,
    length: 1,
    convRule: rule15
  }, {
    start: 11781,
    length: 1,
    convRule: rule19
  }, {
    start: 11782,
    length: 3,
    convRule: rule2
  }, {
    start: 11785,
    length: 1,
    convRule: rule15
  }, {
    start: 11786,
    length: 1,
    convRule: rule19
  }, {
    start: 11787,
    length: 1,
    convRule: rule2
  }, {
    start: 11788,
    length: 1,
    convRule: rule15
  }, {
    start: 11789,
    length: 1,
    convRule: rule19
  }, {
    start: 11790,
    length: 9,
    convRule: rule2
  }, {
    start: 11799,
    length: 1,
    convRule: rule7
  }, {
    start: 11800,
    length: 2,
    convRule: rule2
  }, {
    start: 11802,
    length: 1,
    convRule: rule7
  }, {
    start: 11803,
    length: 1,
    convRule: rule2
  }, {
    start: 11804,
    length: 1,
    convRule: rule15
  }, {
    start: 11805,
    length: 1,
    convRule: rule19
  }, {
    start: 11806,
    length: 2,
    convRule: rule2
  }, {
    start: 11808,
    length: 1,
    convRule: rule15
  }, {
    start: 11809,
    length: 1,
    convRule: rule19
  }, {
    start: 11810,
    length: 1,
    convRule: rule4
  }, {
    start: 11811,
    length: 1,
    convRule: rule5
  }, {
    start: 11812,
    length: 1,
    convRule: rule4
  }, {
    start: 11813,
    length: 1,
    convRule: rule5
  }, {
    start: 11814,
    length: 1,
    convRule: rule4
  }, {
    start: 11815,
    length: 1,
    convRule: rule5
  }, {
    start: 11816,
    length: 1,
    convRule: rule4
  }, {
    start: 11817,
    length: 1,
    convRule: rule5
  }, {
    start: 11818,
    length: 5,
    convRule: rule2
  }, {
    start: 11823,
    length: 1,
    convRule: rule91
  }, {
    start: 11824,
    length: 10,
    convRule: rule2
  }, {
    start: 11834,
    length: 2,
    convRule: rule7
  }, {
    start: 11836,
    length: 4,
    convRule: rule2
  }, {
    start: 11840,
    length: 1,
    convRule: rule7
  }, {
    start: 11841,
    length: 1,
    convRule: rule2
  }, {
    start: 11842,
    length: 1,
    convRule: rule4
  }, {
    start: 11843,
    length: 13,
    convRule: rule2
  }, {
    start: 11856,
    length: 2,
    convRule: rule13
  }, {
    start: 11858,
    length: 1,
    convRule: rule2
  }, {
    start: 11904,
    length: 26,
    convRule: rule13
  }, {
    start: 11931,
    length: 89,
    convRule: rule13
  }, {
    start: 12032,
    length: 214,
    convRule: rule13
  }, {
    start: 12272,
    length: 12,
    convRule: rule13
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }, {
    start: 12289,
    length: 3,
    convRule: rule2
  }, {
    start: 12292,
    length: 1,
    convRule: rule13
  }, {
    start: 12293,
    length: 1,
    convRule: rule91
  }, {
    start: 12294,
    length: 1,
    convRule: rule14
  }, {
    start: 12295,
    length: 1,
    convRule: rule128
  }, {
    start: 12296,
    length: 1,
    convRule: rule4
  }, {
    start: 12297,
    length: 1,
    convRule: rule5
  }, {
    start: 12298,
    length: 1,
    convRule: rule4
  }, {
    start: 12299,
    length: 1,
    convRule: rule5
  }, {
    start: 12300,
    length: 1,
    convRule: rule4
  }, {
    start: 12301,
    length: 1,
    convRule: rule5
  }, {
    start: 12302,
    length: 1,
    convRule: rule4
  }, {
    start: 12303,
    length: 1,
    convRule: rule5
  }, {
    start: 12304,
    length: 1,
    convRule: rule4
  }, {
    start: 12305,
    length: 1,
    convRule: rule5
  }, {
    start: 12306,
    length: 2,
    convRule: rule13
  }, {
    start: 12308,
    length: 1,
    convRule: rule4
  }, {
    start: 12309,
    length: 1,
    convRule: rule5
  }, {
    start: 12310,
    length: 1,
    convRule: rule4
  }, {
    start: 12311,
    length: 1,
    convRule: rule5
  }, {
    start: 12312,
    length: 1,
    convRule: rule4
  }, {
    start: 12313,
    length: 1,
    convRule: rule5
  }, {
    start: 12314,
    length: 1,
    convRule: rule4
  }, {
    start: 12315,
    length: 1,
    convRule: rule5
  }, {
    start: 12316,
    length: 1,
    convRule: rule7
  }, {
    start: 12317,
    length: 1,
    convRule: rule4
  }, {
    start: 12318,
    length: 2,
    convRule: rule5
  }, {
    start: 12320,
    length: 1,
    convRule: rule13
  }, {
    start: 12321,
    length: 9,
    convRule: rule128
  }, {
    start: 12330,
    length: 4,
    convRule: rule92
  }, {
    start: 12334,
    length: 2,
    convRule: rule124
  }, {
    start: 12336,
    length: 1,
    convRule: rule7
  }, {
    start: 12337,
    length: 5,
    convRule: rule91
  }, {
    start: 12342,
    length: 2,
    convRule: rule13
  }, {
    start: 12344,
    length: 3,
    convRule: rule128
  }, {
    start: 12347,
    length: 1,
    convRule: rule91
  }, {
    start: 12348,
    length: 1,
    convRule: rule14
  }, {
    start: 12349,
    length: 1,
    convRule: rule2
  }, {
    start: 12350,
    length: 2,
    convRule: rule13
  }, {
    start: 12353,
    length: 86,
    convRule: rule14
  }, {
    start: 12441,
    length: 2,
    convRule: rule92
  }, {
    start: 12443,
    length: 2,
    convRule: rule10
  }, {
    start: 12445,
    length: 2,
    convRule: rule91
  }, {
    start: 12447,
    length: 1,
    convRule: rule14
  }, {
    start: 12448,
    length: 1,
    convRule: rule7
  }, {
    start: 12449,
    length: 90,
    convRule: rule14
  }, {
    start: 12539,
    length: 1,
    convRule: rule2
  }, {
    start: 12540,
    length: 3,
    convRule: rule91
  }, {
    start: 12543,
    length: 1,
    convRule: rule14
  }, {
    start: 12549,
    length: 43,
    convRule: rule14
  }, {
    start: 12593,
    length: 94,
    convRule: rule14
  }, {
    start: 12688,
    length: 2,
    convRule: rule13
  }, {
    start: 12690,
    length: 4,
    convRule: rule17
  }, {
    start: 12694,
    length: 10,
    convRule: rule13
  }, {
    start: 12704,
    length: 32,
    convRule: rule14
  }, {
    start: 12736,
    length: 36,
    convRule: rule13
  }, {
    start: 12784,
    length: 16,
    convRule: rule14
  }, {
    start: 12800,
    length: 31,
    convRule: rule13
  }, {
    start: 12832,
    length: 10,
    convRule: rule17
  }, {
    start: 12842,
    length: 30,
    convRule: rule13
  }, {
    start: 12872,
    length: 8,
    convRule: rule17
  }, {
    start: 12880,
    length: 1,
    convRule: rule13
  }, {
    start: 12881,
    length: 15,
    convRule: rule17
  }, {
    start: 12896,
    length: 32,
    convRule: rule13
  }, {
    start: 12928,
    length: 10,
    convRule: rule17
  }, {
    start: 12938,
    length: 39,
    convRule: rule13
  }, {
    start: 12977,
    length: 15,
    convRule: rule17
  }, {
    start: 12992,
    length: 320,
    convRule: rule13
  }, {
    start: 13312,
    length: 6592,
    convRule: rule14
  }, {
    start: 19904,
    length: 64,
    convRule: rule13
  }, {
    start: 19968,
    length: 20989,
    convRule: rule14
  }, {
    start: 40960,
    length: 21,
    convRule: rule14
  }, {
    start: 40981,
    length: 1,
    convRule: rule91
  }, {
    start: 40982,
    length: 1143,
    convRule: rule14
  }, {
    start: 42128,
    length: 55,
    convRule: rule13
  }, {
    start: 42192,
    length: 40,
    convRule: rule14
  }, {
    start: 42232,
    length: 6,
    convRule: rule91
  }, {
    start: 42238,
    length: 2,
    convRule: rule2
  }, {
    start: 42240,
    length: 268,
    convRule: rule14
  }, {
    start: 42508,
    length: 1,
    convRule: rule91
  }, {
    start: 42509,
    length: 3,
    convRule: rule2
  }, {
    start: 42512,
    length: 16,
    convRule: rule14
  }, {
    start: 42528,
    length: 10,
    convRule: rule8
  }, {
    start: 42538,
    length: 2,
    convRule: rule14
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42606,
    length: 1,
    convRule: rule14
  }, {
    start: 42607,
    length: 1,
    convRule: rule92
  }, {
    start: 42608,
    length: 3,
    convRule: rule119
  }, {
    start: 42611,
    length: 1,
    convRule: rule2
  }, {
    start: 42612,
    length: 10,
    convRule: rule92
  }, {
    start: 42622,
    length: 1,
    convRule: rule2
  }, {
    start: 42623,
    length: 1,
    convRule: rule91
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42652,
    length: 2,
    convRule: rule91
  }, {
    start: 42654,
    length: 2,
    convRule: rule92
  }, {
    start: 42656,
    length: 70,
    convRule: rule14
  }, {
    start: 42726,
    length: 10,
    convRule: rule128
  }, {
    start: 42736,
    length: 2,
    convRule: rule92
  }, {
    start: 42738,
    length: 6,
    convRule: rule2
  }, {
    start: 42752,
    length: 23,
    convRule: rule10
  }, {
    start: 42775,
    length: 9,
    convRule: rule91
  }, {
    start: 42784,
    length: 2,
    convRule: rule10
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42800,
    length: 2,
    convRule: rule20
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42864,
    length: 1,
    convRule: rule91
  }, {
    start: 42865,
    length: 8,
    convRule: rule20
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42888,
    length: 1,
    convRule: rule91
  }, {
    start: 42889,
    length: 2,
    convRule: rule10
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42894,
    length: 1,
    convRule: rule20
  }, {
    start: 42895,
    length: 1,
    convRule: rule14
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42901,
    length: 1,
    convRule: rule20
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42927,
    length: 1,
    convRule: rule20
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 42999,
    length: 1,
    convRule: rule14
  }, {
    start: 43e3,
    length: 2,
    convRule: rule91
  }, {
    start: 43002,
    length: 1,
    convRule: rule20
  }, {
    start: 43003,
    length: 7,
    convRule: rule14
  }, {
    start: 43010,
    length: 1,
    convRule: rule92
  }, {
    start: 43011,
    length: 3,
    convRule: rule14
  }, {
    start: 43014,
    length: 1,
    convRule: rule92
  }, {
    start: 43015,
    length: 4,
    convRule: rule14
  }, {
    start: 43019,
    length: 1,
    convRule: rule92
  }, {
    start: 43020,
    length: 23,
    convRule: rule14
  }, {
    start: 43043,
    length: 2,
    convRule: rule124
  }, {
    start: 43045,
    length: 2,
    convRule: rule92
  }, {
    start: 43047,
    length: 1,
    convRule: rule124
  }, {
    start: 43048,
    length: 4,
    convRule: rule13
  }, {
    start: 43052,
    length: 1,
    convRule: rule92
  }, {
    start: 43056,
    length: 6,
    convRule: rule17
  }, {
    start: 43062,
    length: 2,
    convRule: rule13
  }, {
    start: 43064,
    length: 1,
    convRule: rule3
  }, {
    start: 43065,
    length: 1,
    convRule: rule13
  }, {
    start: 43072,
    length: 52,
    convRule: rule14
  }, {
    start: 43124,
    length: 4,
    convRule: rule2
  }, {
    start: 43136,
    length: 2,
    convRule: rule124
  }, {
    start: 43138,
    length: 50,
    convRule: rule14
  }, {
    start: 43188,
    length: 16,
    convRule: rule124
  }, {
    start: 43204,
    length: 2,
    convRule: rule92
  }, {
    start: 43214,
    length: 2,
    convRule: rule2
  }, {
    start: 43216,
    length: 10,
    convRule: rule8
  }, {
    start: 43232,
    length: 18,
    convRule: rule92
  }, {
    start: 43250,
    length: 6,
    convRule: rule14
  }, {
    start: 43256,
    length: 3,
    convRule: rule2
  }, {
    start: 43259,
    length: 1,
    convRule: rule14
  }, {
    start: 43260,
    length: 1,
    convRule: rule2
  }, {
    start: 43261,
    length: 2,
    convRule: rule14
  }, {
    start: 43263,
    length: 1,
    convRule: rule92
  }, {
    start: 43264,
    length: 10,
    convRule: rule8
  }, {
    start: 43274,
    length: 28,
    convRule: rule14
  }, {
    start: 43302,
    length: 8,
    convRule: rule92
  }, {
    start: 43310,
    length: 2,
    convRule: rule2
  }, {
    start: 43312,
    length: 23,
    convRule: rule14
  }, {
    start: 43335,
    length: 11,
    convRule: rule92
  }, {
    start: 43346,
    length: 2,
    convRule: rule124
  }, {
    start: 43359,
    length: 1,
    convRule: rule2
  }, {
    start: 43360,
    length: 29,
    convRule: rule14
  }, {
    start: 43392,
    length: 3,
    convRule: rule92
  }, {
    start: 43395,
    length: 1,
    convRule: rule124
  }, {
    start: 43396,
    length: 47,
    convRule: rule14
  }, {
    start: 43443,
    length: 1,
    convRule: rule92
  }, {
    start: 43444,
    length: 2,
    convRule: rule124
  }, {
    start: 43446,
    length: 4,
    convRule: rule92
  }, {
    start: 43450,
    length: 2,
    convRule: rule124
  }, {
    start: 43452,
    length: 2,
    convRule: rule92
  }, {
    start: 43454,
    length: 3,
    convRule: rule124
  }, {
    start: 43457,
    length: 13,
    convRule: rule2
  }, {
    start: 43471,
    length: 1,
    convRule: rule91
  }, {
    start: 43472,
    length: 10,
    convRule: rule8
  }, {
    start: 43486,
    length: 2,
    convRule: rule2
  }, {
    start: 43488,
    length: 5,
    convRule: rule14
  }, {
    start: 43493,
    length: 1,
    convRule: rule92
  }, {
    start: 43494,
    length: 1,
    convRule: rule91
  }, {
    start: 43495,
    length: 9,
    convRule: rule14
  }, {
    start: 43504,
    length: 10,
    convRule: rule8
  }, {
    start: 43514,
    length: 5,
    convRule: rule14
  }, {
    start: 43520,
    length: 41,
    convRule: rule14
  }, {
    start: 43561,
    length: 6,
    convRule: rule92
  }, {
    start: 43567,
    length: 2,
    convRule: rule124
  }, {
    start: 43569,
    length: 2,
    convRule: rule92
  }, {
    start: 43571,
    length: 2,
    convRule: rule124
  }, {
    start: 43573,
    length: 2,
    convRule: rule92
  }, {
    start: 43584,
    length: 3,
    convRule: rule14
  }, {
    start: 43587,
    length: 1,
    convRule: rule92
  }, {
    start: 43588,
    length: 8,
    convRule: rule14
  }, {
    start: 43596,
    length: 1,
    convRule: rule92
  }, {
    start: 43597,
    length: 1,
    convRule: rule124
  }, {
    start: 43600,
    length: 10,
    convRule: rule8
  }, {
    start: 43612,
    length: 4,
    convRule: rule2
  }, {
    start: 43616,
    length: 16,
    convRule: rule14
  }, {
    start: 43632,
    length: 1,
    convRule: rule91
  }, {
    start: 43633,
    length: 6,
    convRule: rule14
  }, {
    start: 43639,
    length: 3,
    convRule: rule13
  }, {
    start: 43642,
    length: 1,
    convRule: rule14
  }, {
    start: 43643,
    length: 1,
    convRule: rule124
  }, {
    start: 43644,
    length: 1,
    convRule: rule92
  }, {
    start: 43645,
    length: 1,
    convRule: rule124
  }, {
    start: 43646,
    length: 50,
    convRule: rule14
  }, {
    start: 43696,
    length: 1,
    convRule: rule92
  }, {
    start: 43697,
    length: 1,
    convRule: rule14
  }, {
    start: 43698,
    length: 3,
    convRule: rule92
  }, {
    start: 43701,
    length: 2,
    convRule: rule14
  }, {
    start: 43703,
    length: 2,
    convRule: rule92
  }, {
    start: 43705,
    length: 5,
    convRule: rule14
  }, {
    start: 43710,
    length: 2,
    convRule: rule92
  }, {
    start: 43712,
    length: 1,
    convRule: rule14
  }, {
    start: 43713,
    length: 1,
    convRule: rule92
  }, {
    start: 43714,
    length: 1,
    convRule: rule14
  }, {
    start: 43739,
    length: 2,
    convRule: rule14
  }, {
    start: 43741,
    length: 1,
    convRule: rule91
  }, {
    start: 43742,
    length: 2,
    convRule: rule2
  }, {
    start: 43744,
    length: 11,
    convRule: rule14
  }, {
    start: 43755,
    length: 1,
    convRule: rule124
  }, {
    start: 43756,
    length: 2,
    convRule: rule92
  }, {
    start: 43758,
    length: 2,
    convRule: rule124
  }, {
    start: 43760,
    length: 2,
    convRule: rule2
  }, {
    start: 43762,
    length: 1,
    convRule: rule14
  }, {
    start: 43763,
    length: 2,
    convRule: rule91
  }, {
    start: 43765,
    length: 1,
    convRule: rule124
  }, {
    start: 43766,
    length: 1,
    convRule: rule92
  }, {
    start: 43777,
    length: 6,
    convRule: rule14
  }, {
    start: 43785,
    length: 6,
    convRule: rule14
  }, {
    start: 43793,
    length: 6,
    convRule: rule14
  }, {
    start: 43808,
    length: 7,
    convRule: rule14
  }, {
    start: 43816,
    length: 7,
    convRule: rule14
  }, {
    start: 43824,
    length: 35,
    convRule: rule20
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43860,
    length: 7,
    convRule: rule20
  }, {
    start: 43867,
    length: 1,
    convRule: rule10
  }, {
    start: 43868,
    length: 4,
    convRule: rule91
  }, {
    start: 43872,
    length: 9,
    convRule: rule20
  }, {
    start: 43881,
    length: 1,
    convRule: rule91
  }, {
    start: 43882,
    length: 2,
    convRule: rule10
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 43968,
    length: 35,
    convRule: rule14
  }, {
    start: 44003,
    length: 2,
    convRule: rule124
  }, {
    start: 44005,
    length: 1,
    convRule: rule92
  }, {
    start: 44006,
    length: 2,
    convRule: rule124
  }, {
    start: 44008,
    length: 1,
    convRule: rule92
  }, {
    start: 44009,
    length: 2,
    convRule: rule124
  }, {
    start: 44011,
    length: 1,
    convRule: rule2
  }, {
    start: 44012,
    length: 1,
    convRule: rule124
  }, {
    start: 44013,
    length: 1,
    convRule: rule92
  }, {
    start: 44016,
    length: 10,
    convRule: rule8
  }, {
    start: 44032,
    length: 11172,
    convRule: rule14
  }, {
    start: 55216,
    length: 23,
    convRule: rule14
  }, {
    start: 55243,
    length: 49,
    convRule: rule14
  }, {
    start: 55296,
    length: 896,
    convRule: rule199
  }, {
    start: 56192,
    length: 128,
    convRule: rule199
  }, {
    start: 56320,
    length: 1024,
    convRule: rule199
  }, {
    start: 57344,
    length: 6400,
    convRule: rule200
  }, {
    start: 63744,
    length: 366,
    convRule: rule14
  }, {
    start: 64112,
    length: 106,
    convRule: rule14
  }, {
    start: 64256,
    length: 7,
    convRule: rule20
  }, {
    start: 64275,
    length: 5,
    convRule: rule20
  }, {
    start: 64285,
    length: 1,
    convRule: rule14
  }, {
    start: 64286,
    length: 1,
    convRule: rule92
  }, {
    start: 64287,
    length: 10,
    convRule: rule14
  }, {
    start: 64297,
    length: 1,
    convRule: rule6
  }, {
    start: 64298,
    length: 13,
    convRule: rule14
  }, {
    start: 64312,
    length: 5,
    convRule: rule14
  }, {
    start: 64318,
    length: 1,
    convRule: rule14
  }, {
    start: 64320,
    length: 2,
    convRule: rule14
  }, {
    start: 64323,
    length: 2,
    convRule: rule14
  }, {
    start: 64326,
    length: 108,
    convRule: rule14
  }, {
    start: 64434,
    length: 16,
    convRule: rule10
  }, {
    start: 64467,
    length: 363,
    convRule: rule14
  }, {
    start: 64830,
    length: 1,
    convRule: rule5
  }, {
    start: 64831,
    length: 1,
    convRule: rule4
  }, {
    start: 64848,
    length: 64,
    convRule: rule14
  }, {
    start: 64914,
    length: 54,
    convRule: rule14
  }, {
    start: 65008,
    length: 12,
    convRule: rule14
  }, {
    start: 65020,
    length: 1,
    convRule: rule3
  }, {
    start: 65021,
    length: 1,
    convRule: rule13
  }, {
    start: 65024,
    length: 16,
    convRule: rule92
  }, {
    start: 65040,
    length: 7,
    convRule: rule2
  }, {
    start: 65047,
    length: 1,
    convRule: rule4
  }, {
    start: 65048,
    length: 1,
    convRule: rule5
  }, {
    start: 65049,
    length: 1,
    convRule: rule2
  }, {
    start: 65056,
    length: 16,
    convRule: rule92
  }, {
    start: 65072,
    length: 1,
    convRule: rule2
  }, {
    start: 65073,
    length: 2,
    convRule: rule7
  }, {
    start: 65075,
    length: 2,
    convRule: rule11
  }, {
    start: 65077,
    length: 1,
    convRule: rule4
  }, {
    start: 65078,
    length: 1,
    convRule: rule5
  }, {
    start: 65079,
    length: 1,
    convRule: rule4
  }, {
    start: 65080,
    length: 1,
    convRule: rule5
  }, {
    start: 65081,
    length: 1,
    convRule: rule4
  }, {
    start: 65082,
    length: 1,
    convRule: rule5
  }, {
    start: 65083,
    length: 1,
    convRule: rule4
  }, {
    start: 65084,
    length: 1,
    convRule: rule5
  }, {
    start: 65085,
    length: 1,
    convRule: rule4
  }, {
    start: 65086,
    length: 1,
    convRule: rule5
  }, {
    start: 65087,
    length: 1,
    convRule: rule4
  }, {
    start: 65088,
    length: 1,
    convRule: rule5
  }, {
    start: 65089,
    length: 1,
    convRule: rule4
  }, {
    start: 65090,
    length: 1,
    convRule: rule5
  }, {
    start: 65091,
    length: 1,
    convRule: rule4
  }, {
    start: 65092,
    length: 1,
    convRule: rule5
  }, {
    start: 65093,
    length: 2,
    convRule: rule2
  }, {
    start: 65095,
    length: 1,
    convRule: rule4
  }, {
    start: 65096,
    length: 1,
    convRule: rule5
  }, {
    start: 65097,
    length: 4,
    convRule: rule2
  }, {
    start: 65101,
    length: 3,
    convRule: rule11
  }, {
    start: 65104,
    length: 3,
    convRule: rule2
  }, {
    start: 65108,
    length: 4,
    convRule: rule2
  }, {
    start: 65112,
    length: 1,
    convRule: rule7
  }, {
    start: 65113,
    length: 1,
    convRule: rule4
  }, {
    start: 65114,
    length: 1,
    convRule: rule5
  }, {
    start: 65115,
    length: 1,
    convRule: rule4
  }, {
    start: 65116,
    length: 1,
    convRule: rule5
  }, {
    start: 65117,
    length: 1,
    convRule: rule4
  }, {
    start: 65118,
    length: 1,
    convRule: rule5
  }, {
    start: 65119,
    length: 3,
    convRule: rule2
  }, {
    start: 65122,
    length: 1,
    convRule: rule6
  }, {
    start: 65123,
    length: 1,
    convRule: rule7
  }, {
    start: 65124,
    length: 3,
    convRule: rule6
  }, {
    start: 65128,
    length: 1,
    convRule: rule2
  }, {
    start: 65129,
    length: 1,
    convRule: rule3
  }, {
    start: 65130,
    length: 2,
    convRule: rule2
  }, {
    start: 65136,
    length: 5,
    convRule: rule14
  }, {
    start: 65142,
    length: 135,
    convRule: rule14
  }, {
    start: 65279,
    length: 1,
    convRule: rule16
  }, {
    start: 65281,
    length: 3,
    convRule: rule2
  }, {
    start: 65284,
    length: 1,
    convRule: rule3
  }, {
    start: 65285,
    length: 3,
    convRule: rule2
  }, {
    start: 65288,
    length: 1,
    convRule: rule4
  }, {
    start: 65289,
    length: 1,
    convRule: rule5
  }, {
    start: 65290,
    length: 1,
    convRule: rule2
  }, {
    start: 65291,
    length: 1,
    convRule: rule6
  }, {
    start: 65292,
    length: 1,
    convRule: rule2
  }, {
    start: 65293,
    length: 1,
    convRule: rule7
  }, {
    start: 65294,
    length: 2,
    convRule: rule2
  }, {
    start: 65296,
    length: 10,
    convRule: rule8
  }, {
    start: 65306,
    length: 2,
    convRule: rule2
  }, {
    start: 65308,
    length: 3,
    convRule: rule6
  }, {
    start: 65311,
    length: 2,
    convRule: rule2
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65339,
    length: 1,
    convRule: rule4
  }, {
    start: 65340,
    length: 1,
    convRule: rule2
  }, {
    start: 65341,
    length: 1,
    convRule: rule5
  }, {
    start: 65342,
    length: 1,
    convRule: rule10
  }, {
    start: 65343,
    length: 1,
    convRule: rule11
  }, {
    start: 65344,
    length: 1,
    convRule: rule10
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 65371,
    length: 1,
    convRule: rule4
  }, {
    start: 65372,
    length: 1,
    convRule: rule6
  }, {
    start: 65373,
    length: 1,
    convRule: rule5
  }, {
    start: 65374,
    length: 1,
    convRule: rule6
  }, {
    start: 65375,
    length: 1,
    convRule: rule4
  }, {
    start: 65376,
    length: 1,
    convRule: rule5
  }, {
    start: 65377,
    length: 1,
    convRule: rule2
  }, {
    start: 65378,
    length: 1,
    convRule: rule4
  }, {
    start: 65379,
    length: 1,
    convRule: rule5
  }, {
    start: 65380,
    length: 2,
    convRule: rule2
  }, {
    start: 65382,
    length: 10,
    convRule: rule14
  }, {
    start: 65392,
    length: 1,
    convRule: rule91
  }, {
    start: 65393,
    length: 45,
    convRule: rule14
  }, {
    start: 65438,
    length: 2,
    convRule: rule91
  }, {
    start: 65440,
    length: 31,
    convRule: rule14
  }, {
    start: 65474,
    length: 6,
    convRule: rule14
  }, {
    start: 65482,
    length: 6,
    convRule: rule14
  }, {
    start: 65490,
    length: 6,
    convRule: rule14
  }, {
    start: 65498,
    length: 3,
    convRule: rule14
  }, {
    start: 65504,
    length: 2,
    convRule: rule3
  }, {
    start: 65506,
    length: 1,
    convRule: rule6
  }, {
    start: 65507,
    length: 1,
    convRule: rule10
  }, {
    start: 65508,
    length: 1,
    convRule: rule13
  }, {
    start: 65509,
    length: 2,
    convRule: rule3
  }, {
    start: 65512,
    length: 1,
    convRule: rule13
  }, {
    start: 65513,
    length: 4,
    convRule: rule6
  }, {
    start: 65517,
    length: 2,
    convRule: rule13
  }, {
    start: 65529,
    length: 3,
    convRule: rule16
  }, {
    start: 65532,
    length: 2,
    convRule: rule13
  }, {
    start: 65536,
    length: 12,
    convRule: rule14
  }, {
    start: 65549,
    length: 26,
    convRule: rule14
  }, {
    start: 65576,
    length: 19,
    convRule: rule14
  }, {
    start: 65596,
    length: 2,
    convRule: rule14
  }, {
    start: 65599,
    length: 15,
    convRule: rule14
  }, {
    start: 65616,
    length: 14,
    convRule: rule14
  }, {
    start: 65664,
    length: 123,
    convRule: rule14
  }, {
    start: 65792,
    length: 3,
    convRule: rule2
  }, {
    start: 65799,
    length: 45,
    convRule: rule17
  }, {
    start: 65847,
    length: 9,
    convRule: rule13
  }, {
    start: 65856,
    length: 53,
    convRule: rule128
  }, {
    start: 65909,
    length: 4,
    convRule: rule17
  }, {
    start: 65913,
    length: 17,
    convRule: rule13
  }, {
    start: 65930,
    length: 2,
    convRule: rule17
  }, {
    start: 65932,
    length: 3,
    convRule: rule13
  }, {
    start: 65936,
    length: 13,
    convRule: rule13
  }, {
    start: 65952,
    length: 1,
    convRule: rule13
  }, {
    start: 66e3,
    length: 45,
    convRule: rule13
  }, {
    start: 66045,
    length: 1,
    convRule: rule92
  }, {
    start: 66176,
    length: 29,
    convRule: rule14
  }, {
    start: 66208,
    length: 49,
    convRule: rule14
  }, {
    start: 66272,
    length: 1,
    convRule: rule92
  }, {
    start: 66273,
    length: 27,
    convRule: rule17
  }, {
    start: 66304,
    length: 32,
    convRule: rule14
  }, {
    start: 66336,
    length: 4,
    convRule: rule17
  }, {
    start: 66349,
    length: 20,
    convRule: rule14
  }, {
    start: 66369,
    length: 1,
    convRule: rule128
  }, {
    start: 66370,
    length: 8,
    convRule: rule14
  }, {
    start: 66378,
    length: 1,
    convRule: rule128
  }, {
    start: 66384,
    length: 38,
    convRule: rule14
  }, {
    start: 66422,
    length: 5,
    convRule: rule92
  }, {
    start: 66432,
    length: 30,
    convRule: rule14
  }, {
    start: 66463,
    length: 1,
    convRule: rule2
  }, {
    start: 66464,
    length: 36,
    convRule: rule14
  }, {
    start: 66504,
    length: 8,
    convRule: rule14
  }, {
    start: 66512,
    length: 1,
    convRule: rule2
  }, {
    start: 66513,
    length: 5,
    convRule: rule128
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66640,
    length: 78,
    convRule: rule14
  }, {
    start: 66720,
    length: 10,
    convRule: rule8
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 66816,
    length: 40,
    convRule: rule14
  }, {
    start: 66864,
    length: 52,
    convRule: rule14
  }, {
    start: 66927,
    length: 1,
    convRule: rule2
  }, {
    start: 67072,
    length: 311,
    convRule: rule14
  }, {
    start: 67392,
    length: 22,
    convRule: rule14
  }, {
    start: 67424,
    length: 8,
    convRule: rule14
  }, {
    start: 67584,
    length: 6,
    convRule: rule14
  }, {
    start: 67592,
    length: 1,
    convRule: rule14
  }, {
    start: 67594,
    length: 44,
    convRule: rule14
  }, {
    start: 67639,
    length: 2,
    convRule: rule14
  }, {
    start: 67644,
    length: 1,
    convRule: rule14
  }, {
    start: 67647,
    length: 23,
    convRule: rule14
  }, {
    start: 67671,
    length: 1,
    convRule: rule2
  }, {
    start: 67672,
    length: 8,
    convRule: rule17
  }, {
    start: 67680,
    length: 23,
    convRule: rule14
  }, {
    start: 67703,
    length: 2,
    convRule: rule13
  }, {
    start: 67705,
    length: 7,
    convRule: rule17
  }, {
    start: 67712,
    length: 31,
    convRule: rule14
  }, {
    start: 67751,
    length: 9,
    convRule: rule17
  }, {
    start: 67808,
    length: 19,
    convRule: rule14
  }, {
    start: 67828,
    length: 2,
    convRule: rule14
  }, {
    start: 67835,
    length: 5,
    convRule: rule17
  }, {
    start: 67840,
    length: 22,
    convRule: rule14
  }, {
    start: 67862,
    length: 6,
    convRule: rule17
  }, {
    start: 67871,
    length: 1,
    convRule: rule2
  }, {
    start: 67872,
    length: 26,
    convRule: rule14
  }, {
    start: 67903,
    length: 1,
    convRule: rule2
  }, {
    start: 67968,
    length: 56,
    convRule: rule14
  }, {
    start: 68028,
    length: 2,
    convRule: rule17
  }, {
    start: 68030,
    length: 2,
    convRule: rule14
  }, {
    start: 68032,
    length: 16,
    convRule: rule17
  }, {
    start: 68050,
    length: 46,
    convRule: rule17
  }, {
    start: 68096,
    length: 1,
    convRule: rule14
  }, {
    start: 68097,
    length: 3,
    convRule: rule92
  }, {
    start: 68101,
    length: 2,
    convRule: rule92
  }, {
    start: 68108,
    length: 4,
    convRule: rule92
  }, {
    start: 68112,
    length: 4,
    convRule: rule14
  }, {
    start: 68117,
    length: 3,
    convRule: rule14
  }, {
    start: 68121,
    length: 29,
    convRule: rule14
  }, {
    start: 68152,
    length: 3,
    convRule: rule92
  }, {
    start: 68159,
    length: 1,
    convRule: rule92
  }, {
    start: 68160,
    length: 9,
    convRule: rule17
  }, {
    start: 68176,
    length: 9,
    convRule: rule2
  }, {
    start: 68192,
    length: 29,
    convRule: rule14
  }, {
    start: 68221,
    length: 2,
    convRule: rule17
  }, {
    start: 68223,
    length: 1,
    convRule: rule2
  }, {
    start: 68224,
    length: 29,
    convRule: rule14
  }, {
    start: 68253,
    length: 3,
    convRule: rule17
  }, {
    start: 68288,
    length: 8,
    convRule: rule14
  }, {
    start: 68296,
    length: 1,
    convRule: rule13
  }, {
    start: 68297,
    length: 28,
    convRule: rule14
  }, {
    start: 68325,
    length: 2,
    convRule: rule92
  }, {
    start: 68331,
    length: 5,
    convRule: rule17
  }, {
    start: 68336,
    length: 7,
    convRule: rule2
  }, {
    start: 68352,
    length: 54,
    convRule: rule14
  }, {
    start: 68409,
    length: 7,
    convRule: rule2
  }, {
    start: 68416,
    length: 22,
    convRule: rule14
  }, {
    start: 68440,
    length: 8,
    convRule: rule17
  }, {
    start: 68448,
    length: 19,
    convRule: rule14
  }, {
    start: 68472,
    length: 8,
    convRule: rule17
  }, {
    start: 68480,
    length: 18,
    convRule: rule14
  }, {
    start: 68505,
    length: 4,
    convRule: rule2
  }, {
    start: 68521,
    length: 7,
    convRule: rule17
  }, {
    start: 68608,
    length: 73,
    convRule: rule14
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 68858,
    length: 6,
    convRule: rule17
  }, {
    start: 68864,
    length: 36,
    convRule: rule14
  }, {
    start: 68900,
    length: 4,
    convRule: rule92
  }, {
    start: 68912,
    length: 10,
    convRule: rule8
  }, {
    start: 69216,
    length: 31,
    convRule: rule17
  }, {
    start: 69248,
    length: 42,
    convRule: rule14
  }, {
    start: 69291,
    length: 2,
    convRule: rule92
  }, {
    start: 69293,
    length: 1,
    convRule: rule7
  }, {
    start: 69296,
    length: 2,
    convRule: rule14
  }, {
    start: 69376,
    length: 29,
    convRule: rule14
  }, {
    start: 69405,
    length: 10,
    convRule: rule17
  }, {
    start: 69415,
    length: 1,
    convRule: rule14
  }, {
    start: 69424,
    length: 22,
    convRule: rule14
  }, {
    start: 69446,
    length: 11,
    convRule: rule92
  }, {
    start: 69457,
    length: 4,
    convRule: rule17
  }, {
    start: 69461,
    length: 5,
    convRule: rule2
  }, {
    start: 69552,
    length: 21,
    convRule: rule14
  }, {
    start: 69573,
    length: 7,
    convRule: rule17
  }, {
    start: 69600,
    length: 23,
    convRule: rule14
  }, {
    start: 69632,
    length: 1,
    convRule: rule124
  }, {
    start: 69633,
    length: 1,
    convRule: rule92
  }, {
    start: 69634,
    length: 1,
    convRule: rule124
  }, {
    start: 69635,
    length: 53,
    convRule: rule14
  }, {
    start: 69688,
    length: 15,
    convRule: rule92
  }, {
    start: 69703,
    length: 7,
    convRule: rule2
  }, {
    start: 69714,
    length: 20,
    convRule: rule17
  }, {
    start: 69734,
    length: 10,
    convRule: rule8
  }, {
    start: 69759,
    length: 3,
    convRule: rule92
  }, {
    start: 69762,
    length: 1,
    convRule: rule124
  }, {
    start: 69763,
    length: 45,
    convRule: rule14
  }, {
    start: 69808,
    length: 3,
    convRule: rule124
  }, {
    start: 69811,
    length: 4,
    convRule: rule92
  }, {
    start: 69815,
    length: 2,
    convRule: rule124
  }, {
    start: 69817,
    length: 2,
    convRule: rule92
  }, {
    start: 69819,
    length: 2,
    convRule: rule2
  }, {
    start: 69821,
    length: 1,
    convRule: rule16
  }, {
    start: 69822,
    length: 4,
    convRule: rule2
  }, {
    start: 69837,
    length: 1,
    convRule: rule16
  }, {
    start: 69840,
    length: 25,
    convRule: rule14
  }, {
    start: 69872,
    length: 10,
    convRule: rule8
  }, {
    start: 69888,
    length: 3,
    convRule: rule92
  }, {
    start: 69891,
    length: 36,
    convRule: rule14
  }, {
    start: 69927,
    length: 5,
    convRule: rule92
  }, {
    start: 69932,
    length: 1,
    convRule: rule124
  }, {
    start: 69933,
    length: 8,
    convRule: rule92
  }, {
    start: 69942,
    length: 10,
    convRule: rule8
  }, {
    start: 69952,
    length: 4,
    convRule: rule2
  }, {
    start: 69956,
    length: 1,
    convRule: rule14
  }, {
    start: 69957,
    length: 2,
    convRule: rule124
  }, {
    start: 69959,
    length: 1,
    convRule: rule14
  }, {
    start: 69968,
    length: 35,
    convRule: rule14
  }, {
    start: 70003,
    length: 1,
    convRule: rule92
  }, {
    start: 70004,
    length: 2,
    convRule: rule2
  }, {
    start: 70006,
    length: 1,
    convRule: rule14
  }, {
    start: 70016,
    length: 2,
    convRule: rule92
  }, {
    start: 70018,
    length: 1,
    convRule: rule124
  }, {
    start: 70019,
    length: 48,
    convRule: rule14
  }, {
    start: 70067,
    length: 3,
    convRule: rule124
  }, {
    start: 70070,
    length: 9,
    convRule: rule92
  }, {
    start: 70079,
    length: 2,
    convRule: rule124
  }, {
    start: 70081,
    length: 4,
    convRule: rule14
  }, {
    start: 70085,
    length: 4,
    convRule: rule2
  }, {
    start: 70089,
    length: 4,
    convRule: rule92
  }, {
    start: 70093,
    length: 1,
    convRule: rule2
  }, {
    start: 70094,
    length: 1,
    convRule: rule124
  }, {
    start: 70095,
    length: 1,
    convRule: rule92
  }, {
    start: 70096,
    length: 10,
    convRule: rule8
  }, {
    start: 70106,
    length: 1,
    convRule: rule14
  }, {
    start: 70107,
    length: 1,
    convRule: rule2
  }, {
    start: 70108,
    length: 1,
    convRule: rule14
  }, {
    start: 70109,
    length: 3,
    convRule: rule2
  }, {
    start: 70113,
    length: 20,
    convRule: rule17
  }, {
    start: 70144,
    length: 18,
    convRule: rule14
  }, {
    start: 70163,
    length: 25,
    convRule: rule14
  }, {
    start: 70188,
    length: 3,
    convRule: rule124
  }, {
    start: 70191,
    length: 3,
    convRule: rule92
  }, {
    start: 70194,
    length: 2,
    convRule: rule124
  }, {
    start: 70196,
    length: 1,
    convRule: rule92
  }, {
    start: 70197,
    length: 1,
    convRule: rule124
  }, {
    start: 70198,
    length: 2,
    convRule: rule92
  }, {
    start: 70200,
    length: 6,
    convRule: rule2
  }, {
    start: 70206,
    length: 1,
    convRule: rule92
  }, {
    start: 70272,
    length: 7,
    convRule: rule14
  }, {
    start: 70280,
    length: 1,
    convRule: rule14
  }, {
    start: 70282,
    length: 4,
    convRule: rule14
  }, {
    start: 70287,
    length: 15,
    convRule: rule14
  }, {
    start: 70303,
    length: 10,
    convRule: rule14
  }, {
    start: 70313,
    length: 1,
    convRule: rule2
  }, {
    start: 70320,
    length: 47,
    convRule: rule14
  }, {
    start: 70367,
    length: 1,
    convRule: rule92
  }, {
    start: 70368,
    length: 3,
    convRule: rule124
  }, {
    start: 70371,
    length: 8,
    convRule: rule92
  }, {
    start: 70384,
    length: 10,
    convRule: rule8
  }, {
    start: 70400,
    length: 2,
    convRule: rule92
  }, {
    start: 70402,
    length: 2,
    convRule: rule124
  }, {
    start: 70405,
    length: 8,
    convRule: rule14
  }, {
    start: 70415,
    length: 2,
    convRule: rule14
  }, {
    start: 70419,
    length: 22,
    convRule: rule14
  }, {
    start: 70442,
    length: 7,
    convRule: rule14
  }, {
    start: 70450,
    length: 2,
    convRule: rule14
  }, {
    start: 70453,
    length: 5,
    convRule: rule14
  }, {
    start: 70459,
    length: 2,
    convRule: rule92
  }, {
    start: 70461,
    length: 1,
    convRule: rule14
  }, {
    start: 70462,
    length: 2,
    convRule: rule124
  }, {
    start: 70464,
    length: 1,
    convRule: rule92
  }, {
    start: 70465,
    length: 4,
    convRule: rule124
  }, {
    start: 70471,
    length: 2,
    convRule: rule124
  }, {
    start: 70475,
    length: 3,
    convRule: rule124
  }, {
    start: 70480,
    length: 1,
    convRule: rule14
  }, {
    start: 70487,
    length: 1,
    convRule: rule124
  }, {
    start: 70493,
    length: 5,
    convRule: rule14
  }, {
    start: 70498,
    length: 2,
    convRule: rule124
  }, {
    start: 70502,
    length: 7,
    convRule: rule92
  }, {
    start: 70512,
    length: 5,
    convRule: rule92
  }, {
    start: 70656,
    length: 53,
    convRule: rule14
  }, {
    start: 70709,
    length: 3,
    convRule: rule124
  }, {
    start: 70712,
    length: 8,
    convRule: rule92
  }, {
    start: 70720,
    length: 2,
    convRule: rule124
  }, {
    start: 70722,
    length: 3,
    convRule: rule92
  }, {
    start: 70725,
    length: 1,
    convRule: rule124
  }, {
    start: 70726,
    length: 1,
    convRule: rule92
  }, {
    start: 70727,
    length: 4,
    convRule: rule14
  }, {
    start: 70731,
    length: 5,
    convRule: rule2
  }, {
    start: 70736,
    length: 10,
    convRule: rule8
  }, {
    start: 70746,
    length: 2,
    convRule: rule2
  }, {
    start: 70749,
    length: 1,
    convRule: rule2
  }, {
    start: 70750,
    length: 1,
    convRule: rule92
  }, {
    start: 70751,
    length: 3,
    convRule: rule14
  }, {
    start: 70784,
    length: 48,
    convRule: rule14
  }, {
    start: 70832,
    length: 3,
    convRule: rule124
  }, {
    start: 70835,
    length: 6,
    convRule: rule92
  }, {
    start: 70841,
    length: 1,
    convRule: rule124
  }, {
    start: 70842,
    length: 1,
    convRule: rule92
  }, {
    start: 70843,
    length: 4,
    convRule: rule124
  }, {
    start: 70847,
    length: 2,
    convRule: rule92
  }, {
    start: 70849,
    length: 1,
    convRule: rule124
  }, {
    start: 70850,
    length: 2,
    convRule: rule92
  }, {
    start: 70852,
    length: 2,
    convRule: rule14
  }, {
    start: 70854,
    length: 1,
    convRule: rule2
  }, {
    start: 70855,
    length: 1,
    convRule: rule14
  }, {
    start: 70864,
    length: 10,
    convRule: rule8
  }, {
    start: 71040,
    length: 47,
    convRule: rule14
  }, {
    start: 71087,
    length: 3,
    convRule: rule124
  }, {
    start: 71090,
    length: 4,
    convRule: rule92
  }, {
    start: 71096,
    length: 4,
    convRule: rule124
  }, {
    start: 71100,
    length: 2,
    convRule: rule92
  }, {
    start: 71102,
    length: 1,
    convRule: rule124
  }, {
    start: 71103,
    length: 2,
    convRule: rule92
  }, {
    start: 71105,
    length: 23,
    convRule: rule2
  }, {
    start: 71128,
    length: 4,
    convRule: rule14
  }, {
    start: 71132,
    length: 2,
    convRule: rule92
  }, {
    start: 71168,
    length: 48,
    convRule: rule14
  }, {
    start: 71216,
    length: 3,
    convRule: rule124
  }, {
    start: 71219,
    length: 8,
    convRule: rule92
  }, {
    start: 71227,
    length: 2,
    convRule: rule124
  }, {
    start: 71229,
    length: 1,
    convRule: rule92
  }, {
    start: 71230,
    length: 1,
    convRule: rule124
  }, {
    start: 71231,
    length: 2,
    convRule: rule92
  }, {
    start: 71233,
    length: 3,
    convRule: rule2
  }, {
    start: 71236,
    length: 1,
    convRule: rule14
  }, {
    start: 71248,
    length: 10,
    convRule: rule8
  }, {
    start: 71264,
    length: 13,
    convRule: rule2
  }, {
    start: 71296,
    length: 43,
    convRule: rule14
  }, {
    start: 71339,
    length: 1,
    convRule: rule92
  }, {
    start: 71340,
    length: 1,
    convRule: rule124
  }, {
    start: 71341,
    length: 1,
    convRule: rule92
  }, {
    start: 71342,
    length: 2,
    convRule: rule124
  }, {
    start: 71344,
    length: 6,
    convRule: rule92
  }, {
    start: 71350,
    length: 1,
    convRule: rule124
  }, {
    start: 71351,
    length: 1,
    convRule: rule92
  }, {
    start: 71352,
    length: 1,
    convRule: rule14
  }, {
    start: 71360,
    length: 10,
    convRule: rule8
  }, {
    start: 71424,
    length: 27,
    convRule: rule14
  }, {
    start: 71453,
    length: 3,
    convRule: rule92
  }, {
    start: 71456,
    length: 2,
    convRule: rule124
  }, {
    start: 71458,
    length: 4,
    convRule: rule92
  }, {
    start: 71462,
    length: 1,
    convRule: rule124
  }, {
    start: 71463,
    length: 5,
    convRule: rule92
  }, {
    start: 71472,
    length: 10,
    convRule: rule8
  }, {
    start: 71482,
    length: 2,
    convRule: rule17
  }, {
    start: 71484,
    length: 3,
    convRule: rule2
  }, {
    start: 71487,
    length: 1,
    convRule: rule13
  }, {
    start: 71680,
    length: 44,
    convRule: rule14
  }, {
    start: 71724,
    length: 3,
    convRule: rule124
  }, {
    start: 71727,
    length: 9,
    convRule: rule92
  }, {
    start: 71736,
    length: 1,
    convRule: rule124
  }, {
    start: 71737,
    length: 2,
    convRule: rule92
  }, {
    start: 71739,
    length: 1,
    convRule: rule2
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 71904,
    length: 10,
    convRule: rule8
  }, {
    start: 71914,
    length: 9,
    convRule: rule17
  }, {
    start: 71935,
    length: 8,
    convRule: rule14
  }, {
    start: 71945,
    length: 1,
    convRule: rule14
  }, {
    start: 71948,
    length: 8,
    convRule: rule14
  }, {
    start: 71957,
    length: 2,
    convRule: rule14
  }, {
    start: 71960,
    length: 24,
    convRule: rule14
  }, {
    start: 71984,
    length: 6,
    convRule: rule124
  }, {
    start: 71991,
    length: 2,
    convRule: rule124
  }, {
    start: 71995,
    length: 2,
    convRule: rule92
  }, {
    start: 71997,
    length: 1,
    convRule: rule124
  }, {
    start: 71998,
    length: 1,
    convRule: rule92
  }, {
    start: 71999,
    length: 1,
    convRule: rule14
  }, {
    start: 72e3,
    length: 1,
    convRule: rule124
  }, {
    start: 72001,
    length: 1,
    convRule: rule14
  }, {
    start: 72002,
    length: 1,
    convRule: rule124
  }, {
    start: 72003,
    length: 1,
    convRule: rule92
  }, {
    start: 72004,
    length: 3,
    convRule: rule2
  }, {
    start: 72016,
    length: 10,
    convRule: rule8
  }, {
    start: 72096,
    length: 8,
    convRule: rule14
  }, {
    start: 72106,
    length: 39,
    convRule: rule14
  }, {
    start: 72145,
    length: 3,
    convRule: rule124
  }, {
    start: 72148,
    length: 4,
    convRule: rule92
  }, {
    start: 72154,
    length: 2,
    convRule: rule92
  }, {
    start: 72156,
    length: 4,
    convRule: rule124
  }, {
    start: 72160,
    length: 1,
    convRule: rule92
  }, {
    start: 72161,
    length: 1,
    convRule: rule14
  }, {
    start: 72162,
    length: 1,
    convRule: rule2
  }, {
    start: 72163,
    length: 1,
    convRule: rule14
  }, {
    start: 72164,
    length: 1,
    convRule: rule124
  }, {
    start: 72192,
    length: 1,
    convRule: rule14
  }, {
    start: 72193,
    length: 10,
    convRule: rule92
  }, {
    start: 72203,
    length: 40,
    convRule: rule14
  }, {
    start: 72243,
    length: 6,
    convRule: rule92
  }, {
    start: 72249,
    length: 1,
    convRule: rule124
  }, {
    start: 72250,
    length: 1,
    convRule: rule14
  }, {
    start: 72251,
    length: 4,
    convRule: rule92
  }, {
    start: 72255,
    length: 8,
    convRule: rule2
  }, {
    start: 72263,
    length: 1,
    convRule: rule92
  }, {
    start: 72272,
    length: 1,
    convRule: rule14
  }, {
    start: 72273,
    length: 6,
    convRule: rule92
  }, {
    start: 72279,
    length: 2,
    convRule: rule124
  }, {
    start: 72281,
    length: 3,
    convRule: rule92
  }, {
    start: 72284,
    length: 46,
    convRule: rule14
  }, {
    start: 72330,
    length: 13,
    convRule: rule92
  }, {
    start: 72343,
    length: 1,
    convRule: rule124
  }, {
    start: 72344,
    length: 2,
    convRule: rule92
  }, {
    start: 72346,
    length: 3,
    convRule: rule2
  }, {
    start: 72349,
    length: 1,
    convRule: rule14
  }, {
    start: 72350,
    length: 5,
    convRule: rule2
  }, {
    start: 72384,
    length: 57,
    convRule: rule14
  }, {
    start: 72704,
    length: 9,
    convRule: rule14
  }, {
    start: 72714,
    length: 37,
    convRule: rule14
  }, {
    start: 72751,
    length: 1,
    convRule: rule124
  }, {
    start: 72752,
    length: 7,
    convRule: rule92
  }, {
    start: 72760,
    length: 6,
    convRule: rule92
  }, {
    start: 72766,
    length: 1,
    convRule: rule124
  }, {
    start: 72767,
    length: 1,
    convRule: rule92
  }, {
    start: 72768,
    length: 1,
    convRule: rule14
  }, {
    start: 72769,
    length: 5,
    convRule: rule2
  }, {
    start: 72784,
    length: 10,
    convRule: rule8
  }, {
    start: 72794,
    length: 19,
    convRule: rule17
  }, {
    start: 72816,
    length: 2,
    convRule: rule2
  }, {
    start: 72818,
    length: 30,
    convRule: rule14
  }, {
    start: 72850,
    length: 22,
    convRule: rule92
  }, {
    start: 72873,
    length: 1,
    convRule: rule124
  }, {
    start: 72874,
    length: 7,
    convRule: rule92
  }, {
    start: 72881,
    length: 1,
    convRule: rule124
  }, {
    start: 72882,
    length: 2,
    convRule: rule92
  }, {
    start: 72884,
    length: 1,
    convRule: rule124
  }, {
    start: 72885,
    length: 2,
    convRule: rule92
  }, {
    start: 72960,
    length: 7,
    convRule: rule14
  }, {
    start: 72968,
    length: 2,
    convRule: rule14
  }, {
    start: 72971,
    length: 38,
    convRule: rule14
  }, {
    start: 73009,
    length: 6,
    convRule: rule92
  }, {
    start: 73018,
    length: 1,
    convRule: rule92
  }, {
    start: 73020,
    length: 2,
    convRule: rule92
  }, {
    start: 73023,
    length: 7,
    convRule: rule92
  }, {
    start: 73030,
    length: 1,
    convRule: rule14
  }, {
    start: 73031,
    length: 1,
    convRule: rule92
  }, {
    start: 73040,
    length: 10,
    convRule: rule8
  }, {
    start: 73056,
    length: 6,
    convRule: rule14
  }, {
    start: 73063,
    length: 2,
    convRule: rule14
  }, {
    start: 73066,
    length: 32,
    convRule: rule14
  }, {
    start: 73098,
    length: 5,
    convRule: rule124
  }, {
    start: 73104,
    length: 2,
    convRule: rule92
  }, {
    start: 73107,
    length: 2,
    convRule: rule124
  }, {
    start: 73109,
    length: 1,
    convRule: rule92
  }, {
    start: 73110,
    length: 1,
    convRule: rule124
  }, {
    start: 73111,
    length: 1,
    convRule: rule92
  }, {
    start: 73112,
    length: 1,
    convRule: rule14
  }, {
    start: 73120,
    length: 10,
    convRule: rule8
  }, {
    start: 73440,
    length: 19,
    convRule: rule14
  }, {
    start: 73459,
    length: 2,
    convRule: rule92
  }, {
    start: 73461,
    length: 2,
    convRule: rule124
  }, {
    start: 73463,
    length: 2,
    convRule: rule2
  }, {
    start: 73648,
    length: 1,
    convRule: rule14
  }, {
    start: 73664,
    length: 21,
    convRule: rule17
  }, {
    start: 73685,
    length: 8,
    convRule: rule13
  }, {
    start: 73693,
    length: 4,
    convRule: rule3
  }, {
    start: 73697,
    length: 17,
    convRule: rule13
  }, {
    start: 73727,
    length: 1,
    convRule: rule2
  }, {
    start: 73728,
    length: 922,
    convRule: rule14
  }, {
    start: 74752,
    length: 111,
    convRule: rule128
  }, {
    start: 74864,
    length: 5,
    convRule: rule2
  }, {
    start: 74880,
    length: 196,
    convRule: rule14
  }, {
    start: 77824,
    length: 1071,
    convRule: rule14
  }, {
    start: 78896,
    length: 9,
    convRule: rule16
  }, {
    start: 82944,
    length: 583,
    convRule: rule14
  }, {
    start: 92160,
    length: 569,
    convRule: rule14
  }, {
    start: 92736,
    length: 31,
    convRule: rule14
  }, {
    start: 92768,
    length: 10,
    convRule: rule8
  }, {
    start: 92782,
    length: 2,
    convRule: rule2
  }, {
    start: 92880,
    length: 30,
    convRule: rule14
  }, {
    start: 92912,
    length: 5,
    convRule: rule92
  }, {
    start: 92917,
    length: 1,
    convRule: rule2
  }, {
    start: 92928,
    length: 48,
    convRule: rule14
  }, {
    start: 92976,
    length: 7,
    convRule: rule92
  }, {
    start: 92983,
    length: 5,
    convRule: rule2
  }, {
    start: 92988,
    length: 4,
    convRule: rule13
  }, {
    start: 92992,
    length: 4,
    convRule: rule91
  }, {
    start: 92996,
    length: 1,
    convRule: rule2
  }, {
    start: 92997,
    length: 1,
    convRule: rule13
  }, {
    start: 93008,
    length: 10,
    convRule: rule8
  }, {
    start: 93019,
    length: 7,
    convRule: rule17
  }, {
    start: 93027,
    length: 21,
    convRule: rule14
  }, {
    start: 93053,
    length: 19,
    convRule: rule14
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 93824,
    length: 23,
    convRule: rule17
  }, {
    start: 93847,
    length: 4,
    convRule: rule2
  }, {
    start: 93952,
    length: 75,
    convRule: rule14
  }, {
    start: 94031,
    length: 1,
    convRule: rule92
  }, {
    start: 94032,
    length: 1,
    convRule: rule14
  }, {
    start: 94033,
    length: 55,
    convRule: rule124
  }, {
    start: 94095,
    length: 4,
    convRule: rule92
  }, {
    start: 94099,
    length: 13,
    convRule: rule91
  }, {
    start: 94176,
    length: 2,
    convRule: rule91
  }, {
    start: 94178,
    length: 1,
    convRule: rule2
  }, {
    start: 94179,
    length: 1,
    convRule: rule91
  }, {
    start: 94180,
    length: 1,
    convRule: rule92
  }, {
    start: 94192,
    length: 2,
    convRule: rule124
  }, {
    start: 94208,
    length: 6136,
    convRule: rule14
  }, {
    start: 100352,
    length: 1238,
    convRule: rule14
  }, {
    start: 101632,
    length: 9,
    convRule: rule14
  }, {
    start: 110592,
    length: 287,
    convRule: rule14
  }, {
    start: 110928,
    length: 3,
    convRule: rule14
  }, {
    start: 110948,
    length: 4,
    convRule: rule14
  }, {
    start: 110960,
    length: 396,
    convRule: rule14
  }, {
    start: 113664,
    length: 107,
    convRule: rule14
  }, {
    start: 113776,
    length: 13,
    convRule: rule14
  }, {
    start: 113792,
    length: 9,
    convRule: rule14
  }, {
    start: 113808,
    length: 10,
    convRule: rule14
  }, {
    start: 113820,
    length: 1,
    convRule: rule13
  }, {
    start: 113821,
    length: 2,
    convRule: rule92
  }, {
    start: 113823,
    length: 1,
    convRule: rule2
  }, {
    start: 113824,
    length: 4,
    convRule: rule16
  }, {
    start: 118784,
    length: 246,
    convRule: rule13
  }, {
    start: 119040,
    length: 39,
    convRule: rule13
  }, {
    start: 119081,
    length: 60,
    convRule: rule13
  }, {
    start: 119141,
    length: 2,
    convRule: rule124
  }, {
    start: 119143,
    length: 3,
    convRule: rule92
  }, {
    start: 119146,
    length: 3,
    convRule: rule13
  }, {
    start: 119149,
    length: 6,
    convRule: rule124
  }, {
    start: 119155,
    length: 8,
    convRule: rule16
  }, {
    start: 119163,
    length: 8,
    convRule: rule92
  }, {
    start: 119171,
    length: 2,
    convRule: rule13
  }, {
    start: 119173,
    length: 7,
    convRule: rule92
  }, {
    start: 119180,
    length: 30,
    convRule: rule13
  }, {
    start: 119210,
    length: 4,
    convRule: rule92
  }, {
    start: 119214,
    length: 59,
    convRule: rule13
  }, {
    start: 119296,
    length: 66,
    convRule: rule13
  }, {
    start: 119362,
    length: 3,
    convRule: rule92
  }, {
    start: 119365,
    length: 1,
    convRule: rule13
  }, {
    start: 119520,
    length: 20,
    convRule: rule17
  }, {
    start: 119552,
    length: 87,
    convRule: rule13
  }, {
    start: 119648,
    length: 25,
    convRule: rule17
  }, {
    start: 119808,
    length: 26,
    convRule: rule107
  }, {
    start: 119834,
    length: 26,
    convRule: rule20
  }, {
    start: 119860,
    length: 26,
    convRule: rule107
  }, {
    start: 119886,
    length: 7,
    convRule: rule20
  }, {
    start: 119894,
    length: 18,
    convRule: rule20
  }, {
    start: 119912,
    length: 26,
    convRule: rule107
  }, {
    start: 119938,
    length: 26,
    convRule: rule20
  }, {
    start: 119964,
    length: 1,
    convRule: rule107
  }, {
    start: 119966,
    length: 2,
    convRule: rule107
  }, {
    start: 119970,
    length: 1,
    convRule: rule107
  }, {
    start: 119973,
    length: 2,
    convRule: rule107
  }, {
    start: 119977,
    length: 4,
    convRule: rule107
  }, {
    start: 119982,
    length: 8,
    convRule: rule107
  }, {
    start: 119990,
    length: 4,
    convRule: rule20
  }, {
    start: 119995,
    length: 1,
    convRule: rule20
  }, {
    start: 119997,
    length: 7,
    convRule: rule20
  }, {
    start: 120005,
    length: 11,
    convRule: rule20
  }, {
    start: 120016,
    length: 26,
    convRule: rule107
  }, {
    start: 120042,
    length: 26,
    convRule: rule20
  }, {
    start: 120068,
    length: 2,
    convRule: rule107
  }, {
    start: 120071,
    length: 4,
    convRule: rule107
  }, {
    start: 120077,
    length: 8,
    convRule: rule107
  }, {
    start: 120086,
    length: 7,
    convRule: rule107
  }, {
    start: 120094,
    length: 26,
    convRule: rule20
  }, {
    start: 120120,
    length: 2,
    convRule: rule107
  }, {
    start: 120123,
    length: 4,
    convRule: rule107
  }, {
    start: 120128,
    length: 5,
    convRule: rule107
  }, {
    start: 120134,
    length: 1,
    convRule: rule107
  }, {
    start: 120138,
    length: 7,
    convRule: rule107
  }, {
    start: 120146,
    length: 26,
    convRule: rule20
  }, {
    start: 120172,
    length: 26,
    convRule: rule107
  }, {
    start: 120198,
    length: 26,
    convRule: rule20
  }, {
    start: 120224,
    length: 26,
    convRule: rule107
  }, {
    start: 120250,
    length: 26,
    convRule: rule20
  }, {
    start: 120276,
    length: 26,
    convRule: rule107
  }, {
    start: 120302,
    length: 26,
    convRule: rule20
  }, {
    start: 120328,
    length: 26,
    convRule: rule107
  }, {
    start: 120354,
    length: 26,
    convRule: rule20
  }, {
    start: 120380,
    length: 26,
    convRule: rule107
  }, {
    start: 120406,
    length: 26,
    convRule: rule20
  }, {
    start: 120432,
    length: 26,
    convRule: rule107
  }, {
    start: 120458,
    length: 28,
    convRule: rule20
  }, {
    start: 120488,
    length: 25,
    convRule: rule107
  }, {
    start: 120513,
    length: 1,
    convRule: rule6
  }, {
    start: 120514,
    length: 25,
    convRule: rule20
  }, {
    start: 120539,
    length: 1,
    convRule: rule6
  }, {
    start: 120540,
    length: 6,
    convRule: rule20
  }, {
    start: 120546,
    length: 25,
    convRule: rule107
  }, {
    start: 120571,
    length: 1,
    convRule: rule6
  }, {
    start: 120572,
    length: 25,
    convRule: rule20
  }, {
    start: 120597,
    length: 1,
    convRule: rule6
  }, {
    start: 120598,
    length: 6,
    convRule: rule20
  }, {
    start: 120604,
    length: 25,
    convRule: rule107
  }, {
    start: 120629,
    length: 1,
    convRule: rule6
  }, {
    start: 120630,
    length: 25,
    convRule: rule20
  }, {
    start: 120655,
    length: 1,
    convRule: rule6
  }, {
    start: 120656,
    length: 6,
    convRule: rule20
  }, {
    start: 120662,
    length: 25,
    convRule: rule107
  }, {
    start: 120687,
    length: 1,
    convRule: rule6
  }, {
    start: 120688,
    length: 25,
    convRule: rule20
  }, {
    start: 120713,
    length: 1,
    convRule: rule6
  }, {
    start: 120714,
    length: 6,
    convRule: rule20
  }, {
    start: 120720,
    length: 25,
    convRule: rule107
  }, {
    start: 120745,
    length: 1,
    convRule: rule6
  }, {
    start: 120746,
    length: 25,
    convRule: rule20
  }, {
    start: 120771,
    length: 1,
    convRule: rule6
  }, {
    start: 120772,
    length: 6,
    convRule: rule20
  }, {
    start: 120778,
    length: 1,
    convRule: rule107
  }, {
    start: 120779,
    length: 1,
    convRule: rule20
  }, {
    start: 120782,
    length: 50,
    convRule: rule8
  }, {
    start: 120832,
    length: 512,
    convRule: rule13
  }, {
    start: 121344,
    length: 55,
    convRule: rule92
  }, {
    start: 121399,
    length: 4,
    convRule: rule13
  }, {
    start: 121403,
    length: 50,
    convRule: rule92
  }, {
    start: 121453,
    length: 8,
    convRule: rule13
  }, {
    start: 121461,
    length: 1,
    convRule: rule92
  }, {
    start: 121462,
    length: 14,
    convRule: rule13
  }, {
    start: 121476,
    length: 1,
    convRule: rule92
  }, {
    start: 121477,
    length: 2,
    convRule: rule13
  }, {
    start: 121479,
    length: 5,
    convRule: rule2
  }, {
    start: 121499,
    length: 5,
    convRule: rule92
  }, {
    start: 121505,
    length: 15,
    convRule: rule92
  }, {
    start: 122880,
    length: 7,
    convRule: rule92
  }, {
    start: 122888,
    length: 17,
    convRule: rule92
  }, {
    start: 122907,
    length: 7,
    convRule: rule92
  }, {
    start: 122915,
    length: 2,
    convRule: rule92
  }, {
    start: 122918,
    length: 5,
    convRule: rule92
  }, {
    start: 123136,
    length: 45,
    convRule: rule14
  }, {
    start: 123184,
    length: 7,
    convRule: rule92
  }, {
    start: 123191,
    length: 7,
    convRule: rule91
  }, {
    start: 123200,
    length: 10,
    convRule: rule8
  }, {
    start: 123214,
    length: 1,
    convRule: rule14
  }, {
    start: 123215,
    length: 1,
    convRule: rule13
  }, {
    start: 123584,
    length: 44,
    convRule: rule14
  }, {
    start: 123628,
    length: 4,
    convRule: rule92
  }, {
    start: 123632,
    length: 10,
    convRule: rule8
  }, {
    start: 123647,
    length: 1,
    convRule: rule3
  }, {
    start: 124928,
    length: 197,
    convRule: rule14
  }, {
    start: 125127,
    length: 9,
    convRule: rule17
  }, {
    start: 125136,
    length: 7,
    convRule: rule92
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }, {
    start: 125252,
    length: 7,
    convRule: rule92
  }, {
    start: 125259,
    length: 1,
    convRule: rule91
  }, {
    start: 125264,
    length: 10,
    convRule: rule8
  }, {
    start: 125278,
    length: 2,
    convRule: rule2
  }, {
    start: 126065,
    length: 59,
    convRule: rule17
  }, {
    start: 126124,
    length: 1,
    convRule: rule13
  }, {
    start: 126125,
    length: 3,
    convRule: rule17
  }, {
    start: 126128,
    length: 1,
    convRule: rule3
  }, {
    start: 126129,
    length: 4,
    convRule: rule17
  }, {
    start: 126209,
    length: 45,
    convRule: rule17
  }, {
    start: 126254,
    length: 1,
    convRule: rule13
  }, {
    start: 126255,
    length: 15,
    convRule: rule17
  }, {
    start: 126464,
    length: 4,
    convRule: rule14
  }, {
    start: 126469,
    length: 27,
    convRule: rule14
  }, {
    start: 126497,
    length: 2,
    convRule: rule14
  }, {
    start: 126500,
    length: 1,
    convRule: rule14
  }, {
    start: 126503,
    length: 1,
    convRule: rule14
  }, {
    start: 126505,
    length: 10,
    convRule: rule14
  }, {
    start: 126516,
    length: 4,
    convRule: rule14
  }, {
    start: 126521,
    length: 1,
    convRule: rule14
  }, {
    start: 126523,
    length: 1,
    convRule: rule14
  }, {
    start: 126530,
    length: 1,
    convRule: rule14
  }, {
    start: 126535,
    length: 1,
    convRule: rule14
  }, {
    start: 126537,
    length: 1,
    convRule: rule14
  }, {
    start: 126539,
    length: 1,
    convRule: rule14
  }, {
    start: 126541,
    length: 3,
    convRule: rule14
  }, {
    start: 126545,
    length: 2,
    convRule: rule14
  }, {
    start: 126548,
    length: 1,
    convRule: rule14
  }, {
    start: 126551,
    length: 1,
    convRule: rule14
  }, {
    start: 126553,
    length: 1,
    convRule: rule14
  }, {
    start: 126555,
    length: 1,
    convRule: rule14
  }, {
    start: 126557,
    length: 1,
    convRule: rule14
  }, {
    start: 126559,
    length: 1,
    convRule: rule14
  }, {
    start: 126561,
    length: 2,
    convRule: rule14
  }, {
    start: 126564,
    length: 1,
    convRule: rule14
  }, {
    start: 126567,
    length: 4,
    convRule: rule14
  }, {
    start: 126572,
    length: 7,
    convRule: rule14
  }, {
    start: 126580,
    length: 4,
    convRule: rule14
  }, {
    start: 126585,
    length: 4,
    convRule: rule14
  }, {
    start: 126590,
    length: 1,
    convRule: rule14
  }, {
    start: 126592,
    length: 10,
    convRule: rule14
  }, {
    start: 126603,
    length: 17,
    convRule: rule14
  }, {
    start: 126625,
    length: 3,
    convRule: rule14
  }, {
    start: 126629,
    length: 5,
    convRule: rule14
  }, {
    start: 126635,
    length: 17,
    convRule: rule14
  }, {
    start: 126704,
    length: 2,
    convRule: rule6
  }, {
    start: 126976,
    length: 44,
    convRule: rule13
  }, {
    start: 127024,
    length: 100,
    convRule: rule13
  }, {
    start: 127136,
    length: 15,
    convRule: rule13
  }, {
    start: 127153,
    length: 15,
    convRule: rule13
  }, {
    start: 127169,
    length: 15,
    convRule: rule13
  }, {
    start: 127185,
    length: 37,
    convRule: rule13
  }, {
    start: 127232,
    length: 13,
    convRule: rule17
  }, {
    start: 127245,
    length: 161,
    convRule: rule13
  }, {
    start: 127462,
    length: 29,
    convRule: rule13
  }, {
    start: 127504,
    length: 44,
    convRule: rule13
  }, {
    start: 127552,
    length: 9,
    convRule: rule13
  }, {
    start: 127568,
    length: 2,
    convRule: rule13
  }, {
    start: 127584,
    length: 6,
    convRule: rule13
  }, {
    start: 127744,
    length: 251,
    convRule: rule13
  }, {
    start: 127995,
    length: 5,
    convRule: rule10
  }, {
    start: 128e3,
    length: 728,
    convRule: rule13
  }, {
    start: 128736,
    length: 13,
    convRule: rule13
  }, {
    start: 128752,
    length: 13,
    convRule: rule13
  }, {
    start: 128768,
    length: 116,
    convRule: rule13
  }, {
    start: 128896,
    length: 89,
    convRule: rule13
  }, {
    start: 128992,
    length: 12,
    convRule: rule13
  }, {
    start: 129024,
    length: 12,
    convRule: rule13
  }, {
    start: 129040,
    length: 56,
    convRule: rule13
  }, {
    start: 129104,
    length: 10,
    convRule: rule13
  }, {
    start: 129120,
    length: 40,
    convRule: rule13
  }, {
    start: 129168,
    length: 30,
    convRule: rule13
  }, {
    start: 129200,
    length: 2,
    convRule: rule13
  }, {
    start: 129280,
    length: 121,
    convRule: rule13
  }, {
    start: 129402,
    length: 82,
    convRule: rule13
  }, {
    start: 129485,
    length: 135,
    convRule: rule13
  }, {
    start: 129632,
    length: 14,
    convRule: rule13
  }, {
    start: 129648,
    length: 5,
    convRule: rule13
  }, {
    start: 129656,
    length: 3,
    convRule: rule13
  }, {
    start: 129664,
    length: 7,
    convRule: rule13
  }, {
    start: 129680,
    length: 25,
    convRule: rule13
  }, {
    start: 129712,
    length: 7,
    convRule: rule13
  }, {
    start: 129728,
    length: 3,
    convRule: rule13
  }, {
    start: 129744,
    length: 7,
    convRule: rule13
  }, {
    start: 129792,
    length: 147,
    convRule: rule13
  }, {
    start: 129940,
    length: 55,
    convRule: rule13
  }, {
    start: 130032,
    length: 10,
    convRule: rule8
  }, {
    start: 131072,
    length: 42718,
    convRule: rule14
  }, {
    start: 173824,
    length: 4149,
    convRule: rule14
  }, {
    start: 177984,
    length: 222,
    convRule: rule14
  }, {
    start: 178208,
    length: 5762,
    convRule: rule14
  }, {
    start: 183984,
    length: 7473,
    convRule: rule14
  }, {
    start: 194560,
    length: 542,
    convRule: rule14
  }, {
    start: 196608,
    length: 4939,
    convRule: rule14
  }, {
    start: 917505,
    length: 1,
    convRule: rule16
  }, {
    start: 917536,
    length: 96,
    convRule: rule16
  }, {
    start: 917760,
    length: 240,
    convRule: rule92
  }, {
    start: 983040,
    length: 65534,
    convRule: rule200
  }, {
    start: 1048576,
    length: 65534,
    convRule: rule200
  }];
  var checkAttr = function(categories) {
    return function($$char2) {
      var numOfBlocks = function() {
        var $43 = $$char2 < 256;
        if ($43) {
          return numLat1Blocks;
        }
        ;
        return numBlocks;
      }();
      var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
  var uIswalpha = /* @__PURE__ */ checkAttr([gencatLL, gencatLU, gencatLT, gencatLM, gencatLO]);
  var uIswupper = /* @__PURE__ */ checkAttr([gencatLU, gencatLT]);

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var modify5 = unsafeCoerce2;
  var toLowerSimple = /* @__PURE__ */ modify5(uTowlower);
  var toUpperSimple = /* @__PURE__ */ modify5(uTowupper);
  var isUpper = function($66) {
    return uIswupper(fromEnum4($66));
  };
  var isSpace = function(c) {
    var uc = fromEnum4(c);
    var $28 = uc <= 823;
    if ($28) {
      return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
    }
    ;
    return uIswspace(uc);
  };
  var isOctDigit = function(c) {
    var diff = fromEnum4(c) - toCharCode2("0") | 0;
    return diff <= 7 && diff >= 0;
  };
  var isDecDigit = function(c) {
    var diff = fromEnum4(c) - toCharCode2("0") | 0;
    return diff <= 9 && diff >= 0;
  };
  var isHexDigit = function(c) {
    return isDecDigit(c) || (function() {
      var diff = fromEnum4(c) - toCharCode2("A") | 0;
      return diff <= 5 && diff >= 0;
    }() || function() {
      var diff = fromEnum4(c) - toCharCode2("a") | 0;
      return diff <= 5 && diff >= 0;
    }());
  };
  var isAlphaNum = function($70) {
    return uIswalnum(fromEnum4($70));
  };
  var isAlpha = function($71) {
    return uIswalpha(fromEnum4($71));
  };
  var hexDigitToInt = function(c) {
    var hexUpper = fromEnum4(c) - toCharCode2("A") | 0;
    var hexLower = fromEnum4(c) - toCharCode2("a") | 0;
    var dec = fromEnum4(c) - toCharCode2("0") | 0;
    var result = function() {
      if (dec <= 9 && dec >= 0) {
        return new Just(dec);
      }
      ;
      if (hexLower <= 5 && hexLower >= 0) {
        return new Just(hexLower + 10 | 0);
      }
      ;
      if (hexUpper <= 5 && hexUpper >= 0) {
        return new Just(hexUpper + 10 | 0);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + []);
    }();
    return result;
  };

  // output/Parsing.String.Basic/index.js
  var elem1 = /* @__PURE__ */ elem2(eqChar);
  var show13 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showChar));
  var notElem1 = /* @__PURE__ */ notElem2(eqChar);
  var satisfyCP = function(p2) {
    return satisfy(function($32) {
      return p2(codePointFromChar($32));
    });
  };
  var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
  var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
  var oneOf2 = function(ss) {
    return withLazyErrorMessage(satisfy(flip(elem1)(ss)))(function(v) {
      return "one of " + show13(ss);
    });
  };
  var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
  var noneOf = function(ss) {
    return withLazyErrorMessage(satisfy(flip(notElem1)(ss)))(function(v) {
      return "none of " + show13(ss);
    });
  };
  var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
  var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
  var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
  var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

  // output/Data.String.Unicode/index.js
  var map24 = /* @__PURE__ */ map(functorArray);
  var convert = function(f) {
    var $6 = map24(f);
    return function($7) {
      return fromCodePointArray($6(toCodePointArray($7)));
    };
  };
  var toLowerSimple2 = /* @__PURE__ */ convert(toLowerSimple);
  var toUpperSimple2 = /* @__PURE__ */ convert(toUpperSimple);

  // output/Parsing.Token/index.js
  var bind7 = /* @__PURE__ */ bind(bindParserT);
  var pure10 = /* @__PURE__ */ pure(applicativeParserT);
  var sort2 = /* @__PURE__ */ sort(ordString);
  var map25 = /* @__PURE__ */ map(functorArray);
  var applySecond3 = /* @__PURE__ */ applySecond(applyParserT);
  var compare3 = /* @__PURE__ */ compare(ordString);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var fix2 = /* @__PURE__ */ fix(lazyParserT);
  var alt6 = /* @__PURE__ */ alt(altParserT);
  var $$void8 = /* @__PURE__ */ $$void(functorParserT);
  var voidLeft5 = /* @__PURE__ */ voidLeft(functorParserT);
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var many3 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var map110 = /* @__PURE__ */ map(functorMaybe);
  var some3 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
  var foldl4 = /* @__PURE__ */ foldl(foldableArray);
  var applyFirst3 = /* @__PURE__ */ applyFirst(applyParserT);
  var show3 = /* @__PURE__ */ show(showString);
  var bind15 = /* @__PURE__ */ bind(bindMaybe);
  var pure13 = /* @__PURE__ */ pure(applicativeMaybe);
  var foldr4 = /* @__PURE__ */ foldr(foldableArray);
  var map26 = /* @__PURE__ */ map(functorParserT);
  var choice2 = /* @__PURE__ */ choice(foldableArray);
  var many1 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var toUnfoldable3 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var foldr12 = /* @__PURE__ */ foldr(foldableList);
  var unGenLanguageDef = function(v) {
    return v;
  };
  var theReservedNames = function(v) {
    if (v.caseSensitive) {
      return sort2(v.reservedNames);
    }
    ;
    if (otherwise) {
      return sort2(map25(toLower)(v.reservedNames));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 825, column 1 - line 825, column 70): " + [v.constructor.name]);
  };
  var simpleSpace = /* @__PURE__ */ skipMany1(/* @__PURE__ */ satisfyCodePoint(isSpace));
  var oneLineComment = function(v) {
    return applySecond3($$try3(string(v.commentLine)))(skipMany(satisfy(function(v1) {
      return v1 !== "\n";
    })));
  };
  var isReserved = function($copy_names) {
    return function($copy_name) {
      var $tco_var_names = $copy_names;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(names, name15) {
        var v = uncons(names);
        if (v instanceof Nothing) {
          $tco_done = true;
          return false;
        }
        ;
        if (v instanceof Just) {
          var v1 = compare3(v.value0.head)(name15);
          if (v1 instanceof LT) {
            $tco_var_names = v.value0.tail;
            $copy_name = name15;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return true;
          }
          ;
          if (v1 instanceof GT) {
            $tco_done = true;
            return false;
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 820, column 35 - line 823, column 18): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 818, column 3 - line 823, column 18): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_names, $copy_name);
      }
      ;
      return $tco_result;
    };
  };
  var isReservedName = function(v) {
    return function(name15) {
      var caseName = function() {
        if (v.caseSensitive) {
          return name15;
        }
        ;
        if (otherwise) {
          return toLower(name15);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 812, column 3 - line 814, column 31): " + []);
      }();
      return isReserved(theReservedNames(v))(caseName);
    };
  };
  var inCommentSingle = function(v) {
    var startEnd = append7(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix2(function(p2) {
      return alt6($$void8($$try3(string(v.commentEnd))))(alt6(applySecond3(skipMany1(noneOf(startEnd)))(p2))(withErrorMessage(applySecond3(oneOf2(startEnd))(p2))("end of comment")));
    });
  };
  var multiLineComment = function(v) {
    return applySecond3($$try3(string(v.commentStart)))(inComment(v));
  };
  var inCommentMulti = function(v) {
    var startEnd = append7(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix2(function(p2) {
      return alt6($$void8($$try3(string(v.commentEnd))))(alt6(applySecond3(multiLineComment(v))(p2))(alt6(applySecond3(skipMany1(noneOf(startEnd)))(p2))(withErrorMessage(applySecond3(oneOf2(startEnd))(p2))("end of comment"))));
    });
  };
  var inComment = function(v) {
    if (v.nestedComments) {
      return inCommentMulti(v);
    }
    ;
    return inCommentSingle(v);
  };
  var whiteSpace$prime = function(v) {
    if ($$null2(v.commentLine) && $$null2(v.commentStart)) {
      return skipMany(withErrorMessage(simpleSpace)(""));
    }
    ;
    if ($$null2(v.commentLine)) {
      return skipMany(alt6(simpleSpace)(withErrorMessage(multiLineComment(v))("")));
    }
    ;
    if ($$null2(v.commentStart)) {
      return skipMany(alt6(simpleSpace)(withErrorMessage(oneLineComment(v))("")));
    }
    ;
    if (otherwise) {
      return skipMany(alt6(simpleSpace)(alt6(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 834, column 1 - line 834, column 74): " + [v.constructor.name]);
  };
  var makeTokenParser = function(v) {
    var stringLetter = satisfy(function(c) {
      return c !== '"' && (c !== "\\" && c > "");
    });
    var sign2 = function(dictRing) {
      return alt6(voidLeft5($$char("-"))(negate(dictRing)))(alt6(voidLeft5($$char("+"))(identity8))(pure10(identity8)));
    };
    var sign1 = sign2(ringInt);
    var oper = function() {
      var go2 = bind7(v.opStart)(function(c) {
        return bind7(many3(v.opLetter))(function(cs) {
          return pure10(singleton6(c) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go2)("operator");
    }();
    var number = function(base2) {
      return function(baseDigit) {
        var folder = function(v1) {
          return function(v2) {
            if (v1 instanceof Nothing) {
              return Nothing.value;
            }
            ;
            if (v1 instanceof Just) {
              return map110(function(v3) {
                return (base2 * v1.value0 | 0) + v3 | 0;
              })(hexDigitToInt(codePointFromChar(v2)));
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 704, column 5 - line 704, column 45): " + [v1.constructor.name, v2.constructor.name]);
          };
        };
        return bind7(some3(baseDigit))(function(digits) {
          return maybe(fail2("not digits"))(pure10)(foldl4(folder)(new Just(0))(digits));
        });
      };
    };
    var octal = applySecond3(oneOf2(["o", "O"]))(number(8)(octDigit));
    var lexeme2 = function(p2) {
      return applyFirst3(p2)(whiteSpace$prime(v));
    };
    var reservedOp = function(name15) {
      var go2 = bind7(string(name15))(function() {
        return withErrorMessage(notFollowedBy(v.opLetter))("end of " + name15);
      });
      return lexeme2($$try3(go2));
    };
    var symbol2 = function(name15) {
      return voidLeft5(lexeme2(string(name15)))(name15);
    };
    var parens = function(p2) {
      return between(symbol2("("))(symbol2(")"))(p2);
    };
    var semi = symbol2(";");
    var semiSep = function(p2) {
      return sepBy(p2)(semi);
    };
    var semiSep1 = function(p2) {
      return sepBy1(p2)(semi);
    };
    var isReservedOp = function(name15) {
      return isReserved(sort2(v.reservedOpNames))(name15);
    };
    var operator = function() {
      var go2 = bind7(oper)(function(name15) {
        var $114 = isReservedOp(name15);
        if ($114) {
          return fail2("reserved operator " + name15);
        }
        ;
        return pure10(name15);
      });
      return lexeme2($$try3(go2));
    }();
    var ident = function() {
      var go2 = bind7(v.identStart)(function(c) {
        return bind7(many3(v.identLetter))(function(cs) {
          return pure10(singleton6(c) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go2)("identifier");
    }();
    var identifier2 = function() {
      var go2 = bind7(ident)(function(name15) {
        var $115 = isReservedName(v)(name15);
        if ($115) {
          return fail2("reserved word " + show3(name15));
        }
        ;
        return pure10(name15);
      });
      return lexeme2($$try3(go2));
    }();
    var hexadecimal2 = applySecond3(oneOf2(["x", "X"]))(number(16)(hexDigit));
    var fraction = function() {
      var op = function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          if (v2 instanceof Just) {
            return bind15(hexDigitToInt(codePointFromChar(v1)))(function(int$prime) {
              return pure13((v2.value0 + toNumber(int$prime)) / 10);
            });
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 651, column 5 - line 651, column 47): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      return asErrorMessage("fraction")(bind7($$char("."))(function() {
        return bind7(withErrorMessage(some3(digit))("fraction"))(function(digits) {
          return maybe(fail2("not digit"))(pure10)(foldr4(op)(new Just(0))(digits));
        });
      }));
    }();
    var escapeGap = withErrorMessage(applySecond3(some3(space))($$char("\\")))("end of string gap");
    var escapeEmpty = $$char("&");
    var escMap = zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"]);
    var dot = symbol2(".");
    var decimal = number(10)(digit);
    var exponent$prime = function() {
      var power = function(e) {
        if (e < 0) {
          return 1 / power(-e | 0);
        }
        ;
        if (otherwise) {
          return pow(10)(toNumber(e));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 664, column 5 - line 664, column 27): " + [e.constructor.name]);
      };
      return asErrorMessage("exponent")(bind7(oneOf2(["e", "E"]))(function() {
        return bind7(sign1)(function(f) {
          return bind7(withErrorMessage(decimal)("exponent"))(function(e) {
            return pure10(power(f(e)));
          });
        });
      }));
    }();
    var fractExponent = function(n) {
      var justExponent = bind7(exponent$prime)(function(expo) {
        return pure10(toNumber(n) * expo);
      });
      var fractExponent$prime = bind7(fraction)(function(fract) {
        return bind7(option2(1)(exponent$prime))(function(expo) {
          return pure10((toNumber(n) + fract) * expo);
        });
      });
      return alt6(fractExponent$prime)(justExponent);
    };
    var fractFloat = function(n) {
      return map26(Right.create)(fractExponent(n));
    };
    var decimalFloat = bind7(decimal)(function(n) {
      return option2(new Left(n))(fractFloat(n));
    });
    var zeroNumFloat = alt6(map26(Left.create)(alt6(hexadecimal2)(octal)))(alt6(decimalFloat)(alt6(fractFloat(0))(pure10(new Left(0)))));
    var natFloat = alt6(applySecond3($$char("0"))(zeroNumFloat))(decimalFloat);
    var naturalOrFloat = withErrorMessage(lexeme2(natFloat))("number");
    var floating = bind7(decimal)(fractExponent);
    var $$float = withErrorMessage(lexeme2(floating))("float");
    var zeroNumber = withErrorMessage(applySecond3($$char("0"))(alt6(hexadecimal2)(alt6(octal)(alt6(decimal)(pure10(0))))))("");
    var nat = alt6(zeroNumber)(decimal);
    var $$int = bind7(lexeme2(sign1))(function(f) {
      return bind7(nat)(function(n) {
        return pure10(f(n));
      });
    });
    var integer = withErrorMessage(lexeme2($$int))("integer");
    var natural = withErrorMessage(lexeme2(nat))("natural");
    var comma = symbol2(",");
    var commaSep = function(p2) {
      return sepBy(p2)(comma);
    };
    var commaSep1 = function(p2) {
      return sepBy1(p2)(comma);
    };
    var colon = symbol2(":");
    var charNum = bind7(alt6(decimal)(alt6(applySecond3($$char("o"))(number(8)(octDigit)))(applySecond3($$char("x"))(number(16)(hexDigit)))))(function(code3) {
      var $120 = code3 > 1114111;
      if ($120) {
        return fail2("invalid escape sequence");
      }
      ;
      var v1 = fromCharCode3(code3);
      if (v1 instanceof Just) {
        return pure10(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return fail2("invalid character code (should not happen)");
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 498, column 10 - line 500, column 67): " + [v1.constructor.name]);
    });
    var charLetter = satisfy(function(c) {
      return c !== "'" && (c !== "\\" && c > "");
    });
    var charEsc = function() {
      var parseEsc = function(v1) {
        return voidLeft5($$char(v1.value0))(v1.value1);
      };
      return choice2(map25(parseEsc)(escMap));
    }();
    var charControl = bind7($$char("^"))(function() {
      return bind7(upper2)(function(code3) {
        var v1 = fromCharCode3((toCharCode2(code3) - toCharCode2("A") | 0) + 1 | 0);
        if (v1 instanceof Just) {
          return pure10(v1.value0);
        }
        ;
        if (v1 instanceof Nothing) {
          return fail2("invalid character code (should not happen)");
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 488, column 5 - line 490, column 67): " + [v1.constructor.name]);
      });
    });
    var caseString = function(name15) {
      if (v.caseSensitive) {
        return voidLeft5(string(name15))(name15);
      }
      ;
      if (otherwise) {
        var msg = show3(name15);
        var caseChar = function(c) {
          var v1 = function(v2) {
            if (otherwise) {
              return $$char(c);
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 355, column 1 - line 355, column 80): " + [c.constructor.name]);
          };
          var $131 = isAlpha(codePointFromChar(c));
          if ($131) {
            var $132 = toChar(toLowerSimple2(singleton6(c)));
            if ($132 instanceof Just) {
              var $133 = toChar(toUpperSimple2(singleton6(c)));
              if ($133 instanceof Just) {
                return alt6($$char($132.value0))($$char($133.value0));
              }
              ;
              return v1(true);
            }
            ;
            return v1(true);
          }
          ;
          return v1(true);
        };
        var walk = function(name$prime) {
          var v1 = uncons3(name$prime);
          if (v1 instanceof Nothing) {
            return pure10(unit);
          }
          ;
          if (v1 instanceof Just) {
            return applySecond3(withErrorMessage(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 757, column 22 - line 759, column 72): " + [v1.constructor.name]);
        };
        return voidLeft5(walk(name15))(name15);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 751, column 3 - line 751, column 50): " + [name15.constructor.name]);
    };
    var reserved = function(name15) {
      var go2 = applySecond3(caseString(name15))(withErrorMessage(notFollowedBy(v.identLetter))("end of " + name15));
      return lexeme2($$try3(go2));
    };
    var brackets = function(p2) {
      return between(symbol2("["))(symbol2("]"))(p2);
    };
    var braces = function(p2) {
      return between(symbol2("{"))(symbol2("}"))(p2);
    };
    var ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"];
    var ascii3 = ["\0", "", "", "", "", "", "", "\x07", "", "", "", "", "", "", "", "", "", "", "\x1B", "\x7F"];
    var ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"];
    var ascii2 = ["\b", "	", "\n", "\v", "\f", "\r", "", "", "", "", "", "", "", " "];
    var asciiMap = zip(append7(ascii3codes)(ascii2codes))(append7(ascii3)(ascii2));
    var charAscii = function() {
      var parseAscii = function(v1) {
        return $$try3(voidLeft5(string(v1.value0))(v1.value1));
      };
      return choice2(map25(parseAscii)(asciiMap));
    }();
    var escapeCode = alt6(charEsc)(alt6(charNum)(alt6(charAscii)(withErrorMessage(charControl)("escape code"))));
    var charEscape = applySecond3($$char("\\"))(escapeCode);
    var characterChar = alt6(charLetter)(withErrorMessage(charEscape)("literal character"));
    var charLiteral = function() {
      var go2 = between($$char("'"))(withErrorMessage($$char("'"))("end of character"))(characterChar);
      return withErrorMessage(lexeme2(go2))("character");
    }();
    var stringEscape = bind7($$char("\\"))(function() {
      return alt6(voidLeft5(escapeGap)(Nothing.value))(alt6(voidLeft5(escapeEmpty)(Nothing.value))(map26(Just.create)(escapeCode)));
    });
    var stringChar = alt6(map26(Just.create)(stringLetter))(withErrorMessage(stringEscape)("string character"));
    var stringLiteral = function() {
      var folder = function(v1) {
        return function(v2) {
          if (v1 instanceof Nothing) {
            return v2;
          }
          ;
          if (v1 instanceof Just) {
            return new Cons(v1.value0, v2);
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 455, column 5 - line 455, column 51): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      var go2 = bind7(between($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many1(stringChar)))(function(maybeChars) {
        return pure10(fromCharArray(toUnfoldable3(foldr12(folder)(Nil.value)(maybeChars))));
      });
      return lexeme2(withErrorMessage(go2)("literal string"));
    }();
    var angles = function(p2) {
      return between(symbol2("<"))(symbol2(">"))(p2);
    };
    return {
      identifier: identifier2,
      reserved,
      operator,
      reservedOp,
      charLiteral,
      stringLiteral,
      natural,
      integer,
      "float": $$float,
      naturalOrFloat,
      decimal,
      hexadecimal: hexadecimal2,
      octal,
      symbol: symbol2,
      lexeme: lexeme2,
      whiteSpace: whiteSpace$prime(v),
      parens,
      braces,
      angles,
      brackets,
      semi,
      comma,
      colon,
      dot,
      semiSep,
      semiSep1,
      commaSep,
      commaSep1
    };
  };

  // output/Parsing.Language/index.js
  var alt7 = /* @__PURE__ */ alt(altParserT);
  var emptyDef = /* @__PURE__ */ function() {
    var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    return {
      commentStart: "",
      commentEnd: "",
      commentLine: "",
      nestedComments: true,
      identStart: alt7(letter)($$char("_")),
      identLetter: alt7(alphaNum)(oneOf2(["_", "'"])),
      opStart: op$prime,
      opLetter: op$prime,
      reservedOpNames: [],
      reservedNames: [],
      caseSensitive: true
    };
  }();
  var haskellStyle = /* @__PURE__ */ function() {
    var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    var v = unGenLanguageDef(emptyDef);
    return {
      commentStart: "{-",
      commentEnd: "-}",
      commentLine: "--",
      nestedComments: true,
      identStart: letter,
      identLetter: alt7(alphaNum)(oneOf2(["_", "'"])),
      opStart: op$prime,
      opLetter: op$prime,
      reservedOpNames: [],
      reservedNames: [],
      caseSensitive: true
    };
  }();

  // output/UTLC.Utils/index.js
  var unlines = /* @__PURE__ */ function() {
    var go2 = function(here) {
      return function(rest) {
        var $3 = $$null2(rest);
        if ($3) {
          return here;
        }
        ;
        return here + ("\n" + rest);
      };
    };
    return foldr(foldableArray)(go2)("");
  }();
  var error4 = function($4) {
    return unsafePerformEffect($$throw($4));
  };

  // output/UTLC.Syntax.Name/index.js
  var map27 = /* @__PURE__ */ map(functorList);
  var map111 = /* @__PURE__ */ map(functorTuple);
  var show4 = /* @__PURE__ */ show(showInt);
  var find3 = /* @__PURE__ */ find(foldableList);
  var elem3 = /* @__PURE__ */ elem(foldableList)(eqString);
  var FV = /* @__PURE__ */ function() {
    function FV2(value0) {
      this.value0 = value0;
    }
    ;
    FV2.create = function(value0) {
      return new FV2(value0);
    };
    return FV2;
  }();
  var BV = /* @__PURE__ */ function() {
    function BV2(value0) {
      this.value0 = value0;
    }
    ;
    BV2.create = function(value0) {
      return new BV2(value0);
    };
    return BV2;
  }();
  var Lvl = function(x) {
    return x;
  };
  var LvlName = /* @__PURE__ */ function() {
    function LvlName2(value0) {
      this.value0 = value0;
    }
    ;
    LvlName2.create = function(value0) {
      return new LvlName2(value0);
    };
    return LvlName2;
  }();
  var IxName = /* @__PURE__ */ function() {
    function IxName2(value0) {
      this.value0 = value0;
    }
    ;
    IxName2.create = function(value0) {
      return new IxName2(value0);
    };
    return IxName2;
  }();
  var functorV = {
    map: function(f) {
      return function(m) {
        if (m instanceof FV) {
          return new FV(m.value0);
        }
        ;
        if (m instanceof BV) {
          return new BV(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at UTLC.Syntax.Name (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var functorNamingCtx$prime = {
    map: function(f) {
      return function(m) {
        return map27(map111(f))(m);
      };
    }
  };
  var map28 = /* @__PURE__ */ map(functorNamingCtx$prime);
  var showIxName = function(v) {
    return v.value0.name;
  };
  var showIx = function(v) {
    return show4(v);
  };
  var shiftIx = function(v) {
    return v + 1 | 0;
  };
  var nextLevel = function(v) {
    return v + 1 | 0;
  };
  var nameToIx_maybe = function(v) {
    return function(n) {
      var v1 = find3(function($93) {
        return function(v2) {
          return v2 === n;
        }(fst($93));
      })(v);
      if (v1 instanceof Just) {
        return new Just(v1.value0.value1);
      }
      ;
      if (v1 instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at UTLC.Syntax.Name (line 130, column 3 - line 132, column 23): " + [v1.constructor.name]);
    };
  };
  var mkLevel = function($94) {
    return Lvl(length2($94));
  };
  var lvlIx = function(depth) {
    return function(v) {
      return (depth - v | 0) - 1 | 0;
    };
  };
  var lvlNameIx = function(v) {
    return function(v1) {
      return new IxName({
        name: v1.value0.name,
        ix: lvlIx(v)(v1.value0.lvl)
      });
    };
  };
  var lookupIx_maybe = function(xs) {
    return function(v) {
      return index(xs)(v);
    };
  };
  var lookupIx = function(xs) {
    return function(i2) {
      var v = lookupIx_maybe(xs)(i2);
      if (v instanceof Nothing) {
        return error4("Cannot find binding for ix " + showIx(i2));
      }
      ;
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at UTLC.Syntax.Name (line 87, column 3 - line 89, column 16): " + [v.constructor.name]);
    };
  };
  var ixHere = 0;
  var liftNamingCtx = function(name15) {
    return function(ctx) {
      var v = map28(shiftIx)(ctx);
      return new Cons(new Tuple(name15, ixHere), v);
    };
  };
  var initialLevel = 0;
  var fresh2 = function(name15) {
    return function(ctx) {
      var go2 = function($copy_uniq) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(uniq) {
          var newName = name15 + show4(uniq);
          var $91 = elem3(newName)(ctx);
          if ($91) {
            $copy_uniq = uniq + 1 | 0;
            return;
          }
          ;
          $tco_done = true;
          return newName;
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_uniq);
        }
        ;
        return $tco_result;
      };
      var $92 = elem3(name15)(ctx);
      if ($92) {
        return go2(0);
      }
      ;
      return name15;
    };
  };
  var emptyNamingCtx = /* @__PURE__ */ function() {
    return Nil.value;
  }();

  // output/UTLC.Syntax.Term/index.js
  var $runtime_lazy13 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var defer5 = /* @__PURE__ */ defer(lazyParserT);
  var alt8 = /* @__PURE__ */ alt(altParserT);
  var applyFirst4 = /* @__PURE__ */ applyFirst(applyParserT);
  var applySecond4 = /* @__PURE__ */ applySecond(applyParserT);
  var bind8 = /* @__PURE__ */ bind(bindParserT);
  var pure11 = /* @__PURE__ */ pure(applicativeParserT);
  var some4 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
  var Var = /* @__PURE__ */ function() {
    function Var2(value0) {
      this.value0 = value0;
    }
    ;
    Var2.create = function(value0) {
      return new Var2(value0);
    };
    return Var2;
  }();
  var Lam = /* @__PURE__ */ function() {
    function Lam2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Lam2.create = function(value0) {
      return function(value1) {
        return new Lam2(value0, value1);
      };
    };
    return Lam2;
  }();
  var App2 = /* @__PURE__ */ function() {
    function App3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    App3.create = function(value0) {
      return function(value1) {
        return new App3(value0, value1);
      };
    };
    return App3;
  }();
  var Print = /* @__PURE__ */ function() {
    function Print2() {
    }
    ;
    Print2.value = new Print2();
    return Print2;
  }();
  var Def = /* @__PURE__ */ function() {
    function Def2(value0) {
      this.value0 = value0;
    }
    ;
    Def2.create = function(value0) {
      return new Def2(value0);
    };
    return Def2;
  }();
  var toNamed = /* @__PURE__ */ function() {
    var go2 = function(v) {
      return function(v1) {
        if (v1 instanceof Var && v1.value0 instanceof FV) {
          return new Var(v1.value0.value0);
        }
        ;
        if (v1 instanceof Var && v1.value0 instanceof BV) {
          return new Var(function() {
            var v2 = lookupIx_maybe(v)(v1.value0.value0.value0.ix);
            if (v2 instanceof Nothing) {
              return showIxName(v1.value0.value0);
            }
            ;
            if (v2 instanceof Just) {
              return v2.value0;
            }
            ;
            throw new Error("Failed pattern match at UTLC.Syntax.Term (line 92, column 7 - line 94, column 28): " + [v2.constructor.name]);
          }());
        }
        ;
        if (v1 instanceof Lam) {
          var x = fresh2(v1.value0)(v);
          return new Lam(x, go2(new Cons(x, v))(v1.value1));
        }
        ;
        if (v1 instanceof App2) {
          return new App2(go2(v)(v1.value0), go2(v)(v1.value1));
        }
        ;
        if (v1 instanceof Print) {
          return Print.value;
        }
        ;
        throw new Error("Failed pattern match at UTLC.Syntax.Term (line 88, column 5 - line 88, column 43): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    return go2(mempty(monoidList));
  }();
  var showTerm = function(v) {
    if (v instanceof Var) {
      return v.value0;
    }
    ;
    if (v instanceof Lam) {
      return "\\" + (v.value0 + (". " + showTerm(v.value1)));
    }
    ;
    if (v instanceof App2) {
      var parens = function(s) {
        return "(" + (s + ")");
      };
      var showFn = function(v1) {
        if (v1 instanceof Lam) {
          return parens(showTerm(v1));
        }
        ;
        return showTerm(v1);
      };
      var isNested = function(v1) {
        if (v1 instanceof Var) {
          return false;
        }
        ;
        if (v1 instanceof Lam) {
          return true;
        }
        ;
        if (v1 instanceof App2) {
          return true;
        }
        ;
        if (v1 instanceof Print) {
          return false;
        }
        ;
        throw new Error("Failed pattern match at UTLC.Syntax.Term (line 117, column 5 - line 117, column 29): " + [v1.constructor.name]);
      };
      var showParens = function(e) {
        var $147 = isNested(e);
        if ($147) {
          return parens(showTerm(e));
        }
        ;
        return showTerm(e);
      };
      return showFn(v.value0) + (" " + showParens(v.value1));
    }
    ;
    if (v instanceof Print) {
      return "print";
    }
    ;
    throw new Error("Failed pattern match at UTLC.Syntax.Term (line 102, column 1 - line 102, column 32): " + [v.constructor.name]);
  };
  var keywords = ["print"];
  var tokenParser = /* @__PURE__ */ makeTokenParser(/* @__PURE__ */ function() {
    var v = unGenLanguageDef(haskellStyle);
    return {
      commentStart: v.commentStart,
      commentEnd: v.commentEnd,
      commentLine: v.commentLine,
      nestedComments: v.nestedComments,
      identStart: v.identStart,
      identLetter: v.identLetter,
      opStart: v.opStart,
      opLetter: v.opLetter,
      reservedOpNames: v.reservedOpNames,
      caseSensitive: v.caseSensitive,
      reservedNames: keywords
    };
  }());
  var lexeme = /* @__PURE__ */ function() {
    return tokenParser.lexeme;
  }();
  var symbol = /* @__PURE__ */ function() {
    return tokenParser.symbol;
  }();
  var keyword = /* @__PURE__ */ function() {
    return tokenParser.reserved;
  }();
  var parsePrint = /* @__PURE__ */ function() {
    return lexeme(voidLeft(functorParserT)(keyword("print"))(Print.value));
  }();
  var identifier = /* @__PURE__ */ function() {
    return tokenParser.identifier;
  }();
  var parseVar = /* @__PURE__ */ function() {
    return lexeme(map(functorParserT)(Var.create)(identifier));
  }();
  var $lazy_parseApp = /* @__PURE__ */ $runtime_lazy13("parseApp", "UTLC.Syntax.Term", function() {
    return lexeme(defer5(function(v) {
      return bind8($lazy_parseTerm$prime(157))(function(f) {
        return bind8(some4($lazy_parseTerm$prime(158)))(function(args) {
          return pure11(foldl2(App2.create)(f)(args));
        });
      });
    }));
  });
  var $lazy_parseLam = /* @__PURE__ */ $runtime_lazy13("parseLam", "UTLC.Syntax.Term", function() {
    return lexeme(bind8(symbol("\\"))(function() {
      return bind8(identifier)(function(x) {
        return bind8(symbol("."))(function() {
          return bind8($lazy_parseTerm(149))(function(body2) {
            return pure11(new Lam(x, body2));
          });
        });
      });
    }));
  });
  var $lazy_parseTerm = /* @__PURE__ */ $runtime_lazy13("parseTerm", "UTLC.Syntax.Term", function() {
    return defer5(function(v) {
      return alt8($$try3($lazy_parseLam(137)))(alt8($$try3($lazy_parseApp(138)))($$try3($lazy_parseTerm$prime(139))));
    });
  });
  var $lazy_parseTerm$prime = /* @__PURE__ */ $runtime_lazy13("parseTerm'", "UTLC.Syntax.Term", function() {
    return defer5(function(v) {
      return alt8($$try3(parsePrint))(alt8($$try3(parseVar))($$try3(applyFirst4(applySecond4(symbol("("))($lazy_parseTerm(133)))(symbol(")")))));
    });
  });
  var parseTerm = /* @__PURE__ */ $lazy_parseTerm(135);
  var parseDef = /* @__PURE__ */ lexeme(/* @__PURE__ */ bind8(identifier)(function(name15) {
    return bind8(symbol(":="))(function() {
      return bind8(parseTerm)(function(body2) {
        return bind8(symbol(";"))(function() {
          return pure11(new Def({
            name: name15,
            body: body2
          }));
        });
      });
    });
  }));
  var findDef = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v instanceof Nil) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Cons) {
          var $154 = v.value0.value0.name === v1;
          if ($154) {
            $tco_done = true;
            return new Just(new Def(v.value0.value0));
          }
          ;
          $tco_var_v = v.value1;
          $copy_v1 = v1;
          return;
        }
        ;
        throw new Error("Failed pattern match at UTLC.Syntax.Term (line 50, column 1 - line 50, column 53): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var fromNamed = function(defs) {
    var go2 = function(v) {
      return function(v1) {
        if (v1 instanceof Var) {
          var v2 = nameToIx_maybe(v)(v1.value0);
          if (v2 instanceof Just) {
            return new Var(new BV(new IxName({
              name: v1.value0,
              ix: v2.value0
            })));
          }
          ;
          if (v2 instanceof Nothing) {
            var v3 = findDef(defs)(v1.value0);
            if (v3 instanceof Just) {
              return fromNamed(defs)(v3.value0.value0.body);
            }
            ;
            if (v3 instanceof Nothing) {
              return new Var(new FV(v1.value0));
            }
            ;
            throw new Error("Failed pattern match at UTLC.Syntax.Term (line 75, column 11 - line 77, column 37): " + [v3.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at UTLC.Syntax.Term (line 72, column 7 - line 77, column 37): " + [v2.constructor.name]);
        }
        ;
        if (v1 instanceof App2) {
          return new App2(go2(v)(v1.value0), go2(v)(v1.value1));
        }
        ;
        if (v1 instanceof Lam) {
          var nCtx$prime = liftNamingCtx(v1.value0)(v);
          return new Lam(v1.value0, go2(nCtx$prime)(v1.value1));
        }
        ;
        if (v1 instanceof Print) {
          return Print.value;
        }
        ;
        throw new Error("Failed pattern match at UTLC.Syntax.Term (line 70, column 5 - line 70, column 43): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    return go2(emptyNamingCtx);
  };

  // output/UTLC.Backend.Python/index.js
  var elem4 = /* @__PURE__ */ elem(foldableList)(eqString);
  var show5 = /* @__PURE__ */ show(showString);
  var map29 = /* @__PURE__ */ map(functorList);
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var toPythonVar = function(x) {
    return "_" + x;
  };
  var toPythonExpr = function(v) {
    return function(v1) {
      if (v1 instanceof Var) {
        var $12 = elem4(v1.value0)(v);
        if ($12) {
          return toPythonVar(v1.value0);
        }
        ;
        return show5(v1.value0);
      }
      ;
      if (v1 instanceof Lam) {
        return "lambda " + (toPythonVar(v1.value0) + (": " + toPythonExpr(new Cons(v1.value0, v))(v1.value1)));
      }
      ;
      if (v1 instanceof App2) {
        var showFn = function() {
          if (v1.value0 instanceof Var) {
            return toPythonExpr(v)(v1.value0);
          }
          ;
          return "(" + (toPythonExpr(v)(v1.value0) + ")");
        }();
        return showFn + ("(" + (toPythonExpr(v)(v1.value1) + ")"));
      }
      ;
      if (v1 instanceof Print) {
        return "printImpl";
      }
      ;
      throw new Error("Failed pattern match at UTLC.Backend.Python (line 15, column 1 - line 15, column 49): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var toPythonDef = function(env) {
    return function(v) {
      return toPythonVar(v.value0.name) + (" = " + toPythonExpr(env)(v.value0.body));
    };
  };
  var toPython = function(defs) {
    return function(term) {
      var defNames = map29(function(v) {
        return v.value0.name;
      })(defs);
      return unlines(toUnfoldable4(map29(toPythonDef(defNames))(defs))) + ("\n" + toPythonExpr(defNames)(term));
    };
  };
  var pythonPrelude = /* @__PURE__ */ unlines(["def printImpl(x):", "  def result(y):", "    print(x)", "    return y", "  return result"]);

  // output/UTLC.Eval.NbE/index.js
  var monadWriterT2 = /* @__PURE__ */ monadWriterT(monoidList)(monadEither);
  var pure14 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadWriterT2));
  var map30 = /* @__PURE__ */ map(functorV);
  var applyStateT2 = /* @__PURE__ */ applyStateT(monadWriterT2);
  var lift22 = /* @__PURE__ */ lift2(applyStateT2);
  var bindStateT2 = /* @__PURE__ */ bindStateT(monadWriterT2);
  var bind9 = /* @__PURE__ */ bind(bindStateT2);
  var map112 = /* @__PURE__ */ map(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorWriterT(functorEither)));
  var discard5 = /* @__PURE__ */ discard(discardUnit)(bindStateT2);
  var tell3 = /* @__PURE__ */ tell(/* @__PURE__ */ monadTellStateT(/* @__PURE__ */ monadTellWriterT(monoidList)(monadEither)));
  var applySecond5 = /* @__PURE__ */ applySecond(applyStateT2);
  var bindFlipped10 = /* @__PURE__ */ bindFlipped(bindStateT2);
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(bindStateT2);
  var TooManySteps = /* @__PURE__ */ function() {
    function TooManySteps2() {
    }
    ;
    TooManySteps2.value = new TooManySteps2();
    return TooManySteps2;
  }();
  var UnknownName = /* @__PURE__ */ function() {
    function UnknownName2(value0) {
      this.value0 = value0;
    }
    ;
    UnknownName2.create = function(value0) {
      return new UnknownName2(value0);
    };
    return UnknownName2;
  }();
  var VLam = /* @__PURE__ */ function() {
    function VLam2(value0) {
      this.value0 = value0;
    }
    ;
    VLam2.create = function(value0) {
      return new VLam2(value0);
    };
    return VLam2;
  }();
  var VNeutral = /* @__PURE__ */ function() {
    function VNeutral2(value0) {
      this.value0 = value0;
    }
    ;
    VNeutral2.create = function(value0) {
      return new VNeutral2(value0);
    };
    return VNeutral2;
  }();
  var NVar = /* @__PURE__ */ function() {
    function NVar2(value0) {
      this.value0 = value0;
    }
    ;
    NVar2.create = function(value0) {
      return new NVar2(value0);
    };
    return NVar2;
  }();
  var NApp = /* @__PURE__ */ function() {
    function NApp2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NApp2.create = function(value0) {
      return function(value1) {
        return new NApp2(value0, value1);
      };
    };
    return NApp2;
  }();
  var NPrint = /* @__PURE__ */ function() {
    function NPrint2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NPrint2.create = function(value0) {
      return function(value1) {
        return new NPrint2(value0, value1);
      };
    };
    return NPrint2;
  }();
  var toValueIxName = {
    toValue: function(env) {
      return function(v) {
        return pure14(lookupIx(env)(v.value0.ix));
      };
    }
  };
  var toValue = function(dict) {
    return dict.toValue;
  };
  var toValueV = function(dictToValue) {
    var toValue1 = toValue(dictToValue);
    return {
      toValue: function(v) {
        return function(v1) {
          if (v1 instanceof FV) {
            return pure14(new VNeutral(new NVar(new FV(v1.value0))));
          }
          ;
          if (v1 instanceof BV) {
            return toValue1(v)(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at UTLC.Eval.NbE (line 84, column 1 - line 86, column 37): " + [v.constructor.name, v1.constructor.name]);
        };
      }
    };
  };
  var step4 = /* @__PURE__ */ pure14(unit);
  var runEval = function(m) {
    var v = runWriterT(flip(runStateT)(0)(m));
    if (v instanceof Left && v.value0 instanceof TooManySteps) {
      return new Left("Too many steps");
    }
    ;
    if (v instanceof Left && v.value0 instanceof UnknownName) {
      return new Left("Unknown name " + v.value0.value0);
    }
    ;
    if (v instanceof Right) {
      return new Right(new Tuple(v.value0.value1, v.value0.value0.value0));
    }
    ;
    throw new Error("Failed pattern match at UTLC.Eval.NbE (line 59, column 3 - line 62, column 63): " + [v.constructor.name]);
  };
  var reifyNeutral = function(v) {
    return function(v1) {
      if (v1 instanceof NVar) {
        return pure14(new Var(map30(lvlNameIx(v))(v1.value0)));
      }
      ;
      if (v1 instanceof NApp) {
        return lift22(App2.create)(reifyNeutral(v)(v1.value0))(reify(v)(v1.value1));
      }
      ;
      if (v1 instanceof NPrint) {
        return bind9(reify(v)(v1.value0))(function(x$prime) {
          return bind9(reifyNeutral(v)(v1.value1))(function(y$prime) {
            return pure14(new App2(new App2(Print.value, x$prime), y$prime));
          });
        });
      }
      ;
      throw new Error("Failed pattern match at UTLC.Eval.NbE (line 151, column 1 - line 151, column 46): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var reify = function(v) {
    return function(v1) {
      if (v1 instanceof VLam) {
        return bind9(v1.value0.fn(new VNeutral(new NVar(new BV(new LvlName({
          name: v1.value0.argName,
          lvl: v
        }))))))(function(v2) {
          return map112(Lam.create(v1.value0.argName))(reify(nextLevel(v))(v2));
        });
      }
      ;
      if (v1 instanceof VNeutral) {
        return reifyNeutral(v)(v1.value0);
      }
      ;
      throw new Error("Failed pattern match at UTLC.Eval.NbE (line 142, column 1 - line 142, column 37): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var extend2 = function(env) {
    return function(val) {
      return new Cons(val, env);
    };
  };
  var evalPrint = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof VLam) {
          return bind9(reify(mkLevel(v))(v1))(function(valTerm) {
            return discard5(tell3(singleton3(showTerm(toNamed(valTerm)))))(function() {
              return pure14(v2);
            });
          });
        }
        ;
        if (v2 instanceof VNeutral) {
          return pure14(new VNeutral(new NPrint(v1, v2.value0)));
        }
        ;
        throw new Error("Failed pattern match at UTLC.Eval.NbE (line 135, column 1 - line 135, column 49): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var toValueTerm = function(dictToValue) {
    return {
      toValue: function(env) {
        return function(e) {
          return applySecond5(step4)(eval$prime(dictToValue)(env)(e));
        };
      }
    };
  };
  var eval$prime = function(dictToValue) {
    var toValue1 = toValue(dictToValue);
    return function(v) {
      return function(v1) {
        if (v1 instanceof Var) {
          return toValue1(v)(v1.value0);
        }
        ;
        if (v1 instanceof App2) {
          return bind9($$eval(dictToValue)(v)(v1.value0))(function(vA) {
            if (vA instanceof VNeutral) {
              return bind9($$eval(dictToValue)(v)(v1.value1))(function(vB) {
                return pure14(new VNeutral(new NApp(vA.value0, vB)));
              });
            }
            ;
            if (vA instanceof VLam) {
              return bindFlipped10(vA.value0.fn)($$eval(dictToValue)(v)(v1.value1));
            }
            ;
            throw new Error("Failed pattern match at UTLC.Eval.NbE (line 102, column 3 - line 116, column 30): " + [vA.constructor.name]);
          });
        }
        ;
        if (v1 instanceof Lam) {
          return pure14(new VLam({
            argName: v1.value0,
            fn: function(v2) {
              return $$eval(dictToValue)(extend2(v)(v2))(v1.value1);
            }
          }));
        }
        ;
        if (v1 instanceof Print) {
          return pure14(new VLam({
            argName: "x1",
            fn: function(v11) {
              return pure14(new VLam({
                argName: "x2",
                fn: function(v2) {
                  return evalPrint(v)(v11)(v2);
                }
              }));
            }
          }));
        }
        ;
        throw new Error("Failed pattern match at UTLC.Eval.NbE (line 97, column 1 - line 97, column 60): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var $$eval = function(dictToValue) {
    return toValue(toValueTerm(dictToValue));
  };
  var eval1 = /* @__PURE__ */ $$eval(/* @__PURE__ */ toValueV(toValueIxName));
  var normalize2 = function(env) {
    return composeKleisliFlipped4(reify(initialLevel))(eval1(env));
  };

  // output/Web.UIEvent.KeyboardEvent/foreign.js
  function key(e) {
    return e.key;
  }

  // output/Main/index.js
  var modify_3 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var discard6 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var bind10 = /* @__PURE__ */ bind(bindHalogenM);
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeHalogenM);
  var traverse_14 = /* @__PURE__ */ traverse_7(foldableMaybe);
  var applySecond6 = /* @__PURE__ */ applySecond(applyParserT);
  var map31 = /* @__PURE__ */ map(functorParserT);
  var applyFirst5 = /* @__PURE__ */ applyFirst(applyParserT);
  var show6 = /* @__PURE__ */ show(showParseError);
  var get2 = /* @__PURE__ */ get(monadStateHalogenM);
  var mempty2 = /* @__PURE__ */ mempty(monoidList);
  var traverse_24 = /* @__PURE__ */ traverse_7(foldableList);
  var map113 = /* @__PURE__ */ map(functorArray);
  var pure15 = /* @__PURE__ */ pure(applicativeHalogenM);
  var many4 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var show14 = /* @__PURE__ */ show(showString);
  var show23 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showString));
  var value13 = /* @__PURE__ */ value12(isPropString);
  var type_19 = /* @__PURE__ */ type_18(isPropInputType);
  var ReplTerm = /* @__PURE__ */ function() {
    function ReplTerm2(value0) {
      this.value0 = value0;
    }
    ;
    ReplTerm2.create = function(value0) {
      return new ReplTerm2(value0);
    };
    return ReplTerm2;
  }();
  var Compile = /* @__PURE__ */ function() {
    function Compile2(value0) {
      this.value0 = value0;
    }
    ;
    Compile2.create = function(value0) {
      return new Compile2(value0);
    };
    return Compile2;
  }();
  var Nop = /* @__PURE__ */ function() {
    function Nop2() {
    }
    ;
    Nop2.value = new Nop2();
    return Nop2;
  }();
  var Focus = /* @__PURE__ */ function() {
    function Focus2() {
    }
    ;
    Focus2.value = new Focus2();
    return Focus2;
  }();
  var UpdateInput = /* @__PURE__ */ function() {
    function UpdateInput2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateInput2.create = function(value0) {
      return new UpdateInput2(value0);
    };
    return UpdateInput2;
  }();
  var UpdateDefString = /* @__PURE__ */ function() {
    function UpdateDefString2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateDefString2.create = function(value0) {
      return new UpdateDefString2(value0);
    };
    return UpdateDefString2;
  }();
  var ReloadDefs = /* @__PURE__ */ function() {
    function ReloadDefs2() {
    }
    ;
    ReloadDefs2.value = new ReloadDefs2();
    return ReloadDefs2;
  }();
  var ExecuteCommand = /* @__PURE__ */ function() {
    function ExecuteCommand2() {
    }
    ;
    ExecuteCommand2.value = new ExecuteCommand2();
    return ExecuteCommand2;
  }();
  var updateTerminal = function(dictMonadAff) {
    return function(resultMsg) {
      return modify_3(function(st) {
        var $60 = {};
        for (var $61 in st) {
          if ({}.hasOwnProperty.call(st, $61)) {
            $60[$61] = st[$61];
          }
          ;
        }
        ;
        $60.history = append8(st.history)([resultMsg]);
        $60.message = resultMsg;
        return $60;
      });
    };
  };
  var updateInput = function(x) {
    return new UpdateInput(x);
  };
  var terminalRef = "terminalRef";
  var replInputRef = "replInput";
  var refocus = function(dictMonadAff) {
    var liftEffect7 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return discard6(bind10(getHTMLElementRef(replInputRef))(traverse_14(function(el) {
      return liftEffect7(focus(el));
    })))(function() {
      return bind10(getHTMLElementRef(terminalRef))(traverse_14(function(el) {
        return liftEffect7(setScrollLeft(0)(toElement(el)));
      }));
    });
  };
  var prompt = ">> ";
  var parseReplCmd = /* @__PURE__ */ function() {
    return alt(altParserT)($$try3(applySecond6(applySecond6(symbol(":"))(keyword("compile")))(map31(Compile.create)(parseTerm))))($$try3(map31(ReplTerm.create)(parseTerm)));
  }();
  var runInput = function(dictMonadAff) {
    var updateTerminal1 = updateTerminal(dictMonadAff);
    return function(input3) {
      var v = runParser(input3)(applyFirst5(parseReplCmd)(eof));
      if (v instanceof Left) {
        return updateTerminal1("Parse error: " + show6(v.value0));
      }
      ;
      if (v instanceof Right && v.value0 instanceof Compile) {
        return bind10(get2)(function(st) {
          var pyCode = pythonPrelude + ("\n" + toPython(st.defs)(v.value0.value0));
          return updateTerminal1("# Python code:\n" + pyCode);
        });
      }
      ;
      if (v instanceof Right && v.value0 instanceof ReplTerm) {
        return bind10(get2)(function(st) {
          var v1 = runEval(normalize2(mempty2)(fromNamed(st.defs)(v.value0.value0)));
          if (v1 instanceof Left) {
            return updateTerminal1(v1.value0);
          }
          ;
          if (v1 instanceof Right) {
            return discard6(traverse_24(updateTerminal1)(v1.value0.value0))(function() {
              return updateTerminal1("Normal form: " + showTerm(toNamed(v1.value0.value1)));
            });
          }
          ;
          throw new Error("Failed pattern match at Main (line 288, column 7 - line 292, column 69): " + [v1.constructor.name]);
        });
      }
      ;
      throw new Error("Failed pattern match at Main (line 277, column 3 - line 292, column 69): " + [v.constructor.name]);
    };
  };
  var monospace = function(str) {
    return span3([class_("monospace")])([text5(str)]);
  };
  var printWord = /* @__PURE__ */ monospace("print");
  var mkAction = function(ev) {
    var $74 = key(ev) === "Enter";
    if ($74) {
      return ExecuteCommand.value;
    }
    ;
    return Nop.value;
  };
  var instructions = [/* @__PURE__ */ table_([/* @__PURE__ */ tr_([/* @__PURE__ */ th_([/* @__PURE__ */ text5("Description")]), /* @__PURE__ */ th_([/* @__PURE__ */ text5("Syntax")])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Anonymous function")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("\\<var>. <term>")])])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Function application")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("<term> <term>")])])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Print the argument")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("print")])])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Definition")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("<var> := <term>;")])])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Comment")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("-- This is a comment")])])]), /* @__PURE__ */ tr_([/* @__PURE__ */ td_([/* @__PURE__ */ text5("Compile to Python (this is a REPL command)")]), /* @__PURE__ */ td_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5(":compile <term>")])])])]), br_, /* @__PURE__ */ h2_([/* @__PURE__ */ text5("Examples")]), /* @__PURE__ */ ul_([/* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("(\\x. x) a")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("(\\x. x x) a")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("print b")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("print b (\\x. x)")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("print b id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("print c (print d id)")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("two")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("add two one")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("minus six two")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("two (print c) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("add two four (print c) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("mult two three (print c) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("exp two three (print c) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("minus six two (print a) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("isZero zero (print a) (print b) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("isZero one (print a) (print b) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("factorial three (print a) id")])]), /* @__PURE__ */ li_([/* @__PURE__ */ pre_([/* @__PURE__ */ text5("fibonacci five (print a) id")])])]), br_, /* @__PURE__ */ h2_([/* @__PURE__ */ text5("Notes")]), /* @__PURE__ */ ul_([/* @__PURE__ */ li_([/* @__PURE__ */ text5("Each expression entered into the REPL is evaluated into a normal form and then this is printed.")]), /* @__PURE__ */ li_([/* @__PURE__ */ monospace("id"), /* @__PURE__ */ text5(" is just a name for the identity function. See the definitions panel. The identity function is "), /* @__PURE__ */ monospace("\\x. x")]), /* @__PURE__ */ li_([/* @__PURE__ */ text5("Note that the "), printWord, /* @__PURE__ */ text5(" function prints its argument (after normalizing it) whenever an application of "), printWord, /* @__PURE__ */ text5(" to an argument is evaluated. More specifically, when applied to an argument it gives back a function that will print when that function is applied to a function. This whole thing normalizes to the function its applied to. See the examples.")]), /* @__PURE__ */ li_([/* @__PURE__ */ text5("There is *nothing* else in the language that is not described above. Everything is either: an anonymous function, a function application, "), /* @__PURE__ */ monospace("print"), /* @__PURE__ */ text5(" or a variable. Definitions simply provide a way to name expressions as a shorthand. This could be eliminated from the language without changing any properties of the language, though it would be much harder to work with.")])])];
  var historyToHtml = function(st) {
    return intersperse(br_)(map113(text5)(st.history));
  };
  var handleAction = function(dictMonadAff) {
    var refocus1 = refocus(dictMonadAff);
    var updateTerminal1 = updateTerminal(dictMonadAff);
    var liftEffect7 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var runInput1 = runInput(dictMonadAff);
    return function(v) {
      if (v instanceof Nop) {
        return pure15(unit);
      }
      ;
      if (v instanceof Focus) {
        return refocus1;
      }
      ;
      if (v instanceof UpdateInput) {
        return modify_3(function(st) {
          var $76 = {};
          for (var $77 in st) {
            if ({}.hasOwnProperty.call(st, $77)) {
              $76[$77] = st[$77];
            }
            ;
          }
          ;
          $76.input = v.value0;
          return $76;
        });
      }
      ;
      if (v instanceof UpdateDefString) {
        return modify_3(function(st) {
          var $80 = {};
          for (var $81 in st) {
            if ({}.hasOwnProperty.call(st, $81)) {
              $80[$81] = st[$81];
            }
            ;
          }
          ;
          $80.defsString = v.value0;
          return $80;
        });
      }
      ;
      if (v instanceof ReloadDefs) {
        return bind10(get2)(function(st) {
          var v1 = runParser(st.defsString)(applyFirst5(many4(parseDef))(eof));
          if (v1 instanceof Left) {
            return discard6(updateTerminal1("Parse error: " + show6(v1.value0)))(function() {
              return updateTerminal1("In: " + show14(st.defsString));
            });
          }
          ;
          if (v1 instanceof Right) {
            return modify_3(function(st1) {
              var $86 = {};
              for (var $87 in st1) {
                if ({}.hasOwnProperty.call(st1, $87)) {
                  $86[$87] = st1[$87];
                }
                ;
              }
              ;
              $86.defs = v1.value0;
              return $86;
            });
          }
          ;
          throw new Error("Failed pattern match at Main (line 169, column 5 - line 174, column 44): " + [v1.constructor.name]);
        });
      }
      ;
      if (v instanceof ExecuteCommand) {
        return bind10(get2)(function(st) {
          return discard6(liftEffect7(log("history = " + show23(st.history))))(function() {
            return discard6(modify_3(function(st1) {
              var $90 = {};
              for (var $91 in st1) {
                if ({}.hasOwnProperty.call(st1, $91)) {
                  $90[$91] = st1[$91];
                }
                ;
              }
              ;
              $90.history = append8(st1.history)([prompt + st1.input]);
              return $90;
            }))(function() {
              return discard6(runInput1(st.input))(function() {
                return refocus1;
              });
            });
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Main (line 160, column 16 - line 182, column 12): " + [v.constructor.name]);
    };
  };
  var defsRef = "defsRef";
  var defaultDefString = /* @__PURE__ */ unlines(["zero := \\f. \\x. x;", "succ := \\n. (\\f. (\\x. (f (n f x))));", "add := \\m. \\n. \\f. \\x. m f (n f x);", "mult := \\m. \\n. \\f. \\x. m (n f) x;", "exp := \\m. \\n. n m;", "", "one := succ zero;", "two := succ one;", "three := succ two;", "four := succ three;", "five := succ four;", "six := succ five;", "", "pred :=", "  \\n. \\f. \\x.", "  n (\\g. \\h. h (g f)) (\\u. x) (\\u. u);", "", "minus :=", "  \\m. \\n. n pred m;", "", "", "id := \\x. x;", "const := \\x. \\y. x;", "", "Y := \\f. (\\x. f (\\v. x x v)) (\\x. f (\\v. x x v));", "", "true := \\t. \\f. t;", "false := \\t. \\f. f;", "and := \\p. \\q. p q p;", "or := \\p. \\q. p p q;", "if := \\p. \\t. \\f. p t f;", "", "isZero := \\n. n (\\x. false) true;", "", "equal :=", "  \\x. \\y.", "  Y (\\f.", "    \\m. \\n.", "    if (and (isZero m) (isZero n))", "      (\\v. true)", "      (if (or (isZero m) (isZero n))", "         (\\v. false)", "         (\\v. f (pred m) (pred n) v)))", "  x y dummy;", "", "factorial :=", "  \\arg.", "  Y (\\f.", "      \\n.", "      if (isZero n)", "        (\\v. one)", "        (\\v. mult n (f (pred n) v)))", "   arg dummy; -- The dummy argument is just because we need a thunk, since we are call-by-value", "", "fibonacci :=", "  \\arg.", "  Y (\\f.", "    \\n.", "    if (isZero n)", "      (\\v. one)", "      (if (isZero (pred n))  -- n = 1?", "        (\\v. one)", "        (\\v. add (f (pred n) v) (f (pred (pred n)) v))))", "   arg dummy;", "", "lessEqual :=", "  \\n.", "  \\m.", "  Y (\\f.", "    \\x.", "    \\y.", "    if (isZero x)", "       (\\v. true)", "       (if (isZero y)", "           (\\v. false)", "           (\\v. f (pred x) (pred y) v)))", "  n", "  m", "  dummy;", "", "mod :=", "  \\n.", "  \\m.", "  Y (\\f.", "    \\x.", "    \\y.", "    if (lessThan x y)", "       (\\v. x)", "       (\\v. f (minus x y) v))", "  n", "  m", "  dummy;", "", "sumThreeFiveMultiples :=", "  \\n.", "  Y (\\f.", "    \\x.", "    if (isZero x)", "      (\\v. zero)", "      if (or (equal (mod x three) zero)", "             (equal (mod x five) zero))", "         (\\v. add x (f (pred x) v))", "         (\\v. f (pred x) v))", "  n dummy;", "", "ten := mult two five;", "oneThousand := mult ten (mult ten ten);", "", "projectEuler1 :=", "  sumThreeFiveMultiples oneThousand;"]);
  var initialState = /* @__PURE__ */ function() {
    return {
      input: "",
      history: [],
      message: "",
      defsString: defaultDefString,
      defs: Nil.value
    };
  }();
  var render = function(dictMonadAff) {
    return function(st) {
      return div_([h1([class_("toolHeader")])([text5("UTLC Evaluator")]), div2([id2("panels")])([div2([id2("definitionsPanel")])([h2([class_("panelHeader")])([text5("Definitions")]), textarea([id2("definitionsArea"), ref2(defsRef), rows4(10), value13(defaultDefString), onValueInput(UpdateDefString.create)]), button([onClick(function(v) {
        return ReloadDefs.value;
      }), id2("reloadButton")])([text5("Reload")])]), div2([id2("replPanel")])([h2([class_("panelHeader")])([text5("REPL")]), div2([id2("terminal"), ref2(terminalRef)])(append8(historyToHtml(st))([div2([class_("line"), id2("line1")])([span3([id2("promptSpan"), class_("prompt")])([text5(prompt)]), input2([id2("replInput"), type_19(InputText.value), class_("input"), onKeyDown(mkAction), ref2(replInputRef), onValueInput(updateInput), onClick(function(v) {
        return Focus.value;
      }), onLoad(function(v) {
        return Focus.value;
      })])])]))]), div2([id2("instructionsPanel")])([h2([class_("panelHeader")])([text5("Instructions")]), div2([class_("instructionsContent")])(instructions)])])]);
    };
  };
  var replComponent = function(dictMonadAff) {
    return mkComponent({
      initialState: function(v) {
        return initialState;
      },
      render: render(dictMonadAff),
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction(dictMonadAff),
        initialize: new Just(ReloadDefs.value)
      })
    });
  };
  var replComponent1 = /* @__PURE__ */ replComponent(monadAffAff);
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(replComponent1)(unit)(body2);
  }));

  // <stdin>
  main2();
})();
