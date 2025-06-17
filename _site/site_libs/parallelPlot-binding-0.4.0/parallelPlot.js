(() => {
  // node_modules/d3-array/src/ascending.js
  function ascending(a, b) {
    return a == null || b == null ? NaN : a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-array/src/descending.js
  function descending(a, b) {
    return a == null || b == null ? NaN : b < a ? -1 : b > a ? 1 : b >= a ? 0 : NaN;
  }

  // node_modules/d3-array/src/bisector.js
  function bisector(f) {
    let compare1, compare2, delta;
    if (f.length !== 2) {
      compare1 = ascending;
      compare2 = (d, x2) => ascending(f(d), x2);
      delta = (d, x2) => f(d) - x2;
    } else {
      compare1 = f === ascending || f === descending ? f : zero;
      compare2 = f;
      delta = f;
    }
    function left2(a, x2, lo = 0, hi = a.length) {
      if (lo < hi) {
        if (compare1(x2, x2) !== 0)
          return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a[mid], x2) < 0)
            lo = mid + 1;
          else
            hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function right2(a, x2, lo = 0, hi = a.length) {
      if (lo < hi) {
        if (compare1(x2, x2) !== 0)
          return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a[mid], x2) <= 0)
            lo = mid + 1;
          else
            hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function center2(a, x2, lo = 0, hi = a.length) {
      const i = left2(a, x2, lo, hi - 1);
      return i > lo && delta(a[i - 1], x2) > -delta(a[i], x2) ? i - 1 : i;
    }
    return { left: left2, center: center2, right: right2 };
  }
  function zero() {
    return 0;
  }

  // node_modules/d3-array/src/number.js
  function number(x2) {
    return x2 === null ? NaN : +x2;
  }

  // node_modules/d3-array/src/bisect.js
  var ascendingBisect = bisector(ascending);
  var bisectRight = ascendingBisect.right;
  var bisectLeft = ascendingBisect.left;
  var bisectCenter = bisector(number).center;
  var bisect_default = bisectRight;

  // node_modules/d3-array/src/count.js
  function count(values, valueof) {
    let count2 = 0;
    if (valueof === void 0) {
      for (let value of values) {
        if (value != null && (value = +value) >= value) {
          ++count2;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (value = +value) >= value) {
          ++count2;
        }
      }
    }
    return count2;
  }

  // node_modules/d3-array/src/extent.js
  function extent(values, valueof) {
    let min2;
    let max3;
    if (valueof === void 0) {
      for (const value of values) {
        if (value != null) {
          if (min2 === void 0) {
            if (value >= value)
              min2 = max3 = value;
          } else {
            if (min2 > value)
              min2 = value;
            if (max3 < value)
              max3 = value;
          }
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null) {
          if (min2 === void 0) {
            if (value >= value)
              min2 = max3 = value;
          } else {
            if (min2 > value)
              min2 = value;
            if (max3 < value)
              max3 = value;
          }
        }
      }
    }
    return [min2, max3];
  }

  // node_modules/internmap/src/index.js
  var InternMap = class extends Map {
    constructor(entries, key = keyof) {
      super();
      Object.defineProperties(this, { _intern: { value: /* @__PURE__ */ new Map() }, _key: { value: key } });
      if (entries != null)
        for (const [key2, value] of entries)
          this.set(key2, value);
    }
    get(key) {
      return super.get(intern_get(this, key));
    }
    has(key) {
      return super.has(intern_get(this, key));
    }
    set(key, value) {
      return super.set(intern_set(this, key), value);
    }
    delete(key) {
      return super.delete(intern_delete(this, key));
    }
  };
  function intern_get({ _intern, _key }, value) {
    const key = _key(value);
    return _intern.has(key) ? _intern.get(key) : value;
  }
  function intern_set({ _intern, _key }, value) {
    const key = _key(value);
    if (_intern.has(key))
      return _intern.get(key);
    _intern.set(key, value);
    return value;
  }
  function intern_delete({ _intern, _key }, value) {
    const key = _key(value);
    if (_intern.has(key)) {
      value = _intern.get(key);
      _intern.delete(key);
    }
    return value;
  }
  function keyof(value) {
    return value !== null && typeof value === "object" ? value.valueOf() : value;
  }

  // node_modules/d3-array/src/identity.js
  function identity(x2) {
    return x2;
  }

  // node_modules/d3-array/src/array.js
  var array = Array.prototype;
  var slice = array.slice;
  var map = array.map;

  // node_modules/d3-array/src/constant.js
  function constant(x2) {
    return () => x2;
  }

  // node_modules/d3-array/src/ticks.js
  var e10 = Math.sqrt(50);
  var e5 = Math.sqrt(10);
  var e2 = Math.sqrt(2);
  function tickSpec(start2, stop, count2) {
    const step = (stop - start2) / Math.max(0, count2), power = Math.floor(Math.log10(step)), error = step / Math.pow(10, power), factor = error >= e10 ? 10 : error >= e5 ? 5 : error >= e2 ? 2 : 1;
    let i1, i2, inc;
    if (power < 0) {
      inc = Math.pow(10, -power) / factor;
      i1 = Math.round(start2 * inc);
      i2 = Math.round(stop * inc);
      if (i1 / inc < start2)
        ++i1;
      if (i2 / inc > stop)
        --i2;
      inc = -inc;
    } else {
      inc = Math.pow(10, power) * factor;
      i1 = Math.round(start2 / inc);
      i2 = Math.round(stop / inc);
      if (i1 * inc < start2)
        ++i1;
      if (i2 * inc > stop)
        --i2;
    }
    if (i2 < i1 && 0.5 <= count2 && count2 < 2)
      return tickSpec(start2, stop, count2 * 2);
    return [i1, i2, inc];
  }
  function ticks(start2, stop, count2) {
    stop = +stop, start2 = +start2, count2 = +count2;
    if (!(count2 > 0))
      return [];
    if (start2 === stop)
      return [start2];
    const reverse = stop < start2, [i1, i2, inc] = reverse ? tickSpec(stop, start2, count2) : tickSpec(start2, stop, count2);
    if (!(i2 >= i1))
      return [];
    const n = i2 - i1 + 1, ticks2 = new Array(n);
    if (reverse) {
      if (inc < 0)
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i2 - i) / -inc;
      else
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i2 - i) * inc;
    } else {
      if (inc < 0)
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i1 + i) / -inc;
      else
        for (let i = 0; i < n; ++i)
          ticks2[i] = (i1 + i) * inc;
    }
    return ticks2;
  }
  function tickIncrement(start2, stop, count2) {
    stop = +stop, start2 = +start2, count2 = +count2;
    return tickSpec(start2, stop, count2)[2];
  }
  function tickStep(start2, stop, count2) {
    stop = +stop, start2 = +start2, count2 = +count2;
    const reverse = stop < start2, inc = reverse ? tickIncrement(stop, start2, count2) : tickIncrement(start2, stop, count2);
    return (reverse ? -1 : 1) * (inc < 0 ? 1 / -inc : inc);
  }

  // node_modules/d3-array/src/nice.js
  function nice(start2, stop, count2) {
    let prestep;
    while (true) {
      const step = tickIncrement(start2, stop, count2);
      if (step === prestep || step === 0 || !isFinite(step)) {
        return [start2, stop];
      } else if (step > 0) {
        start2 = Math.floor(start2 / step) * step;
        stop = Math.ceil(stop / step) * step;
      } else if (step < 0) {
        start2 = Math.ceil(start2 * step) / step;
        stop = Math.floor(stop * step) / step;
      }
      prestep = step;
    }
  }

  // node_modules/d3-array/src/threshold/sturges.js
  function thresholdSturges(values) {
    return Math.max(1, Math.ceil(Math.log(count(values)) / Math.LN2) + 1);
  }

  // node_modules/d3-array/src/bin.js
  function bin() {
    var value = identity, domain = extent, threshold = thresholdSturges;
    function histogram(data) {
      if (!Array.isArray(data))
        data = Array.from(data);
      var i, n = data.length, x2, step, values = new Array(n);
      for (i = 0; i < n; ++i) {
        values[i] = value(data[i], i, data);
      }
      var xz = domain(values), x0 = xz[0], x1 = xz[1], tz = threshold(values, x0, x1);
      if (!Array.isArray(tz)) {
        const max3 = x1, tn = +tz;
        if (domain === extent)
          [x0, x1] = nice(x0, x1, tn);
        tz = ticks(x0, x1, tn);
        if (tz[0] <= x0)
          step = tickIncrement(x0, x1, tn);
        if (tz[tz.length - 1] >= x1) {
          if (max3 >= x1 && domain === extent) {
            const step2 = tickIncrement(x0, x1, tn);
            if (isFinite(step2)) {
              if (step2 > 0) {
                x1 = (Math.floor(x1 / step2) + 1) * step2;
              } else if (step2 < 0) {
                x1 = (Math.ceil(x1 * -step2) + 1) / -step2;
              }
            }
          } else {
            tz.pop();
          }
        }
      }
      var m = tz.length, a = 0, b = m;
      while (tz[a] <= x0)
        ++a;
      while (tz[b - 1] > x1)
        --b;
      if (a || b < m)
        tz = tz.slice(a, b), m = b - a;
      var bins = new Array(m + 1), bin2;
      for (i = 0; i <= m; ++i) {
        bin2 = bins[i] = [];
        bin2.x0 = i > 0 ? tz[i - 1] : x0;
        bin2.x1 = i < m ? tz[i] : x1;
      }
      if (isFinite(step)) {
        if (step > 0) {
          for (i = 0; i < n; ++i) {
            if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
              bins[Math.min(m, Math.floor((x2 - x0) / step))].push(data[i]);
            }
          }
        } else if (step < 0) {
          for (i = 0; i < n; ++i) {
            if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
              const j = Math.floor((x0 - x2) * step);
              bins[Math.min(m, j + (tz[j] <= x2))].push(data[i]);
            }
          }
        }
      } else {
        for (i = 0; i < n; ++i) {
          if ((x2 = values[i]) != null && x0 <= x2 && x2 <= x1) {
            bins[bisect_default(tz, x2, 0, m)].push(data[i]);
          }
        }
      }
      return bins;
    }
    histogram.value = function(_) {
      return arguments.length ? (value = typeof _ === "function" ? _ : constant(_), histogram) : value;
    };
    histogram.domain = function(_) {
      return arguments.length ? (domain = typeof _ === "function" ? _ : constant([_[0], _[1]]), histogram) : domain;
    };
    histogram.thresholds = function(_) {
      return arguments.length ? (threshold = typeof _ === "function" ? _ : constant(Array.isArray(_) ? slice.call(_) : _), histogram) : threshold;
    };
    return histogram;
  }

  // node_modules/d3-array/src/max.js
  function max(values, valueof) {
    let max3;
    if (valueof === void 0) {
      for (const value of values) {
        if (value != null && (max3 < value || max3 === void 0 && value >= value)) {
          max3 = value;
        }
      }
    } else {
      let index = -1;
      for (let value of values) {
        if ((value = valueof(value, ++index, values)) != null && (max3 < value || max3 === void 0 && value >= value)) {
          max3 = value;
        }
      }
    }
    return max3;
  }

  // node_modules/d3-array/src/range.js
  function range(start2, stop, step) {
    start2 = +start2, stop = +stop, step = (n = arguments.length) < 2 ? (stop = start2, start2 = 0, 1) : n < 3 ? 1 : +step;
    var i = -1, n = Math.max(0, Math.ceil((stop - start2) / step)) | 0, range2 = new Array(n);
    while (++i < n) {
      range2[i] = start2 + i * step;
    }
    return range2;
  }

  // node_modules/d3-axis/src/identity.js
  function identity_default(x2) {
    return x2;
  }

  // node_modules/d3-axis/src/axis.js
  var top = 1;
  var right = 2;
  var bottom = 3;
  var left = 4;
  var epsilon = 1e-6;
  function translateX(x2) {
    return "translate(" + x2 + ",0)";
  }
  function translateY(y2) {
    return "translate(0," + y2 + ")";
  }
  function number2(scale) {
    return (d) => +scale(d);
  }
  function center(scale, offset) {
    offset = Math.max(0, scale.bandwidth() - offset * 2) / 2;
    if (scale.round())
      offset = Math.round(offset);
    return (d) => +scale(d) + offset;
  }
  function entering() {
    return !this.__axis;
  }
  function axis(orient, scale) {
    var tickArguments = [], tickValues = null, tickFormat2 = null, tickSizeInner = 6, tickSizeOuter = 6, tickPadding = 3, offset = typeof window !== "undefined" && window.devicePixelRatio > 1 ? 0 : 0.5, k = orient === top || orient === left ? -1 : 1, x2 = orient === left || orient === right ? "x" : "y", transform2 = orient === top || orient === bottom ? translateX : translateY;
    function axis2(context) {
      var values = tickValues == null ? scale.ticks ? scale.ticks.apply(scale, tickArguments) : scale.domain() : tickValues, format2 = tickFormat2 == null ? scale.tickFormat ? scale.tickFormat.apply(scale, tickArguments) : identity_default : tickFormat2, spacing = Math.max(tickSizeInner, 0) + tickPadding, range2 = scale.range(), range0 = +range2[0] + offset, range1 = +range2[range2.length - 1] + offset, position = (scale.bandwidth ? center : number2)(scale.copy(), offset), selection2 = context.selection ? context.selection() : context, path2 = selection2.selectAll(".domain").data([null]), tick = selection2.selectAll(".tick").data(values, scale).order(), tickExit = tick.exit(), tickEnter = tick.enter().append("g").attr("class", "tick"), line = tick.select("line"), text = tick.select("text");
      path2 = path2.merge(path2.enter().insert("path", ".tick").attr("class", "domain").attr("stroke", "currentColor"));
      tick = tick.merge(tickEnter);
      line = line.merge(tickEnter.append("line").attr("stroke", "currentColor").attr(x2 + "2", k * tickSizeInner));
      text = text.merge(tickEnter.append("text").attr("fill", "currentColor").attr(x2, k * spacing).attr("dy", orient === top ? "0em" : orient === bottom ? "0.71em" : "0.32em"));
      if (context !== selection2) {
        path2 = path2.transition(context);
        tick = tick.transition(context);
        line = line.transition(context);
        text = text.transition(context);
        tickExit = tickExit.transition(context).attr("opacity", epsilon).attr("transform", function(d) {
          return isFinite(d = position(d)) ? transform2(d + offset) : this.getAttribute("transform");
        });
        tickEnter.attr("opacity", epsilon).attr("transform", function(d) {
          var p = this.parentNode.__axis;
          return transform2((p && isFinite(p = p(d)) ? p : position(d)) + offset);
        });
      }
      tickExit.remove();
      path2.attr("d", orient === left || orient === right ? tickSizeOuter ? "M" + k * tickSizeOuter + "," + range0 + "H" + offset + "V" + range1 + "H" + k * tickSizeOuter : "M" + offset + "," + range0 + "V" + range1 : tickSizeOuter ? "M" + range0 + "," + k * tickSizeOuter + "V" + offset + "H" + range1 + "V" + k * tickSizeOuter : "M" + range0 + "," + offset + "H" + range1);
      tick.attr("opacity", 1).attr("transform", function(d) {
        return transform2(position(d) + offset);
      });
      line.attr(x2 + "2", k * tickSizeInner);
      text.attr(x2, k * spacing).text(format2);
      selection2.filter(entering).attr("fill", "none").attr("font-size", 10).attr("font-family", "sans-serif").attr("text-anchor", orient === right ? "start" : orient === left ? "end" : "middle");
      selection2.each(function() {
        this.__axis = position;
      });
    }
    axis2.scale = function(_) {
      return arguments.length ? (scale = _, axis2) : scale;
    };
    axis2.ticks = function() {
      return tickArguments = Array.from(arguments), axis2;
    };
    axis2.tickArguments = function(_) {
      return arguments.length ? (tickArguments = _ == null ? [] : Array.from(_), axis2) : tickArguments.slice();
    };
    axis2.tickValues = function(_) {
      return arguments.length ? (tickValues = _ == null ? null : Array.from(_), axis2) : tickValues && tickValues.slice();
    };
    axis2.tickFormat = function(_) {
      return arguments.length ? (tickFormat2 = _, axis2) : tickFormat2;
    };
    axis2.tickSize = function(_) {
      return arguments.length ? (tickSizeInner = tickSizeOuter = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeInner = function(_) {
      return arguments.length ? (tickSizeInner = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeOuter = function(_) {
      return arguments.length ? (tickSizeOuter = +_, axis2) : tickSizeOuter;
    };
    axis2.tickPadding = function(_) {
      return arguments.length ? (tickPadding = +_, axis2) : tickPadding;
    };
    axis2.offset = function(_) {
      return arguments.length ? (offset = +_, axis2) : offset;
    };
    return axis2;
  }
  function axisBottom(scale) {
    return axis(bottom, scale);
  }
  function axisLeft(scale) {
    return axis(left, scale);
  }

  // node_modules/d3-dispatch/src/dispatch.js
  var noop = { value: () => {
  } };
  function dispatch() {
    for (var i = 0, n = arguments.length, _ = {}, t; i < n; ++i) {
      if (!(t = arguments[i] + "") || t in _ || /[\s.]/.test(t))
        throw new Error("illegal type: " + t);
      _[t] = [];
    }
    return new Dispatch(_);
  }
  function Dispatch(_) {
    this._ = _;
  }
  function parseTypenames(typenames, types) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name = "", i = t.indexOf(".");
      if (i >= 0)
        name = t.slice(i + 1), t = t.slice(0, i);
      if (t && !types.hasOwnProperty(t))
        throw new Error("unknown type: " + t);
      return { type: t, name };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i < n)
          if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name)))
            return t;
        return;
      }
      if (callback != null && typeof callback !== "function")
        throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type)
          _[t] = set(_[t], typename.name, callback);
        else if (callback == null)
          for (t in _)
            _[t] = set(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy3 = {}, _ = this._;
      for (var t in _)
        copy3[t] = _[t].slice();
      return new Dispatch(copy3);
    },
    call: function(type2, that) {
      if ((n = arguments.length - 2) > 0)
        for (var args = new Array(n), i = 0, n, t; i < n; ++i)
          args[i] = arguments[i + 2];
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    },
    apply: function(type2, that, args) {
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (var t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    }
  };
  function get(type2, name) {
    for (var i = 0, n = type2.length, c2; i < n; ++i) {
      if ((c2 = type2[i]).name === name) {
        return c2.value;
      }
    }
  }
  function set(type2, name, callback) {
    for (var i = 0, n = type2.length; i < n; ++i) {
      if (type2[i].name === name) {
        type2[i] = noop, type2 = type2.slice(0, i).concat(type2.slice(i + 1));
        break;
      }
    }
    if (callback != null)
      type2.push({ name, value: callback });
    return type2;
  }
  var dispatch_default = dispatch;

  // node_modules/d3-selection/src/namespaces.js
  var xhtml = "http://www.w3.org/1999/xhtml";
  var namespaces_default = {
    svg: "http://www.w3.org/2000/svg",
    xhtml,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-selection/src/namespace.js
  function namespace_default(name) {
    var prefix = name += "", i = prefix.indexOf(":");
    if (i >= 0 && (prefix = name.slice(0, i)) !== "xmlns")
      name = name.slice(i + 1);
    return namespaces_default.hasOwnProperty(prefix) ? { space: namespaces_default[prefix], local: name } : name;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document2.documentElement.namespaceURI === xhtml ? document2.createElement(name) : document2.createElementNS(uri, name);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name) {
    var fullname = namespace_default(name);
    return (fullname.local ? creatorFixed : creatorInherit)(fullname);
  }

  // node_modules/d3-selection/src/selector.js
  function none() {
  }
  function selector_default(selector) {
    return selector == null ? none : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-selection/src/selection/select.js
  function select_default(select) {
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group[i]) && (subnode = select.call(node, node.__data__, i, group))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/array.js
  function array2(x2) {
    return x2 == null ? [] : Array.isArray(x2) ? x2 : Array.from(x2);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectAll.js
  function arrayAll(select) {
    return function() {
      return array2(select.apply(this, arguments));
    };
  }
  function selectAll_default(select) {
    if (typeof select === "function")
      select = arrayAll(select);
    else
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          subgroups.push(select.call(node, node.__data__, i, group));
          parents.push(node);
        }
      }
    }
    return new Selection(subgroups, parents);
  }

  // node_modules/d3-selection/src/matcher.js
  function matcher_default(selector) {
    return function() {
      return this.matches(selector);
    };
  }
  function childMatcher(selector) {
    return function(node) {
      return node.matches(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectChild.js
  var find = Array.prototype.find;
  function childFind(match) {
    return function() {
      return find.call(this.children, match);
    };
  }
  function childFirst() {
    return this.firstElementChild;
  }
  function selectChild_default(match) {
    return this.select(match == null ? childFirst : childFind(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/selectChildren.js
  var filter = Array.prototype.filter;
  function children() {
    return Array.from(this.children);
  }
  function childrenFilter(match) {
    return function() {
      return filter.call(this.children, match);
    };
  }
  function selectChildren_default(match) {
    return this.selectAll(match == null ? children : childrenFilter(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/filter.js
  function filter_default(match) {
    if (typeof match !== "function")
      match = matcher_default(match);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group[i]) && match.call(node, node.__data__, i, group)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/selection/sparse.js
  function sparse_default(update) {
    return new Array(update.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent, group, enter, update, exit, data) {
    var i = 0, node, groupLength = group.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group[i]) {
        node.__data__ = data[i];
        update[i] = node;
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey(parent, group, enter, update, exit, data, key) {
    var i, node, nodeByKeyValue = /* @__PURE__ */ new Map(), groupLength = group.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group[i]) {
        keyValues[i] = keyValue = key.call(node, node.__data__, i, group) + "";
        if (nodeByKeyValue.has(keyValue)) {
          exit[i] = node;
        } else {
          nodeByKeyValue.set(keyValue, node);
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = key.call(parent, data[i], i, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group[i]) && nodeByKeyValue.get(keyValues[i]) === node) {
        exit[i] = node;
      }
    }
  }
  function datum(node) {
    return node.__data__;
  }
  function data_default(value, key) {
    if (!arguments.length)
      return Array.from(this, datum);
    var bind = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value !== "function")
      value = constant_default(value);
    for (var m = groups.length, update = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent = parents[j], group = groups[j], groupLength = group.length, data = arraylike(value.call(parent, parent && parent.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind(parent, group, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update = new Selection(update, parents);
    update._enter = enter;
    update._exit = exit;
    return update;
  }
  function arraylike(data) {
    return typeof data === "object" && "length" in data ? data : Array.from(data);
  }

  // node_modules/d3-selection/src/selection/exit.js
  function exit_default() {
    return new Selection(this._exit || this._groups.map(sparse_default), this._parents);
  }

  // node_modules/d3-selection/src/selection/join.js
  function join_default(onenter, onupdate, onexit) {
    var enter = this.enter(), update = this, exit = this.exit();
    if (typeof onenter === "function") {
      enter = onenter(enter);
      if (enter)
        enter = enter.selection();
    } else {
      enter = enter.append(onenter + "");
    }
    if (onupdate != null) {
      update = onupdate(update);
      if (update)
        update = update.selection();
    }
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update ? enter.merge(update).order() : update;
  }

  // node_modules/d3-selection/src/selection/merge.js
  function merge_default(context) {
    var selection2 = context.selection ? context.selection() : context;
    for (var groups0 = this._groups, groups1 = selection2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection(merges, this._parents);
  }

  // node_modules/d3-selection/src/selection/order.js
  function order_default() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group = groups[j], i = group.length - 1, next = group[i], node; --i >= 0; ) {
        if (node = group[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare) {
    if (!compare)
      compare = ascending2;
    function compareNode(a, b) {
      return a && b ? compare(a.__data__, b.__data__) : !a - !b;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection(sortgroups, this._parents).order();
  }
  function ascending2(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-selection/src/selection/call.js
  function call_default() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-selection/src/selection/nodes.js
  function nodes_default() {
    return Array.from(this);
  }

  // node_modules/d3-selection/src/selection/node.js
  function node_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group = groups[j], i = 0, n = group.length; i < n; ++i) {
        var node = group[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-selection/src/selection/size.js
  function size_default() {
    let size = 0;
    for (const node of this)
      ++size;
    return size;
  }

  // node_modules/d3-selection/src/selection/empty.js
  function empty_default() {
    return !this.node();
  }

  // node_modules/d3-selection/src/selection/each.js
  function each_default(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group = groups[j], i = 0, n = group.length, node; i < n; ++i) {
        if (node = group[i])
          callback.call(node, node.__data__, i, group);
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/attr.js
  function attrRemove(name) {
    return function() {
      this.removeAttribute(name);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name, value) {
    return function() {
      this.setAttribute(name, value);
    };
  }
  function attrConstantNS(fullname, value) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value);
    };
  }
  function attrFunction(name, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name);
      else
        this.setAttribute(name, v);
    };
  }
  function attrFunctionNS(fullname, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name, value) {
    var fullname = namespace_default(name);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name) {
    return function() {
      this.style.removeProperty(name);
    };
  }
  function styleConstant(name, value, priority) {
    return function() {
      this.style.setProperty(name, value, priority);
    };
  }
  function styleFunction(name, value, priority) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name);
      else
        this.style.setProperty(name, v, priority);
    };
  }
  function style_default(name, value, priority) {
    return arguments.length > 1 ? this.each((value == null ? styleRemove : typeof value === "function" ? styleFunction : styleConstant)(name, value, priority == null ? "" : priority)) : styleValue(this.node(), name);
  }
  function styleValue(node, name) {
    return node.style.getPropertyValue(name) || window_default(node).getComputedStyle(node, null).getPropertyValue(name);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name) {
    return function() {
      delete this[name];
    };
  }
  function propertyConstant(name, value) {
    return function() {
      this[name] = value;
    };
  }
  function propertyFunction(name, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (v == null)
        delete this[name];
      else
        this[name] = v;
    };
  }
  function property_default(name, value) {
    return arguments.length > 1 ? this.each((value == null ? propertyRemove : typeof value === "function" ? propertyFunction : propertyConstant)(name, value)) : this.node()[name];
  }

  // node_modules/d3-selection/src/selection/classed.js
  function classArray(string) {
    return string.trim().split(/^|\s+/);
  }
  function classList(node) {
    return node.classList || new ClassList(node);
  }
  function ClassList(node) {
    this._node = node;
    this._names = classArray(node.getAttribute("class") || "");
  }
  ClassList.prototype = {
    add: function(name) {
      var i = this._names.indexOf(name);
      if (i < 0) {
        this._names.push(name);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name) {
      var i = this._names.indexOf(name);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name) {
      return this._names.indexOf(name) >= 0;
    }
  };
  function classedAdd(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue(names) {
    return function() {
      classedAdd(this, names);
    };
  }
  function classedFalse(names) {
    return function() {
      classedRemove(this, names);
    };
  }
  function classedFunction(names, value) {
    return function() {
      (value.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name, value) {
    var names = classArray(name + "");
    if (arguments.length < 2) {
      var list = classList(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value === "function" ? classedFunction : value ? classedTrue : classedFalse)(names, value));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value) {
    return arguments.length ? this.each(value == null ? textRemove : (typeof value === "function" ? textFunction : textConstant)(value)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value) {
    return function() {
      this.innerHTML = value;
    };
  }
  function htmlFunction(value) {
    return function() {
      var v = value.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value) {
    return arguments.length ? this.each(value == null ? htmlRemove : (typeof value === "function" ? htmlFunction : htmlConstant)(value)) : this.node().innerHTML;
  }

  // node_modules/d3-selection/src/selection/raise.js
  function raise() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default() {
    return this.each(raise);
  }

  // node_modules/d3-selection/src/selection/lower.js
  function lower() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default() {
    return this.each(lower);
  }

  // node_modules/d3-selection/src/selection/append.js
  function append_default(name) {
    var create2 = typeof name === "function" ? name : creator_default(name);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name, before) {
    var create2 = typeof name === "function" ? name : creator_default(name), select = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default() {
    return this.each(remove);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value) {
    return arguments.length ? this.property("__data__", value) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event) {
      listener.call(this, event, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name = "", i = t.indexOf(".");
      if (i >= 0)
        name = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name };
    });
  }
  function onRemove(typename) {
    return function() {
      var on = this.__on;
      if (!on)
        return;
      for (var j = 0, i = -1, m = on.length, o; j < m; ++j) {
        if (o = on[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on[++i] = o;
        }
      }
      if (++i)
        on.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd(typename, value, options) {
    return function() {
      var on = this.__on, o, listener = contextListener(value);
      if (on)
        for (var j = 0, m = on.length; j < m; ++j) {
          if ((o = on[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.options);
            this.addEventListener(o.type, o.listener = listener, o.options = options);
            o.value = value;
            return;
          }
        }
      this.addEventListener(typename.type, listener, options);
      o = { type: typename.type, name: typename.name, value, listener, options };
      if (!on)
        this.__on = [o];
      else
        on.push(o);
    };
  }
  function on_default(typename, value, options) {
    var typenames = parseTypenames2(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on = this.node().__on;
      if (on)
        for (var j = 0, m = on.length, o; j < m; ++j) {
          for (i = 0, o = on[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on = value ? onAdd : onRemove;
    for (i = 0; i < n; ++i)
      this.each(on(typenames[i], value, options));
    return this;
  }

  // node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent(node, type2, params) {
    var window2 = window_default(node), event = window2.CustomEvent;
    if (typeof event === "function") {
      event = new event(type2, params);
    } else {
      event = window2.document.createEvent("Event");
      if (params)
        event.initEvent(type2, params.bubbles, params.cancelable), event.detail = params.detail;
      else
        event.initEvent(type2, false, false);
    }
    node.dispatchEvent(event);
  }
  function dispatchConstant(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params);
    };
  }
  function dispatchFunction(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default2(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction : dispatchConstant)(type2, params));
  }

  // node_modules/d3-selection/src/selection/iterator.js
  function* iterator_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group = groups[j], i = 0, n = group.length, node; i < n; ++i) {
        if (node = group[i])
          yield node;
      }
    }
  }

  // node_modules/d3-selection/src/selection/index.js
  var root = [null];
  function Selection(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection() {
    return new Selection([[document.documentElement]], root);
  }
  function selection_selection() {
    return this;
  }
  Selection.prototype = selection.prototype = {
    constructor: Selection,
    select: select_default,
    selectAll: selectAll_default,
    selectChild: selectChild_default,
    selectChildren: selectChildren_default,
    filter: filter_default,
    data: data_default,
    enter: enter_default,
    exit: exit_default,
    join: join_default,
    merge: merge_default,
    selection: selection_selection,
    order: order_default,
    sort: sort_default,
    call: call_default,
    nodes: nodes_default,
    node: node_default,
    size: size_default,
    empty: empty_default,
    each: each_default,
    attr: attr_default,
    style: style_default,
    property: property_default,
    classed: classed_default,
    text: text_default,
    html: html_default,
    raise: raise_default,
    lower: lower_default,
    append: append_default,
    insert: insert_default,
    remove: remove_default,
    clone: clone_default,
    datum: datum_default,
    on: on_default,
    dispatch: dispatch_default2,
    [Symbol.iterator]: iterator_default
  };
  var selection_default = selection;

  // node_modules/d3-selection/src/select.js
  function select_default2(selector) {
    return typeof selector === "string" ? new Selection([[document.querySelector(selector)]], [document.documentElement]) : new Selection([[selector]], root);
  }

  // node_modules/d3-selection/src/sourceEvent.js
  function sourceEvent_default(event) {
    let sourceEvent;
    while (sourceEvent = event.sourceEvent)
      event = sourceEvent;
    return event;
  }

  // node_modules/d3-selection/src/pointer.js
  function pointer_default(event, node) {
    event = sourceEvent_default(event);
    if (node === void 0)
      node = event.currentTarget;
    if (node) {
      var svg = node.ownerSVGElement || node;
      if (svg.createSVGPoint) {
        var point2 = svg.createSVGPoint();
        point2.x = event.clientX, point2.y = event.clientY;
        point2 = point2.matrixTransform(node.getScreenCTM().inverse());
        return [point2.x, point2.y];
      }
      if (node.getBoundingClientRect) {
        var rect = node.getBoundingClientRect();
        return [event.clientX - rect.left - node.clientLeft, event.clientY - rect.top - node.clientTop];
      }
    }
    return [event.pageX, event.pageY];
  }

  // node_modules/d3-selection/src/selectAll.js
  function selectAll_default2(selector) {
    return typeof selector === "string" ? new Selection([document.querySelectorAll(selector)], [document.documentElement]) : new Selection([array2(selector)], root);
  }

  // node_modules/d3-drag/src/noevent.js
  var nonpassive = { passive: false };
  var nonpassivecapture = { capture: true, passive: false };
  function nopropagation(event) {
    event.stopImmediatePropagation();
  }
  function noevent_default(event) {
    event.preventDefault();
    event.stopImmediatePropagation();
  }

  // node_modules/d3-drag/src/nodrag.js
  function nodrag_default(view) {
    var root2 = view.document.documentElement, selection2 = select_default2(view).on("dragstart.drag", noevent_default, nonpassivecapture);
    if ("onselectstart" in root2) {
      selection2.on("selectstart.drag", noevent_default, nonpassivecapture);
    } else {
      root2.__noselect = root2.style.MozUserSelect;
      root2.style.MozUserSelect = "none";
    }
  }
  function yesdrag(view, noclick) {
    var root2 = view.document.documentElement, selection2 = select_default2(view).on("dragstart.drag", null);
    if (noclick) {
      selection2.on("click.drag", noevent_default, nonpassivecapture);
      setTimeout(function() {
        selection2.on("click.drag", null);
      }, 0);
    }
    if ("onselectstart" in root2) {
      selection2.on("selectstart.drag", null);
    } else {
      root2.style.MozUserSelect = root2.__noselect;
      delete root2.__noselect;
    }
  }

  // node_modules/d3-drag/src/constant.js
  var constant_default2 = (x2) => () => x2;

  // node_modules/d3-drag/src/event.js
  function DragEvent(type2, {
    sourceEvent,
    subject,
    target,
    identifier,
    active,
    x: x2,
    y: y2,
    dx,
    dy,
    dispatch: dispatch2
  }) {
    Object.defineProperties(this, {
      type: { value: type2, enumerable: true, configurable: true },
      sourceEvent: { value: sourceEvent, enumerable: true, configurable: true },
      subject: { value: subject, enumerable: true, configurable: true },
      target: { value: target, enumerable: true, configurable: true },
      identifier: { value: identifier, enumerable: true, configurable: true },
      active: { value: active, enumerable: true, configurable: true },
      x: { value: x2, enumerable: true, configurable: true },
      y: { value: y2, enumerable: true, configurable: true },
      dx: { value: dx, enumerable: true, configurable: true },
      dy: { value: dy, enumerable: true, configurable: true },
      _: { value: dispatch2 }
    });
  }
  DragEvent.prototype.on = function() {
    var value = this._.on.apply(this._, arguments);
    return value === this._ ? this : value;
  };

  // node_modules/d3-drag/src/drag.js
  function defaultFilter(event) {
    return !event.ctrlKey && !event.button;
  }
  function defaultContainer() {
    return this.parentNode;
  }
  function defaultSubject(event, d) {
    return d == null ? { x: event.x, y: event.y } : d;
  }
  function defaultTouchable() {
    return navigator.maxTouchPoints || "ontouchstart" in this;
  }
  function drag_default() {
    var filter2 = defaultFilter, container = defaultContainer, subject = defaultSubject, touchable = defaultTouchable, gestures = {}, listeners = dispatch_default("start", "drag", "end"), active = 0, mousedownx, mousedowny, mousemoving, touchending, clickDistance2 = 0;
    function drag(selection2) {
      selection2.on("mousedown.drag", mousedowned).filter(touchable).on("touchstart.drag", touchstarted).on("touchmove.drag", touchmoved, nonpassive).on("touchend.drag touchcancel.drag", touchended).style("touch-action", "none").style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
    }
    function mousedowned(event, d) {
      if (touchending || !filter2.call(this, event, d))
        return;
      var gesture = beforestart(this, container.call(this, event, d), event, d, "mouse");
      if (!gesture)
        return;
      select_default2(event.view).on("mousemove.drag", mousemoved, nonpassivecapture).on("mouseup.drag", mouseupped, nonpassivecapture);
      nodrag_default(event.view);
      nopropagation(event);
      mousemoving = false;
      mousedownx = event.clientX;
      mousedowny = event.clientY;
      gesture("start", event);
    }
    function mousemoved(event) {
      noevent_default(event);
      if (!mousemoving) {
        var dx = event.clientX - mousedownx, dy = event.clientY - mousedowny;
        mousemoving = dx * dx + dy * dy > clickDistance2;
      }
      gestures.mouse("drag", event);
    }
    function mouseupped(event) {
      select_default2(event.view).on("mousemove.drag mouseup.drag", null);
      yesdrag(event.view, mousemoving);
      noevent_default(event);
      gestures.mouse("end", event);
    }
    function touchstarted(event, d) {
      if (!filter2.call(this, event, d))
        return;
      var touches = event.changedTouches, c2 = container.call(this, event, d), n = touches.length, i, gesture;
      for (i = 0; i < n; ++i) {
        if (gesture = beforestart(this, c2, event, d, touches[i].identifier, touches[i])) {
          nopropagation(event);
          gesture("start", event, touches[i]);
        }
      }
    }
    function touchmoved(event) {
      var touches = event.changedTouches, n = touches.length, i, gesture;
      for (i = 0; i < n; ++i) {
        if (gesture = gestures[touches[i].identifier]) {
          noevent_default(event);
          gesture("drag", event, touches[i]);
        }
      }
    }
    function touchended(event) {
      var touches = event.changedTouches, n = touches.length, i, gesture;
      if (touchending)
        clearTimeout(touchending);
      touchending = setTimeout(function() {
        touchending = null;
      }, 500);
      for (i = 0; i < n; ++i) {
        if (gesture = gestures[touches[i].identifier]) {
          nopropagation(event);
          gesture("end", event, touches[i]);
        }
      }
    }
    function beforestart(that, container2, event, d, identifier, touch) {
      var dispatch2 = listeners.copy(), p = pointer_default(touch || event, container2), dx, dy, s;
      if ((s = subject.call(that, new DragEvent("beforestart", {
        sourceEvent: event,
        target: drag,
        identifier,
        active,
        x: p[0],
        y: p[1],
        dx: 0,
        dy: 0,
        dispatch: dispatch2
      }), d)) == null)
        return;
      dx = s.x - p[0] || 0;
      dy = s.y - p[1] || 0;
      return function gesture(type2, event2, touch2) {
        var p0 = p, n;
        switch (type2) {
          case "start":
            gestures[identifier] = gesture, n = active++;
            break;
          case "end":
            delete gestures[identifier], --active;
          case "drag":
            p = pointer_default(touch2 || event2, container2), n = active;
            break;
        }
        dispatch2.call(
          type2,
          that,
          new DragEvent(type2, {
            sourceEvent: event2,
            subject: s,
            target: drag,
            identifier,
            active: n,
            x: p[0] + dx,
            y: p[1] + dy,
            dx: p[0] - p0[0],
            dy: p[1] - p0[1],
            dispatch: dispatch2
          }),
          d
        );
      };
    }
    drag.filter = function(_) {
      return arguments.length ? (filter2 = typeof _ === "function" ? _ : constant_default2(!!_), drag) : filter2;
    };
    drag.container = function(_) {
      return arguments.length ? (container = typeof _ === "function" ? _ : constant_default2(_), drag) : container;
    };
    drag.subject = function(_) {
      return arguments.length ? (subject = typeof _ === "function" ? _ : constant_default2(_), drag) : subject;
    };
    drag.touchable = function(_) {
      return arguments.length ? (touchable = typeof _ === "function" ? _ : constant_default2(!!_), drag) : touchable;
    };
    drag.on = function() {
      var value = listeners.on.apply(listeners, arguments);
      return value === listeners ? drag : value;
    };
    drag.clickDistance = function(_) {
      return arguments.length ? (clickDistance2 = (_ = +_) * _, drag) : Math.sqrt(clickDistance2);
    };
    return drag;
  }

  // node_modules/d3-color/src/define.js
  function define_default(constructor, factory, prototype) {
    constructor.prototype = factory.prototype = prototype;
    prototype.constructor = constructor;
  }
  function extend(parent, definition) {
    var prototype = Object.create(parent.prototype);
    for (var key in definition)
      prototype[key] = definition[key];
    return prototype;
  }

  // node_modules/d3-color/src/color.js
  function Color() {
  }
  var darker = 0.7;
  var brighter = 1 / darker;
  var reI = "\\s*([+-]?\\d+)\\s*";
  var reN = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)\\s*";
  var reP = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
  var reHex = /^#([0-9a-f]{3,8})$/;
  var reRgbInteger = new RegExp(`^rgb\\(${reI},${reI},${reI}\\)$`);
  var reRgbPercent = new RegExp(`^rgb\\(${reP},${reP},${reP}\\)$`);
  var reRgbaInteger = new RegExp(`^rgba\\(${reI},${reI},${reI},${reN}\\)$`);
  var reRgbaPercent = new RegExp(`^rgba\\(${reP},${reP},${reP},${reN}\\)$`);
  var reHslPercent = new RegExp(`^hsl\\(${reN},${reP},${reP}\\)$`);
  var reHslaPercent = new RegExp(`^hsla\\(${reN},${reP},${reP},${reN}\\)$`);
  var named = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074
  };
  define_default(Color, color, {
    copy(channels) {
      return Object.assign(new this.constructor(), this, channels);
    },
    displayable() {
      return this.rgb().displayable();
    },
    hex: color_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: color_formatHex,
    formatHex8: color_formatHex8,
    formatHsl: color_formatHsl,
    formatRgb: color_formatRgb,
    toString: color_formatRgb
  });
  function color_formatHex() {
    return this.rgb().formatHex();
  }
  function color_formatHex8() {
    return this.rgb().formatHex8();
  }
  function color_formatHsl() {
    return hslConvert(this).formatHsl();
  }
  function color_formatRgb() {
    return this.rgb().formatRgb();
  }
  function color(format2) {
    var m, l;
    format2 = (format2 + "").trim().toLowerCase();
    return (m = reHex.exec(format2)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) : l === 3 ? new Rgb(m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, (m & 15) << 4 | m & 15, 1) : l === 8 ? rgba(m >> 24 & 255, m >> 16 & 255, m >> 8 & 255, (m & 255) / 255) : l === 4 ? rgba(m >> 12 & 15 | m >> 8 & 240, m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, ((m & 15) << 4 | m & 15) / 255) : null) : (m = reRgbInteger.exec(format2)) ? new Rgb(m[1], m[2], m[3], 1) : (m = reRgbPercent.exec(format2)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) : (m = reRgbaInteger.exec(format2)) ? rgba(m[1], m[2], m[3], m[4]) : (m = reRgbaPercent.exec(format2)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) : (m = reHslPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) : (m = reHslaPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) : named.hasOwnProperty(format2) ? rgbn(named[format2]) : format2 === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
  }
  function rgbn(n) {
    return new Rgb(n >> 16 & 255, n >> 8 & 255, n & 255, 1);
  }
  function rgba(r, g, b, a) {
    if (a <= 0)
      r = g = b = NaN;
    return new Rgb(r, g, b, a);
  }
  function rgbConvert(o) {
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Rgb();
    o = o.rgb();
    return new Rgb(o.r, o.g, o.b, o.opacity);
  }
  function rgb(r, g, b, opacity) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
  }
  function Rgb(r, g, b, opacity) {
    this.r = +r;
    this.g = +g;
    this.b = +b;
    this.opacity = +opacity;
  }
  define_default(Rgb, rgb, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    rgb() {
      return this;
    },
    clamp() {
      return new Rgb(clampi(this.r), clampi(this.g), clampi(this.b), clampa(this.opacity));
    },
    displayable() {
      return -0.5 <= this.r && this.r < 255.5 && (-0.5 <= this.g && this.g < 255.5) && (-0.5 <= this.b && this.b < 255.5) && (0 <= this.opacity && this.opacity <= 1);
    },
    hex: rgb_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: rgb_formatHex,
    formatHex8: rgb_formatHex8,
    formatRgb: rgb_formatRgb,
    toString: rgb_formatRgb
  }));
  function rgb_formatHex() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}`;
  }
  function rgb_formatHex8() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}${hex((isNaN(this.opacity) ? 1 : this.opacity) * 255)}`;
  }
  function rgb_formatRgb() {
    const a = clampa(this.opacity);
    return `${a === 1 ? "rgb(" : "rgba("}${clampi(this.r)}, ${clampi(this.g)}, ${clampi(this.b)}${a === 1 ? ")" : `, ${a})`}`;
  }
  function clampa(opacity) {
    return isNaN(opacity) ? 1 : Math.max(0, Math.min(1, opacity));
  }
  function clampi(value) {
    return Math.max(0, Math.min(255, Math.round(value) || 0));
  }
  function hex(value) {
    value = clampi(value);
    return (value < 16 ? "0" : "") + value.toString(16);
  }
  function hsla(h, s, l, a) {
    if (a <= 0)
      h = s = l = NaN;
    else if (l <= 0 || l >= 1)
      h = s = NaN;
    else if (s <= 0)
      h = NaN;
    return new Hsl(h, s, l, a);
  }
  function hslConvert(o) {
    if (o instanceof Hsl)
      return new Hsl(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Hsl();
    if (o instanceof Hsl)
      return o;
    o = o.rgb();
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, min2 = Math.min(r, g, b), max3 = Math.max(r, g, b), h = NaN, s = max3 - min2, l = (max3 + min2) / 2;
    if (s) {
      if (r === max3)
        h = (g - b) / s + (g < b) * 6;
      else if (g === max3)
        h = (b - r) / s + 2;
      else
        h = (r - g) / s + 4;
      s /= l < 0.5 ? max3 + min2 : 2 - max3 - min2;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
  }
  function Hsl(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Hsl, hsl, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    rgb() {
      var h = this.h % 360 + (this.h < 0) * 360, s = isNaN(h) || isNaN(this.s) ? 0 : this.s, l = this.l, m2 = l + (l < 0.5 ? l : 1 - l) * s, m1 = 2 * l - m2;
      return new Rgb(
        hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
        this.opacity
      );
    },
    clamp() {
      return new Hsl(clamph(this.h), clampt(this.s), clampt(this.l), clampa(this.opacity));
    },
    displayable() {
      return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && (0 <= this.l && this.l <= 1) && (0 <= this.opacity && this.opacity <= 1);
    },
    formatHsl() {
      const a = clampa(this.opacity);
      return `${a === 1 ? "hsl(" : "hsla("}${clamph(this.h)}, ${clampt(this.s) * 100}%, ${clampt(this.l) * 100}%${a === 1 ? ")" : `, ${a})`}`;
    }
  }));
  function clamph(value) {
    value = (value || 0) % 360;
    return value < 0 ? value + 360 : value;
  }
  function clampt(value) {
    return Math.max(0, Math.min(1, value || 0));
  }
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-color/src/math.js
  var radians = Math.PI / 180;
  var degrees = 180 / Math.PI;

  // node_modules/d3-color/src/cubehelix.js
  var A = -0.14861;
  var B = 1.78277;
  var C = -0.29227;
  var D = -0.90649;
  var E = 1.97294;
  var ED = E * D;
  var EB = E * B;
  var BC_DA = B * C - D * A;
  function cubehelixConvert(o) {
    if (o instanceof Cubehelix)
      return new Cubehelix(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Rgb))
      o = rgbConvert(o);
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, l = (BC_DA * b + ED * r - EB * g) / (BC_DA + ED - EB), bl = b - l, k = (E * (g - l) - C * bl) / D, s = Math.sqrt(k * k + bl * bl) / (E * l * (1 - l)), h = s ? Math.atan2(k, bl) * degrees - 120 : NaN;
    return new Cubehelix(h < 0 ? h + 360 : h, s, l, o.opacity);
  }
  function cubehelix(h, s, l, opacity) {
    return arguments.length === 1 ? cubehelixConvert(h) : new Cubehelix(h, s, l, opacity == null ? 1 : opacity);
  }
  function Cubehelix(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Cubehelix, cubehelix, extend(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
    },
    rgb() {
      var h = isNaN(this.h) ? 0 : (this.h + 120) * radians, l = +this.l, a = isNaN(this.s) ? 0 : this.s * l * (1 - l), cosh = Math.cos(h), sinh = Math.sin(h);
      return new Rgb(
        255 * (l + a * (A * cosh + B * sinh)),
        255 * (l + a * (C * cosh + D * sinh)),
        255 * (l + a * (E * cosh)),
        this.opacity
      );
    }
  }));

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values) {
    var n = values.length - 1;
    return function(t) {
      var i = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values[i], v2 = values[i + 1], v0 = i > 0 ? values[i - 1] : 2 * v1 - v2, v3 = i < n - 1 ? values[i + 2] : 2 * v2 - v1;
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values) {
    var n = values.length;
    return function(t) {
      var i = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values[(i + n - 1) % n], v1 = values[i % n], v2 = values[(i + 1) % n], v3 = values[(i + 2) % n];
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default3 = (x2) => () => x2;

  // node_modules/d3-interpolate/src/color.js
  function linear(a, d) {
    return function(t) {
      return a + t * d;
    };
  }
  function exponential(a, b, y2) {
    return a = Math.pow(a, y2), b = Math.pow(b, y2) - a, y2 = 1 / y2, function(t) {
      return Math.pow(a + t * b, y2);
    };
  }
  function hue(a, b) {
    var d = b - a;
    return d ? linear(a, d > 180 || d < -180 ? d - 360 * Math.round(d / 360) : d) : constant_default3(isNaN(a) ? b : a);
  }
  function gamma(y2) {
    return (y2 = +y2) === 1 ? nogamma : function(a, b) {
      return b - a ? exponential(a, b, y2) : constant_default3(isNaN(a) ? b : a);
    };
  }
  function nogamma(a, b) {
    var d = b - a;
    return d ? linear(a, d) : constant_default3(isNaN(a) ? b : a);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = function rgbGamma(y2) {
    var color2 = gamma(y2);
    function rgb2(start2, end) {
      var r = color2((start2 = rgb(start2)).r, (end = rgb(end)).r), g = color2(start2.g, end.g), b = color2(start2.b, end.b), opacity = nogamma(start2.opacity, end.opacity);
      return function(t) {
        start2.r = r(t);
        start2.g = g(t);
        start2.b = b(t);
        start2.opacity = opacity(t);
        return start2 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  }(1);
  function rgbSpline(spline) {
    return function(colors) {
      var n = colors.length, r = new Array(n), g = new Array(n), b = new Array(n), i, color2;
      for (i = 0; i < n; ++i) {
        color2 = rgb(colors[i]);
        r[i] = color2.r || 0;
        g[i] = color2.g || 0;
        b[i] = color2.b || 0;
      }
      r = spline(r);
      g = spline(g);
      b = spline(b);
      color2.opacity = 1;
      return function(t) {
        color2.r = r(t);
        color2.g = g(t);
        color2.b = b(t);
        return color2 + "";
      };
    };
  }
  var rgbBasis = rgbSpline(basis_default);
  var rgbBasisClosed = rgbSpline(basisClosed_default);

  // node_modules/d3-interpolate/src/numberArray.js
  function numberArray_default(a, b) {
    if (!b)
      b = [];
    var n = a ? Math.min(b.length, a.length) : 0, c2 = b.slice(), i;
    return function(t) {
      for (i = 0; i < n; ++i)
        c2[i] = a[i] * (1 - t) + b[i] * t;
      return c2;
    };
  }
  function isNumberArray(x2) {
    return ArrayBuffer.isView(x2) && !(x2 instanceof DataView);
  }

  // node_modules/d3-interpolate/src/array.js
  function genericArray(a, b) {
    var nb = b ? b.length : 0, na = a ? Math.min(nb, a.length) : 0, x2 = new Array(na), c2 = new Array(nb), i;
    for (i = 0; i < na; ++i)
      x2[i] = value_default(a[i], b[i]);
    for (; i < nb; ++i)
      c2[i] = b[i];
    return function(t) {
      for (i = 0; i < na; ++i)
        c2[i] = x2[i](t);
      return c2;
    };
  }

  // node_modules/d3-interpolate/src/date.js
  function date_default(a, b) {
    var d = /* @__PURE__ */ new Date();
    return a = +a, b = +b, function(t) {
      return d.setTime(a * (1 - t) + b * t), d;
    };
  }

  // node_modules/d3-interpolate/src/number.js
  function number_default(a, b) {
    return a = +a, b = +b, function(t) {
      return a * (1 - t) + b * t;
    };
  }

  // node_modules/d3-interpolate/src/object.js
  function object_default(a, b) {
    var i = {}, c2 = {}, k;
    if (a === null || typeof a !== "object")
      a = {};
    if (b === null || typeof b !== "object")
      b = {};
    for (k in b) {
      if (k in a) {
        i[k] = value_default(a[k], b[k]);
      } else {
        c2[k] = b[k];
      }
    }
    return function(t) {
      for (k in i)
        c2[k] = i[k](t);
      return c2;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero2(b) {
    return function() {
      return b;
    };
  }
  function one(b) {
    return function(t) {
      return b(t) + "";
    };
  }
  function string_default(a, b) {
    var bi = reA.lastIndex = reB.lastIndex = 0, am, bm, bs, i = -1, s = [], q = [];
    a = a + "", b = b + "";
    while ((am = reA.exec(a)) && (bm = reB.exec(b))) {
      if ((bs = bm.index) > bi) {
        bs = b.slice(bi, bs);
        if (s[i])
          s[i] += bs;
        else
          s[++i] = bs;
      }
      if ((am = am[0]) === (bm = bm[0])) {
        if (s[i])
          s[i] += bm;
        else
          s[++i] = bm;
      } else {
        s[++i] = null;
        q.push({ i, x: number_default(am, bm) });
      }
      bi = reB.lastIndex;
    }
    if (bi < b.length) {
      bs = b.slice(bi);
      if (s[i])
        s[i] += bs;
      else
        s[++i] = bs;
    }
    return s.length < 2 ? q[0] ? one(q[0].x) : zero2(b) : (b = q.length, function(t) {
      for (var i2 = 0, o; i2 < b; ++i2)
        s[(o = q[i2]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/value.js
  function value_default(a, b) {
    var t = typeof b, c2;
    return b == null || t === "boolean" ? constant_default3(b) : (t === "number" ? number_default : t === "string" ? (c2 = color(b)) ? (b = c2, rgb_default) : string_default : b instanceof color ? rgb_default : b instanceof Date ? date_default : isNumberArray(b) ? numberArray_default : Array.isArray(b) ? genericArray : typeof b.valueOf !== "function" && typeof b.toString !== "function" || isNaN(b) ? object_default : number_default)(a, b);
  }

  // node_modules/d3-interpolate/src/round.js
  function round_default(a, b) {
    return a = +a, b = +b, function(t) {
      return Math.round(a * (1 - t) + b * t);
    };
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees2 = 180 / Math.PI;
  var identity2 = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a, b, c2, d, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a * a + b * b))
      a /= scaleX, b /= scaleX;
    if (skewX = a * c2 + b * d)
      c2 -= a * skewX, d -= b * skewX;
    if (scaleY = Math.sqrt(c2 * c2 + d * d))
      c2 /= scaleY, d /= scaleY, skewX /= scaleY;
    if (a * d < b * c2)
      a = -a, b = -b, skewX = -skewX, scaleX = -scaleX;
    return {
      translateX: e,
      translateY: f,
      rotate: Math.atan2(b, a) * degrees2,
      skewX: Math.atan(skewX) * degrees2,
      scaleX,
      scaleY
    };
  }

  // node_modules/d3-interpolate/src/transform/parse.js
  var svgNode;
  function parseCss(value) {
    const m = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value + "");
    return m.isIdentity ? identity2 : decompose_default(m.a, m.b, m.c, m.d, m.e, m.f);
  }
  function parseSvg(value) {
    if (value == null)
      return identity2;
    if (!svgNode)
      svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value);
    if (!(value = svgNode.transform.baseVal.consolidate()))
      return identity2;
    value = value.matrix;
    return decompose_default(value.a, value.b, value.c, value.d, value.e, value.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse, pxComma, pxParen, degParen) {
    function pop(s) {
      return s.length ? s.pop() + " " : "";
    }
    function translate(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push("translate(", null, pxComma, null, pxParen);
        q.push({ i: i - 4, x: number_default(xa, xb) }, { i: i - 2, x: number_default(ya, yb) });
      } else if (xb || yb) {
        s.push("translate(" + xb + pxComma + yb + pxParen);
      }
    }
    function rotate(a, b, s, q) {
      if (a !== b) {
        if (a - b > 180)
          b += 360;
        else if (b - a > 180)
          a += 360;
        q.push({ i: s.push(pop(s) + "rotate(", null, degParen) - 2, x: number_default(a, b) });
      } else if (b) {
        s.push(pop(s) + "rotate(" + b + degParen);
      }
    }
    function skewX(a, b, s, q) {
      if (a !== b) {
        q.push({ i: s.push(pop(s) + "skewX(", null, degParen) - 2, x: number_default(a, b) });
      } else if (b) {
        s.push(pop(s) + "skewX(" + b + degParen);
      }
    }
    function scale(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push(pop(s) + "scale(", null, ",", null, ")");
        q.push({ i: i - 4, x: number_default(xa, xb) }, { i: i - 2, x: number_default(ya, yb) });
      } else if (xb !== 1 || yb !== 1) {
        s.push(pop(s) + "scale(" + xb + "," + yb + ")");
      }
    }
    return function(a, b) {
      var s = [], q = [];
      a = parse(a), b = parse(b);
      translate(a.translateX, a.translateY, b.translateX, b.translateY, s, q);
      rotate(a.rotate, b.rotate, s, q);
      skewX(a.skewX, b.skewX, s, q);
      scale(a.scaleX, a.scaleY, b.scaleX, b.scaleY, s, q);
      a = b = null;
      return function(t) {
        var i = -1, n = q.length, o;
        while (++i < n)
          s[(o = q[i]).i] = o.x(t);
        return s.join("");
      };
    };
  }
  var interpolateTransformCss = interpolateTransform(parseCss, "px, ", "px)", "deg)");
  var interpolateTransformSvg = interpolateTransform(parseSvg, ", ", ")", ")");

  // node_modules/d3-interpolate/src/cubehelix.js
  function cubehelix2(hue2) {
    return function cubehelixGamma(y2) {
      y2 = +y2;
      function cubehelix3(start2, end) {
        var h = hue2((start2 = cubehelix(start2)).h, (end = cubehelix(end)).h), s = nogamma(start2.s, end.s), l = nogamma(start2.l, end.l), opacity = nogamma(start2.opacity, end.opacity);
        return function(t) {
          start2.h = h(t);
          start2.s = s(t);
          start2.l = l(Math.pow(t, y2));
          start2.opacity = opacity(t);
          return start2 + "";
        };
      }
      cubehelix3.gamma = cubehelixGamma;
      return cubehelix3;
    }(1);
  }
  var cubehelix_default = cubehelix2(hue);
  var cubehelixLong = cubehelix2(nogamma);

  // node_modules/d3-timer/src/timer.js
  var frame = 0;
  var timeout = 0;
  var interval = 0;
  var pokeDelay = 1e3;
  var taskHead;
  var taskTail;
  var clockLast = 0;
  var clockNow = 0;
  var clockSkew = 0;
  var clock = typeof performance === "object" && performance.now ? performance : Date;
  var setFrame = typeof window === "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(f) {
    setTimeout(f, 17);
  };
  function now() {
    return clockNow || (setFrame(clearNow), clockNow = clock.now() + clockSkew);
  }
  function clearNow() {
    clockNow = 0;
  }
  function Timer() {
    this._call = this._time = this._next = null;
  }
  Timer.prototype = timer.prototype = {
    constructor: Timer,
    restart: function(callback, delay, time) {
      if (typeof callback !== "function")
        throw new TypeError("callback is not a function");
      time = (time == null ? now() : +time) + (delay == null ? 0 : +delay);
      if (!this._next && taskTail !== this) {
        if (taskTail)
          taskTail._next = this;
        else
          taskHead = this;
        taskTail = this;
      }
      this._call = callback;
      this._time = time;
      sleep();
    },
    stop: function() {
      if (this._call) {
        this._call = null;
        this._time = Infinity;
        sleep();
      }
    }
  };
  function timer(callback, delay, time) {
    var t = new Timer();
    t.restart(callback, delay, time);
    return t;
  }
  function timerFlush() {
    now();
    ++frame;
    var t = taskHead, e;
    while (t) {
      if ((e = clockNow - t._time) >= 0)
        t._call.call(void 0, e);
      t = t._next;
    }
    --frame;
  }
  function wake() {
    clockNow = (clockLast = clock.now()) + clockSkew;
    frame = timeout = 0;
    try {
      timerFlush();
    } finally {
      frame = 0;
      nap();
      clockNow = 0;
    }
  }
  function poke() {
    var now2 = clock.now(), delay = now2 - clockLast;
    if (delay > pokeDelay)
      clockSkew -= delay, clockLast = now2;
  }
  function nap() {
    var t0, t1 = taskHead, t2, time = Infinity;
    while (t1) {
      if (t1._call) {
        if (time > t1._time)
          time = t1._time;
        t0 = t1, t1 = t1._next;
      } else {
        t2 = t1._next, t1._next = null;
        t1 = t0 ? t0._next = t2 : taskHead = t2;
      }
    }
    taskTail = t0;
    sleep(time);
  }
  function sleep(time) {
    if (frame)
      return;
    if (timeout)
      timeout = clearTimeout(timeout);
    var delay = time - clockNow;
    if (delay > 24) {
      if (time < Infinity)
        timeout = setTimeout(wake, time - clock.now() - clockSkew);
      if (interval)
        interval = clearInterval(interval);
    } else {
      if (!interval)
        clockLast = clock.now(), interval = setInterval(poke, pokeDelay);
      frame = 1, setFrame(wake);
    }
  }

  // node_modules/d3-timer/src/timeout.js
  function timeout_default(callback, delay, time) {
    var t = new Timer();
    delay = delay == null ? 0 : +delay;
    t.restart((elapsed) => {
      t.stop();
      callback(elapsed + delay);
    }, delay, time);
    return t;
  }

  // node_modules/d3-transition/src/transition/schedule.js
  var emptyOn = dispatch_default("start", "end", "cancel", "interrupt");
  var emptyTween = [];
  var CREATED = 0;
  var SCHEDULED = 1;
  var STARTING = 2;
  var STARTED = 3;
  var RUNNING = 4;
  var ENDING = 5;
  var ENDED = 6;
  function schedule_default(node, name, id2, index, group, timing) {
    var schedules = node.__transition;
    if (!schedules)
      node.__transition = {};
    else if (id2 in schedules)
      return;
    create(node, id2, {
      name,
      index,
      // For context during callback.
      group,
      // For context during callback.
      on: emptyOn,
      tween: emptyTween,
      time: timing.time,
      delay: timing.delay,
      duration: timing.duration,
      ease: timing.ease,
      timer: null,
      state: CREATED
    });
  }
  function init(node, id2) {
    var schedule = get2(node, id2);
    if (schedule.state > CREATED)
      throw new Error("too late; already scheduled");
    return schedule;
  }
  function set2(node, id2) {
    var schedule = get2(node, id2);
    if (schedule.state > STARTED)
      throw new Error("too late; already running");
    return schedule;
  }
  function get2(node, id2) {
    var schedule = node.__transition;
    if (!schedule || !(schedule = schedule[id2]))
      throw new Error("transition not found");
    return schedule;
  }
  function create(node, id2, self) {
    var schedules = node.__transition, tween;
    schedules[id2] = self;
    self.timer = timer(schedule, 0, self.time);
    function schedule(elapsed) {
      self.state = SCHEDULED;
      self.timer.restart(start2, self.delay, self.time);
      if (self.delay <= elapsed)
        start2(elapsed - self.delay);
    }
    function start2(elapsed) {
      var i, j, n, o;
      if (self.state !== SCHEDULED)
        return stop();
      for (i in schedules) {
        o = schedules[i];
        if (o.name !== self.name)
          continue;
        if (o.state === STARTED)
          return timeout_default(start2);
        if (o.state === RUNNING) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("interrupt", node, node.__data__, o.index, o.group);
          delete schedules[i];
        } else if (+i < id2) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("cancel", node, node.__data__, o.index, o.group);
          delete schedules[i];
        }
      }
      timeout_default(function() {
        if (self.state === STARTED) {
          self.state = RUNNING;
          self.timer.restart(tick, self.delay, self.time);
          tick(elapsed);
        }
      });
      self.state = STARTING;
      self.on.call("start", node, node.__data__, self.index, self.group);
      if (self.state !== STARTING)
        return;
      self.state = STARTED;
      tween = new Array(n = self.tween.length);
      for (i = 0, j = -1; i < n; ++i) {
        if (o = self.tween[i].value.call(node, node.__data__, self.index, self.group)) {
          tween[++j] = o;
        }
      }
      tween.length = j + 1;
    }
    function tick(elapsed) {
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop), self.state = ENDING, 1), i = -1, n = tween.length;
      while (++i < n) {
        tween[i].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop();
      }
    }
    function stop() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id2];
      for (var i in schedules)
        return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name) {
    var schedules = node.__transition, schedule, active, empty3 = true, i;
    if (!schedules)
      return;
    name = name == null ? null : name + "";
    for (i in schedules) {
      if ((schedule = schedules[i]).name !== name) {
        empty3 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i];
    }
    if (empty3)
      delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name) {
    return this.each(function() {
      interrupt_default(this, name);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id2, name) {
    var tween0, tween1;
    return function() {
      var schedule = set2(this, id2), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name) {
            tween1 = tween1.slice();
            tween1.splice(i, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id2, name, value) {
    var tween0, tween1;
    if (typeof value !== "function")
      throw new Error();
    return function() {
      var schedule = set2(this, id2), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name, value }, i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name) {
            tween1[i] = t;
            break;
          }
        }
        if (i === n)
          tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name, value) {
    var id2 = this._id;
    name += "";
    if (arguments.length < 2) {
      var tween = get2(this.node(), id2).tween;
      for (var i = 0, n = tween.length, t; i < n; ++i) {
        if ((t = tween[i]).name === name) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value == null ? tweenRemove : tweenFunction)(id2, name, value));
  }
  function tweenValue(transition2, name, value) {
    var id2 = transition2._id;
    transition2.each(function() {
      var schedule = set2(this, id2);
      (schedule.value || (schedule.value = {}))[name] = value.apply(this, arguments);
    });
    return function(node) {
      return get2(node, id2).value[name];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a, b) {
    var c2;
    return (typeof b === "number" ? number_default : b instanceof color ? rgb_default : (c2 = color(b)) ? (b = c2, rgb_default) : string_default)(a, b);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name) {
    return function() {
      this.removeAttribute(name);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrConstantNS2(fullname, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttributeNS(fullname.space, fullname.local);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrFunction2(name, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttribute(name);
      string0 = this.getAttribute(name);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value(this), string1;
      if (value1 == null)
        return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name, value) {
    var fullname = namespace_default(name), i = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name, typeof value === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i, tweenValue(this, "attr." + name, value)) : value == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i, value));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name, i) {
    return function(t) {
      this.setAttribute(name, i.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i.call(this, t));
    };
  }
  function attrTweenNS(fullname, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolateNS(fullname, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween(name, value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolate(name, i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function attrTween_default(name, value) {
    var key = "attr." + name;
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    var fullname = namespace_default(name);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id2, value) {
    return function() {
      init(this, id2).delay = +value.apply(this, arguments);
    };
  }
  function delayConstant(id2, value) {
    return value = +value, function() {
      init(this, id2).delay = value;
    };
  }
  function delay_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? delayFunction : delayConstant)(id2, value)) : get2(this.node(), id2).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id2, value) {
    return function() {
      set2(this, id2).duration = +value.apply(this, arguments);
    };
  }
  function durationConstant(id2, value) {
    return value = +value, function() {
      set2(this, id2).duration = value;
    };
  }
  function duration_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each((typeof value === "function" ? durationFunction : durationConstant)(id2, value)) : get2(this.node(), id2).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id2, value) {
    if (typeof value !== "function")
      throw new Error();
    return function() {
      set2(this, id2).ease = value;
    };
  }
  function ease_default(value) {
    var id2 = this._id;
    return arguments.length ? this.each(easeConstant(id2, value)) : get2(this.node(), id2).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id2, value) {
    return function() {
      var v = value.apply(this, arguments);
      if (typeof v !== "function")
        throw new Error();
      set2(this, id2).ease = v;
    };
  }
  function easeVarying_default(value) {
    if (typeof value !== "function")
      throw new Error();
    return this.each(easeVarying(this._id, value));
  }

  // node_modules/d3-transition/src/transition/filter.js
  function filter_default2(match) {
    if (typeof match !== "function")
      match = matcher_default(match);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group[i]) && match.call(node, node.__data__, i, group)) {
          subgroup.push(node);
        }
      }
    }
    return new Transition(subgroups, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/merge.js
  function merge_default2(transition2) {
    if (transition2._id !== this._id)
      throw new Error();
    for (var groups0 = this._groups, groups1 = transition2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Transition(merges, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/on.js
  function start(name) {
    return (name + "").trim().split(/^|\s+/).every(function(t) {
      var i = t.indexOf(".");
      if (i >= 0)
        t = t.slice(0, i);
      return !t || t === "start";
    });
  }
  function onFunction(id2, name, listener) {
    var on0, on1, sit = start(name) ? init : set2;
    return function() {
      var schedule = sit(this, id2), on = schedule.on;
      if (on !== on0)
        (on1 = (on0 = on).copy()).on(name, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name, listener) {
    var id2 = this._id;
    return arguments.length < 2 ? get2(this.node(), id2).on.on(name) : this.each(onFunction(id2, name, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id2) {
    return function() {
      var parent = this.parentNode;
      for (var i in this.__transition)
        if (+i !== id2)
          return;
      if (parent)
        parent.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default3(select) {
    var name = this._name, id2 = this._id;
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group[i]) && (subnode = select.call(node, node.__data__, i, group))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
          schedule_default(subgroup[i], name, id2, i, subgroup, get2(node, id2));
        }
      }
    }
    return new Transition(subgroups, this._parents, name, id2);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default3(select) {
    var name = this._name, id2 = this._id;
    if (typeof select !== "function")
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          for (var children2 = select.call(node, node.__data__, i, group), child, inherit2 = get2(node, id2), k = 0, l = children2.length; k < l; ++k) {
            if (child = children2[k]) {
              schedule_default(child, name, id2, k, children2, inherit2);
            }
          }
          subgroups.push(children2);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name, id2);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name), string1 = (this.style.removeProperty(name), styleValue(this, name));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name) {
    return function() {
      this.style.removeProperty(name);
    };
  }
  function styleConstant2(name, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name, interpolate, value) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name), value1 = value(this), string1 = value1 + "";
      if (value1 == null)
        string1 = value1 = (this.style.removeProperty(name), styleValue(this, name));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id2, name) {
    var on0, on1, listener0, key = "style." + name, event = "end." + key, remove2;
    return function() {
      var schedule = set2(this, id2), on = schedule.on, listener = schedule.value[key] == null ? remove2 || (remove2 = styleRemove2(name)) : void 0;
      if (on !== on0 || listener0 !== listener)
        (on1 = (on0 = on).copy()).on(event, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name, value, priority) {
    var i = (name += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value == null ? this.styleTween(name, styleNull(name, i)).on("end.style." + name, styleRemove2(name)) : typeof value === "function" ? this.styleTween(name, styleFunction2(name, i, tweenValue(this, "style." + name, value))).each(styleMaybeRemove(this._id, name)) : this.styleTween(name, styleConstant2(name, i, value), priority).on("end.style." + name, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name, i, priority) {
    return function(t) {
      this.style.setProperty(name, i.call(this, t), priority);
    };
  }
  function styleTween(name, value, priority) {
    var t, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t = (i0 = i) && styleInterpolate(name, i, priority);
      return t;
    }
    tween._value = value;
    return tween;
  }
  function styleTween_default(name, value, priority) {
    var key = "style." + (name += "");
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, styleTween(name, value, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value) {
    return function() {
      this.textContent = value;
    };
  }
  function textFunction2(value) {
    return function() {
      var value1 = value(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value) {
    return this.tween("text", typeof value === "function" ? textFunction2(tweenValue(this, "text", value)) : textConstant2(value == null ? "" : value + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i) {
    return function(t) {
      this.textContent = i.call(this, t);
    };
  }
  function textTween(value) {
    var t0, i0;
    function tween() {
      var i = value.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && textInterpolate(i);
      return t0;
    }
    tween._value = value;
    return tween;
  }
  function textTween_default(value) {
    var key = "text";
    if (arguments.length < 1)
      return (key = this.tween(key)) && key._value;
    if (value == null)
      return this.tween(key, null);
    if (typeof value !== "function")
      throw new Error();
    return this.tween(key, textTween(value));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          var inherit2 = get2(node, id0);
          schedule_default(node, name, id1, i, group, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id2 = that._id, size = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size === 0)
          resolve();
      } };
      that.each(function() {
        var schedule = set2(this, id2), on = schedule.on;
        if (on !== on0) {
          on1 = (on0 = on).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size === 0)
        resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id = 0;
  function Transition(groups, parents, name, id2) {
    this._groups = groups;
    this._parents = parents;
    this._name = name;
    this._id = id2;
  }
  function transition(name) {
    return selection_default().transition(name);
  }
  function newId() {
    return ++id;
  }
  var selection_prototype = selection_default.prototype;
  Transition.prototype = transition.prototype = {
    constructor: Transition,
    select: select_default3,
    selectAll: selectAll_default3,
    selectChild: selection_prototype.selectChild,
    selectChildren: selection_prototype.selectChildren,
    filter: filter_default2,
    merge: merge_default2,
    selection: selection_default2,
    transition: transition_default,
    call: selection_prototype.call,
    nodes: selection_prototype.nodes,
    node: selection_prototype.node,
    size: selection_prototype.size,
    empty: selection_prototype.empty,
    each: selection_prototype.each,
    on: on_default2,
    attr: attr_default2,
    attrTween: attrTween_default,
    style: style_default2,
    styleTween: styleTween_default,
    text: text_default2,
    textTween: textTween_default,
    remove: remove_default2,
    tween: tween_default,
    delay: delay_default,
    duration: duration_default,
    ease: ease_default,
    easeVarying: easeVarying_default,
    end: end_default,
    [Symbol.iterator]: selection_prototype[Symbol.iterator]
  };

  // node_modules/d3-ease/src/cubic.js
  function cubicInOut(t) {
    return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
  }

  // node_modules/d3-ease/src/back.js
  var overshoot = 1.70158;
  var backIn = function custom(s) {
    s = +s;
    function backIn2(t) {
      return (t = +t) * t * (s * (t - 1) + t);
    }
    backIn2.overshoot = custom;
    return backIn2;
  }(overshoot);
  var backOut = function custom2(s) {
    s = +s;
    function backOut2(t) {
      return --t * t * ((t + 1) * s + t) + 1;
    }
    backOut2.overshoot = custom2;
    return backOut2;
  }(overshoot);
  var backInOut = function custom3(s) {
    s = +s;
    function backInOut2(t) {
      return ((t *= 2) < 1 ? t * t * ((s + 1) * t - s) : (t -= 2) * t * ((s + 1) * t + s) + 2) / 2;
    }
    backInOut2.overshoot = custom3;
    return backInOut2;
  }(overshoot);

  // node_modules/d3-transition/src/selection/transition.js
  var defaultTiming = {
    time: null,
    // Set on use.
    delay: 0,
    duration: 250,
    ease: cubicInOut
  };
  function inherit(node, id2) {
    var timing;
    while (!(timing = node.__transition) || !(timing = timing[id2])) {
      if (!(node = node.parentNode)) {
        throw new Error(`transition ${id2} not found`);
      }
    }
    return timing;
  }
  function transition_default2(name) {
    var id2, timing;
    if (name instanceof Transition) {
      id2 = name._id, name = name._name;
    } else {
      id2 = newId(), (timing = defaultTiming).time = now(), name = name == null ? null : name + "";
    }
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
        if (node = group[i]) {
          schedule_default(node, name, id2, i, group, timing || inherit(node, id2));
        }
      }
    }
    return new Transition(groups, this._parents, name, id2);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/constant.js
  var constant_default4 = (x2) => () => x2;

  // node_modules/d3-brush/src/event.js
  function BrushEvent(type2, {
    sourceEvent,
    target,
    selection: selection2,
    mode,
    dispatch: dispatch2
  }) {
    Object.defineProperties(this, {
      type: { value: type2, enumerable: true, configurable: true },
      sourceEvent: { value: sourceEvent, enumerable: true, configurable: true },
      target: { value: target, enumerable: true, configurable: true },
      selection: { value: selection2, enumerable: true, configurable: true },
      mode: { value: mode, enumerable: true, configurable: true },
      _: { value: dispatch2 }
    });
  }

  // node_modules/d3-brush/src/noevent.js
  function nopropagation2(event) {
    event.stopImmediatePropagation();
  }
  function noevent_default2(event) {
    event.preventDefault();
    event.stopImmediatePropagation();
  }

  // node_modules/d3-brush/src/brush.js
  var MODE_DRAG = { name: "drag" };
  var MODE_SPACE = { name: "space" };
  var MODE_HANDLE = { name: "handle" };
  var MODE_CENTER = { name: "center" };
  var { abs, max: max2, min } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number22(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x2, e) {
      return x2 == null ? null : [[+x2[0], e[0][1]], [+x2[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y2, e) {
      return y2 == null ? null : [[e[0][0], +y2[0]], [e[1][0], +y2[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number22(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  var cursors = {
    overlay: "crosshair",
    selection: "move",
    n: "ns-resize",
    e: "ew-resize",
    s: "ns-resize",
    w: "ew-resize",
    nw: "nwse-resize",
    ne: "nesw-resize",
    se: "nwse-resize",
    sw: "nesw-resize"
  };
  var flipX = {
    e: "w",
    w: "e",
    nw: "ne",
    ne: "nw",
    se: "sw",
    sw: "se"
  };
  var flipY = {
    n: "s",
    s: "n",
    nw: "sw",
    ne: "se",
    se: "ne",
    sw: "nw"
  };
  var signsX = {
    overlay: 1,
    selection: 1,
    n: null,
    e: 1,
    s: null,
    w: -1,
    nw: -1,
    ne: 1,
    se: 1,
    sw: -1
  };
  var signsY = {
    overlay: 1,
    selection: 1,
    n: -1,
    e: null,
    s: 1,
    w: null,
    nw: -1,
    ne: -1,
    se: 1,
    sw: 1
  };
  function type(t) {
    return { type: t };
  }
  function defaultFilter2(event) {
    return !event.ctrlKey && !event.button;
  }
  function defaultExtent() {
    var svg = this.ownerSVGElement || this;
    if (svg.hasAttribute("viewBox")) {
      svg = svg.viewBox.baseVal;
      return [[svg.x, svg.y], [svg.x + svg.width, svg.y + svg.height]];
    }
    return [[0, 0], [svg.width.baseVal.value, svg.height.baseVal.value]];
  }
  function defaultTouchable2() {
    return navigator.maxTouchPoints || "ontouchstart" in this;
  }
  function local(node) {
    while (!node.__brush)
      if (!(node = node.parentNode))
        return;
    return node.__brush;
  }
  function empty2(extent2) {
    return extent2[0][0] === extent2[1][0] || extent2[0][1] === extent2[1][1];
  }
  function brushSelection(node) {
    var state = node.__brush;
    return state ? state.dim.output(state.selection) : null;
  }
  function brushX() {
    return brush(X);
  }
  function brushY() {
    return brush(Y);
  }
  function brush(dim) {
    var extent2 = defaultExtent, filter2 = defaultFilter2, touchable = defaultTouchable2, keys = true, listeners = dispatch_default("start", "brush", "end"), handleSize = 6, touchending;
    function brush2(group) {
      var overlay = group.property("__brush", initialize).selectAll(".overlay").data([type("overlay")]);
      overlay.enter().append("rect").attr("class", "overlay").attr("pointer-events", "all").attr("cursor", cursors.overlay).merge(overlay).each(function() {
        var extent3 = local(this).extent;
        select_default2(this).attr("x", extent3[0][0]).attr("y", extent3[0][1]).attr("width", extent3[1][0] - extent3[0][0]).attr("height", extent3[1][1] - extent3[0][1]);
      });
      group.selectAll(".selection").data([type("selection")]).enter().append("rect").attr("class", "selection").attr("cursor", cursors.selection).attr("fill", "#777").attr("fill-opacity", 0.3).attr("stroke", "#fff").attr("shape-rendering", "crispEdges");
      var handle = group.selectAll(".handle").data(dim.handles, function(d) {
        return d.type;
      });
      handle.exit().remove();
      handle.enter().append("rect").attr("class", function(d) {
        return "handle handle--" + d.type;
      }).attr("cursor", function(d) {
        return cursors[d.type];
      });
      group.each(redraw).attr("fill", "none").attr("pointer-events", "all").on("mousedown.brush", started).filter(touchable).on("touchstart.brush", started).on("touchmove.brush", touchmoved).on("touchend.brush touchcancel.brush", touchended).style("touch-action", "none").style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
    }
    brush2.move = function(group, selection2, event) {
      if (group.tween) {
        group.on("start.brush", function(event2) {
          emitter(this, arguments).beforestart().start(event2);
        }).on("interrupt.brush end.brush", function(event2) {
          emitter(this, arguments).end(event2);
        }).tween("brush", function() {
          var that = this, state = that.__brush, emit = emitter(that, arguments), selection0 = state.selection, selection1 = dim.input(typeof selection2 === "function" ? selection2.apply(this, arguments) : selection2, state.extent), i = value_default(selection0, selection1);
          function tween(t) {
            state.selection = t === 1 && selection1 === null ? null : i(t);
            redraw.call(that);
            emit.brush();
          }
          return selection0 !== null && selection1 !== null ? tween : tween(1);
        });
      } else {
        group.each(function() {
          var that = this, args = arguments, state = that.__brush, selection1 = dim.input(typeof selection2 === "function" ? selection2.apply(that, args) : selection2, state.extent), emit = emitter(that, args).beforestart();
          interrupt_default(that);
          state.selection = selection1 === null ? null : selection1;
          redraw.call(that);
          emit.start(event).brush(event).end(event);
        });
      }
    };
    brush2.clear = function(group, event) {
      brush2.move(group, null, event);
    };
    function redraw() {
      var group = select_default2(this), selection2 = local(this).selection;
      if (selection2) {
        group.selectAll(".selection").style("display", null).attr("x", selection2[0][0]).attr("y", selection2[0][1]).attr("width", selection2[1][0] - selection2[0][0]).attr("height", selection2[1][1] - selection2[0][1]);
        group.selectAll(".handle").style("display", null).attr("x", function(d) {
          return d.type[d.type.length - 1] === "e" ? selection2[1][0] - handleSize / 2 : selection2[0][0] - handleSize / 2;
        }).attr("y", function(d) {
          return d.type[0] === "s" ? selection2[1][1] - handleSize / 2 : selection2[0][1] - handleSize / 2;
        }).attr("width", function(d) {
          return d.type === "n" || d.type === "s" ? selection2[1][0] - selection2[0][0] + handleSize : handleSize;
        }).attr("height", function(d) {
          return d.type === "e" || d.type === "w" ? selection2[1][1] - selection2[0][1] + handleSize : handleSize;
        });
      } else {
        group.selectAll(".selection,.handle").style("display", "none").attr("x", null).attr("y", null).attr("width", null).attr("height", null);
      }
    }
    function emitter(that, args, clean) {
      var emit = that.__brush.emitter;
      return emit && (!clean || !emit.clean) ? emit : new Emitter(that, args, clean);
    }
    function Emitter(that, args, clean) {
      this.that = that;
      this.args = args;
      this.state = that.__brush;
      this.active = 0;
      this.clean = clean;
    }
    Emitter.prototype = {
      beforestart: function() {
        if (++this.active === 1)
          this.state.emitter = this, this.starting = true;
        return this;
      },
      start: function(event, mode) {
        if (this.starting)
          this.starting = false, this.emit("start", event, mode);
        else
          this.emit("brush", event);
        return this;
      },
      brush: function(event, mode) {
        this.emit("brush", event, mode);
        return this;
      },
      end: function(event, mode) {
        if (--this.active === 0)
          delete this.state.emitter, this.emit("end", event, mode);
        return this;
      },
      emit: function(type2, event, mode) {
        var d = select_default2(this.that).datum();
        listeners.call(
          type2,
          this.that,
          new BrushEvent(type2, {
            sourceEvent: event,
            target: brush2,
            selection: dim.output(this.state.selection),
            mode,
            dispatch: listeners
          }),
          d
        );
      }
    };
    function started(event) {
      if (touchending && !event.touches)
        return;
      if (!filter2.apply(this, arguments))
        return;
      var that = this, type2 = event.target.__data__.type, mode = (keys && event.metaKey ? type2 = "overlay" : type2) === "selection" ? MODE_DRAG : keys && event.altKey ? MODE_CENTER : MODE_HANDLE, signX = dim === Y ? null : signsX[type2], signY = dim === X ? null : signsY[type2], state = local(that), extent3 = state.extent, selection2 = state.selection, W = extent3[0][0], w0, w1, N = extent3[0][1], n0, n1, E2 = extent3[1][0], e0, e1, S = extent3[1][1], s0, s1, dx = 0, dy = 0, moving, shifting = signX && signY && keys && event.shiftKey, lockX, lockY, points = Array.from(event.touches || [event], (t) => {
        const i = t.identifier;
        t = pointer_default(t, that);
        t.point0 = t.slice();
        t.identifier = i;
        return t;
      });
      interrupt_default(that);
      var emit = emitter(that, arguments, true).beforestart();
      if (type2 === "overlay") {
        if (selection2)
          moving = true;
        const pts = [points[0], points[1] || points[0]];
        state.selection = selection2 = [[
          w0 = dim === Y ? W : min(pts[0][0], pts[1][0]),
          n0 = dim === X ? N : min(pts[0][1], pts[1][1])
        ], [
          e0 = dim === Y ? E2 : max2(pts[0][0], pts[1][0]),
          s0 = dim === X ? S : max2(pts[0][1], pts[1][1])
        ]];
        if (points.length > 1)
          move(event);
      } else {
        w0 = selection2[0][0];
        n0 = selection2[0][1];
        e0 = selection2[1][0];
        s0 = selection2[1][1];
      }
      w1 = w0;
      n1 = n0;
      e1 = e0;
      s1 = s0;
      var group = select_default2(that).attr("pointer-events", "none");
      var overlay = group.selectAll(".overlay").attr("cursor", cursors[type2]);
      if (event.touches) {
        emit.moved = moved;
        emit.ended = ended;
      } else {
        var view = select_default2(event.view).on("mousemove.brush", moved, true).on("mouseup.brush", ended, true);
        if (keys)
          view.on("keydown.brush", keydowned, true).on("keyup.brush", keyupped, true);
        nodrag_default(event.view);
      }
      redraw.call(that);
      emit.start(event, mode.name);
      function moved(event2) {
        for (const p of event2.changedTouches || [event2]) {
          for (const d of points)
            if (d.identifier === p.identifier)
              d.cur = pointer_default(p, that);
        }
        if (shifting && !lockX && !lockY && points.length === 1) {
          const point2 = points[0];
          if (abs(point2.cur[0] - point2[0]) > abs(point2.cur[1] - point2[1]))
            lockY = true;
          else
            lockX = true;
        }
        for (const point2 of points)
          if (point2.cur)
            point2[0] = point2.cur[0], point2[1] = point2.cur[1];
        moving = true;
        noevent_default2(event2);
        move(event2);
      }
      function move(event2) {
        const point2 = points[0], point0 = point2.point0;
        var t;
        dx = point2[0] - point0[0];
        dy = point2[1] - point0[1];
        switch (mode) {
          case MODE_SPACE:
          case MODE_DRAG: {
            if (signX)
              dx = max2(W - w0, min(E2 - e0, dx)), w1 = w0 + dx, e1 = e0 + dx;
            if (signY)
              dy = max2(N - n0, min(S - s0, dy)), n1 = n0 + dy, s1 = s0 + dy;
            break;
          }
          case MODE_HANDLE: {
            if (points[1]) {
              if (signX)
                w1 = max2(W, min(E2, points[0][0])), e1 = max2(W, min(E2, points[1][0])), signX = 1;
              if (signY)
                n1 = max2(N, min(S, points[0][1])), s1 = max2(N, min(S, points[1][1])), signY = 1;
            } else {
              if (signX < 0)
                dx = max2(W - w0, min(E2 - w0, dx)), w1 = w0 + dx, e1 = e0;
              else if (signX > 0)
                dx = max2(W - e0, min(E2 - e0, dx)), w1 = w0, e1 = e0 + dx;
              if (signY < 0)
                dy = max2(N - n0, min(S - n0, dy)), n1 = n0 + dy, s1 = s0;
              else if (signY > 0)
                dy = max2(N - s0, min(S - s0, dy)), n1 = n0, s1 = s0 + dy;
            }
            break;
          }
          case MODE_CENTER: {
            if (signX)
              w1 = max2(W, min(E2, w0 - dx * signX)), e1 = max2(W, min(E2, e0 + dx * signX));
            if (signY)
              n1 = max2(N, min(S, n0 - dy * signY)), s1 = max2(N, min(S, s0 + dy * signY));
            break;
          }
        }
        if (e1 < w1) {
          signX *= -1;
          t = w0, w0 = e0, e0 = t;
          t = w1, w1 = e1, e1 = t;
          if (type2 in flipX)
            overlay.attr("cursor", cursors[type2 = flipX[type2]]);
        }
        if (s1 < n1) {
          signY *= -1;
          t = n0, n0 = s0, s0 = t;
          t = n1, n1 = s1, s1 = t;
          if (type2 in flipY)
            overlay.attr("cursor", cursors[type2 = flipY[type2]]);
        }
        if (state.selection)
          selection2 = state.selection;
        if (lockX)
          w1 = selection2[0][0], e1 = selection2[1][0];
        if (lockY)
          n1 = selection2[0][1], s1 = selection2[1][1];
        if (selection2[0][0] !== w1 || selection2[0][1] !== n1 || selection2[1][0] !== e1 || selection2[1][1] !== s1) {
          state.selection = [[w1, n1], [e1, s1]];
          redraw.call(that);
          emit.brush(event2, mode.name);
        }
      }
      function ended(event2) {
        nopropagation2(event2);
        if (event2.touches) {
          if (event2.touches.length)
            return;
          if (touchending)
            clearTimeout(touchending);
          touchending = setTimeout(function() {
            touchending = null;
          }, 500);
        } else {
          yesdrag(event2.view, moving);
          view.on("keydown.brush keyup.brush mousemove.brush mouseup.brush", null);
        }
        group.attr("pointer-events", "all");
        overlay.attr("cursor", cursors.overlay);
        if (state.selection)
          selection2 = state.selection;
        if (empty2(selection2))
          state.selection = null, redraw.call(that);
        emit.end(event2, mode.name);
      }
      function keydowned(event2) {
        switch (event2.keyCode) {
          case 16: {
            shifting = signX && signY;
            break;
          }
          case 18: {
            if (mode === MODE_HANDLE) {
              if (signX)
                e0 = e1 - dx * signX, w0 = w1 + dx * signX;
              if (signY)
                s0 = s1 - dy * signY, n0 = n1 + dy * signY;
              mode = MODE_CENTER;
              move(event2);
            }
            break;
          }
          case 32: {
            if (mode === MODE_HANDLE || mode === MODE_CENTER) {
              if (signX < 0)
                e0 = e1 - dx;
              else if (signX > 0)
                w0 = w1 - dx;
              if (signY < 0)
                s0 = s1 - dy;
              else if (signY > 0)
                n0 = n1 - dy;
              mode = MODE_SPACE;
              overlay.attr("cursor", cursors.selection);
              move(event2);
            }
            break;
          }
          default:
            return;
        }
        noevent_default2(event2);
      }
      function keyupped(event2) {
        switch (event2.keyCode) {
          case 16: {
            if (shifting) {
              lockX = lockY = shifting = false;
              move(event2);
            }
            break;
          }
          case 18: {
            if (mode === MODE_CENTER) {
              if (signX < 0)
                e0 = e1;
              else if (signX > 0)
                w0 = w1;
              if (signY < 0)
                s0 = s1;
              else if (signY > 0)
                n0 = n1;
              mode = MODE_HANDLE;
              move(event2);
            }
            break;
          }
          case 32: {
            if (mode === MODE_SPACE) {
              if (event2.altKey) {
                if (signX)
                  e0 = e1 - dx * signX, w0 = w1 + dx * signX;
                if (signY)
                  s0 = s1 - dy * signY, n0 = n1 + dy * signY;
                mode = MODE_CENTER;
              } else {
                if (signX < 0)
                  e0 = e1;
                else if (signX > 0)
                  w0 = w1;
                if (signY < 0)
                  s0 = s1;
                else if (signY > 0)
                  n0 = n1;
                mode = MODE_HANDLE;
              }
              overlay.attr("cursor", cursors[type2]);
              move(event2);
            }
            break;
          }
          default:
            return;
        }
        noevent_default2(event2);
      }
    }
    function touchmoved(event) {
      emitter(this, arguments).moved(event);
    }
    function touchended(event) {
      emitter(this, arguments).ended(event);
    }
    function initialize() {
      var state = this.__brush || { selection: null };
      state.extent = number22(extent2.apply(this, arguments));
      state.dim = dim;
      return state;
    }
    brush2.extent = function(_) {
      return arguments.length ? (extent2 = typeof _ === "function" ? _ : constant_default4(number22(_)), brush2) : extent2;
    };
    brush2.filter = function(_) {
      return arguments.length ? (filter2 = typeof _ === "function" ? _ : constant_default4(!!_), brush2) : filter2;
    };
    brush2.touchable = function(_) {
      return arguments.length ? (touchable = typeof _ === "function" ? _ : constant_default4(!!_), brush2) : touchable;
    };
    brush2.handleSize = function(_) {
      return arguments.length ? (handleSize = +_, brush2) : handleSize;
    };
    brush2.keyModifiers = function(_) {
      return arguments.length ? (keys = !!_, brush2) : keys;
    };
    brush2.on = function() {
      var value = listeners.on.apply(listeners, arguments);
      return value === listeners ? brush2 : value;
    };
    return brush2;
  }

  // node_modules/d3-path/src/path.js
  var pi = Math.PI;
  var tau = 2 * pi;
  var epsilon2 = 1e-6;
  var tauEpsilon = tau - epsilon2;
  function append(strings) {
    this._ += strings[0];
    for (let i = 1, n = strings.length; i < n; ++i) {
      this._ += arguments[i] + strings[i];
    }
  }
  function appendRound(digits) {
    let d = Math.floor(digits);
    if (!(d >= 0))
      throw new Error(`invalid digits: ${digits}`);
    if (d > 15)
      return append;
    const k = 10 ** d;
    return function(strings) {
      this._ += strings[0];
      for (let i = 1, n = strings.length; i < n; ++i) {
        this._ += Math.round(arguments[i] * k) / k + strings[i];
      }
    };
  }
  var Path = class {
    constructor(digits) {
      this._x0 = this._y0 = // start of current subpath
      this._x1 = this._y1 = null;
      this._ = "";
      this._append = digits == null ? append : appendRound(digits);
    }
    moveTo(x2, y2) {
      this._append`M${this._x0 = this._x1 = +x2},${this._y0 = this._y1 = +y2}`;
    }
    closePath() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._append`Z`;
      }
    }
    lineTo(x2, y2) {
      this._append`L${this._x1 = +x2},${this._y1 = +y2}`;
    }
    quadraticCurveTo(x1, y1, x2, y2) {
      this._append`Q${+x1},${+y1},${this._x1 = +x2},${this._y1 = +y2}`;
    }
    bezierCurveTo(x1, y1, x2, y2, x3, y3) {
      this._append`C${+x1},${+y1},${+x2},${+y2},${this._x1 = +x3},${this._y1 = +y3}`;
    }
    arcTo(x1, y1, x2, y2, r) {
      x1 = +x1, y1 = +y1, x2 = +x2, y2 = +y2, r = +r;
      if (r < 0)
        throw new Error(`negative radius: ${r}`);
      let x0 = this._x1, y0 = this._y1, x21 = x2 - x1, y21 = y2 - y1, x01 = x0 - x1, y01 = y0 - y1, l01_2 = x01 * x01 + y01 * y01;
      if (this._x1 === null) {
        this._append`M${this._x1 = x1},${this._y1 = y1}`;
      } else if (!(l01_2 > epsilon2))
        ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon2) || !r) {
        this._append`L${this._x1 = x1},${this._y1 = y1}`;
      } else {
        let x20 = x2 - x0, y20 = y2 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon2) {
          this._append`L${x1 + t01 * x01},${y1 + t01 * y01}`;
        }
        this._append`A${r},${r},0,0,${+(y01 * x20 > x01 * y20)},${this._x1 = x1 + t21 * x21},${this._y1 = y1 + t21 * y21}`;
      }
    }
    arc(x2, y2, r, a0, a1, ccw) {
      x2 = +x2, y2 = +y2, r = +r, ccw = !!ccw;
      if (r < 0)
        throw new Error(`negative radius: ${r}`);
      let dx = r * Math.cos(a0), dy = r * Math.sin(a0), x0 = x2 + dx, y0 = y2 + dy, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (this._x1 === null) {
        this._append`M${x0},${y0}`;
      } else if (Math.abs(this._x1 - x0) > epsilon2 || Math.abs(this._y1 - y0) > epsilon2) {
        this._append`L${x0},${y0}`;
      }
      if (!r)
        return;
      if (da < 0)
        da = da % tau + tau;
      if (da > tauEpsilon) {
        this._append`A${r},${r},0,1,${cw},${x2 - dx},${y2 - dy}A${r},${r},0,1,${cw},${this._x1 = x0},${this._y1 = y0}`;
      } else if (da > epsilon2) {
        this._append`A${r},${r},0,${+(da >= pi)},${cw},${this._x1 = x2 + r * Math.cos(a1)},${this._y1 = y2 + r * Math.sin(a1)}`;
      }
    }
    rect(x2, y2, w, h) {
      this._append`M${this._x0 = this._x1 = +x2},${this._y0 = this._y1 = +y2}h${w = +w}v${+h}h${-w}Z`;
    }
    toString() {
      return this._;
    }
  };
  function path() {
    return new Path();
  }
  path.prototype = Path.prototype;

  // node_modules/d3-format/src/formatDecimal.js
  function formatDecimal_default(x2) {
    return Math.abs(x2 = Math.round(x2)) >= 1e21 ? x2.toLocaleString("en").replace(/,/g, "") : x2.toString(10);
  }
  function formatDecimalParts(x2, p) {
    if ((i = (x2 = p ? x2.toExponential(p - 1) : x2.toExponential()).indexOf("e")) < 0)
      return null;
    var i, coefficient = x2.slice(0, i);
    return [
      coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
      +x2.slice(i + 1)
    ];
  }

  // node_modules/d3-format/src/exponent.js
  function exponent_default(x2) {
    return x2 = formatDecimalParts(Math.abs(x2)), x2 ? x2[1] : NaN;
  }

  // node_modules/d3-format/src/formatGroup.js
  function formatGroup_default(grouping, thousands) {
    return function(value, width) {
      var i = value.length, t = [], j = 0, g = grouping[0], length = 0;
      while (i > 0 && g > 0) {
        if (length + g + 1 > width)
          g = Math.max(1, width - length);
        t.push(value.substring(i -= g, i + g));
        if ((length += g + 1) > width)
          break;
        g = grouping[j = (j + 1) % grouping.length];
      }
      return t.reverse().join(thousands);
    };
  }

  // node_modules/d3-format/src/formatNumerals.js
  function formatNumerals_default(numerals) {
    return function(value) {
      return value.replace(/[0-9]/g, function(i) {
        return numerals[+i];
      });
    };
  }

  // node_modules/d3-format/src/formatSpecifier.js
  var re = /^(?:(.)?([<>=^]))?([+\-( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?(~)?([a-z%])?$/i;
  function formatSpecifier(specifier) {
    if (!(match = re.exec(specifier)))
      throw new Error("invalid format: " + specifier);
    var match;
    return new FormatSpecifier({
      fill: match[1],
      align: match[2],
      sign: match[3],
      symbol: match[4],
      zero: match[5],
      width: match[6],
      comma: match[7],
      precision: match[8] && match[8].slice(1),
      trim: match[9],
      type: match[10]
    });
  }
  formatSpecifier.prototype = FormatSpecifier.prototype;
  function FormatSpecifier(specifier) {
    this.fill = specifier.fill === void 0 ? " " : specifier.fill + "";
    this.align = specifier.align === void 0 ? ">" : specifier.align + "";
    this.sign = specifier.sign === void 0 ? "-" : specifier.sign + "";
    this.symbol = specifier.symbol === void 0 ? "" : specifier.symbol + "";
    this.zero = !!specifier.zero;
    this.width = specifier.width === void 0 ? void 0 : +specifier.width;
    this.comma = !!specifier.comma;
    this.precision = specifier.precision === void 0 ? void 0 : +specifier.precision;
    this.trim = !!specifier.trim;
    this.type = specifier.type === void 0 ? "" : specifier.type + "";
  }
  FormatSpecifier.prototype.toString = function() {
    return this.fill + this.align + this.sign + this.symbol + (this.zero ? "0" : "") + (this.width === void 0 ? "" : Math.max(1, this.width | 0)) + (this.comma ? "," : "") + (this.precision === void 0 ? "" : "." + Math.max(0, this.precision | 0)) + (this.trim ? "~" : "") + this.type;
  };

  // node_modules/d3-format/src/formatTrim.js
  function formatTrim_default(s) {
    out:
      for (var n = s.length, i = 1, i0 = -1, i1; i < n; ++i) {
        switch (s[i]) {
          case ".":
            i0 = i1 = i;
            break;
          case "0":
            if (i0 === 0)
              i0 = i;
            i1 = i;
            break;
          default:
            if (!+s[i])
              break out;
            if (i0 > 0)
              i0 = 0;
            break;
        }
      }
    return i0 > 0 ? s.slice(0, i0) + s.slice(i1 + 1) : s;
  }

  // node_modules/d3-format/src/formatPrefixAuto.js
  var prefixExponent;
  function formatPrefixAuto_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1], i = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1, n = coefficient.length;
    return i === n ? coefficient : i > n ? coefficient + new Array(i - n + 1).join("0") : i > 0 ? coefficient.slice(0, i) + "." + coefficient.slice(i) : "0." + new Array(1 - i).join("0") + formatDecimalParts(x2, Math.max(0, p + i - 1))[0];
  }

  // node_modules/d3-format/src/formatRounded.js
  function formatRounded_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1];
    return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1) : coefficient + new Array(exponent - coefficient.length + 2).join("0");
  }

  // node_modules/d3-format/src/formatTypes.js
  var formatTypes_default = {
    "%": (x2, p) => (x2 * 100).toFixed(p),
    "b": (x2) => Math.round(x2).toString(2),
    "c": (x2) => x2 + "",
    "d": formatDecimal_default,
    "e": (x2, p) => x2.toExponential(p),
    "f": (x2, p) => x2.toFixed(p),
    "g": (x2, p) => x2.toPrecision(p),
    "o": (x2) => Math.round(x2).toString(8),
    "p": (x2, p) => formatRounded_default(x2 * 100, p),
    "r": formatRounded_default,
    "s": formatPrefixAuto_default,
    "X": (x2) => Math.round(x2).toString(16).toUpperCase(),
    "x": (x2) => Math.round(x2).toString(16)
  };

  // node_modules/d3-format/src/identity.js
  function identity_default2(x2) {
    return x2;
  }

  // node_modules/d3-format/src/locale.js
  var map2 = Array.prototype.map;
  var prefixes = ["y", "z", "a", "f", "p", "n", "\xB5", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"];
  function locale_default(locale2) {
    var group = locale2.grouping === void 0 || locale2.thousands === void 0 ? identity_default2 : formatGroup_default(map2.call(locale2.grouping, Number), locale2.thousands + ""), currencyPrefix = locale2.currency === void 0 ? "" : locale2.currency[0] + "", currencySuffix = locale2.currency === void 0 ? "" : locale2.currency[1] + "", decimal = locale2.decimal === void 0 ? "." : locale2.decimal + "", numerals = locale2.numerals === void 0 ? identity_default2 : formatNumerals_default(map2.call(locale2.numerals, String)), percent = locale2.percent === void 0 ? "%" : locale2.percent + "", minus = locale2.minus === void 0 ? "\u2212" : locale2.minus + "", nan = locale2.nan === void 0 ? "NaN" : locale2.nan + "";
    function newFormat(specifier) {
      specifier = formatSpecifier(specifier);
      var fill = specifier.fill, align = specifier.align, sign = specifier.sign, symbol = specifier.symbol, zero3 = specifier.zero, width = specifier.width, comma = specifier.comma, precision = specifier.precision, trim = specifier.trim, type2 = specifier.type;
      if (type2 === "n")
        comma = true, type2 = "g";
      else if (!formatTypes_default[type2])
        precision === void 0 && (precision = 12), trim = true, type2 = "g";
      if (zero3 || fill === "0" && align === "=")
        zero3 = true, fill = "0", align = "=";
      var prefix = symbol === "$" ? currencyPrefix : symbol === "#" && /[boxX]/.test(type2) ? "0" + type2.toLowerCase() : "", suffix = symbol === "$" ? currencySuffix : /[%p]/.test(type2) ? percent : "";
      var formatType = formatTypes_default[type2], maybeSuffix = /[defgprs%]/.test(type2);
      precision = precision === void 0 ? 6 : /[gprs]/.test(type2) ? Math.max(1, Math.min(21, precision)) : Math.max(0, Math.min(20, precision));
      function format2(value) {
        var valuePrefix = prefix, valueSuffix = suffix, i, n, c2;
        if (type2 === "c") {
          valueSuffix = formatType(value) + valueSuffix;
          value = "";
        } else {
          value = +value;
          var valueNegative = value < 0 || 1 / value < 0;
          value = isNaN(value) ? nan : formatType(Math.abs(value), precision);
          if (trim)
            value = formatTrim_default(value);
          if (valueNegative && +value === 0 && sign !== "+")
            valueNegative = false;
          valuePrefix = (valueNegative ? sign === "(" ? sign : minus : sign === "-" || sign === "(" ? "" : sign) + valuePrefix;
          valueSuffix = (type2 === "s" ? prefixes[8 + prefixExponent / 3] : "") + valueSuffix + (valueNegative && sign === "(" ? ")" : "");
          if (maybeSuffix) {
            i = -1, n = value.length;
            while (++i < n) {
              if (c2 = value.charCodeAt(i), 48 > c2 || c2 > 57) {
                valueSuffix = (c2 === 46 ? decimal + value.slice(i + 1) : value.slice(i)) + valueSuffix;
                value = value.slice(0, i);
                break;
              }
            }
          }
        }
        if (comma && !zero3)
          value = group(value, Infinity);
        var length = valuePrefix.length + value.length + valueSuffix.length, padding = length < width ? new Array(width - length + 1).join(fill) : "";
        if (comma && zero3)
          value = group(padding + value, padding.length ? width - valueSuffix.length : Infinity), padding = "";
        switch (align) {
          case "<":
            value = valuePrefix + value + valueSuffix + padding;
            break;
          case "=":
            value = valuePrefix + padding + value + valueSuffix;
            break;
          case "^":
            value = padding.slice(0, length = padding.length >> 1) + valuePrefix + value + valueSuffix + padding.slice(length);
            break;
          default:
            value = padding + valuePrefix + value + valueSuffix;
            break;
        }
        return numerals(value);
      }
      format2.toString = function() {
        return specifier + "";
      };
      return format2;
    }
    function formatPrefix2(specifier, value) {
      var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)), e = Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3, k = Math.pow(10, -e), prefix = prefixes[8 + e / 3];
      return function(value2) {
        return f(k * value2) + prefix;
      };
    }
    return {
      format: newFormat,
      formatPrefix: formatPrefix2
    };
  }

  // node_modules/d3-format/src/defaultLocale.js
  var locale;
  var format;
  var formatPrefix;
  defaultLocale({
    thousands: ",",
    grouping: [3],
    currency: ["$", ""]
  });
  function defaultLocale(definition) {
    locale = locale_default(definition);
    format = locale.format;
    formatPrefix = locale.formatPrefix;
    return locale;
  }

  // node_modules/d3-format/src/precisionFixed.js
  function precisionFixed_default(step) {
    return Math.max(0, -exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionPrefix.js
  function precisionPrefix_default(step, value) {
    return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent_default(value) / 3))) * 3 - exponent_default(Math.abs(step)));
  }

  // node_modules/d3-format/src/precisionRound.js
  function precisionRound_default(step, max3) {
    step = Math.abs(step), max3 = Math.abs(max3) - step;
    return Math.max(0, exponent_default(max3) - exponent_default(step)) + 1;
  }

  // node_modules/d3-scale/src/init.js
  function initRange(domain, range2) {
    switch (arguments.length) {
      case 0:
        break;
      case 1:
        this.range(domain);
        break;
      default:
        this.range(range2).domain(domain);
        break;
    }
    return this;
  }
  function initInterpolator(domain, interpolator) {
    switch (arguments.length) {
      case 0:
        break;
      case 1: {
        if (typeof domain === "function")
          this.interpolator(domain);
        else
          this.range(domain);
        break;
      }
      default: {
        this.domain(domain);
        if (typeof interpolator === "function")
          this.interpolator(interpolator);
        else
          this.range(interpolator);
        break;
      }
    }
    return this;
  }

  // node_modules/d3-scale/src/ordinal.js
  var implicit = Symbol("implicit");
  function ordinal() {
    var index = new InternMap(), domain = [], range2 = [], unknown = implicit;
    function scale(d) {
      let i = index.get(d);
      if (i === void 0) {
        if (unknown !== implicit)
          return unknown;
        index.set(d, i = domain.push(d) - 1);
      }
      return range2[i % range2.length];
    }
    scale.domain = function(_) {
      if (!arguments.length)
        return domain.slice();
      domain = [], index = new InternMap();
      for (const value of _) {
        if (index.has(value))
          continue;
        index.set(value, domain.push(value) - 1);
      }
      return scale;
    };
    scale.range = function(_) {
      return arguments.length ? (range2 = Array.from(_), scale) : range2.slice();
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    scale.copy = function() {
      return ordinal(domain, range2).unknown(unknown);
    };
    initRange.apply(scale, arguments);
    return scale;
  }

  // node_modules/d3-scale/src/band.js
  function band() {
    var scale = ordinal().unknown(void 0), domain = scale.domain, ordinalRange = scale.range, r0 = 0, r1 = 1, step, bandwidth, round = false, paddingInner = 0, paddingOuter = 0, align = 0.5;
    delete scale.unknown;
    function rescale() {
      var n = domain().length, reverse = r1 < r0, start2 = reverse ? r1 : r0, stop = reverse ? r0 : r1;
      step = (stop - start2) / Math.max(1, n - paddingInner + paddingOuter * 2);
      if (round)
        step = Math.floor(step);
      start2 += (stop - start2 - step * (n - paddingInner)) * align;
      bandwidth = step * (1 - paddingInner);
      if (round)
        start2 = Math.round(start2), bandwidth = Math.round(bandwidth);
      var values = range(n).map(function(i) {
        return start2 + step * i;
      });
      return ordinalRange(reverse ? values.reverse() : values);
    }
    scale.domain = function(_) {
      return arguments.length ? (domain(_), rescale()) : domain();
    };
    scale.range = function(_) {
      return arguments.length ? ([r0, r1] = _, r0 = +r0, r1 = +r1, rescale()) : [r0, r1];
    };
    scale.rangeRound = function(_) {
      return [r0, r1] = _, r0 = +r0, r1 = +r1, round = true, rescale();
    };
    scale.bandwidth = function() {
      return bandwidth;
    };
    scale.step = function() {
      return step;
    };
    scale.round = function(_) {
      return arguments.length ? (round = !!_, rescale()) : round;
    };
    scale.padding = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, paddingOuter = +_), rescale()) : paddingInner;
    };
    scale.paddingInner = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, _), rescale()) : paddingInner;
    };
    scale.paddingOuter = function(_) {
      return arguments.length ? (paddingOuter = +_, rescale()) : paddingOuter;
    };
    scale.align = function(_) {
      return arguments.length ? (align = Math.max(0, Math.min(1, _)), rescale()) : align;
    };
    scale.copy = function() {
      return band(domain(), [r0, r1]).round(round).paddingInner(paddingInner).paddingOuter(paddingOuter).align(align);
    };
    return initRange.apply(rescale(), arguments);
  }
  function pointish(scale) {
    var copy3 = scale.copy;
    scale.padding = scale.paddingOuter;
    delete scale.paddingInner;
    delete scale.paddingOuter;
    scale.copy = function() {
      return pointish(copy3());
    };
    return scale;
  }
  function point() {
    return pointish(band.apply(null, arguments).paddingInner(1));
  }

  // node_modules/d3-scale/src/constant.js
  function constants(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-scale/src/number.js
  function number3(x2) {
    return +x2;
  }

  // node_modules/d3-scale/src/continuous.js
  var unit = [0, 1];
  function identity3(x2) {
    return x2;
  }
  function normalize(a, b) {
    return (b -= a = +a) ? function(x2) {
      return (x2 - a) / b;
    } : constants(isNaN(b) ? NaN : 0.5);
  }
  function clamper(a, b) {
    var t;
    if (a > b)
      t = a, a = b, b = t;
    return function(x2) {
      return Math.max(a, Math.min(b, x2));
    };
  }
  function bimap(domain, range2, interpolate) {
    var d0 = domain[0], d1 = domain[1], r0 = range2[0], r1 = range2[1];
    if (d1 < d0)
      d0 = normalize(d1, d0), r0 = interpolate(r1, r0);
    else
      d0 = normalize(d0, d1), r0 = interpolate(r0, r1);
    return function(x2) {
      return r0(d0(x2));
    };
  }
  function polymap(domain, range2, interpolate) {
    var j = Math.min(domain.length, range2.length) - 1, d = new Array(j), r = new Array(j), i = -1;
    if (domain[j] < domain[0]) {
      domain = domain.slice().reverse();
      range2 = range2.slice().reverse();
    }
    while (++i < j) {
      d[i] = normalize(domain[i], domain[i + 1]);
      r[i] = interpolate(range2[i], range2[i + 1]);
    }
    return function(x2) {
      var i2 = bisect_default(domain, x2, 1, j) - 1;
      return r[i2](d[i2](x2));
    };
  }
  function copy(source, target) {
    return target.domain(source.domain()).range(source.range()).interpolate(source.interpolate()).clamp(source.clamp()).unknown(source.unknown());
  }
  function transformer() {
    var domain = unit, range2 = unit, interpolate = value_default, transform2, untransform, unknown, clamp = identity3, piecewise, output, input;
    function rescale() {
      var n = Math.min(domain.length, range2.length);
      if (clamp !== identity3)
        clamp = clamper(domain[0], domain[n - 1]);
      piecewise = n > 2 ? polymap : bimap;
      output = input = null;
      return scale;
    }
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : (output || (output = piecewise(domain.map(transform2), range2, interpolate)))(transform2(clamp(x2)));
    }
    scale.invert = function(y2) {
      return clamp(untransform((input || (input = piecewise(range2, domain.map(transform2), number_default)))(y2)));
    };
    scale.domain = function(_) {
      return arguments.length ? (domain = Array.from(_, number3), rescale()) : domain.slice();
    };
    scale.range = function(_) {
      return arguments.length ? (range2 = Array.from(_), rescale()) : range2.slice();
    };
    scale.rangeRound = function(_) {
      return range2 = Array.from(_), interpolate = round_default, rescale();
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = _ ? true : identity3, rescale()) : clamp !== identity3;
    };
    scale.interpolate = function(_) {
      return arguments.length ? (interpolate = _, rescale()) : interpolate;
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t, u) {
      transform2 = t, untransform = u;
      return rescale();
    };
  }
  function continuous() {
    return transformer()(identity3, identity3);
  }

  // node_modules/d3-scale/src/tickFormat.js
  function tickFormat(start2, stop, count2, specifier) {
    var step = tickStep(start2, stop, count2), precision;
    specifier = formatSpecifier(specifier == null ? ",f" : specifier);
    switch (specifier.type) {
      case "s": {
        var value = Math.max(Math.abs(start2), Math.abs(stop));
        if (specifier.precision == null && !isNaN(precision = precisionPrefix_default(step, value)))
          specifier.precision = precision;
        return formatPrefix(specifier, value);
      }
      case "":
      case "e":
      case "g":
      case "p":
      case "r": {
        if (specifier.precision == null && !isNaN(precision = precisionRound_default(step, Math.max(Math.abs(start2), Math.abs(stop)))))
          specifier.precision = precision - (specifier.type === "e");
        break;
      }
      case "f":
      case "%": {
        if (specifier.precision == null && !isNaN(precision = precisionFixed_default(step)))
          specifier.precision = precision - (specifier.type === "%") * 2;
        break;
      }
    }
    return format(specifier);
  }

  // node_modules/d3-scale/src/linear.js
  function linearish(scale) {
    var domain = scale.domain;
    scale.ticks = function(count2) {
      var d = domain();
      return ticks(d[0], d[d.length - 1], count2 == null ? 10 : count2);
    };
    scale.tickFormat = function(count2, specifier) {
      var d = domain();
      return tickFormat(d[0], d[d.length - 1], count2 == null ? 10 : count2, specifier);
    };
    scale.nice = function(count2) {
      if (count2 == null)
        count2 = 10;
      var d = domain();
      var i0 = 0;
      var i1 = d.length - 1;
      var start2 = d[i0];
      var stop = d[i1];
      var prestep;
      var step;
      var maxIter = 10;
      if (stop < start2) {
        step = start2, start2 = stop, stop = step;
        step = i0, i0 = i1, i1 = step;
      }
      while (maxIter-- > 0) {
        step = tickIncrement(start2, stop, count2);
        if (step === prestep) {
          d[i0] = start2;
          d[i1] = stop;
          return domain(d);
        } else if (step > 0) {
          start2 = Math.floor(start2 / step) * step;
          stop = Math.ceil(stop / step) * step;
        } else if (step < 0) {
          start2 = Math.ceil(start2 * step) / step;
          stop = Math.floor(stop * step) / step;
        } else {
          break;
        }
        prestep = step;
      }
      return scale;
    };
    return scale;
  }
  function linear2() {
    var scale = continuous();
    scale.copy = function() {
      return copy(scale, linear2());
    };
    initRange.apply(scale, arguments);
    return linearish(scale);
  }

  // node_modules/d3-scale/src/quantize.js
  function quantize() {
    var x0 = 0, x1 = 1, n = 1, domain = [0.5], range2 = [0, 1], unknown;
    function scale(x2) {
      return x2 != null && x2 <= x2 ? range2[bisect_default(domain, x2, 0, n)] : unknown;
    }
    function rescale() {
      var i = -1;
      domain = new Array(n);
      while (++i < n)
        domain[i] = ((i + 1) * x1 - (i - n) * x0) / (n + 1);
      return scale;
    }
    scale.domain = function(_) {
      return arguments.length ? ([x0, x1] = _, x0 = +x0, x1 = +x1, rescale()) : [x0, x1];
    };
    scale.range = function(_) {
      return arguments.length ? (n = (range2 = Array.from(_)).length - 1, rescale()) : range2.slice();
    };
    scale.invertExtent = function(y2) {
      var i = range2.indexOf(y2);
      return i < 0 ? [NaN, NaN] : i < 1 ? [x0, domain[0]] : i >= n ? [domain[n - 1], x1] : [domain[i - 1], domain[i]];
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : scale;
    };
    scale.thresholds = function() {
      return domain.slice();
    };
    scale.copy = function() {
      return quantize().domain([x0, x1]).range(range2).unknown(unknown);
    };
    return initRange.apply(linearish(scale), arguments);
  }

  // node_modules/d3-scale/src/sequential.js
  function transformer2() {
    var x0 = 0, x1 = 1, t0, t1, k10, transform2, interpolator = identity3, clamp = false, unknown;
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : interpolator(k10 === 0 ? 0.5 : (x2 = (transform2(x2) - t0) * k10, clamp ? Math.max(0, Math.min(1, x2)) : x2));
    }
    scale.domain = function(_) {
      return arguments.length ? ([x0, x1] = _, t0 = transform2(x0 = +x0), t1 = transform2(x1 = +x1), k10 = t0 === t1 ? 0 : 1 / (t1 - t0), scale) : [x0, x1];
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = !!_, scale) : clamp;
    };
    scale.interpolator = function(_) {
      return arguments.length ? (interpolator = _, scale) : interpolator;
    };
    function range2(interpolate) {
      return function(_) {
        var r0, r1;
        return arguments.length ? ([r0, r1] = _, interpolator = interpolate(r0, r1), scale) : [interpolator(0), interpolator(1)];
      };
    }
    scale.range = range2(value_default);
    scale.rangeRound = range2(round_default);
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t) {
      transform2 = t, t0 = t(x0), t1 = t(x1), k10 = t0 === t1 ? 0 : 1 / (t1 - t0);
      return scale;
    };
  }
  function copy2(source, target) {
    return target.domain(source.domain()).interpolator(source.interpolator()).clamp(source.clamp()).unknown(source.unknown());
  }
  function sequential() {
    var scale = linearish(transformer2()(identity3));
    scale.copy = function() {
      return copy2(scale, sequential());
    };
    return initInterpolator.apply(scale, arguments);
  }

  // node_modules/d3-scale-chromatic/src/colors.js
  function colors_default(specifier) {
    var n = specifier.length / 6 | 0, colors = new Array(n), i = 0;
    while (i < n)
      colors[i] = "#" + specifier.slice(i * 6, ++i * 6);
    return colors;
  }

  // node_modules/d3-scale-chromatic/src/categorical/category10.js
  var category10_default = colors_default("1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf");

  // node_modules/d3-scale-chromatic/src/categorical/Accent.js
  var Accent_default = colors_default("7fc97fbeaed4fdc086ffff99386cb0f0027fbf5b17666666");

  // node_modules/d3-scale-chromatic/src/categorical/Dark2.js
  var Dark2_default = colors_default("1b9e77d95f027570b3e7298a66a61ee6ab02a6761d666666");

  // node_modules/d3-scale-chromatic/src/categorical/Paired.js
  var Paired_default = colors_default("a6cee31f78b4b2df8a33a02cfb9a99e31a1cfdbf6fff7f00cab2d66a3d9affff99b15928");

  // node_modules/d3-scale-chromatic/src/categorical/Set1.js
  var Set1_default = colors_default("e41a1c377eb84daf4a984ea3ff7f00ffff33a65628f781bf999999");

  // node_modules/d3-scale-chromatic/src/ramp.js
  var ramp_default = (scheme20) => rgbBasis(scheme20[scheme20.length - 1]);

  // node_modules/d3-scale-chromatic/src/diverging/RdBu.js
  var scheme = new Array(3).concat(
    "ef8a62f7f7f767a9cf",
    "ca0020f4a58292c5de0571b0",
    "ca0020f4a582f7f7f792c5de0571b0",
    "b2182bef8a62fddbc7d1e5f067a9cf2166ac",
    "b2182bef8a62fddbc7f7f7f7d1e5f067a9cf2166ac",
    "b2182bd6604df4a582fddbc7d1e5f092c5de4393c32166ac",
    "b2182bd6604df4a582fddbc7f7f7f7d1e5f092c5de4393c32166ac",
    "67001fb2182bd6604df4a582fddbc7d1e5f092c5de4393c32166ac053061",
    "67001fb2182bd6604df4a582fddbc7f7f7f7d1e5f092c5de4393c32166ac053061"
  ).map(colors_default);
  var RdBu_default = ramp_default(scheme);

  // node_modules/d3-scale-chromatic/src/sequential-multi/BuGn.js
  var scheme2 = new Array(3).concat(
    "e5f5f999d8c92ca25f",
    "edf8fbb2e2e266c2a4238b45",
    "edf8fbb2e2e266c2a42ca25f006d2c",
    "edf8fbccece699d8c966c2a42ca25f006d2c",
    "edf8fbccece699d8c966c2a441ae76238b45005824",
    "f7fcfde5f5f9ccece699d8c966c2a441ae76238b45005824",
    "f7fcfde5f5f9ccece699d8c966c2a441ae76238b45006d2c00441b"
  ).map(colors_default);
  var BuGn_default = ramp_default(scheme2);

  // node_modules/d3-scale-chromatic/src/sequential-multi/BuPu.js
  var scheme3 = new Array(3).concat(
    "e0ecf49ebcda8856a7",
    "edf8fbb3cde38c96c688419d",
    "edf8fbb3cde38c96c68856a7810f7c",
    "edf8fbbfd3e69ebcda8c96c68856a7810f7c",
    "edf8fbbfd3e69ebcda8c96c68c6bb188419d6e016b",
    "f7fcfde0ecf4bfd3e69ebcda8c96c68c6bb188419d6e016b",
    "f7fcfde0ecf4bfd3e69ebcda8c96c68c6bb188419d810f7c4d004b"
  ).map(colors_default);
  var BuPu_default = ramp_default(scheme3);

  // node_modules/d3-scale-chromatic/src/sequential-multi/GnBu.js
  var scheme4 = new Array(3).concat(
    "e0f3dba8ddb543a2ca",
    "f0f9e8bae4bc7bccc42b8cbe",
    "f0f9e8bae4bc7bccc443a2ca0868ac",
    "f0f9e8ccebc5a8ddb57bccc443a2ca0868ac",
    "f0f9e8ccebc5a8ddb57bccc44eb3d32b8cbe08589e",
    "f7fcf0e0f3dbccebc5a8ddb57bccc44eb3d32b8cbe08589e",
    "f7fcf0e0f3dbccebc5a8ddb57bccc44eb3d32b8cbe0868ac084081"
  ).map(colors_default);
  var GnBu_default = ramp_default(scheme4);

  // node_modules/d3-scale-chromatic/src/sequential-multi/OrRd.js
  var scheme5 = new Array(3).concat(
    "fee8c8fdbb84e34a33",
    "fef0d9fdcc8afc8d59d7301f",
    "fef0d9fdcc8afc8d59e34a33b30000",
    "fef0d9fdd49efdbb84fc8d59e34a33b30000",
    "fef0d9fdd49efdbb84fc8d59ef6548d7301f990000",
    "fff7ecfee8c8fdd49efdbb84fc8d59ef6548d7301f990000",
    "fff7ecfee8c8fdd49efdbb84fc8d59ef6548d7301fb300007f0000"
  ).map(colors_default);
  var OrRd_default = ramp_default(scheme5);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuBuGn.js
  var scheme6 = new Array(3).concat(
    "ece2f0a6bddb1c9099",
    "f6eff7bdc9e167a9cf02818a",
    "f6eff7bdc9e167a9cf1c9099016c59",
    "f6eff7d0d1e6a6bddb67a9cf1c9099016c59",
    "f6eff7d0d1e6a6bddb67a9cf3690c002818a016450",
    "fff7fbece2f0d0d1e6a6bddb67a9cf3690c002818a016450",
    "fff7fbece2f0d0d1e6a6bddb67a9cf3690c002818a016c59014636"
  ).map(colors_default);
  var PuBuGn_default = ramp_default(scheme6);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuBu.js
  var scheme7 = new Array(3).concat(
    "ece7f2a6bddb2b8cbe",
    "f1eef6bdc9e174a9cf0570b0",
    "f1eef6bdc9e174a9cf2b8cbe045a8d",
    "f1eef6d0d1e6a6bddb74a9cf2b8cbe045a8d",
    "f1eef6d0d1e6a6bddb74a9cf3690c00570b0034e7b",
    "fff7fbece7f2d0d1e6a6bddb74a9cf3690c00570b0034e7b",
    "fff7fbece7f2d0d1e6a6bddb74a9cf3690c00570b0045a8d023858"
  ).map(colors_default);
  var PuBu_default = ramp_default(scheme7);

  // node_modules/d3-scale-chromatic/src/sequential-multi/PuRd.js
  var scheme8 = new Array(3).concat(
    "e7e1efc994c7dd1c77",
    "f1eef6d7b5d8df65b0ce1256",
    "f1eef6d7b5d8df65b0dd1c77980043",
    "f1eef6d4b9dac994c7df65b0dd1c77980043",
    "f1eef6d4b9dac994c7df65b0e7298ace125691003f",
    "f7f4f9e7e1efd4b9dac994c7df65b0e7298ace125691003f",
    "f7f4f9e7e1efd4b9dac994c7df65b0e7298ace125698004367001f"
  ).map(colors_default);
  var PuRd_default = ramp_default(scheme8);

  // node_modules/d3-scale-chromatic/src/sequential-multi/RdPu.js
  var scheme9 = new Array(3).concat(
    "fde0ddfa9fb5c51b8a",
    "feebe2fbb4b9f768a1ae017e",
    "feebe2fbb4b9f768a1c51b8a7a0177",
    "feebe2fcc5c0fa9fb5f768a1c51b8a7a0177",
    "feebe2fcc5c0fa9fb5f768a1dd3497ae017e7a0177",
    "fff7f3fde0ddfcc5c0fa9fb5f768a1dd3497ae017e7a0177",
    "fff7f3fde0ddfcc5c0fa9fb5f768a1dd3497ae017e7a017749006a"
  ).map(colors_default);
  var RdPu_default = ramp_default(scheme9);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlGnBu.js
  var scheme10 = new Array(3).concat(
    "edf8b17fcdbb2c7fb8",
    "ffffcca1dab441b6c4225ea8",
    "ffffcca1dab441b6c42c7fb8253494",
    "ffffccc7e9b47fcdbb41b6c42c7fb8253494",
    "ffffccc7e9b47fcdbb41b6c41d91c0225ea80c2c84",
    "ffffd9edf8b1c7e9b47fcdbb41b6c41d91c0225ea80c2c84",
    "ffffd9edf8b1c7e9b47fcdbb41b6c41d91c0225ea8253494081d58"
  ).map(colors_default);
  var YlGnBu_default = ramp_default(scheme10);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlGn.js
  var scheme11 = new Array(3).concat(
    "f7fcb9addd8e31a354",
    "ffffccc2e69978c679238443",
    "ffffccc2e69978c67931a354006837",
    "ffffccd9f0a3addd8e78c67931a354006837",
    "ffffccd9f0a3addd8e78c67941ab5d238443005a32",
    "ffffe5f7fcb9d9f0a3addd8e78c67941ab5d238443005a32",
    "ffffe5f7fcb9d9f0a3addd8e78c67941ab5d238443006837004529"
  ).map(colors_default);
  var YlGn_default = ramp_default(scheme11);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlOrBr.js
  var scheme12 = new Array(3).concat(
    "fff7bcfec44fd95f0e",
    "ffffd4fed98efe9929cc4c02",
    "ffffd4fed98efe9929d95f0e993404",
    "ffffd4fee391fec44ffe9929d95f0e993404",
    "ffffd4fee391fec44ffe9929ec7014cc4c028c2d04",
    "ffffe5fff7bcfee391fec44ffe9929ec7014cc4c028c2d04",
    "ffffe5fff7bcfee391fec44ffe9929ec7014cc4c02993404662506"
  ).map(colors_default);
  var YlOrBr_default = ramp_default(scheme12);

  // node_modules/d3-scale-chromatic/src/sequential-multi/YlOrRd.js
  var scheme13 = new Array(3).concat(
    "ffeda0feb24cf03b20",
    "ffffb2fecc5cfd8d3ce31a1c",
    "ffffb2fecc5cfd8d3cf03b20bd0026",
    "ffffb2fed976feb24cfd8d3cf03b20bd0026",
    "ffffb2fed976feb24cfd8d3cfc4e2ae31a1cb10026",
    "ffffccffeda0fed976feb24cfd8d3cfc4e2ae31a1cb10026",
    "ffffccffeda0fed976feb24cfd8d3cfc4e2ae31a1cbd0026800026"
  ).map(colors_default);
  var YlOrRd_default = ramp_default(scheme13);

  // node_modules/d3-scale-chromatic/src/sequential-single/Blues.js
  var scheme14 = new Array(3).concat(
    "deebf79ecae13182bd",
    "eff3ffbdd7e76baed62171b5",
    "eff3ffbdd7e76baed63182bd08519c",
    "eff3ffc6dbef9ecae16baed63182bd08519c",
    "eff3ffc6dbef9ecae16baed64292c62171b5084594",
    "f7fbffdeebf7c6dbef9ecae16baed64292c62171b5084594",
    "f7fbffdeebf7c6dbef9ecae16baed64292c62171b508519c08306b"
  ).map(colors_default);
  var Blues_default = ramp_default(scheme14);

  // node_modules/d3-scale-chromatic/src/sequential-single/Greens.js
  var scheme15 = new Array(3).concat(
    "e5f5e0a1d99b31a354",
    "edf8e9bae4b374c476238b45",
    "edf8e9bae4b374c47631a354006d2c",
    "edf8e9c7e9c0a1d99b74c47631a354006d2c",
    "edf8e9c7e9c0a1d99b74c47641ab5d238b45005a32",
    "f7fcf5e5f5e0c7e9c0a1d99b74c47641ab5d238b45005a32",
    "f7fcf5e5f5e0c7e9c0a1d99b74c47641ab5d238b45006d2c00441b"
  ).map(colors_default);
  var Greens_default = ramp_default(scheme15);

  // node_modules/d3-scale-chromatic/src/sequential-single/Greys.js
  var scheme16 = new Array(3).concat(
    "f0f0f0bdbdbd636363",
    "f7f7f7cccccc969696525252",
    "f7f7f7cccccc969696636363252525",
    "f7f7f7d9d9d9bdbdbd969696636363252525",
    "f7f7f7d9d9d9bdbdbd969696737373525252252525",
    "fffffff0f0f0d9d9d9bdbdbd969696737373525252252525",
    "fffffff0f0f0d9d9d9bdbdbd969696737373525252252525000000"
  ).map(colors_default);
  var Greys_default = ramp_default(scheme16);

  // node_modules/d3-scale-chromatic/src/sequential-single/Purples.js
  var scheme17 = new Array(3).concat(
    "efedf5bcbddc756bb1",
    "f2f0f7cbc9e29e9ac86a51a3",
    "f2f0f7cbc9e29e9ac8756bb154278f",
    "f2f0f7dadaebbcbddc9e9ac8756bb154278f",
    "f2f0f7dadaebbcbddc9e9ac8807dba6a51a34a1486",
    "fcfbfdefedf5dadaebbcbddc9e9ac8807dba6a51a34a1486",
    "fcfbfdefedf5dadaebbcbddc9e9ac8807dba6a51a354278f3f007d"
  ).map(colors_default);
  var Purples_default = ramp_default(scheme17);

  // node_modules/d3-scale-chromatic/src/sequential-single/Reds.js
  var scheme18 = new Array(3).concat(
    "fee0d2fc9272de2d26",
    "fee5d9fcae91fb6a4acb181d",
    "fee5d9fcae91fb6a4ade2d26a50f15",
    "fee5d9fcbba1fc9272fb6a4ade2d26a50f15",
    "fee5d9fcbba1fc9272fb6a4aef3b2ccb181d99000d",
    "fff5f0fee0d2fcbba1fc9272fb6a4aef3b2ccb181d99000d",
    "fff5f0fee0d2fcbba1fc9272fb6a4aef3b2ccb181da50f1567000d"
  ).map(colors_default);
  var Reds_default = ramp_default(scheme18);

  // node_modules/d3-scale-chromatic/src/sequential-single/Oranges.js
  var scheme19 = new Array(3).concat(
    "fee6cefdae6be6550d",
    "feeddefdbe85fd8d3cd94701",
    "feeddefdbe85fd8d3ce6550da63603",
    "feeddefdd0a2fdae6bfd8d3ce6550da63603",
    "feeddefdd0a2fdae6bfd8d3cf16913d948018c2d04",
    "fff5ebfee6cefdd0a2fdae6bfd8d3cf16913d948018c2d04",
    "fff5ebfee6cefdd0a2fdae6bfd8d3cf16913d94801a636037f2704"
  ).map(colors_default);
  var Oranges_default = ramp_default(scheme19);

  // node_modules/d3-scale-chromatic/src/sequential-multi/cubehelix.js
  var cubehelix_default2 = cubehelixLong(cubehelix(300, 0.5, 0), cubehelix(-240, 0.5, 1));

  // node_modules/d3-scale-chromatic/src/sequential-multi/rainbow.js
  var warm = cubehelixLong(cubehelix(-100, 0.75, 0.35), cubehelix(80, 1.5, 0.8));
  var cool = cubehelixLong(cubehelix(260, 0.75, 0.35), cubehelix(80, 1.5, 0.8));
  var c = cubehelix();
  function rainbow_default(t) {
    if (t < 0 || t > 1)
      t -= Math.floor(t);
    var ts = Math.abs(t - 0.5);
    c.h = 360 * t - 100;
    c.s = 1.5 - 1.5 * ts;
    c.l = 0.8 - 0.9 * ts;
    return c + "";
  }

  // node_modules/d3-scale-chromatic/src/sequential-multi/viridis.js
  function ramp(range2) {
    var n = range2.length;
    return function(t) {
      return range2[Math.max(0, Math.min(n - 1, Math.floor(t * n)))];
    };
  }
  var viridis_default = ramp(colors_default("44015444025645045745055946075a46085c460a5d460b5e470d60470e6147106347116447136548146748166848176948186a481a6c481b6d481c6e481d6f481f70482071482173482374482475482576482677482878482979472a7a472c7a472d7b472e7c472f7d46307e46327e46337f463480453581453781453882443983443a83443b84433d84433e85423f854240864241864142874144874045884046883f47883f48893e49893e4a893e4c8a3d4d8a3d4e8a3c4f8a3c508b3b518b3b528b3a538b3a548c39558c39568c38588c38598c375a8c375b8d365c8d365d8d355e8d355f8d34608d34618d33628d33638d32648e32658e31668e31678e31688e30698e306a8e2f6b8e2f6c8e2e6d8e2e6e8e2e6f8e2d708e2d718e2c718e2c728e2c738e2b748e2b758e2a768e2a778e2a788e29798e297a8e297b8e287c8e287d8e277e8e277f8e27808e26818e26828e26828e25838e25848e25858e24868e24878e23888e23898e238a8d228b8d228c8d228d8d218e8d218f8d21908d21918c20928c20928c20938c1f948c1f958b1f968b1f978b1f988b1f998a1f9a8a1e9b8a1e9c891e9d891f9e891f9f881fa0881fa1881fa1871fa28720a38620a48621a58521a68522a78522a88423a98324aa8325ab8225ac8226ad8127ad8128ae8029af7f2ab07f2cb17e2db27d2eb37c2fb47c31b57b32b67a34b67935b77937b87838b9773aba763bbb753dbc743fbc7340bd7242be7144bf7046c06f48c16e4ac16d4cc26c4ec36b50c46a52c56954c56856c66758c7655ac8645cc8635ec96260ca6063cb5f65cb5e67cc5c69cd5b6ccd5a6ece5870cf5773d05675d05477d1537ad1517cd2507fd34e81d34d84d44b86d54989d5488bd6468ed64590d74393d74195d84098d83e9bd93c9dd93ba0da39a2da37a5db36a8db34aadc32addc30b0dd2fb2dd2db5de2bb8de29bade28bddf26c0df25c2df23c5e021c8e020cae11fcde11dd0e11cd2e21bd5e21ad8e219dae319dde318dfe318e2e418e5e419e7e419eae51aece51befe51cf1e51df4e61ef6e620f8e621fbe723fde725"));
  var magma = ramp(colors_default("00000401000501010601010802010902020b02020d03030f03031204041405041606051806051a07061c08071e0907200a08220b09240c09260d0a290e0b2b100b2d110c2f120d31130d34140e36150e38160f3b180f3d19103f1a10421c10441d11471e114920114b21114e22115024125325125527125829115a2a115c2c115f2d11612f116331116533106734106936106b38106c390f6e3b0f703d0f713f0f72400f74420f75440f764510774710784910784a10794c117a4e117b4f127b51127c52137c54137d56147d57157e59157e5a167e5c167f5d177f5f187f601880621980641a80651a80671b80681c816a1c816b1d816d1d816e1e81701f81721f817320817521817621817822817922827b23827c23827e24828025828125818326818426818627818827818928818b29818c29818e2a81902a81912b81932b80942c80962c80982d80992d809b2e7f9c2e7f9e2f7fa02f7fa1307ea3307ea5317ea6317da8327daa337dab337cad347cae347bb0357bb2357bb3367ab5367ab73779b83779ba3878bc3978bd3977bf3a77c03a76c23b75c43c75c53c74c73d73c83e73ca3e72cc3f71cd4071cf4070d0416fd2426fd3436ed5446dd6456cd8456cd9466bdb476adc4869de4968df4a68e04c67e24d66e34e65e44f64e55064e75263e85362e95462ea5661eb5760ec5860ed5a5fee5b5eef5d5ef05f5ef1605df2625df2645cf3655cf4675cf4695cf56b5cf66c5cf66e5cf7705cf7725cf8745cf8765cf9785df9795df97b5dfa7d5efa7f5efa815ffb835ffb8560fb8761fc8961fc8a62fc8c63fc8e64fc9065fd9266fd9467fd9668fd9869fd9a6afd9b6bfe9d6cfe9f6dfea16efea36ffea571fea772fea973feaa74feac76feae77feb078feb27afeb47bfeb67cfeb77efeb97ffebb81febd82febf84fec185fec287fec488fec68afec88cfeca8dfecc8ffecd90fecf92fed194fed395fed597fed799fed89afdda9cfddc9efddea0fde0a1fde2a3fde3a5fde5a7fde7a9fde9aafdebacfcecaefceeb0fcf0b2fcf2b4fcf4b6fcf6b8fcf7b9fcf9bbfcfbbdfcfdbf"));
  var inferno = ramp(colors_default("00000401000501010601010802010a02020c02020e03021004031204031405041706041907051b08051d09061f0a07220b07240c08260d08290e092b10092d110a30120a32140b34150b37160b39180c3c190c3e1b0c411c0c431e0c451f0c48210c4a230c4c240c4f260c51280b53290b552b0b572d0b592f0a5b310a5c320a5e340a5f3609613809623909633b09643d09653e0966400a67420a68440a68450a69470b6a490b6a4a0c6b4c0c6b4d0d6c4f0d6c510e6c520e6d540f6d550f6d57106e59106e5a116e5c126e5d126e5f136e61136e62146e64156e65156e67166e69166e6a176e6c186e6d186e6f196e71196e721a6e741a6e751b6e771c6d781c6d7a1d6d7c1d6d7d1e6d7f1e6c801f6c82206c84206b85216b87216b88226a8a226a8c23698d23698f24699025689225689326679526679727669827669a28659b29649d29649f2a63a02a63a22b62a32c61a52c60a62d60a82e5fa92e5eab2f5ead305dae305cb0315bb1325ab3325ab43359b63458b73557b93556ba3655bc3754bd3853bf3952c03a51c13a50c33b4fc43c4ec63d4dc73e4cc83f4bca404acb4149cc4248ce4347cf4446d04545d24644d34743d44842d54a41d74b3fd84c3ed94d3dda4e3cdb503bdd513ade5238df5337e05536e15635e25734e35933e45a31e55c30e65d2fe75e2ee8602de9612bea632aeb6429eb6628ec6726ed6925ee6a24ef6c23ef6e21f06f20f1711ff1731df2741cf3761bf37819f47918f57b17f57d15f67e14f68013f78212f78410f8850ff8870ef8890cf98b0bf98c0af98e09fa9008fa9207fa9407fb9606fb9706fb9906fb9b06fb9d07fc9f07fca108fca309fca50afca60cfca80dfcaa0ffcac11fcae12fcb014fcb216fcb418fbb61afbb81dfbba1ffbbc21fbbe23fac026fac228fac42afac62df9c72ff9c932f9cb35f8cd37f8cf3af7d13df7d340f6d543f6d746f5d949f5db4cf4dd4ff4df53f4e156f3e35af3e55df2e661f2e865f2ea69f1ec6df1ed71f1ef75f1f179f2f27df2f482f3f586f3f68af4f88ef5f992f6fa96f8fb9af9fc9dfafda1fcffa4"));
  var plasma = ramp(colors_default("0d088710078813078916078a19068c1b068d1d068e20068f2206902406912605912805922a05932c05942e05952f059631059733059735049837049938049a3a049a3c049b3e049c3f049c41049d43039e44039e46039f48039f4903a04b03a14c02a14e02a25002a25102a35302a35502a45601a45801a45901a55b01a55c01a65e01a66001a66100a76300a76400a76600a76700a86900a86a00a86c00a86e00a86f00a87100a87201a87401a87501a87701a87801a87a02a87b02a87d03a87e03a88004a88104a78305a78405a78606a68707a68808a68a09a58b0aa58d0ba58e0ca48f0da4910ea3920fa39410a29511a19613a19814a099159f9a169f9c179e9d189d9e199da01a9ca11b9ba21d9aa31e9aa51f99a62098a72197a82296aa2395ab2494ac2694ad2793ae2892b02991b12a90b22b8fb32c8eb42e8db52f8cb6308bb7318ab83289ba3388bb3488bc3587bd3786be3885bf3984c03a83c13b82c23c81c33d80c43e7fc5407ec6417dc7427cc8437bc9447aca457acb4679cc4778cc4977cd4a76ce4b75cf4c74d04d73d14e72d24f71d35171d45270d5536fd5546ed6556dd7566cd8576bd9586ada5a6ada5b69db5c68dc5d67dd5e66de5f65de6164df6263e06363e16462e26561e26660e3685fe4695ee56a5de56b5de66c5ce76e5be76f5ae87059e97158e97257ea7457eb7556eb7655ec7754ed7953ed7a52ee7b51ef7c51ef7e50f07f4ff0804ef1814df1834cf2844bf3854bf3874af48849f48948f58b47f58c46f68d45f68f44f79044f79143f79342f89441f89540f9973ff9983ef99a3efa9b3dfa9c3cfa9e3bfb9f3afba139fba238fca338fca537fca636fca835fca934fdab33fdac33fdae32fdaf31fdb130fdb22ffdb42ffdb52efeb72dfeb82cfeba2cfebb2bfebd2afebe2afec029fdc229fdc328fdc527fdc627fdc827fdca26fdcb26fccd25fcce25fcd025fcd225fbd324fbd524fbd724fad824fada24f9dc24f9dd25f8df25f8e125f7e225f7e425f6e626f6e826f5e926f5eb27f4ed27f3ee27f3f027f2f227f1f426f1f525f0f724f0f921"));

  // node_modules/d3-shape/src/constant.js
  function constant_default5(x2) {
    return function constant2() {
      return x2;
    };
  }

  // node_modules/d3-shape/src/path.js
  function withPath(shape) {
    let digits = 3;
    shape.digits = function(_) {
      if (!arguments.length)
        return digits;
      if (_ == null) {
        digits = null;
      } else {
        const d = Math.floor(_);
        if (!(d >= 0))
          throw new RangeError(`invalid digits: ${_}`);
        digits = d;
      }
      return shape;
    };
    return () => new Path(digits);
  }

  // node_modules/d3-shape/src/array.js
  var slice2 = Array.prototype.slice;
  function array_default(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-shape/src/curve/linear.js
  function Linear(context) {
    this._context = context;
  }
  Linear.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._point = 0;
    },
    lineEnd: function() {
      if (this._line || this._line !== 0 && this._point === 1)
        this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x2, y2) {
      x2 = +x2, y2 = +y2;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x2, y2) : this._context.moveTo(x2, y2);
          break;
        case 1:
          this._point = 2;
        default:
          this._context.lineTo(x2, y2);
          break;
      }
    }
  };
  function linear_default(context) {
    return new Linear(context);
  }

  // node_modules/d3-shape/src/point.js
  function x(p) {
    return p[0];
  }
  function y(p) {
    return p[1];
  }

  // node_modules/d3-shape/src/line.js
  function line_default(x2, y2) {
    var defined = constant_default5(true), context = null, curve = linear_default, output = null, path2 = withPath(line);
    x2 = typeof x2 === "function" ? x2 : x2 === void 0 ? x : constant_default5(x2);
    y2 = typeof y2 === "function" ? y2 : y2 === void 0 ? y : constant_default5(y2);
    function line(data) {
      var i, n = (data = array_default(data)).length, d, defined0 = false, buffer;
      if (context == null)
        output = curve(buffer = path2());
      for (i = 0; i <= n; ++i) {
        if (!(i < n && defined(d = data[i], i, data)) === defined0) {
          if (defined0 = !defined0)
            output.lineStart();
          else
            output.lineEnd();
        }
        if (defined0)
          output.point(+x2(d, i, data), +y2(d, i, data));
      }
      if (buffer)
        return output = null, buffer + "" || null;
    }
    line.x = function(_) {
      return arguments.length ? (x2 = typeof _ === "function" ? _ : constant_default5(+_), line) : x2;
    };
    line.y = function(_) {
      return arguments.length ? (y2 = typeof _ === "function" ? _ : constant_default5(+_), line) : y2;
    };
    line.defined = function(_) {
      return arguments.length ? (defined = typeof _ === "function" ? _ : constant_default5(!!_), line) : defined;
    };
    line.curve = function(_) {
      return arguments.length ? (curve = _, context != null && (output = curve(context)), line) : curve;
    };
    line.context = function(_) {
      return arguments.length ? (_ == null ? context = output = null : output = curve(context = _), line) : context;
    };
    return line;
  }

  // node_modules/d3-shape/src/offset/none.js
  function none_default(series, order) {
    if (!((n = series.length) > 1))
      return;
    for (var i = 1, j, s0, s1 = series[order[0]], n, m = s1.length; i < n; ++i) {
      s0 = s1, s1 = series[order[i]];
      for (j = 0; j < m; ++j) {
        s1[j][1] += s1[j][0] = isNaN(s0[j][1]) ? s0[j][0] : s0[j][1];
      }
    }
  }

  // node_modules/d3-shape/src/order/none.js
  function none_default2(series) {
    var n = series.length, o = new Array(n);
    while (--n >= 0)
      o[n] = n;
    return o;
  }

  // node_modules/d3-shape/src/stack.js
  function stackValue(d, key) {
    return d[key];
  }
  function stackSeries(key) {
    const series = [];
    series.key = key;
    return series;
  }
  function stack_default() {
    var keys = constant_default5([]), order = none_default2, offset = none_default, value = stackValue;
    function stack(data) {
      var sz = Array.from(keys.apply(this, arguments), stackSeries), i, n = sz.length, j = -1, oz;
      for (const d of data) {
        for (i = 0, ++j; i < n; ++i) {
          (sz[i][j] = [0, +value(d, sz[i].key, j, data)]).data = d;
        }
      }
      for (i = 0, oz = array_default(order(sz)); i < n; ++i) {
        sz[oz[i]].index = i;
      }
      offset(sz, oz);
      return sz;
    }
    stack.keys = function(_) {
      return arguments.length ? (keys = typeof _ === "function" ? _ : constant_default5(Array.from(_)), stack) : keys;
    };
    stack.value = function(_) {
      return arguments.length ? (value = typeof _ === "function" ? _ : constant_default5(+_), stack) : value;
    };
    stack.order = function(_) {
      return arguments.length ? (order = _ == null ? none_default2 : typeof _ === "function" ? _ : constant_default5(Array.from(_)), stack) : order;
    };
    stack.offset = function(_) {
      return arguments.length ? (offset = _ == null ? none_default : _, stack) : offset;
    };
    return stack;
  }

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x2, y2) {
    this.k = k;
    this.x = x2;
    this.y = y2;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x2, y2) {
      return x2 === 0 & y2 === 0 ? this : new Transform(this.k, this.x + this.k * x2, this.y + this.k * y2);
    },
    apply: function(point2) {
      return [point2[0] * this.k + this.x, point2[1] * this.k + this.y];
    },
    applyX: function(x2) {
      return x2 * this.k + this.x;
    },
    applyY: function(y2) {
      return y2 * this.k + this.y;
    },
    invert: function(location) {
      return [(location[0] - this.x) / this.k, (location[1] - this.y) / this.k];
    },
    invertX: function(x2) {
      return (x2 - this.x) / this.k;
    },
    invertY: function(y2) {
      return (y2 - this.y) / this.k;
    },
    rescaleX: function(x2) {
      return x2.copy().domain(x2.range().map(this.invertX, this).map(x2.invert, x2));
    },
    rescaleY: function(y2) {
      return y2.copy().domain(y2.range().map(this.invertY, this).map(y2.invert, y2));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity4 = new Transform(1, 0, 0);
  transform.prototype = Transform.prototype;
  function transform(node) {
    while (!node.__zoom)
      if (!(node = node.parentNode))
        return identity4;
    return node.__zoom;
  }

  // src/Typescript/brushSlider.ts
  var BrushSlider = class {
    constructor(parallelPlot) {
      this.dimIndexScale = point();
      this.dimIndexScaleInvertFn = quantize();
      this.inSelectionDrag = false;
      this.parallelPlot = parallelPlot;
      this.updateDimIndexScale();
      const axis2 = select_default2(parallelPlot.bindto + " .slider").append("g").attr("pointer-events", "none").attr("class", "axisGroup").call(axisBottom(this.dimIndexScale).tickSize(0).tickFormat(() => ""));
      select_default2(parallelPlot.bindto).append("div").attr("class", "sliderTooltip").style("display", "none");
      this.createBrush();
      axis2.append("line").attr("class", "locatorLine").attr("x1", 50).attr("y1", -4).attr("x2", 50).attr("y2", 4).style("display", "none").attr("pointer-events", "none");
      select_default2(parallelPlot.bindto + " .brushDim").call(
        brushX().move,
        [
          this.dimIndexScale(this.parallelPlot.startingDimIndex),
          this.dimIndexScale(this.parallelPlot.startingDimIndex + this.parallelPlot.visibleDimCount - 1)
        ]
      );
    }
    updateDimIndexScale() {
      const size = this.parallelPlot.width - ParallelPlot.margin.left - ParallelPlot.margin.right;
      this.dimIndexScale.domain(range(this.parallelPlot.dimensions.length)).range([0, size]);
      this.dimIndexScaleInvertFn.domain([0, size]).range(range(this.parallelPlot.dimensions.length));
    }
    centerBrush(indexCenter, moveBrush) {
      const sizeDimVisible = this.parallelPlot.visibleDimensions.length;
      let sizeLeft = Math.round((sizeDimVisible - 1) / 2);
      let sizeRight = sizeDimVisible - 1 - sizeLeft;
      if (indexCenter - sizeLeft < 0) {
        sizeRight = sizeRight + (sizeLeft - indexCenter);
        sizeLeft = indexCenter;
      }
      if (indexCenter + sizeRight > this.parallelPlot.dimensions.length - 1) {
        sizeLeft = sizeLeft + (indexCenter + sizeRight - this.parallelPlot.dimensions.length + 1);
        sizeRight = this.parallelPlot.dimensions.length - 1 - indexCenter;
      }
      const begin = indexCenter - sizeLeft;
      const end = indexCenter + sizeRight;
      if (begin !== this.parallelPlot.startingDimIndex || end !== this.parallelPlot.startingDimIndex + this.parallelPlot.visibleDimCount - 1) {
        this.updateVisibleDimensions(begin, end);
        this.parallelPlot.buildPlotArea();
        this.parallelPlot.style.applyCssRules();
        select_default2(this.parallelPlot.bindto + " .sliderTooltip").style("display", "none");
      }
      if (moveBrush) {
        select_default2(this.parallelPlot.bindto + " .brushDim").call(
          brushX().move,
          [
            this.dimIndexScale(this.parallelPlot.startingDimIndex),
            this.dimIndexScale(this.parallelPlot.startingDimIndex + this.parallelPlot.visibleDimCount - 1)
          ]
        );
      }
    }
    mouseDown(mouse) {
      this.centerBrush(this.dimIndexScaleInvertFn(mouse[0]), true);
    }
    mouseMove(mouse) {
      select_default2(this.parallelPlot.bindto + " .locatorLine").style("display", null).attr("x1", mouse[0]).attr("x2", mouse[0]);
      const dimIndex = this.dimIndexScaleInvertFn(mouse[0]);
      select_default2(this.parallelPlot.bindto + " .sliderTooltip").text(this.parallelPlot.columns[this.parallelPlot.dimensions[dimIndex]].label).style("left", mouse[0] - ParallelPlot.margin.left + "px").style("top", ParallelPlot.margin.top / 4 - 30 + "px").style("display", null);
    }
    mouseExit() {
      select_default2(this.parallelPlot.bindto + " .locatorLine").style("display", "none");
      select_default2(this.parallelPlot.bindto + " .sliderTooltip").style("display", "none");
    }
    // eslint-disable-next-line max-lines-per-function
    createBrush() {
      this.inSelectionDrag = false;
      select_default2(this.parallelPlot.bindto + " .slider").append("g").attr("class", "brushDim").call(this.xBrushBehavior()).call(
        (g) => g.select(this.parallelPlot.bindto + " .overlay").attr("rx", 5).attr("ry", 5).on("mousedown touchstart", (event) => {
          this.mouseDown(pointer_default(event));
          event.stopPropagation();
        }).on("mousemove", (event) => {
          this.mouseMove(pointer_default(event));
        }).on("mouseout", () => {
          this.mouseExit();
        })
      ).call(
        (g) => g.select(this.parallelPlot.bindto + " .selection").attr("rx", 5).attr("ry", 5).on("mousedown", () => {
          this.inSelectionDrag = true;
        }).on("mouseup", () => {
          this.inSelectionDrag = false;
        })
      );
    }
    xBrushBehavior() {
      return brushX().handleSize(4).extent([
        [0, -5],
        [this.parallelPlot.width - ParallelPlot.margin.left - ParallelPlot.margin.right, 5]
      ]).on("brush", (event) => {
        const selection2 = event.selection;
        if (this.inSelectionDrag) {
          const brushCenter = (selection2[0] + selection2[1]) / 2;
          const centerIndex = this.dimIndexScaleInvertFn(brushCenter);
          this.centerBrush(centerIndex, false);
        } else {
          const begin = this.dimIndexScaleInvertFn(selection2[0]);
          const end = this.dimIndexScaleInvertFn(selection2[1]);
          if (begin !== this.parallelPlot.startingDimIndex || end !== this.parallelPlot.startingDimIndex + this.parallelPlot.visibleDimCount - 1) {
            this.updateVisibleDimensions(begin, end);
            this.parallelPlot.buildPlotArea();
            this.parallelPlot.style.applyCssRules();
          }
        }
      }).on("end", () => {
        this.inSelectionDrag = false;
      });
    }
    updateVisibleDimensions(begin, end) {
      if (begin >= 0 && end >= 0) {
        this.parallelPlot.startingDimIndex = begin;
        this.parallelPlot.visibleDimCount = end - begin + 1;
        this.parallelPlot.updateVisibleDimensions();
      }
    }
  };

  // src/Typescript/categorical.ts
  var Categorical = class _Categorical {
    constructor(column, categories) {
      /** for each category, how many rows are to spread */
      this.rowCountByCat = [];
      /** for each category, an array with lower and upper bounds of its 'rect' (scaled values in axis range) */
      this.boundsByCat = [];
      /** 
       * Y-offset to apply for each trace point of this column when 'fromNone' arrange method is used. 
       */
      this.fromNoneOffsets = null;
      /** Y-offset to apply for each trace point of this column when 'fromLeft' arrange method is used. */
      this.fromLeftOffsets = null;
      /** 
       * Y-offset to apply:
       * - for each trace point of this column when 'fromRight' arrange method is used;
       * - for each outgoing trace point of this column when 'fromBoth' arrange method is used.
       */
      this.fromRightOffsets = null;
      /** 
       * Y-offset to apply for each incoming trace point of this column when 'fromBoth' arrange method is used. 
       */
      this.fromBothInOffsets = null;
      this.dragCat = null;
      this.initialDragCategories = null;
      this.keptCatIndexes = null;
      this.column = column;
      this.categories = categories;
      this.histoScale = linear2();
      this.stackGenerator = stack_default().keys(range(this.categories.length)).value((d, key) => {
        return d[key];
      });
      this.initDone = false;
    }
    static {
      this.TOTAL_SPACE_BETWEEN = 30;
    }
    checkInitDone() {
      if (!this.initDone) {
        this.initRowCountByCat();
        this.initBoundsByCat();
        this.initDone = true;
      }
    }
    unvalidateInit() {
      this.fromNoneOffsets = null;
      this.fromLeftOffsets = null;
      this.fromRightOffsets = null;
      this.fromBothInOffsets = null;
      this.initDone = false;
    }
    initRowCountByCat() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      this.rowCountByCat = this.categories.map(() => 0);
      parallelPlot.sampleData.forEach((row) => {
        const rowCount = this.rowCountByCat[row[columnDim]];
        if (typeof rowCount !== "undefined") {
          this.rowCountByCat[row[columnDim]] = rowCount + 1;
        }
      });
    }
    // eslint-disable-next-line max-lines-per-function
    buildOffsetsFor(arrangeMethod) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      let orderedRowIndexes;
      if (arrangeMethod === ParallelPlot.ARRANGE_FROM_LEFT) {
        orderedRowIndexes = this.buildFromLeftRowIndexes();
      } else if (arrangeMethod === ParallelPlot.ARRANGE_FROM_RIGHT) {
        orderedRowIndexes = this.buildFromRightRowIndexes();
      } else if (arrangeMethod === ParallelPlot.ARRANGE_FROM_BOTH) {
        orderedRowIndexes = this.buildFromLeftRowIndexes(true);
      } else {
        orderedRowIndexes = range(parallelPlot.sampleData.length);
      }
      const rowPositionByCat = this.categories.map(() => 0);
      let rowPositionsByRow = [];
      orderedRowIndexes.forEach((orderedRowIndex) => {
        const row = parallelPlot.sampleData[orderedRowIndex];
        const rowPosition = rowPositionByCat[row[columnDim]];
        if (typeof rowPosition === "undefined") {
          rowPositionsByRow.push(NaN);
        } else {
          rowPositionByCat[row[columnDim]] = rowPosition + 1;
          rowPositionsByRow.push(rowPosition);
        }
      });
      const permutedPositions = new Array(rowPositionsByRow.length);
      orderedRowIndexes.forEach((orderedRowIndex, i) => {
        permutedPositions[orderedRowIndex] = rowPositionsByRow[i];
      });
      rowPositionsByRow = permutedPositions;
      const offsets = [];
      parallelPlot.sampleData.forEach((row, i) => {
        const rowCount = this.rowCountByCat[row[columnDim]];
        if (typeof rowCount === "undefined") {
          offsets.push(0);
        } else {
          let spreaderScale = parallelPlot.catSpreaderMap.get(rowCount);
          if (typeof spreaderScale === "undefined") {
            spreaderScale = point().domain(range(rowCount)).padding(0.8);
            parallelPlot.catSpreaderMap.set(rowCount, spreaderScale);
          }
          const bounds = this.boundsByCat[row[columnDim]];
          const halfLength = Math.abs(bounds[1] - bounds[0]) / 2;
          spreaderScale.range([-halfLength, halfLength]);
          const offset = spreaderScale(rowPositionsByRow[i]);
          if (typeof offset === "undefined") {
            offsets.push(0);
          } else {
            offsets.push(offset);
          }
        }
      });
      return offsets;
    }
    getFromNoneOffsets() {
      if (this.fromNoneOffsets === null) {
        this.fromNoneOffsets = this.buildOffsetsFor(ParallelPlot.ARRANGE_FROM_NONE);
      }
      return this.fromNoneOffsets;
    }
    getFromLeftOffsets() {
      if (this.fromLeftOffsets === null) {
        this.fromLeftOffsets = this.buildOffsetsFor(ParallelPlot.ARRANGE_FROM_LEFT);
      }
      return this.fromLeftOffsets;
    }
    getFromRightOffsets() {
      if (this.fromRightOffsets === null) {
        this.fromRightOffsets = this.buildOffsetsFor(ParallelPlot.ARRANGE_FROM_RIGHT);
      }
      return this.fromRightOffsets;
    }
    getFromBothInOffsets() {
      if (this.fromBothInOffsets === null) {
        this.fromBothInOffsets = this.buildOffsetsFor(ParallelPlot.ARRANGE_FROM_BOTH);
      }
      return this.fromBothInOffsets;
    }
    inOffsets() {
      const arrangeMethod = this.column.parallelPlot.arrangeMethod;
      if (arrangeMethod === ParallelPlot.ARRANGE_FROM_LEFT) {
        return this.getFromLeftOffsets();
      }
      if (arrangeMethod === ParallelPlot.ARRANGE_FROM_RIGHT) {
        return this.getFromRightOffsets();
      }
      if (arrangeMethod === ParallelPlot.ARRANGE_FROM_NONE) {
        return this.getFromNoneOffsets();
      }
      return this.getFromBothInOffsets();
    }
    outOffsets() {
      const arrangeMethod = this.column.parallelPlot.arrangeMethod;
      if (arrangeMethod === ParallelPlot.ARRANGE_FROM_BOTH) {
        return this.getFromRightOffsets();
      }
      return this.inOffsets();
    }
    buildFromLeftRowIndexes(bothMethod) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const columnDimIndex = parallelPlot.dimensions.indexOf(columnDim);
      const fromLeftRowIndexes = range(parallelPlot.sampleData.length);
      fromLeftRowIndexes.sort((rowIndex1, rowIndex2) => {
        let diff = 0;
        let usedDimIndex = columnDimIndex - 1;
        let usedColumnName = "";
        while (diff === 0 && usedDimIndex >= 0) {
          usedColumnName = parallelPlot.dimensions[usedDimIndex];
          diff = parallelPlot.sampleData[rowIndex2][usedColumnName] - parallelPlot.sampleData[rowIndex1][usedColumnName];
          const usedColumn = parallelPlot.columns[usedColumnName];
          if (bothMethod && diff === 0 && usedColumn.categorical) {
            const previousOutOffsets = usedColumn.categorical.outOffsets();
            diff = previousOutOffsets[rowIndex1] - previousOutOffsets[rowIndex2];
          }
          usedDimIndex = usedDimIndex - 1;
        }
        if (diff === 0) {
          usedDimIndex = columnDimIndex + 1;
          while (diff === 0 && usedDimIndex <= parallelPlot.dimensions.length - 1) {
            usedColumnName = parallelPlot.dimensions[usedDimIndex];
            diff = parallelPlot.sampleData[rowIndex2][usedColumnName] - parallelPlot.sampleData[rowIndex1][usedColumnName];
            usedDimIndex = usedDimIndex + 1;
          }
        }
        if (diff !== 0) {
          const column = parallelPlot.columns[usedColumnName];
          if (column.continuous && column.continuous.invertedAxe) {
            diff = -diff;
          }
        }
        return diff === 0 ? rowIndex1 - rowIndex2 : diff;
      });
      return fromLeftRowIndexes;
    }
    buildFromRightRowIndexes() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const columnDimIndex = parallelPlot.dimensions.indexOf(columnDim);
      const fromRightRowIndexes = range(parallelPlot.sampleData.length);
      fromRightRowIndexes.sort((rowIndex1, rowIndex2) => {
        let diff = 0;
        let usedDimIndex = columnDimIndex + 1;
        let usedColumnName = "";
        while (diff === 0 && usedDimIndex <= parallelPlot.dimensions.length - 1) {
          usedColumnName = parallelPlot.dimensions[usedDimIndex];
          diff = parallelPlot.sampleData[rowIndex2][usedColumnName] - parallelPlot.sampleData[rowIndex1][usedColumnName];
          usedDimIndex = usedDimIndex + 1;
        }
        if (diff === 0) {
          usedDimIndex = columnDimIndex - 1;
          while (diff === 0 && usedDimIndex >= 0) {
            usedColumnName = parallelPlot.dimensions[usedDimIndex];
            diff = parallelPlot.sampleData[rowIndex2][usedColumnName] - parallelPlot.sampleData[rowIndex1][usedColumnName];
            usedDimIndex = usedDimIndex - 1;
          }
        }
        if (diff !== 0) {
          const column = parallelPlot.columns[usedColumnName];
          if (column.continuous && column.continuous.invertedAxe) {
            diff = -diff;
          }
        }
        return diff === 0 ? rowIndex1 - rowIndex2 : diff;
      });
      return fromRightRowIndexes;
    }
    initBoundsByCat() {
      const parallelPlot = this.column.parallelPlot;
      const yScale = linear2().domain([0, parallelPlot.sampleData.length]).range([parallelPlot.axeHeight - _Categorical.TOTAL_SPACE_BETWEEN, 0]);
      let visibleCatCount;
      if (parallelPlot.catEquallySpacedLines) {
        this.boundsByCat = this.stackGenerator([this.rowCountByCat]).map((series) => series[0].map(yScale));
        visibleCatCount = this.rowCountByCat.filter((rowCount) => rowCount !== 0).length;
      } else {
        const sameCountByCat = [this.rowCountByCat.map(() => parallelPlot.sampleData.length / this.rowCountByCat.length)];
        this.boundsByCat = this.stackGenerator(sameCountByCat).map((series) => series[0].map(yScale));
        visibleCatCount = this.categories.length;
      }
      if (visibleCatCount > 1) {
        const spaceBetween = _Categorical.TOTAL_SPACE_BETWEEN / (visibleCatCount - 1);
        let cumulatedSpaceBetween = spaceBetween;
        for (let i = this.boundsByCat.length - 2; i >= 0; i--) {
          this.boundsByCat[i][0] += cumulatedSpaceBetween;
          this.boundsByCat[i][1] += cumulatedSpaceBetween;
          if (!parallelPlot.catEquallySpacedLines || this.rowCountByCat[i] !== 0) {
            cumulatedSpaceBetween += spaceBetween;
          }
        }
      }
    }
    yValue(catIndex) {
      this.checkInitDone();
      const bounds = this.boundsByCat[catIndex.valueOf()];
      return (bounds[0] + bounds[1]) / 2;
    }
    invertYValue(yValue) {
      this.checkInitDone();
      for (let i = 0; i < this.boundsByCat.length; i++) {
        if (this.boundsByCat[i][0] < this.boundsByCat[i][1] && this.boundsByCat[i][0] < yValue && yValue < this.boundsByCat[i][1]) {
          return i;
        }
        if (this.boundsByCat[i][1] < this.boundsByCat[i][0] && this.boundsByCat[i][1] < yValue && yValue < this.boundsByCat[i][0]) {
          return i;
        }
      }
      return -1;
    }
    yTraceValueIn(rowIndex) {
      this.checkInitDone();
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const value = parallelPlot.sampleData[rowIndex][columnDim];
      return this.yValue(value) + this.inOffsets()[rowIndex];
    }
    yTraceValueOut(rowIndex) {
      this.checkInitDone();
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const value = parallelPlot.sampleData[rowIndex][columnDim];
      return this.yValue(value) + this.outOffsets()[rowIndex];
    }
    format(value) {
      if (value >= 0 && value < this.categories.length) {
        return Number.isInteger(value.valueOf()) ? this.categories[value.valueOf()].toString() : "";
      }
      console.warn(value, " is not valid, it should be between 0 and ", this.categories.length);
      return "";
    }
    catWithoutTraces() {
      return this.categories.filter((_cat, i) => this.rowCountByCat[i] === 0);
    }
    // eslint-disable-next-line max-lines-per-function
    refreshBoxesRep() {
      this.initBoundsByCat();
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.selectAll(".category").style("display", (_cat, i) => {
        return parallelPlot.catEquallySpacedLines && this.rowCountByCat[i] === 0 ? "none" : null;
      }).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", (_cat, i) => {
        return "translate(0," + this.yValue(i) + ")";
      });
      categoricalGroup.selectAll(".category .box").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("y", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return -Math.abs(bounds[1] - bounds[0]) / 2;
      }).attr("height", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return Math.abs(bounds[1] - bounds[0]);
      });
      categoricalGroup.selectAll(".category .barMainHisto").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("y", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return -Math.abs(bounds[1] - bounds[0]) / 4;
      }).attr("height", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return Math.abs(bounds[1] - bounds[0]) / 2;
      });
      categoricalGroup.selectAll(".category .barSecondHisto").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("y", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return -Math.abs(bounds[1] - bounds[0]) / 4;
      }).attr("height", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return Math.abs(bounds[1] - bounds[0]) / 2;
      });
    }
    // eslint-disable-next-line max-lines-per-function
    drawCategories() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.append("rect").attr("class", "catOverlay").attr("x", -ParallelPlot.catClusterWidth).attr("height", parallelPlot.axeHeight).attr("width", 2 * ParallelPlot.catClusterWidth).attr("opacity", 0).attr("cursor", "crosshair").on("click", () => {
        this.toggleCategories();
        this.applyCategoricalCutoffs();
        parallelPlot.applyColumnCutoffs(columnDim, false);
      });
      categoricalGroup.selectAll(".category").data(this.categories).join(
        // eslint-disable-next-line max-lines-per-function
        (enter) => {
          const catGroup = enter.append("g").attr("class", "category tick").attr("transform", (_cat, i) => {
            return "translate(0," + this.yValue(i) + ")";
          }).style("display", (_cat, i) => {
            return parallelPlot.catEquallySpacedLines && this.rowCountByCat[i] === 0 ? "none" : null;
          });
          catGroup.append("rect").attr("class", "box").classed("active", (_cat, i) => {
            return this.isKept(i);
          }).classed("inactive", (_cat, i) => {
            return !this.isKept(i);
          }).attr("x", -ParallelPlot.catClusterWidth / 2).attr("y", (_cat, i) => {
            const bounds = this.boundsByCat[i];
            return -Math.abs(bounds[1] - bounds[0]) / 2;
          }).attr("height", (_cat, i) => {
            const bounds = this.boundsByCat[i];
            return Math.abs(bounds[1] - bounds[0]);
          }).attr("width", ParallelPlot.catClusterWidth).attr("fill-opacity", 0.3).call(
            drag_default().clickDistance(5).container(function() {
              return this.parentNode.parentNode;
            }).on("start", (_event, cat) => {
              this.dragStart(cat);
            }).on("drag", (event) => {
              this.drag(event.y);
            }).on("end", () => {
              this.dragEnd();
            })
          ).on("click", (_event, cat) => {
            const catIndex = this.categories.indexOf(cat);
            this.toggleCategory(catIndex);
            this.applyCategoricalCutoffs();
            parallelPlot.applyColumnCutoffs(columnDim, false);
          }).attr("shape-rendering", "crispEdges");
          catGroup.append("text").text((_cat, i) => {
            return this.categories[i].toString();
          }).attr("text-anchor", "end").attr("x", -ParallelPlot.catClusterWidth);
          return catGroup;
        },
        (update) => update,
        (exit) => exit.remove()
      ).select("rect").attr("fill", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return this.column.dim === parallelPlot.refColumnDim ? parallelPlot.valueColor(catIndex) : "black";
      });
    }
    dragStart(draggedCat) {
      this.dragCat = draggedCat;
      this.initialDragCategories = this.categories.slice();
    }
    drag(y2) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      let position = y2;
      const dragCat = this.dragCat;
      const indexInitialPosition = this.categories.indexOf(dragCat);
      if (indexInitialPosition > 0) {
        const bottomCat = this.categories[indexInitialPosition - 1];
        const bottomY = this.yValue(indexInitialPosition - 1);
        if (bottomY && position > bottomY) {
          if (this.canSwitchDimension(bottomCat, dragCat)) {
            this.switchDimension(bottomCat, dragCat);
          } else {
            position = bottomY;
          }
        }
      }
      if (indexInitialPosition < this.categories.length - 1) {
        const topCat = this.categories[indexInitialPosition + 1];
        const topY = this.yValue(indexInitialPosition + 1);
        if (topY && position < topY) {
          if (this.canSwitchDimension(dragCat, topCat)) {
            this.switchDimension(dragCat, topCat);
          } else {
            position = topY;
          }
        }
      }
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.selectAll(".category").filter((cat) => cat === this.dragCat).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function() {
        return "translate(0," + position + ")";
      });
    }
    canSwitchDimension(cat1, cat2) {
      const indexCat1 = this.categories.indexOf(cat1);
      const indexCat2 = this.categories.indexOf(cat2);
      if (indexCat1 === null || indexCat2 === null) {
        return false;
      }
      if (indexCat1 + 1 !== indexCat2) {
        return false;
      }
      return true;
    }
    switchDimension(bottomCat, topCat) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const bottomCatIndex = this.categories.indexOf(bottomCat);
      const topCatIndex = this.categories.indexOf(topCat);
      if (bottomCatIndex === -1 || topCatIndex === -1) {
        return;
      }
      if (bottomCatIndex + 1 !== topCatIndex) {
        return;
      }
      const beforeDragCategories = this.categories.slice();
      this.categories[bottomCatIndex] = topCat;
      this.categories[topCatIndex] = bottomCat;
      const oldCatMapping = this.categories.map((cat) => beforeDragCategories.indexOf(cat));
      this.rowCountByCat = oldCatMapping.map((oldCatIndex) => this.rowCountByCat[oldCatIndex]);
      this.initBoundsByCat();
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.selectAll(".category").filter(
        (cat) => (cat === topCat || cat === bottomCat) && cat !== this.dragCat
      ).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return "translate(0," + this.yValue(catIndex) + ")";
      });
    }
    dragEnd() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.selectAll(".category").filter((cat) => cat === this.dragCat).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return "translate(0," + this.yValue(catIndex) + ")";
      });
      if (this.initialDragCategories) {
        const newCatMapping = this.initialDragCategories.map((cat) => this.categories.indexOf(cat));
        parallelPlot.sampleData.forEach(function(row) {
          const catValue = row[columnDim];
          const newCatValue = newCatMapping[catValue];
          row[columnDim] = isNaN(newCatValue) ? NaN : newCatValue;
        });
        if (this.keptCatIndexes !== null) {
          this.keptCatIndexes = new Set(
            [...this.keptCatIndexes].map((catValue) => newCatMapping[catValue])
          );
        }
        parallelPlot.refreshTracesPaths();
      }
      this.dragCat = null;
    }
    applyCategoricalCutoffs() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoricalGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim);
      categoricalGroup.selectAll(".category rect").classed("active", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return this.isKept(catIndex);
      }).classed("inactive", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return !this.isKept(catIndex);
      });
    }
    selectedRowCountByCat(selected) {
      const columnDim = this.column.dim;
      const rowCountByCat = this.categories.map(() => 0);
      selected.forEach((row) => {
        const rowCount = rowCountByCat[row[columnDim]];
        if (typeof rowCount !== "undefined") {
          rowCountByCat[row[columnDim]] = rowCount + 1;
        }
      });
      return rowCountByCat;
    }
    // eslint-disable-next-line max-lines-per-function
    drawMainHistogram() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoryGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim).selectAll(".category");
      categoryGroup.select(".histogram").remove();
      if (!this.column.histoVisible) {
        return;
      }
      const histogramGroup = categoryGroup.append("g").attr("class", "histogram").attr("opacity", "0.5").style("display", function() {
        return parallelPlot.selectedRows.size > parallelPlot.sampleData.length / 10 ? null : "none";
      });
      const columnWidth = parallelPlot.xScaleVisibleDimension(parallelPlot.visibleDimensions[0]);
      if (typeof columnWidth === "undefined") {
        console.error("Dim '", parallelPlot.visibleDimensions[0], "' not found");
        return;
      }
      const maxBinLength = max(this.rowCountByCat);
      if (typeof maxBinLength === "undefined") {
        console.error("maxBinLength not found");
        return;
      }
      this.histoScale.range([0, columnWidth * 0.7]).domain([0, maxBinLength]);
      histogramGroup.append("rect").attr("class", "barMainHisto").attr("pointer-events", "none").attr("x", ParallelPlot.catClusterWidth / 2 + 1).attr("y", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return -Math.abs(bounds[1] - bounds[0]) / 4;
      }).attr("width", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return this.histoScale(this.rowCountByCat[catIndex]);
      }).attr("height", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return Math.abs(bounds[1] - bounds[0]) / 2;
      }).style("fill", ParallelPlot.mainHistoColor).style("stroke", "white");
    }
    // eslint-disable-next-line max-lines-per-function
    drawSelectedHistogram(selected) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const categoryGroup = selectAll_default2(parallelPlot.bindto + " .catGroup").filter((dim) => dim === columnDim).selectAll(".category");
      categoryGroup.select(".histogramSelected").remove();
      if (!this.column.histoVisible) {
        return;
      }
      const histogramGroup = categoryGroup.append("g").attr("class", "histogramSelected").attr("opacity", "0.4");
      const selectedRowCountByCat = this.selectedRowCountByCat(selected);
      let selectedHistoScale;
      if (selected.length > parallelPlot.sampleData.length / 10) {
        selectedHistoScale = this.histoScale;
      } else {
        const columnWidth = parallelPlot.xScaleVisibleDimension(
          parallelPlot.visibleDimensions[0]
        );
        if (typeof columnWidth === "undefined") {
          console.error("Dim '", parallelPlot.visibleDimensions[0], "' not found");
          return;
        }
        const maxBinLength = max(selectedRowCountByCat);
        if (typeof maxBinLength === "undefined") {
          console.error("maxBinLength not found");
          return;
        }
        selectedHistoScale = linear2().range([0, columnWidth * 0.7]).domain([0, maxBinLength]);
      }
      histogramGroup.append("rect").attr("class", "barSecondHisto").attr("pointer-events", "none").attr("x", ParallelPlot.catClusterWidth / 2 + 1).attr("y", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return -Math.abs(bounds[1] - bounds[0]) / 4;
      }).attr("height", (_cat, i) => {
        const bounds = this.boundsByCat[i];
        return Math.abs(bounds[1] - bounds[0]) / 2;
      }).attr("width", (cat) => {
        const catIndex = this.categories.indexOf(cat);
        return selectedHistoScale(selectedRowCountByCat[catIndex]);
      }).style("fill", ParallelPlot.secondHistoColor).style("stroke", function() {
        return "white";
      });
    }
    toggleCategory(catIndex) {
      if (this.column.categorical) {
        const categories = this.column.categorical.categories;
        if (this.keptCatIndexes === null) {
          this.keptCatIndexes = new Set(range(categories.length));
          this.keptCatIndexes.delete(catIndex);
        } else if (this.keptCatIndexes.has(catIndex)) {
          this.keptCatIndexes.delete(catIndex);
        } else {
          this.keptCatIndexes.add(catIndex);
          if (this.keptCatIndexes.size === categories.length) {
            this.keptCatIndexes = null;
          }
        }
      } else {
        console.error("categories is null but 'toggleCategory' is called.");
      }
    }
    toggleCategories() {
      if (this.column.categorical === null) {
        console.error("categories is null but 'toggleCategories' is called.");
      } else {
        if (this.keptCatIndexes === null) {
          this.keptCatIndexes = /* @__PURE__ */ new Set();
        } else {
          this.keptCatIndexes = null;
        }
      }
    }
    getCutoffs() {
      const categories = this.categories;
      if (this.keptCatIndexes === null) {
        return null;
      }
      return [...this.keptCatIndexes].map((catIndex) => categories[catIndex]);
    }
    setCutoffs(cutoffs) {
      if (cutoffs) {
        const columnDim = this.column.dim;
        const categories = this.categories;
        if (cutoffs.length !== 0 && typeof cutoffs[0] !== "string" && typeof cutoffs[0] !== "number") {
          console.error("Wrong categorical cutoffs are provided:", cutoffs);
        } else {
          const catCutoffs = cutoffs;
          const indexes = catCutoffs.map((catCo) => {
            const catIndex = categories.indexOf(catCo);
            if (catIndex === -1) {
              console.error(catCo + " is not a category of " + columnDim);
            }
            return catIndex;
          }).filter((index) => index !== -1);
          this.keptCatIndexes = new Set(indexes);
        }
      } else {
        this.keptCatIndexes = null;
      }
    }
    hasFilters() {
      return this.keptCatIndexes !== null;
    }
    isKept(value) {
      if (this.keptCatIndexes !== null) {
        return this.keptCatIndexes.has(value);
      }
      return true;
    }
  };

  // src/Typescript/expFormat.ts
  var ExpFormat = class _ExpFormat {
    static {
      this.NONBREAKING_SPACE = String.fromCharCode(160);
    }
    static {
      this.EXP_FORMATS = {
        "y": "-24",
        "z": "-21",
        "a": "-18",
        "f": "-15",
        "p": "-12",
        "n": "-9",
        "\xB5": "-6",
        "m": "-3",
        "k": "3",
        "M": "6",
        "G": "9",
        "T": "12",
        "P": "15",
        "E": "18",
        "Z": "21",
        "Y": "24"
      };
    }
    static {
      this.f2s = format("~s");
    }
    static {
      this.f3f = format("~r");
    }
    static sToExp(siValue) {
      const siStr = /[yzafpnµmkMGTPEZY]/.exec(siValue);
      if (siStr !== null) {
        return siValue.replace(siStr[0], _ExpFormat.NONBREAKING_SPACE + "E" + _ExpFormat.EXP_FORMATS[siStr[0]]);
      }
      return siValue;
    }
    static format(value) {
      if (value.valueOf() > 1e3 || value.valueOf() < -1e3 || value.valueOf() < 1e-3 && value.valueOf() > -1e-3) {
        return _ExpFormat.sToExp(_ExpFormat.f2s(value));
      }
      return _ExpFormat.f3f(value);
    }
  };

  // src/Typescript/multiBrush.ts
  var MultiBrush = class _MultiBrush {
    constructor(colIndex, plot, dim) {
      // Keep the actual d3-brush functions and their IDs in a list
      this.brushDefList = [];
      this.colIndex = colIndex;
      this.plot = plot;
      this.dim = dim;
      this.addNewBrushDef();
      this.applyDataJoin();
    }
    static multiBrushClass(colIndex) {
      return "multibrush_col" + colIndex;
    }
    static brushClass(colIndex, brushDef) {
      return "brush" + brushDef.id + "_col" + colIndex;
    }
    brushClass(brushDef) {
      return _MultiBrush.brushClass(this.colIndex, brushDef);
    }
    addNewBrushDef(initialCutoff) {
      const brush2 = brushY().handleSize(4).extent([
        [-5, 0],
        [20, this.plot.axeHeight]
      ]).on("brush", () => {
        this.updatePlotCutoffs(true);
      }).on("end", () => {
        this.updatePlotCutoffs(false);
        this.updateBrushDefList();
      });
      const newBrushDef = {
        id: this.brushDefList.length,
        brush: brush2,
        initialCutoff
      };
      this.brushDefList.push(newBrushDef);
      return newBrushDef;
    }
    applyDataJoin() {
      const thisMB = this;
      const brushGroup = select_default2(this.plot.bindto + " ." + _MultiBrush.multiBrushClass(this.colIndex)).selectAll(".brush").data(this.brushDefList, (brushDef) => brushDef.id.toString());
      brushGroup.enter().insert("g", ".brush").attr("class", (brushDef) => {
        return ["brush", this.brushClass(brushDef)].join(" ");
      }).each(function(brushDef) {
        select_default2(this).call(brushDef.brush);
        if (brushDef.initialCutoff) {
          if (!thisMB.plot.columns[thisMB.dim].continuous) {
            throw new Error("'initialCutoff' failed for:" + thisMB.dim);
          }
          const brushSelection2 = brushDef.initialCutoff.map(
            thisMB.plot.columns[thisMB.dim].continuous.y()
          ).sort((a, b) => a - b);
          select_default2(this).call(brushY().move, brushSelection2);
        }
      });
      brushGroup.each(function(brushDef) {
        select_default2(this).selectAll(".overlay").style("pointer-events", function() {
          return brushDef.id === thisMB.brushDefList.length - 1 && brushDef.brush !== void 0 ? "all" : "none";
        }).on("click", function() {
          thisMB.removeBrushes();
        });
      });
      brushGroup.exit().remove();
    }
    removeBrushes() {
      const brushSelections = [];
      this.plot.setContCutoff(brushSelections, this.dim, false);
      this.brushDefList = [];
      this.applyDataJoin();
      this.addNewBrushDef();
      this.applyDataJoin();
    }
    initFrom(cutoffs) {
      this.brushDefList = [];
      this.applyDataJoin();
      if (cutoffs !== null) {
        cutoffs.forEach((cutoff) => {
          this.addNewBrushDef(cutoff);
          this.applyDataJoin();
        });
      }
      this.addNewBrushDef();
      this.applyDataJoin();
    }
    updatePlotCutoffs(adjusting) {
      const brushSelections = [];
      this.brushDefList.forEach((brushDef) => {
        const brushGroup = select_default2(this.plot.bindto + " ." + this.brushClass(brushDef));
        const brushSelection2 = brushSelection(
          brushGroup.node()
        );
        if (brushSelection2 !== null) {
          brushSelections.push(brushSelection2.sort((a, b) => a - b));
        }
      });
      this.plot.setContCutoff(brushSelections, this.dim, adjusting);
    }
    updateBrushDefList() {
      const lastBrushDef = this.brushDefList[this.brushDefList.length - 1];
      const lastBrushGroup = select_default2(this.plot.bindto + " ." + this.brushClass(lastBrushDef));
      const lastBrushSelection = brushSelection(
        lastBrushGroup.node()
      );
      if (lastBrushSelection && lastBrushSelection[0] !== lastBrushSelection[1]) {
        this.addNewBrushDef();
      }
      this.applyDataJoin();
    }
  };

  // src/Typescript/continuous.ts
  var Continuous = class {
    constructor(column, invertedAxe) {
      this.continuousMin = void 0;
      this.continuousMax = void 0;
      this.contCutoffs = null;
      this.column = column;
      this.invertedAxe = invertedAxe;
      this.yScale = linear2();
      this.histoGenerator = bin();
      this.histoScale = linear2();
      this.multiBrush = null;
      this.initDone = false;
    }
    checkInitDone() {
      if (!this.initDone) {
        this.initYScaleAndHisto();
        this.initDone = true;
      }
    }
    initYScaleAndHisto() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      [this.continuousMin, this.continuousMax] = extent(parallelPlot.sampleData, function(row) {
        return +row[columnDim];
      });
      if (typeof this.continuousMin === "undefined" || typeof this.continuousMax === "undefined") {
        console.trace("d3.extent returns 'undefined values'");
        return;
      }
      this.yScale.domain(
        this.invertedAxe ? [this.continuousMax, this.continuousMin] : [this.continuousMin, this.continuousMax]
      ).range([parallelPlot.axeHeight, 0]).nice(this.numbin());
      this.histoGenerator.value(function(row) {
        return row[columnDim];
      }).domain([this.continuousMin, this.continuousMax]).thresholds(this.equiDepthThresholds(this.continuousMin, this.continuousMax));
    }
    equiDepthThresholds(min2, max3) {
      const binBounds = [];
      const depth = (max3 - min2) / this.numbin();
      for (let j = 0; j < this.numbin(); j++) {
        binBounds.push(min2 + j * depth);
      }
      return binBounds;
    }
    numbin() {
      const parallelPlot = this.column.parallelPlot;
      return Math.ceil(2.5 * Math.pow(parallelPlot.sampleData.length, 0.25));
    }
    y() {
      this.checkInitDone();
      return this.yScale;
    }
    yTraceValue(rowIndex) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const value = parallelPlot.sampleData[rowIndex][columnDim];
      return this.y()(value);
    }
    histo() {
      this.checkInitDone();
      return this.histoGenerator;
    }
    setInvertedAxe(invertedAxe) {
      if (this.invertedAxe !== invertedAxe) {
        this.invertedAxe = invertedAxe;
        if (this.initDone && typeof this.continuousMin !== "undefined" && typeof this.continuousMax !== "undefined") {
          this.yScale.domain(this.invertedAxe ? [this.continuousMax, this.continuousMin] : [this.continuousMin, this.continuousMax]).nice(this.numbin());
        }
        return true;
      }
      return false;
    }
    // eslint-disable-next-line max-lines-per-function
    drawMainHistogram() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const dimensionGroup = selectAll_default2(parallelPlot.bindto + " .plotGroup .dimension").filter((dim) => dim === columnDim);
      dimensionGroup.select(".histogram").remove();
      if (!this.column.histoVisible) {
        return;
      }
      const histogramGroup = dimensionGroup.append("g").attr("class", "histogram").attr("opacity", "0.5").style("display", function() {
        return parallelPlot.selectedRows.size > parallelPlot.sampleData.length / 10 ? null : "none";
      });
      const columnWidth = parallelPlot.xScaleVisibleDimension(parallelPlot.visibleDimensions[0]);
      if (typeof columnWidth === "undefined") {
        console.error("Dim '", parallelPlot.visibleDimensions[0], "' not found");
        return;
      }
      const bins = this.histo()(parallelPlot.sampleData);
      const maxBinLength = max(bins.map((b) => b.length));
      if (typeof maxBinLength === "undefined") {
        console.error("maxBinLength not found");
        return;
      }
      this.histoScale.range([0, columnWidth * 0.7]).domain([0, maxBinLength]);
      histogramGroup.selectAll("rect").data(bins).enter().append("rect").attr("class", "barMainHisto").attr("pointer-events", "none").attr("x", 2).attr("transform", (bin2) => {
        if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
          console.error("bin.x1 is undefined");
          return null;
        }
        return "translate(0," + Math.min(
          this.y()(bin2.x1),
          this.y()(bin2.x0)
        ) + ")";
      }).attr("width", (bin2) => this.histoScale(bin2.length)).attr("height", (bin2) => {
        if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
          console.error("bin.x0 or bin.x1 are undefined");
          return null;
        }
        return Math.abs(this.y()(bin2.x0) - this.y()(bin2.x1));
      }).style("fill", ParallelPlot.mainHistoColor).style("stroke", "white");
    }
    // eslint-disable-next-line max-lines-per-function
    drawSelectedHistogram(selected) {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const dimensionGroup = selectAll_default2(parallelPlot.bindto + " .plotGroup .dimension").filter((dim) => dim === columnDim);
      dimensionGroup.select(".histogramSelected").remove();
      if (!this.column.histoVisible) {
        return;
      }
      const histogramGroup = dimensionGroup.append("g").attr("class", "histogramSelected").attr("opacity", "0.4");
      const bins = this.histo()(selected);
      let selectedHistoScale;
      if (selected.length > parallelPlot.sampleData.length / 10) {
        selectedHistoScale = this.histoScale;
      } else {
        const columnWidth = parallelPlot.xScaleVisibleDimension(
          parallelPlot.visibleDimensions[0]
        );
        if (typeof columnWidth === "undefined") {
          console.error("Dim '", parallelPlot.visibleDimensions[0], "' not found");
          return;
        }
        const maxBinLength = max(bins.map((b) => b.length));
        if (typeof maxBinLength === "undefined") {
          console.error("maxBinLength not found");
          return;
        }
        selectedHistoScale = linear2().range([0, columnWidth * 0.7]).domain([0, maxBinLength]);
      }
      histogramGroup.selectAll("rect").data(bins).enter().append("rect").attr("class", "barSecondHisto").attr("pointer-events", "none").attr("x", 2).attr("transform", (bin2) => {
        if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
          console.error("bin.x1 is undefined");
          return null;
        }
        return "translate(0," + Math.min(
          this.y()(bin2.x1),
          this.y()(bin2.x0)
        ) + ")";
      }).attr("height", (bin2) => {
        if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
          console.error("bin.x0 or bin.x1 are undefined");
          return null;
        }
        return Math.abs(this.y()(bin2.x0) - this.y()(bin2.x1));
      }).attr("width", function(bin2) {
        return selectedHistoScale.domain()[1] === 0 ? 0 : selectedHistoScale(bin2.length);
      }).style("fill", ParallelPlot.secondHistoColor).style("stroke", function() {
        return "white";
      });
    }
    getCutoffs() {
      return this.contCutoffs;
    }
    setCutoffs(cutoffs) {
      if (cutoffs) {
        if (typeof cutoffs[0] === "string" || typeof cutoffs[0] === "number") {
          console.error("categories is null but categorical cutoffs are provided:", cutoffs);
        } else {
          this.contCutoffs = cutoffs.map((co) => {
            return co.sort(function(a, b) {
              return b - a;
            });
          });
        }
      } else {
        this.contCutoffs = null;
      }
    }
    hasFilters() {
      return this.contCutoffs !== null;
    }
    isKept(value) {
      if (this.contCutoffs !== null) {
        let active = false;
        this.contCutoffs.forEach(function(contCutoff) {
          active = active || contCutoff[1] <= value && value <= contCutoff[0];
        });
        return active;
      }
      return true;
    }
    initMultiBrush() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      if (this.multiBrush === null || select_default2(parallelPlot.bindto + " ." + MultiBrush.multiBrushClass(this.column.colIndex)).selectAll(".brush").size() === 0) {
        this.multiBrush = new MultiBrush(
          this.column.colIndex,
          parallelPlot,
          columnDim
        );
      }
      this.multiBrush.initFrom(this.contCutoffs);
    }
    drawAxe() {
      const axis2 = axisLeft(this.y()).tickFormat(ExpFormat.format);
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const axisGroup = selectAll_default2(parallelPlot.bindto + " .axisGroup").filter((dim) => dim === columnDim);
      axisGroup.call(axis2.scale(this.y()));
    }
    drawColorScale() {
      const columnDim = this.column.dim;
      const parallelPlot = this.column.parallelPlot;
      const dimensionGroup = selectAll_default2(parallelPlot.bindto + " .plotGroup .dimension").filter((dim) => dim === columnDim);
      dimensionGroup.selectAll(".colorScaleBar").data(range(parallelPlot.axeHeight)).enter().append("rect").attr("pointer-events", "none").attr("class", "colorScaleBar").attr("x", -4).attr("y", function(_d, i) {
        return i;
      }).attr("height", 1).attr("width", 4).attr("opacity", 0.9).style("fill", (pixel) => {
        return parallelPlot.valueColor(
          this.y().invert(pixel)
        );
      });
    }
  };

  // src/Typescript/column.ts
  var Column = class _Column {
    static {
      this.INPUT = "Input";
    }
    static {
      this.OUTPUT = "Output";
    }
    constructor(dim, colIndex, parallelPlot, label, categories, cutoffs, histoVisibility, invertedAxe, ioType) {
      this.colIndex = colIndex;
      this.dim = dim;
      this.label = label;
      this.parallelPlot = parallelPlot;
      if (categories) {
        this.continuous = null;
        this.categorical = new Categorical(this, categories);
      } else {
        this.categorical = null;
        this.continuous = new Continuous(this, invertedAxe);
      }
      this.histoVisible = histoVisibility;
      this.setCutoffs(cutoffs);
      this.ioType = ioType;
    }
    unvalid() {
      if (this.continuous) {
        this.continuous.initDone = false;
      }
      if (this.categorical) {
        this.categorical.initDone = false;
      }
    }
    setInvertedAxe(invertedAxe) {
      if (this.continuous) {
        return this.continuous.setInvertedAxe(invertedAxe);
      }
      return false;
    }
    yTraceValue(rowIndex) {
      if (this.continuous) {
        return this.continuous.yTraceValue(rowIndex);
      }
      if (this.categorical) {
        return this.categorical.yTraceValueIn(rowIndex);
      }
      throw new Error("yTraceValue() failed");
    }
    formatedRowValue(row) {
      return this.formatedValue(row[this.dim]);
    }
    formatedValue(value) {
      if (this.continuous) {
        return ExpFormat.format(value);
      }
      if (this.categorical) {
        return this.categorical.format(value);
      }
      throw new Error("formatedValue() failed");
    }
    labelText() {
      return this.label.replace(/<br>/gi, " ");
    }
    isInput() {
      return this.ioType === _Column.INPUT;
    }
    isOutput() {
      return this.ioType === _Column.OUTPUT;
    }
    drawMainHistogram() {
      if (this.continuous) {
        this.continuous.drawMainHistogram();
      }
      if (this.categorical) {
        this.categorical.drawMainHistogram();
      }
    }
    drawSelectedHistogram(selected) {
      if (this.continuous) {
        this.continuous.drawSelectedHistogram(selected);
      }
      if (this.categorical) {
        this.categorical.drawSelectedHistogram(selected);
      }
    }
    getCutoffs() {
      if (this.continuous) {
        return this.continuous.getCutoffs();
      }
      if (this.categorical) {
        return this.categorical.getCutoffs();
      }
      throw new Error("getCutoffs() failed");
    }
    setCutoffs(cutoffs) {
      if (this.categorical) {
        this.categorical.setCutoffs(cutoffs);
      }
      if (this.continuous) {
        this.continuous.setCutoffs(cutoffs);
      }
    }
    hasFilters() {
      if (this.continuous) {
        return this.continuous.hasFilters();
      }
      if (this.categorical) {
        return this.categorical.hasFilters();
      }
      throw new Error("hasFilters() failed");
    }
    isKept(value) {
      if (this.continuous) {
        return this.continuous.isKept(value);
      }
      if (this.categorical) {
        return this.categorical.isKept(value);
      }
      throw new Error("isKept() failed");
    }
  };

  // src/Typescript/columnHeaders.ts
  var ColumnHeaders = class {
    // eslint-disable-next-line max-lines-per-function
    constructor(parallelPlot) {
      this.dragDimension = null;
      this.clickCount = 0;
      this.dimensionGroup = select_default2(parallelPlot.bindto + " .plotGroup").selectAll(".dimension");
      this.parallelPlot = parallelPlot;
      const thisTextColumns = this;
      this.dimensionGroup.append("text").attr("class", "axisLabel").on("mouseover", function() {
        select_default2(this).attr("font-weight", "bold");
      }).on("mouseout", function() {
        select_default2(this).attr("font-weight", "normal");
      }).call(
        drag_default().clickDistance(5).container(function() {
          return this.parentNode.parentNode;
        }).on("start", (_event, dim) => {
          this.dragDimension = dim;
        }).on("drag", (event, dim) => {
          this.drag(event.x, dim);
        }).on("end", () => {
          this.dragEnd();
        })
      ).on("click", function(event, dim) {
        if (event.defaultPrevented) {
          return;
        }
        if (thisTextColumns.clickCount === 0) {
          thisTextColumns.clickCount = 1;
          setTimeout(function() {
            if (thisTextColumns.clickCount === 1) {
              parallelPlot.changeColorMapOnDimension(dim);
            }
            if (thisTextColumns.clickCount === 2) {
              const continuous2 = parallelPlot.columns[dim].continuous;
              if (continuous2) {
                if (continuous2.setInvertedAxe(!continuous2.invertedAxe)) {
                  thisTextColumns.reverseDomainOnAxis(dim);
                  const invertedAxes = {};
                  invertedAxes[dim] = continuous2.invertedAxe;
                  thisTextColumns.parallelPlot.sendInvertedAxeEvent(invertedAxes);
                  parallelPlot.refreshTracesPaths();
                }
              }
            }
            thisTextColumns.clickCount = 0;
          }, 350);
        } else if (thisTextColumns.clickCount === 1) {
          thisTextColumns.clickCount = 2;
        }
      });
      this.dimensionGroup = select_default2(parallelPlot.bindto + " .plotGroup").selectAll(".dimension");
      parallelPlot.updateColumnLabels();
    }
    // eslint-disable-next-line max-lines-per-function
    drag(x2, draggedDim) {
      let position = x2;
      const parallelPlot = this.parallelPlot;
      const dimensionGroup = this.dimensionGroup;
      const dragDimension = this.dragDimension;
      const indexInitialPosition = parallelPlot.visibleDimensions.indexOf(dragDimension);
      if (indexInitialPosition > 0) {
        const leftDimension = parallelPlot.visibleDimensions[indexInitialPosition - 1];
        const leftX = parallelPlot.xScaleVisibleDimension(leftDimension);
        if (leftX && position < leftX) {
          if (this.canSwitchDimension(leftDimension, dragDimension)) {
            this.switchdimension(leftDimension, dragDimension);
          } else {
            position = leftX;
          }
        }
      }
      if (indexInitialPosition < parallelPlot.visibleDimensions.length - 1) {
        const rightDimension = parallelPlot.visibleDimensions[indexInitialPosition + 1];
        const rightX = parallelPlot.xScaleVisibleDimension(rightDimension);
        if (rightX && position > rightX) {
          if (this.canSwitchDimension(dragDimension, rightDimension)) {
            this.switchdimension(dragDimension, rightDimension);
          } else {
            position = rightX;
          }
        }
      }
      dimensionGroup.filter((dim) => dim === draggedDim).attr("transform", function(dim) {
        return `translate(${position}, ${parallelPlot.yRefRowOffset(dim)})`;
      });
    }
    dragEnd() {
      const parallelPlot = this.parallelPlot;
      const dimensionGroup = this.dimensionGroup;
      dimensionGroup.filter((dim) => dim === this.dragDimension).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function(d) {
        const x2 = parallelPlot.xScaleVisibleDimension(d);
        const yRefRowOffset = parallelPlot.yRefRowOffset(d);
        return `translate(${x2}, ${yRefRowOffset})`;
      });
      parallelPlot.refreshTracesPaths();
      this.dragDimension = null;
    }
    canSwitchDimension(dim1, dim2) {
      const parallelPlot = this.parallelPlot;
      const indexDim1 = parallelPlot.dimensions.indexOf(dim1);
      const indexDim2 = parallelPlot.dimensions.indexOf(dim2);
      if (indexDim1 === null || indexDim2 === null) {
        return false;
      }
      if (indexDim1 + 1 !== indexDim2) {
        return false;
      }
      return true;
    }
    // eslint-disable-next-line max-lines-per-function
    switchdimension(leftDim, rightDim) {
      const parallelPlot = this.parallelPlot;
      const dimensionGroup = this.dimensionGroup;
      const leftVisibleIndex = parallelPlot.visibleDimensions.indexOf(leftDim);
      const rightVisibleIndex = parallelPlot.visibleDimensions.indexOf(rightDim);
      const leftSliderIndex = parallelPlot.dimensions.indexOf(leftDim);
      const rightSliderIndex = parallelPlot.dimensions.indexOf(rightDim);
      if (leftVisibleIndex === -1 || rightVisibleIndex === -1 || leftSliderIndex === -1 || rightSliderIndex === -1) {
        return;
      }
      if (leftVisibleIndex + 1 !== rightVisibleIndex) {
        return;
      }
      if (leftSliderIndex + 1 !== rightSliderIndex) {
        return;
      }
      parallelPlot.dimensions[leftSliderIndex] = rightDim;
      parallelPlot.dimensions[rightSliderIndex] = leftDim;
      parallelPlot.visibleDimensions[leftVisibleIndex] = rightDim;
      parallelPlot.visibleDimensions[rightVisibleIndex] = leftDim;
      parallelPlot.updateXScale();
      dimensionGroup.filter(
        (dim) => (dim === rightDim || dim === leftDim) && dim !== this.dragDimension
      ).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function(d) {
        const x2 = parallelPlot.xScaleVisibleDimension(d);
        const yRefRowOffset = parallelPlot.yRefRowOffset(d);
        return `translate(${x2}, ${yRefRowOffset})`;
      });
    }
    // eslint-disable-next-line max-lines-per-function
    reverseDomainOnAxis(revDim) {
      const dimensionGroup = this.dimensionGroup;
      const parallelPlot = this.parallelPlot;
      const reversedColumn = parallelPlot.columns[revDim];
      if (reversedColumn.continuous === null) {
        return;
      }
      parallelPlot.updateColumnLabels();
      const [old1, old2] = reversedColumn.continuous.y().domain();
      const oldScale = linear2().range(reversedColumn.continuous.y().range()).domain([old2, old1]);
      const axis2 = axisLeft(reversedColumn.continuous.y()).tickFormat(ExpFormat.format);
      dimensionGroup.filter((dim) => dim === revDim).each(function() {
        select_default2(this).selectAll(".axisGroup").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).call(axis2);
      });
      dimensionGroup.filter((dim) => dim === revDim).each(function(dim) {
        select_default2(this).transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function() {
          const x2 = parallelPlot.xScaleVisibleDimension(revDim);
          const yRefRowOffset = parallelPlot.yRefRowOffset(dim);
          return `translate(${x2}, ${yRefRowOffset})`;
        });
        select_default2(this).selectAll(".barMainHisto").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x1 is undefined");
            return null;
          }
          return "translate(0," + Math.min(
            reversedColumn.continuous.y()(bin2.x1),
            reversedColumn.continuous.y()(bin2.x0)
          ) + ")";
        }).attr("height", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x0 or bin.x1 are undefined");
            return null;
          }
          return Math.abs(
            reversedColumn.continuous.y()(bin2.x0) - reversedColumn.continuous.y()(bin2.x1)
          );
        });
        select_default2(this).selectAll(".barSecondHisto").transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).attr("transform", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x1 is undefined");
            return null;
          }
          return "translate(0," + Math.min(
            reversedColumn.continuous.y()(bin2.x1),
            reversedColumn.continuous.y()(bin2.x0)
          ) + ")";
        }).attr("height", function(bin2) {
          if (typeof bin2.x0 === "undefined" || typeof bin2.x1 === "undefined") {
            console.error("bin.x0 or bin.x1 are undefined");
            return null;
          }
          return Math.abs(
            reversedColumn.continuous.y()(bin2.x0) - reversedColumn.continuous.y()(bin2.x1)
          );
        });
        const allDimensions = Object.keys(parallelPlot.sampleData[0]);
        const colIndex = allDimensions.indexOf(dim);
        select_default2(this).selectAll("." + MultiBrush.multiBrushClass(colIndex)).selectAll(".brush").each(function() {
          const brushGroup = select_default2(this);
          const brushSelection2 = brushSelection(
            brushGroup.node()
          );
          if (brushSelection2) {
            const reversedBrushSelection = [
              reversedColumn.continuous.y()(oldScale.invert(brushSelection2[0])),
              reversedColumn.continuous.y()(oldScale.invert(brushSelection2[1]))
            ].sort((a, b) => a - b);
            brushGroup.transition().ease(backOut).duration(ParallelPlot.D3_TRANSITION_DURATION).call(brushY().move, reversedBrushSelection);
          }
        });
        select_default2(this).selectAll(".colorScaleBar").transition().duration(ParallelPlot.D3_TRANSITION_DURATION).style("fill", function(pixel) {
          return parallelPlot.valueColor(reversedColumn.continuous.y().invert(pixel));
        });
      });
    }
  };

  // src/Typescript/style.ts
  var Style = class {
    constructor(bindto) {
      this.bindto = bindto;
    }
    applyCssRules() {
      if (this.cssRules) {
        for (const [selector, declarations] of Object.entries(this.cssRules)) {
          const selection2 = select_default2(this.bindto).selectAll(selector);
          const applyDeclaration = (declaration) => {
            const splitDeclaration = declaration.split(":");
            if (splitDeclaration.length === 2) {
              selection2.style(splitDeclaration[0], splitDeclaration[1]);
            } else {
              console.error("Invalid CSS declaration:", declaration);
            }
          };
          if (Array.isArray(declarations)) {
            declarations.forEach(applyDeclaration);
          }
          if (typeof declarations === "string") {
            applyDeclaration(declarations);
          }
        }
      }
    }
  };

  // src/Typescript/parallelPlot.ts
  var ParallelPlot = class _ParallelPlot {
    constructor(id2, width, height) {
      this.width = 0;
      this.height = 0;
      this.changeColorDuration = 1e3;
      this.axeHeight = 0;
      this.rowLabels = null;
      this.sampleData = [];
      this.dimensions = [];
      this.defaultVisibleDimCount = 0;
      this.visibleDimCount = 0;
      this.startingDimIndex = 0;
      this.visibleDimensions = [];
      /** Set Containing the index of uncut rows. */
      this.selectedRows = /* @__PURE__ */ new Set();
      this.continuousCsId = _ParallelPlot.CONTINUOUS_CS_IDS[0];
      this.categoricalCsId = _ParallelPlot.CATEGORIAL_CS_IDS[0];
      this.refColumnDim = null;
      this.colorScale = null;
      this.columns = {};
      // Column for each dimension
      this.editedRowIndex = null;
      this.editedPointDim = null;
      this.editionMode = _ParallelPlot.EDITION_ON_DRAG_END;
      /**
       * Position of each dimension in X in the drawing domain
       */
      this.xScaleVisibleDimension = point();
      this.refRowIndex = null;
      this.hlPointIndex = null;
      this.dispatch = dispatch_default(_ParallelPlot.PLOT_EVENT);
      this.rotateTitle = false;
      this.catSpreaderMap = /* @__PURE__ */ new Map();
      this.columnHeaders = null;
      this.catEquallySpacedLines = true;
      this.arrangeMethod = _ParallelPlot.ARRANGE_METHODS[1];
      this.useControlWidgets = false;
      this.bindto = "#" + id2;
      this.style = new Style(this.bindto);
      this.width = width ? width : 1200;
      this.height = height ? height : 600;
    }
    static {
      this.mainHistoColor = "#a6f2f2";
    }
    static {
      this.secondHistoColor = "#169c9c";
    }
    static {
      this.CONTINUOUS_CS = {
        // From d3-scale.
        Viridis: viridis_default,
        Inferno: inferno,
        Magma: magma,
        Plasma: plasma,
        Warm: warm,
        Cool: cool,
        Rainbow: rainbow_default,
        CubehelixDefault: cubehelix_default2,
        // From d3-scale-chromatic
        Blues: Blues_default,
        Greens: Greens_default,
        Greys: Greys_default,
        Oranges: Oranges_default,
        Purples: Purples_default,
        Reds: Reds_default,
        BuGn: BuGn_default,
        BuPu: BuPu_default,
        GnBu: GnBu_default,
        OrRd: OrRd_default,
        PuBuGn: PuBuGn_default,
        PuBu: PuBu_default,
        PuRd: PuRd_default,
        RdBu: RdBu_default,
        RdPu: RdPu_default,
        YlGnBu: YlGnBu_default,
        YlGn: YlGn_default,
        YlOrBr: YlOrBr_default,
        YlOrRd: YlOrRd_default
      };
    }
    static {
      this.CONTINUOUS_CS_IDS = Object.keys(_ParallelPlot.CONTINUOUS_CS);
    }
    static {
      this.CATEGORIAL_CS = {
        Category10: ordinal(category10_default),
        Accent: ordinal(Accent_default),
        Dark2: ordinal(Dark2_default),
        Paired: ordinal(Paired_default),
        Set1: ordinal(Set1_default)
      };
    }
    static {
      this.CATEGORIAL_CS_IDS = Object.keys(_ParallelPlot.CATEGORIAL_CS);
    }
    static {
      this.CATEGORIES_REP_LIST = ["EquallySpacedLines", "EquallySizedBoxes"];
    }
    static {
      this.margin = { top: 100, right: 10, bottom: 10, left: 10 };
    }
    static {
      this.catClusterWidth = 12;
    }
    static {
      this.line = line_default();
    }
    static {
      this.PLOT_EVENT = "plotEvent";
    }
    static {
      this.CUTOFF_EVENT = "cutoffChange";
    }
    static {
      this.INVERTED_AXE_EVENT = "axeOrientationChange";
    }
    static {
      this.REF_COLUMN_DIM_EVENT = "refColumnDimChange";
    }
    static {
      this.HL_ROW_EVENT = "hlRowEvent";
    }
    static {
      this.ROW_CLICK_EVENT = "rowClicked";
    }
    static {
      this.EDITION_EVENT = "pointChange";
    }
    static {
      this.CO_ATTR_TYPE = "Cutoffs";
    }
    static {
      this.ST_ATTR_TYPE = "SelectedTraces";
    }
    static {
      this.RC_ATTR_TYPE = "ReferenceColumn";
    }
    static {
      this.EDITION_OFF = "EditionOff";
    }
    static {
      this.EDITION_ON_DRAG = "EditionOnDrag";
    }
    static {
      this.EDITION_ON_DRAG_END = "EditionOnDragEnd";
    }
    static {
      this.EDITION_MODE_IDS = [_ParallelPlot.EDITION_OFF, _ParallelPlot.EDITION_ON_DRAG, _ParallelPlot.EDITION_ON_DRAG_END];
    }
    static {
      this.MAX_VISIBLE_DIMS = 30;
    }
    static {
      this.DEFAULT_SLIDER_POSITION = {
        dimCount: 15,
        startingDimIndex: 0
      };
    }
    static {
      this.ARRANGE_FROM_LEFT = "fromLeft";
    }
    static {
      this.ARRANGE_FROM_RIGHT = "fromRight";
    }
    static {
      this.ARRANGE_FROM_BOTH = "fromBoth";
    }
    static {
      this.ARRANGE_FROM_NONE = "fromNone";
    }
    static {
      this.ARRANGE_METHODS = [_ParallelPlot.ARRANGE_FROM_LEFT, _ParallelPlot.ARRANGE_FROM_RIGHT, _ParallelPlot.ARRANGE_FROM_BOTH, _ParallelPlot.ARRANGE_FROM_NONE];
    }
    static {
      this.D3_TRANSITION_DURATION = 500;
    }
    id() {
      return this.bindto.substring(1);
    }
    resize(width, height) {
      this.width = width ? width : 1200;
      this.height = height ? height : 600;
      select_default2(this.bindto + " svg").attr("width", this.width).attr("height", this.height);
      this.updateXScale();
      select_default2(this.bindto + " .slider .axisGroup").remove();
      select_default2(this.bindto + " .slider .brushDim").remove();
      new BrushSlider(this);
      this.buildPlotArea();
      this.style.applyCssRules();
    }
    getHeight() {
      if (this.useControlWidgets) {
        const controlDivNode = select_default2(this.bindto + " .controlDiv").node();
        if (controlDivNode !== null) {
          const controlDivHeight = controlDivNode.getBoundingClientRect().height;
          if (this.height > controlDivHeight) {
            return this.height - controlDivHeight;
          }
        }
      }
      return this.height;
    }
    initSliderPosition(config) {
      if (config.sliderPosition) {
        if (typeof config.sliderPosition.dimCount !== "number" || config.sliderPosition.dimCount > _ParallelPlot.MAX_VISIBLE_DIMS) {
          this.defaultVisibleDimCount = _ParallelPlot.DEFAULT_SLIDER_POSITION.dimCount;
        } else {
          this.defaultVisibleDimCount = config.sliderPosition.dimCount;
        }
        if (typeof config.sliderPosition.startingDimIndex === "number") {
          this.startingDimIndex = config.sliderPosition.startingDimIndex;
        } else {
          this.startingDimIndex = _ParallelPlot.DEFAULT_SLIDER_POSITION.startingDimIndex;
        }
      } else {
        this.defaultVisibleDimCount = _ParallelPlot.DEFAULT_SLIDER_POSITION.dimCount;
        this.startingDimIndex = _ParallelPlot.DEFAULT_SLIDER_POSITION.startingDimIndex;
      }
      if (this.dimensions.length < this.defaultVisibleDimCount) {
        this.visibleDimCount = this.dimensions.length;
      } else {
        this.visibleDimCount = this.defaultVisibleDimCount;
      }
      if (this.startingDimIndex > this.dimensions.length - this.visibleDimCount) {
        this.startingDimIndex = this.dimensions.length - this.visibleDimCount;
      }
    }
    // eslint-disable-next-line max-lines-per-function
    generate(config) {
      if (select_default2(this.bindto).empty()) {
        throw new Error("'bindto' dom element not found:" + this.bindto);
      }
      this.style.cssRules = config.cssRules;
      select_default2(this.bindto).classed("parallelPlot", true);
      this.checkConfig(config);
      this.rowLabels = config.rowLabels;
      this.catEquallySpacedLines = config.categoriesRep === _ParallelPlot.CATEGORIES_REP_LIST[0];
      this.arrangeMethod = config.arrangeMethod ? config.arrangeMethod : _ParallelPlot.ARRANGE_FROM_RIGHT;
      this.initSampleData(config);
      select_default2(this.bindto).style("position", "relative");
      select_default2(this.bindto).selectAll("div").remove();
      select_default2(this.bindto).append("div").attr("class", "ppDiv").classed("withWidgets", this.useControlWidgets).classed("withoutWidgets", !this.useControlWidgets);
      this.appendControlDiv();
      this.axeHeight = this.getHeight() - _ParallelPlot.margin.top - _ParallelPlot.margin.bottom;
      if (this.refRowIndex !== null) {
        this.axeHeight = this.axeHeight / 2;
      }
      this.rotateTitle = config.rotateTitle ? config.rotateTitle : false;
      const allDimensions = Object.keys(this.sampleData[0]);
      allDimensions.forEach((dim, i) => {
        const isInput = Array.isArray(config.inputColumns) ? config.inputColumns[i] : true;
        const label = Array.isArray(config.columnLabels) ? config.columnLabels[i] : dim;
        const categories = Array.isArray(config.categorical) ? config.categorical[i] : null;
        const cutoffs = Array.isArray(config.cutoffs) ? config.cutoffs[i] : null;
        const histoVisibility = Array.isArray(config.histoVisibility) ? config.histoVisibility[i] : false;
        const invertedAxis = Array.isArray(config.invertedAxes) ? config.invertedAxes[i] : false;
        this.columns[dim] = new Column(
          dim,
          i,
          this,
          label,
          categories,
          cutoffs,
          histoVisibility,
          invertedAxis,
          isInput ? Column.INPUT : Column.OUTPUT
        );
      });
      const nanColumns = allDimensions.map((dim) => this.sampleData.every((row) => isNaN(row[dim])));
      this.dimensions = allDimensions.filter(
        (_dim, i) => !(nanColumns[i] || Array.isArray(config.keptColumns) && !config.keptColumns[i])
      );
      if (!config.refColumnDim) {
        this.refColumnDim = null;
      } else if (this.dimensions.includes(config.refColumnDim)) {
        this.refColumnDim = config.refColumnDim;
      } else {
        console.error("Unknown 'refColumnDim': " + config.refColumnDim);
      }
      this.updateSelectedRows();
      this.initSliderPosition(config);
      if (!config.continuousCS) {
        this.continuousCsId = _ParallelPlot.CONTINUOUS_CS_IDS[0];
      } else if (_ParallelPlot.CONTINUOUS_CS_IDS.includes(config.continuousCS)) {
        this.continuousCsId = config.continuousCS;
      } else {
        console.error("Unknown continuous color scale: " + config.continuousCS);
      }
      if (!config.categoricalCS) {
        this.categoricalCsId = _ParallelPlot.CATEGORIAL_CS_IDS[0];
      } else if (_ParallelPlot.CATEGORIAL_CS_IDS.includes(config.categoricalCS)) {
        this.categoricalCsId = config.categoricalCS;
      } else {
        console.error("Unknown categorical color scale: " + config.categoricalCS);
      }
      if (!config.editionMode) {
        this.editionMode = _ParallelPlot.EDITION_OFF;
      } else if (_ParallelPlot.EDITION_MODE_IDS.includes(config.editionMode)) {
        this.editionMode = config.editionMode;
      } else {
        console.error("Unknown edition mode: " + config.editionMode);
      }
      this.appendPlotSvg();
      this.applyColorMap();
      this.initTraceTooltip();
      this.appendContCsSelect();
      this.appendCatCsSelect();
      this.appendZAxisSelector();
      this.initZAxisUsedCB();
      this.appendCatRepSelect();
      this.appendArrangeMethodSelect();
      this.style.applyCssRules();
    }
    appendControlDiv() {
      const ppDiv = select_default2(this.bindto + " .ppDiv");
      const controlDiv = ppDiv.append("div").attr("class", "controlDiv");
      const csDiv = controlDiv.append("div").attr("class", "csDiv");
      csDiv.append("div").attr("class", "zAxisUsedDiv").html(`<input type="checkbox" id="${this.id()}_zAxisUsed" name="zAxisUsed" checked> <label for="${this.id()}_zAxisUsed">Use Z Axis</label> <span class="ParamSelect ZAxis"></span>`);
      csDiv.append("div").html('Continuous Color Scale: <span class="contCsSelect"></span>');
      csDiv.append("div").html('Categorical Color Scale: <span class="catCsSelect"></span>');
      const catDiv = controlDiv.append("div").attr("class", "catDiv");
      catDiv.append("div").html('Categories Representation: <span class="catRepSelect"></span>');
      catDiv.append("div").html('Arrange Method in Category Boxes: <span class="arrangeMethodSelect"></span>');
    }
    appendContCsSelect() {
      const thisPPlot = this;
      select_default2(this.bindto + " .contCsSelect").append("select").on("change", function() {
        const contCsKey = _ParallelPlot.CONTINUOUS_CS_IDS[this.selectedIndex];
        thisPPlot.setContinuousColorScale(contCsKey);
      }).selectAll("option").data(_ParallelPlot.CONTINUOUS_CS_IDS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const contCsIndex = _ParallelPlot.CONTINUOUS_CS_IDS.indexOf(this.continuousCsId);
      select_default2(this.bindto + " .contCsSelect > select").property("selectedIndex", contCsIndex);
    }
    appendCatCsSelect() {
      const thisPPlot = this;
      select_default2(this.bindto + " .catCsSelect").append("select").on("change", function() {
        const catCsKey = _ParallelPlot.CATEGORIAL_CS_IDS[this.selectedIndex];
        thisPPlot.setCategoricalColorScale(catCsKey);
      }).selectAll("option").data(_ParallelPlot.CATEGORIAL_CS_IDS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const catCsIndex = _ParallelPlot.CATEGORIAL_CS_IDS.indexOf(this.categoricalCsId);
      select_default2(this.bindto + " .catCsSelect > select").property("selectedIndex", catCsIndex);
    }
    appendZAxisSelector() {
      const thisPPlot = this;
      select_default2(this.bindto + " .ParamSelect.ZAxis").append("select").on("change", function() {
        thisPPlot.changeColorMapOnDimension(thisPPlot.dimensions[this.selectedIndex]);
      }).selectAll("option").data(this.dimensions).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      if (this.refColumnDim !== null) {
        const paramIndex = this.dimensions.indexOf(this.refColumnDim);
        select_default2(this.bindto + " .ParamSelect.ZAxis > select").property("selectedIndex", paramIndex);
      }
    }
    initZAxisUsedCB() {
      select_default2(`#${this.id()}_zAxisUsed`).property("checked", this.refColumnDim !== null).on("change", _ParallelPlot.prototype.updateZAxisFromGui.bind(this));
    }
    updateZAxisGui() {
      if (this.refColumnDim !== null) {
        const paramIndex = this.dimensions.indexOf(this.refColumnDim);
        select_default2(this.bindto + " .ParamSelect.ZAxis > select").property("selectedIndex", paramIndex);
      }
      select_default2(`#${this.id()}_zAxisUsed`).property("checked", this.refColumnDim !== null);
    }
    updateZAxisFromGui() {
      if (select_default2(`#${this.id()}_zAxisUsed`).property("checked")) {
        const zAxisSelectNode = select_default2(this.bindto + " .ParamSelect.ZAxis>select").node();
        if (zAxisSelectNode && this.refColumnDim !== this.dimensions[zAxisSelectNode.selectedIndex]) {
          this.refColumnDim = this.dimensions[zAxisSelectNode.selectedIndex];
          this.sendRefColumnDimEvent();
          this.applyColorMap();
        }
      } else if (this.refColumnDim !== null) {
        this.refColumnDim = null;
        this.sendRefColumnDimEvent();
        this.applyColorMap();
      }
    }
    appendCatRepSelect() {
      const thisPPlot = this;
      select_default2(this.bindto + " .catRepSelect").append("select").on("change", function() {
        const catRep2 = _ParallelPlot.CATEGORIES_REP_LIST[this.selectedIndex];
        thisPPlot.setCategoriesRep(catRep2);
      }).selectAll("option").data(_ParallelPlot.CATEGORIES_REP_LIST).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const catRep = this.catEquallySpacedLines ? _ParallelPlot.CATEGORIES_REP_LIST[0] : _ParallelPlot.CATEGORIES_REP_LIST[1];
      const catRepIndex = _ParallelPlot.CATEGORIES_REP_LIST.indexOf(catRep);
      select_default2(this.bindto + " .catRepSelect > select").property("selectedIndex", catRepIndex);
    }
    appendArrangeMethodSelect() {
      const thisPPlot = this;
      select_default2(this.bindto + " .arrangeMethodSelect").append("select").on("change", function() {
        const arrangeMethod = _ParallelPlot.ARRANGE_METHODS[this.selectedIndex];
        thisPPlot.setArrangeMethod(arrangeMethod);
      }).selectAll("option").data(_ParallelPlot.ARRANGE_METHODS).enter().append("option").text(function(d) {
        return d;
      }).attr("value", function(d) {
        return d;
      });
      const arrangeMethodIndex = _ParallelPlot.ARRANGE_METHODS.indexOf(this.arrangeMethod);
      select_default2(this.bindto + " .arrangeMethodSelect > select").property("selectedIndex", arrangeMethodIndex);
    }
    // eslint-disable-next-line max-lines-per-function
    appendPlotSvg() {
      const ppDiv = select_default2(this.bindto + " .ppDiv");
      const svg = ppDiv.append("svg").attr("width", this.width).attr("height", this.getHeight());
      this.addGlow();
      const plotGroup = svg.append("g").attr("class", "plotGroup").attr(
        "transform",
        "translate(" + _ParallelPlot.margin.left + "," + _ParallelPlot.margin.top + ")"
      );
      this.setColorScale();
      svg.append("g").attr("class", "slider").attr(
        "transform",
        "translate(" + _ParallelPlot.margin.left + "," + _ParallelPlot.margin.top / 4 + ")"
      );
      plotGroup.append("g").attr("class", "background").attr("opacity", "0.1");
      plotGroup.append("g").attr("class", "foreground").attr("opacity", "0.8");
      plotGroup.append("g").attr("class", "columns");
      this.updateVisibleDimensions();
      new BrushSlider(this);
      this.buildPlotArea();
      plotGroup.append("path").attr("class", "subHlTrace").attr("d", this.path(0)).attr("stroke-width", 3).attr("fill", "none").attr("pointer-events", "none").style("display", "none").style("filter", "url(#glow)");
      plotGroup.append("path").attr("class", "hlTrace").attr("d", this.path(0)).attr("stroke-width", 2).attr("fill", "none").attr("pointer-events", "none").style("display", "none");
      plotGroup.append("path").attr("class", "subEditedTrace").attr("d", this.path(0)).attr("stroke-width", 3).attr("opacity", "0.8").attr("fill", "none").attr("pointer-events", "none").style("display", "none").style("filter", "url(#glow)");
      plotGroup.append("path").attr("class", "editedTrace").attr("d", this.path(0)).attr("opacity", "0.8").attr("stroke-width", 2).attr("fill", "none").style("display", "none").on("click", () => {
        this.editedRowIndex = null;
        this.drawEditedTrace();
      });
      plotGroup.append("g").attr("class", "editionCircles").style("display", "none");
    }
    checkConfig(config) {
      this.useControlWidgets = typeof config.controlWidgets === "boolean" ? config.controlWidgets : false;
      _ParallelPlot.checkData(config);
      _ParallelPlot.checkCategorical(config);
      _ParallelPlot.checkCategoriesRep(config);
      _ParallelPlot.checkArrangeMethod(config);
      _ParallelPlot.checkColumnLabels(config);
      _ParallelPlot.checkInputColumns(config);
      this.checkRefRowIndex(config);
    }
    static checkData(config) {
      if (!Array.isArray(config.data)) {
        throw new Error("given dataset is not a D3 friendly (row-oriented) data");
      }
      if (config.data.length === 0) {
        throw new Error("given dataset contains no line)");
      }
      if (typeof config.data.columns === "undefined") {
        config.data.columns = Object.keys(config.data[0]);
      }
    }
    static checkCategorical(config) {
      if (config.categorical) {
        if (Array.isArray(config.categorical)) {
          if (config.categorical.length !== config.data.columns.length) {
            console.error("Length of 'categorical' must be equal to the number of columns of 'data'");
            config.categorical = null;
          }
        } else {
          console.error("'categorical' must be an array");
          config.categorical = null;
        }
      }
    }
    static checkCategoriesRep(config) {
      if (!config.categoriesRep) {
        config.categoriesRep = _ParallelPlot.CATEGORIES_REP_LIST[0];
      } else if (!_ParallelPlot.CATEGORIES_REP_LIST.includes(config.categoriesRep)) {
        console.error("Unknown categoriesRep: " + config.categoriesRep);
      }
    }
    static checkArrangeMethod(config) {
      if (!config.arrangeMethod) {
        config.arrangeMethod = null;
      } else if (!_ParallelPlot.ARRANGE_METHODS.includes(config.arrangeMethod)) {
        console.error("Unknown arrangeMethod: " + config.arrangeMethod);
        config.arrangeMethod = null;
      }
    }
    static checkColumnLabels(config) {
      if (config.columnLabels) {
        if (Array.isArray(config.columnLabels)) {
          if (config.columnLabels.length !== config.data.columns.length) {
            console.error("Length of 'columnLabels' must be equal to the number of columns of 'data'");
            config.columnLabels = null;
          }
        } else {
          console.error("'columnLabels' must be an array");
          config.columnLabels = null;
        }
      }
    }
    static checkInputColumns(config) {
      if (config.inputColumns) {
        if (Array.isArray(config.inputColumns)) {
          if (config.inputColumns.length !== config.data.columns.length) {
            console.error("Length of 'inputColumns' must be equal to the number of columns of 'data'");
            config.inputColumns = null;
          }
        } else {
          console.error("'inputColumns' must be an array");
          config.inputColumns = null;
        }
      }
    }
    checkRefRowIndex(config) {
      if (typeof config.refRowIndex === "number") {
        this.refRowIndex = config.refRowIndex;
      } else {
        if (config.refRowIndex) {
          console.error("'refRowIndex' must be of integer type");
        }
        this.refRowIndex = null;
      }
      if (Array.isArray(config.data)) {
        const rowCount = config.data.length;
        if (typeof this.refRowIndex === "number" && (this.refRowIndex < 0 || this.refRowIndex > rowCount)) {
          console.error(`refRowIndex: ${this.refRowIndex} must be a valid row index, it must be in range: [1, ${rowCount - 1}]`);
          this.refRowIndex = null;
        }
      }
    }
    addGlow() {
      const svg = select_default2(this.bindto + " svg");
      const defs = svg.append("defs");
      const filter2 = defs.append("filter").attr("id", "glow");
      filter2.append("feGaussianBlur").attr("stdDeviation", "3.5").attr("result", "coloredBlur");
      const feMerge = filter2.append("feMerge");
      feMerge.append("feMergeNode").attr("in", "coloredBlur");
      feMerge.append("feMergeNode").attr("in", "SourceGraphic");
    }
    updateXScale() {
      this.xScaleVisibleDimension.domain(this.visibleDimensions).range([0, this.width - _ParallelPlot.margin.left - _ParallelPlot.margin.right]).padding(1);
    }
    on(typenames, callback) {
      this.dispatch.on(typenames, callback);
    }
    initSampleData(config) {
      this.sampleData = [];
      config.data.forEach((r) => {
        const curRow = {};
        config.data.columns.forEach((dim, i) => {
          const categories = Array.isArray(config.categorical) ? config.categorical[i] : null;
          const cellValue = r[dim];
          if (typeof cellValue === "undefined") {
            curRow[dim] = NaN;
          } else if (categories) {
            let catIndex = categories.indexOf(cellValue.toString());
            if (catIndex === -1) {
              catIndex = categories.indexOf(+cellValue);
            }
            curRow[dim] = catIndex === -1 ? NaN : catIndex;
          } else {
            curRow[dim] = +cellValue;
          }
        });
        this.sampleData.push(curRow);
      });
    }
    updateVisibleDimensions() {
      const withCutoffs = this.dimensions.filter(
        (dim) => this.columns[dim].hasFilters() || dim === this.refColumnDim
      );
      const requested = this.dimensions.slice(this.startingDimIndex, this.startingDimIndex + this.visibleDimCount);
      const withCutoffsBefore = withCutoffs.filter(
        (dim) => this.dimensions.indexOf(dim) < this.startingDimIndex
      );
      const withCutoffsAfter = withCutoffs.filter(
        (dim) => this.dimensions.indexOf(dim) >= this.startingDimIndex + this.visibleDimCount
      );
      const requestedPlusWithCutoffs = withCutoffsBefore.concat(requested).concat(withCutoffsAfter);
      const withoutCutoffsRequested = requested.filter(
        (dim) => !withCutoffs.includes(dim)
      );
      const requestedToRemoveBefore = withoutCutoffsRequested.splice(
        0,
        withCutoffsBefore.length
      );
      const requestedToRemoveAfter = withoutCutoffsRequested.splice(
        -withCutoffsAfter.length,
        withCutoffsAfter.length
      );
      this.visibleDimensions = requestedPlusWithCutoffs.filter(
        (dim) => !requestedToRemoveBefore.includes(dim) && !requestedToRemoveAfter.includes(dim)
      );
      this.updateXScale();
    }
    xCatOffset(dim) {
      return this.columns[dim].categorical !== null && this.arrangeMethod === _ParallelPlot.ARRANGE_FROM_BOTH ? _ParallelPlot.catClusterWidth / 2 : 0;
    }
    yRefRowOffset(dim) {
      return this.refRowIndex === null ? 0 : this.axeHeight - this.yTraceValue(dim, this.refRowIndex);
    }
    yTraceValue(dim, rowIndex) {
      return this.columns[dim].yTraceValue(rowIndex);
    }
    rowColor(row) {
      if (this.refColumnDim === null) {
        return "#03306B";
      } else {
        if (this.colorScale === null) {
          console.error("Cant't retrieve a color for a row (no color scale defined)");
          return "#03306B";
        }
        return this.colorScale(row[this.refColumnDim]);
      }
    }
    valueColor(value) {
      if (this.colorScale === null) {
        console.error("Cant't retrieve a color for a value (no color scale defined)");
        return "#03306B";
      }
      return this.colorScale(value);
    }
    changeColorMapOnDimension(d) {
      this.refColumnDim = d === this.refColumnDim ? null : d;
      this.sendRefColumnDimEvent();
      this.updateZAxisGui();
      this.applyColorMap();
    }
    setArrangeMethod(arrangeMethod) {
      if (_ParallelPlot.ARRANGE_METHODS.includes(arrangeMethod)) {
        this.arrangeMethod = arrangeMethod;
        this.refreshTracesPaths();
      } else {
        console.error("Unknown arrange method: " + arrangeMethod);
      }
    }
    setCategoriesRep(categoriesRep) {
      if (_ParallelPlot.CATEGORIES_REP_LIST.includes(categoriesRep)) {
        this.catEquallySpacedLines = categoriesRep === _ParallelPlot.CATEGORIES_REP_LIST[0];
        this.refreshTracesPaths();
        this.refreshCategoriesBoxes();
        this.updateColumnLabels();
      } else {
        console.error("Unknown categories representation: " + categoriesRep);
      }
    }
    setRefColumnDim(dim) {
      if (dim === null || this.dimensions.includes(dim)) {
        if (this.refColumnDim !== dim) {
          this.refColumnDim = dim;
          this.updateZAxisGui();
          this.applyColorMap();
        }
      } else {
        console.error("Unknown ref comumn dim: " + dim);
      }
    }
    setContinuousColorScale(continuousCsId) {
      if (_ParallelPlot.CONTINUOUS_CS_IDS.includes(continuousCsId)) {
        this.continuousCsId = continuousCsId;
        this.applyColorMap();
      } else {
        console.error("Unknown continuous color scale: " + continuousCsId);
      }
    }
    setCategoricalColorScale(categoricalCsId) {
      if (_ParallelPlot.CATEGORIAL_CS_IDS.includes(categoricalCsId)) {
        this.categoricalCsId = categoricalCsId;
        this.applyColorMap();
      } else {
        console.error("Unknown categorical color scale: " + categoricalCsId);
      }
    }
    setHistoVisibility(histoVisibility) {
      Object.keys(this.sampleData[0]).forEach((dim, i) => {
        if (Array.isArray(histoVisibility)) {
          this.columns[dim].histoVisible = histoVisibility[i];
        } else {
          if (typeof histoVisibility[dim] === "undefined") {
            return;
          }
          this.columns[dim].histoVisible = histoVisibility[dim];
        }
        this.drawMainHistograms();
        this.drawSelectedHistograms();
      });
    }
    setInvertedAxes(invertedAxes) {
      Object.keys(this.sampleData[0]).forEach((dim, i) => {
        if (Array.isArray(invertedAxes)) {
          if (this.columns[dim].setInvertedAxe(invertedAxes[i]) && this.columnHeaders) {
            this.columnHeaders.reverseDomainOnAxis(dim);
          }
        } else {
          if (typeof invertedAxes[dim] === "undefined") {
            return;
          }
          if (this.columns[dim].setInvertedAxe(invertedAxes[dim]) && this.columnHeaders) {
            this.columnHeaders.reverseDomainOnAxis(dim);
          }
        }
      });
      this.refreshTracesPaths();
    }
    updateColumnLabels() {
      const parallelPlot = this;
      const dimensionGroup = select_default2(parallelPlot.bindto + " .plotGroup").selectAll(".dimension");
      const axisLabel = dimensionGroup.select(".axisLabel");
      axisLabel.each(function(dim) {
        const self = select_default2(this);
        const invertIndicator = parallelPlot.columns[dim].continuous && parallelPlot.columns[dim].continuous?.invertedAxe ? "\u2193 " : "";
        const hiddenCatCount = parallelPlot.catEquallySpacedLines && parallelPlot.columns[dim].categorical ? parallelPlot.columns[dim].categorical?.catWithoutTraces().length : 0;
        const hiddenCatIndicator = hiddenCatCount === 0 ? "" : ` (-${hiddenCatCount})`;
        const labels = (invertIndicator + parallelPlot.columns[dim].label + hiddenCatIndicator).split("<br>");
        self.text(labels[0]);
        for (let i = 1; i < labels.length; i++) {
          self.append("tspan").attr("x", 0).attr("dy", "1em").text(labels[i]);
        }
        if (parallelPlot.rotateTitle) {
          self.attr("y", 0).attr("transform", "rotate(-10) translate(-20," + -15 * labels.length + ")");
        } else {
          self.style("text-anchor", "middle").attr("y", -15 * labels.length);
        }
      });
    }
    refreshCategoriesBoxes() {
      Object.values(this.columns).forEach((column) => {
        if (column.categorical) {
          column.categorical.refreshBoxesRep();
        }
      });
    }
    unvalidateColumnInit() {
      Object.values(this.columns).forEach((column) => {
        if (column.categorical) {
          column.categorical.unvalidateInit();
        }
      });
    }
    refreshTracesPaths() {
      this.unvalidateColumnInit();
      this.updateTracesPaths();
      this.updateEditedTrace();
    }
    updateTracesPaths() {
      this.unvalidateColumnInit();
      select_default2(this.bindto + " .foreground").selectAll("path").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("d", (_d, i) => {
        return this.path(i);
      });
      select_default2(this.bindto + " .background").selectAll("path").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("d", (_d, i) => {
        return this.path(i);
      });
    }
    updateEditedTrace() {
      if (this.editedRowIndex === null) {
        return;
      }
      select_default2(this.bindto + " .subEditedTrace").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("d", this.path(this.editedRowIndex));
      select_default2(this.bindto + " .editedTrace").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("d", this.path(this.editedRowIndex));
      selectAll_default2(this.bindto + " .gEditionCircle").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("transform", (dim) => {
        const x2 = this.xScaleVisibleDimension(dim);
        const xCatOffset = this.xCatOffset(dim);
        const yRefRowOffset = this.yRefRowOffset(dim);
        return `translate(${x2 + xCatOffset}, ${yRefRowOffset})`;
      });
      select_default2(this.bindto + " .editionCircles").selectAll("circle").transition().ease(backOut).duration(_ParallelPlot.D3_TRANSITION_DURATION).attr("cy", (dim) => {
        if (this.editedRowIndex === null) {
          return 0;
        }
        return this.yTraceValue(dim, this.editedRowIndex) + this.yRefRowOffset(dim);
      });
    }
    setCutoffs(cutoffs) {
      Object.keys(this.sampleData[0]).forEach((dim, i) => {
        if (cutoffs === null) {
          this.columns[dim].setCutoffs(null);
        } else if (Array.isArray(cutoffs)) {
          this.columns[dim].setCutoffs(cutoffs[i]);
        } else {
          if (typeof cutoffs[dim] === "undefined") {
            return;
          }
          this.columns[dim].setCutoffs(cutoffs[dim]);
        }
        if (this.columns[dim].continuous) {
          this.columns[dim].continuous.initMultiBrush();
        }
        if (this.columns[dim].categorical) {
          this.columns[dim].categorical.applyCategoricalCutoffs();
        }
      });
      this.updateSelectedRows();
      this.drawSelectedTraces();
      this.drawSelectedHistograms();
    }
    adjustVisibleDimensions() {
      if (this.dimensions.length < this.defaultVisibleDimCount) {
        this.visibleDimCount = this.dimensions.length;
      } else {
        this.visibleDimCount = this.defaultVisibleDimCount;
      }
      this.startingDimIndex = 0;
      this.updateVisibleDimensions();
    }
    setKeptColumns(keptColumns) {
      if (Array.isArray(keptColumns)) {
        this.dimensions = Object.keys(this.sampleData[0]).filter(
          (_dim, i) => keptColumns[i]
        );
      } else {
        this.dimensions = Object.keys(this.sampleData[0]).filter((dim) => {
          let toKeep = this.dimensions.includes(dim);
          if (typeof keptColumns[dim] !== "undefined") {
            toKeep = keptColumns[dim];
          }
          return toKeep;
        });
      }
      this.adjustVisibleDimensions();
      select_default2(this.bindto + " .slider .axisGroup").remove();
      select_default2(this.bindto + " .slider .brushDim").remove();
      new BrushSlider(this);
      this.buildPlotArea();
      this.style.applyCssRules();
    }
    getValue(attrType) {
      if (attrType === _ParallelPlot.CO_ATTR_TYPE) {
        const coMap = /* @__PURE__ */ new Map();
        Object.keys(this.sampleData[0]).forEach((dim) => {
          const cutoffs2 = this.columns[dim].getCutoffs();
          if (cutoffs2 !== null) {
            coMap.set(dim, cutoffs2);
          }
        });
        const cutoffs = {};
        for (const [dim, co] of coMap) {
          cutoffs[dim] = co;
        }
        return cutoffs;
      }
      if (attrType === _ParallelPlot.ST_ATTR_TYPE) {
        return [...this.selectedRows];
      }
      if (attrType === _ParallelPlot.RC_ATTR_TYPE) {
        return this.refColumnDim;
      }
      throw new Error("'getValue' called with an unknown attrType: " + attrType);
    }
    setColorScale() {
      if (this.refColumnDim !== null) {
        if (this.columns[this.refColumnDim].continuous) {
          const [yRefMin, yRefMax] = this.columns[this.refColumnDim].continuous.y().domain();
          this.colorScale = sequential(_ParallelPlot.CONTINUOUS_CS[this.continuousCsId]).domain([yRefMin, yRefMax]);
        }
        if (this.columns[this.refColumnDim].categorical) {
          const yRefMax = this.columns[this.refColumnDim].categorical.categories.length;
          this.colorScale = _ParallelPlot.CATEGORIAL_CS[this.categoricalCsId].domain(range(yRefMax));
        }
      }
    }
    // eslint-disable-next-line max-lines-per-function
    applyColorMap() {
      const thisPlot = this;
      this.setColorScale();
      select_default2(this.bindto + " .foreground").selectAll("path").transition().duration(this.changeColorDuration).attr("stroke", (row) => {
        return this.rowColor(row);
      });
      select_default2(this.bindto + " .background").selectAll("path").transition().duration(this.changeColorDuration).attr("stroke", (row) => {
        return this.rowColor(row);
      });
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").each(function(dim) {
        if (dim === thisPlot.refColumnDim) {
          select_default2(this).selectAll(".category rect").transition().duration(thisPlot.changeColorDuration).attr("fill", function(cat) {
            if (thisPlot.columns[dim].categorical) {
              const catIndex = thisPlot.columns[dim].categorical.categories.indexOf(cat);
              return thisPlot.valueColor(catIndex);
            }
            throw new Error("'applyColorMap' failed for dim:" + dim);
          });
        } else {
          select_default2(this).selectAll(".category rect").transition().duration(thisPlot.changeColorDuration).attr("fill", "black");
        }
      });
      if (this.editedRowIndex !== null) {
        const stroke = this.selectedRows.has(this.editedRowIndex) ? this.rowColor(this.sampleData[this.editedRowIndex]) : "#FFFFFF";
        const greyStroke = _ParallelPlot.greyStroke(stroke);
        select_default2(this.bindto + " .subEditedTrace").transition().duration(this.changeColorDuration).attr("stroke", greyStroke);
        select_default2(this.bindto + " .editedTrace").transition().duration(this.changeColorDuration).attr("stroke", stroke);
      }
      this.drawContinuousCS();
    }
    drawContinuousCS() {
      select_default2(this.bindto + " .plotGroup").selectAll(".colorScaleBar").remove();
      selectAll_default2(this.bindto + " .plotGroup .dimension").each((dim) => {
        if (this.columns[dim].continuous && this.refColumnDim === this.columns[dim].dim) {
          this.columns[dim].continuous.drawColorScale();
        }
      });
    }
    drawContinuousAxes() {
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").append("g").attr("class", "axisGroup").each((dim) => {
        if (this.columns[dim].continuous) {
          this.columns[dim].continuous.drawAxe();
        }
      });
    }
    drawCategoricals() {
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").filter((dim) => this.columns[dim].categorical !== null).append("g").attr("class", "catGroup").each((dim) => {
        this.columns[dim].categorical.drawCategories();
      });
    }
    updateSelectedRows() {
      this.selectedRows.clear();
      this.sampleData.forEach((row, i) => {
        const isKept = this.dimensions.every((dim) => {
          return this.columns[dim].isKept(row[dim]);
        });
        if (isKept) {
          this.selectedRows.add(i);
        }
      });
    }
    buildPlotArea() {
      this.drawBackGroundPath();
      this.drawForeGroundPath();
      this.drawEditedTrace();
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").remove();
      const dimensionGroup = select_default2(this.bindto + " .columns").selectAll(".dimension").data(this.visibleDimensions).enter().append("g").classed("dimension", true).classed("input", (dim) => this.columns[dim].isInput()).classed("output", (dim) => this.columns[dim].isOutput()).attr("transform", (dim) => {
        const x2 = this.xScaleVisibleDimension(dim);
        const yRefRowOffset = this.yRefRowOffset(dim);
        return `translate(${x2}, ${yRefRowOffset})`;
      });
      this.drawContinuousAxes();
      this.drawCategoricals();
      this.drawMainHistograms();
      this.drawSelectedHistograms();
      dimensionGroup.append("g").filter((dim) => this.columns[dim].continuous !== null).attr("class", (dim) => MultiBrush.multiBrushClass(this.columns[dim].colIndex)).each((dim) => {
        this.columns[dim].continuous.initMultiBrush();
      });
      this.columnHeaders = new ColumnHeaders(this);
      this.drawContinuousCS();
    }
    drawBackGroundPath() {
      select_default2(this.bindto + " .background").selectAll("path").remove();
      select_default2(this.bindto + " .background").selectAll("path").data(this.sampleData).enter().append("path").attr("d", (_row, i) => this.path(i)).attr("stroke", (row) => this.rowColor(row)).style("display", (_row, i) => this.selectedRows.has(i) ? "none" : null);
    }
    drawForeGroundPath() {
      const thisPlot = this;
      select_default2(this.bindto + " .foreground").selectAll("path").remove();
      select_default2(this.bindto + " .foreground").selectAll("path").data(this.sampleData).enter().append("path").attr("d", (_row, i) => this.path(i)).attr("stroke", (row) => this.rowColor(row)).attr("stroke-width", 1).style("display", (_row, i) => this.selectedRows.has(i) ? null : "none").on("mouseover", (_event, row) => {
        const clickedRowIndex = thisPlot.sampleData.indexOf(row);
        this.drawHighlightedTrace(clickedRowIndex);
        this.showTraceTooltip(row, clickedRowIndex, this.traceTooltipLocation());
        this.sendHlPointEvent(clickedRowIndex);
      }).on("mouseout", () => {
        this.drawHighlightedTrace(null);
        select_default2(this.bindto + " .ppTooltip").style("display", "none");
        this.sendHlPointEvent(null);
      }).on("click", function(_event, row) {
        const clickedRowIndex = thisPlot.sampleData.indexOf(row);
        if (thisPlot.editionMode !== _ParallelPlot.EDITION_OFF) {
          thisPlot.editedRowIndex = thisPlot.editedRowIndex === clickedRowIndex ? null : clickedRowIndex;
          thisPlot.drawEditedTrace();
        }
        thisPlot.sendClickEvent(clickedRowIndex);
      });
    }
    path(rowIndex) {
      let nodeCount = this.visibleDimensions.length;
      this.visibleDimensions.forEach((dim) => {
        if (this.columns[dim].categorical !== null) {
          nodeCount = nodeCount + 1;
        }
      });
      const lineData = new Array(nodeCount);
      let nodeIndex = 0;
      this.visibleDimensions.forEach((dim) => {
        const x2 = this.xScaleVisibleDimension(dim);
        const xCatOffset = this.xCatOffset(dim);
        const y2 = this.yTraceValue(dim, rowIndex);
        const yRefRowOffset = this.yRefRowOffset(dim);
        lineData[nodeIndex] = [x2 - xCatOffset, y2 + yRefRowOffset];
        nodeIndex = nodeIndex + 1;
        if (this.columns[dim].categorical !== null) {
          const yOut = this.arrangeMethod === _ParallelPlot.ARRANGE_FROM_BOTH ? this.columns[dim].categorical.yTraceValueOut(rowIndex) : y2;
          lineData[nodeIndex] = [x2 + xCatOffset, yOut + yRefRowOffset];
          nodeIndex = nodeIndex + 1;
        }
      });
      return _ParallelPlot.line(lineData);
    }
    editedPath(editedDim, position) {
      return _ParallelPlot.line(
        this.visibleDimensions.map((dim) => {
          const x2 = this.xScaleVisibleDimension(dim);
          const xCatOffset = this.xCatOffset(dim);
          const y2 = dim === editedDim || this.editedRowIndex === null ? position : this.yTraceValue(dim, this.editedRowIndex);
          const yRefRowOffset = this.yRefRowOffset(dim);
          return [x2 + xCatOffset, y2 + yRefRowOffset];
        })
      );
    }
    drawHighlightedTrace(rowIndex) {
      if (this.hlPointIndex !== rowIndex) {
        this.hlPointIndex = rowIndex;
        if (rowIndex === null || this.editedPointDim !== null) {
          select_default2(this.bindto + " .subHlTrace").style("display", "none");
          select_default2(this.bindto + " .hlTrace").style("display", "none");
        } else {
          const row = this.sampleData[rowIndex];
          const stroke = this.rowColor(row);
          const greyStroke = _ParallelPlot.greyStroke(stroke);
          select_default2(this.bindto + " .subHlTrace").attr("d", this.path(rowIndex)).attr("stroke", greyStroke).style("display", null);
          select_default2(this.bindto + " .hlTrace").attr("d", this.path(rowIndex)).attr("stroke", stroke).style("display", null);
        }
        select_default2(this.bindto + " .foreground").attr("opacity", rowIndex === null && this.editedRowIndex === null ? 0.8 : 0.6);
      }
    }
    initTraceTooltip() {
      const rowIndex = 0;
      const coords = this.traceTooltipLocation();
      const tooltipTitle = this.rowLabels ? this.rowLabels[rowIndex] : "Point " + (rowIndex + 1);
      select_default2(this.bindto + " .ppTooltip").remove();
      const ppDiv = select_default2(this.bindto + " .ppDiv");
      const tooltip = ppDiv.append("div").attr("class", "ppTooltip").style("display", "none").style("left", coords[0] + "px").style("top", coords[1] + "px");
      tooltip.append("div").attr("class", "pointIndex title").text(tooltipTitle);
      tooltip.append("div").selectAll("xNameDiv").data(this.visibleDimensions).enter().append("div").attr("class", "xNameDiv").html((dim) => {
        const column = this.columns[dim];
        return `<span class="xName">${column.labelText()}</span>: <span class="xValue">${column.formatedRowValue(this.sampleData[rowIndex])}</span>`;
      });
    }
    showTraceTooltip(row, rowIndex, coords) {
      const tooltipTitle = this.rowLabels ? this.rowLabels[rowIndex] : "Point " + (rowIndex + 1);
      const tooltip = select_default2(this.bindto + " .ppTooltip").style("display", "block").style("left", coords[0] + "px").style("top", coords[1] + "px");
      tooltip.select(".pointIndex.title").text(tooltipTitle);
      const xNameDiv = tooltip.selectAll(".xNameDiv");
      xNameDiv.select(".xName").text((dim) => {
        return this.columns[dim].labelText();
      });
      xNameDiv.select(".xValue").text((dim) => {
        return this.columns[dim].formatedRowValue(row);
      });
    }
    traceTooltipLocation() {
      const ppDivNode = select_default2(this.bindto + " .ppDiv").node();
      const parentBounds = ppDivNode === null ? null : ppDivNode.getBoundingClientRect();
      const xParent = parentBounds === null ? 0 : parentBounds.x;
      const yParent = parentBounds === null ? 0 : parentBounds.y;
      const plotGroup = select_default2(this.bindto + " .plotGroup").node();
      const elementBounds = plotGroup === null ? null : plotGroup.getBoundingClientRect();
      const xRect = elementBounds === null ? 0 : elementBounds.x;
      const yRect = elementBounds === null ? 0 : elementBounds.y;
      const wRect = elementBounds === null ? 0 : elementBounds.width;
      return [xRect - xParent + wRect + 5, yRect - yParent + 20];
    }
    static greyStroke(stroke) {
      const strokeColor = color(stroke);
      let greyStroke = rgb(0, 0, 0);
      if (strokeColor) {
        const rgb2 = strokeColor.rgb();
        const greyComp = Math.round(((rgb2.r ^ 255) + (rgb2.g ^ 255) + (rgb2.b ^ 255)) / 3);
        greyStroke = rgb(greyComp, greyComp, greyComp);
      }
      return greyStroke.hex();
    }
    // eslint-disable-next-line max-lines-per-function
    drawEditedTrace() {
      const editedRowIndex = this.editedRowIndex;
      if (editedRowIndex === null) {
        select_default2(this.bindto + " .subEditedTrace").style("display", "none");
        select_default2(this.bindto + " .editedTrace").style("display", "none");
      } else {
        const row = this.sampleData[editedRowIndex];
        const stroke = this.selectedRows.has(editedRowIndex) ? this.rowColor(row) : "#FFFFFF";
        const greyStroke = _ParallelPlot.greyStroke(stroke);
        select_default2(this.bindto + " .subEditedTrace").attr("d", this.path(editedRowIndex)).attr("stroke", greyStroke).style("display", null);
        select_default2(this.bindto + " .editedTrace").attr("d", this.path(editedRowIndex)).attr("stroke", stroke).style("display", null);
      }
      select_default2(this.bindto + " .foreground").attr("opacity", this.editedRowIndex === null ? 0.8 : 0.6);
      const visibleInputDims = this.visibleDimensions.filter((dim) => this.columns[dim].isInput());
      select_default2(this.bindto + " .editionCircles").style("display", () => this.editedRowIndex === null ? "none" : null).selectAll(".gEditionCircle").data(visibleInputDims).join(
        (enter) => {
          const gEditionCircle = enter.append("g").attr("class", "gEditionCircle");
          gEditionCircle.append("circle").attr("r", 4).call(
            drag_default().on("start", (_event, dim) => {
              this.editedPointDim = dim;
              this.drawEditedTrace();
            }).on("drag", (event, dim) => {
              this.pointDrag(event.y, dim);
            }).on("end", (event, dim) => {
              this.pointDragEnd(event.y, dim);
            })
          );
          return gEditionCircle;
        },
        (update) => update,
        (exit) => exit.remove()
      ).attr("transform", (dim) => {
        const x2 = this.xScaleVisibleDimension(dim);
        const xCatOffset = this.xCatOffset(dim);
        const yRefRowOffset = this.yRefRowOffset(dim);
        return `translate(${x2 + xCatOffset}, ${yRefRowOffset})`;
      }).select("circle").attr("cx", 0).filter((dim) => dim !== this.editedPointDim).attr("cy", (dim) => {
        if (this.editedRowIndex === null) {
          return 0;
        }
        return this.yTraceValue(dim, this.editedRowIndex);
      });
    }
    pointDrag(y2, draggedDim) {
      if (this.editionMode === _ParallelPlot.EDITION_ON_DRAG_END) {
        this.dragEditionPoint(draggedDim, y2);
      } else if (this.editionMode === _ParallelPlot.EDITION_ON_DRAG) {
        this.dragEditionPoint(draggedDim, y2);
        this.askForPointEdition(draggedDim, y2);
      }
    }
    pointDragEnd(y2, draggedDim) {
      if (this.editionMode !== _ParallelPlot.EDITION_OFF) {
        this.askForPointEdition(draggedDim, y2);
        this.editedPointDim = null;
      }
    }
    dragEditionPoint(draggedDim, position) {
      if (this.editedRowIndex === null) {
        console.error("dragEditionPoint is called but editedRowIndex is null");
        return;
      }
      select_default2(this.bindto + " .editionCircles").selectAll("circle").filter((dim) => dim === draggedDim).attr("cy", () => position);
      select_default2(this.bindto + " .foreground").selectAll("path").filter((_row, i) => i === this.editedRowIndex).attr("d", this.editedPath(draggedDim, position));
      select_default2(this.bindto + " .subEditedTrace").attr("d", this.editedPath(draggedDim, position));
      select_default2(this.bindto + " .editedTrace").attr("d", this.editedPath(draggedDim, position));
    }
    askForPointEdition(draggedDim, y2) {
      if (this.editedRowIndex === null) {
        console.error("dragEditedTrace is called but editedRowIndex is null");
        return;
      }
      if (this.columns[draggedDim].categorical) {
        const newValue = this.columns[draggedDim].categorical.invertYValue(y2);
        const categories = this.columns[draggedDim].categorical.categories;
        const catIndex = Math.round(newValue);
        if (catIndex >= 0 && catIndex < categories.length) {
          this.sendPointEditionEvent(draggedDim, this.editedRowIndex, categories[catIndex]);
        }
      }
      if (this.columns[draggedDim].continuous) {
        const newValue = this.columns[draggedDim].continuous.y().invert(y2);
        this.sendPointEditionEvent(draggedDim, this.editedRowIndex, newValue);
      }
    }
    drawSelectedTraces() {
      select_default2(this.bindto + " .foreground").selectAll("path").style("display", (_d, i) => this.selectedRows.has(i) ? null : "none");
      select_default2(this.bindto + " .background").selectAll("path").style("display", (_d, i) => this.selectedRows.has(i) ? "none" : null);
      select_default2(this.bindto + " .plotGroup").selectAll(".histogram").style("display", () => {
        return this.selectedRows.size > this.sampleData.length / 10 ? null : "none";
      });
    }
    setContCutoff(brushSelections, dim, adjusting) {
      if (this.columns[dim].continuous) {
        const contCutoffs = brushSelections.map((interval2) => interval2.map(this.columns[dim].continuous.y().invert)).map((interval2) => {
          return interval2.sort(function(a, b) {
            return b - a;
          });
        });
        if (contCutoffs === null || contCutoffs.length === 0) {
          this.columns[dim].continuous.contCutoffs = null;
        } else {
          this.columns[dim].continuous.contCutoffs = contCutoffs;
        }
      } else {
        throw new Error("'setContCutoff' failed for:" + dim);
      }
      this.applyColumnCutoffs(dim, adjusting);
    }
    applyColumnCutoffs(updatedDim, adjusting) {
      this.updateSelectedRows();
      this.drawSelectedTraces();
      this.drawSelectedHistograms();
      this.sendCutoffEvent(updatedDim, adjusting);
    }
    sendCutoffEvent(updatedDim, adjusting) {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.CUTOFF_EVENT,
          value: { updatedDim, adjusting, cutoffs: this.getValue(_ParallelPlot.CO_ATTR_TYPE), selectedTraces: this.getValue(_ParallelPlot.ST_ATTR_TYPE) }
        }
      );
    }
    sendClickEvent(rowIndex) {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.ROW_CLICK_EVENT,
          value: { rowIndex }
        }
      );
    }
    sendHlPointEvent(rowIndex) {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.HL_ROW_EVENT,
          value: { rowIndex }
        }
      );
    }
    sendPointEditionEvent(dim, rowIndex, newValue) {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.EDITION_EVENT,
          value: { dim, rowIndex, newValue }
        }
      );
    }
    sendInvertedAxeEvent(invertedAxes) {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.INVERTED_AXE_EVENT,
          value: { invertedAxes }
        }
      );
    }
    sendRefColumnDimEvent() {
      this.dispatch.call(
        _ParallelPlot.PLOT_EVENT,
        void 0,
        {
          type: _ParallelPlot.REF_COLUMN_DIM_EVENT,
          value: { refColumnDim: this.refColumnDim }
        }
      );
    }
    highlightRow(rowIndex) {
      this.drawHighlightedTrace(rowIndex);
    }
    changeRow(rowIndex, newValues) {
      const changedRow = this.sampleData[rowIndex];
      Object.keys(this.sampleData[0]).forEach((dim) => {
        this.columns[dim].unvalid();
      });
      Object.keys(newValues).forEach((dim) => {
        if (this.columns[dim].categorical) {
          const categories = this.columns[dim].categorical.categories;
          changedRow[dim] = categories.indexOf(newValues[dim].toString());
        } else {
          changedRow[dim] = +newValues[dim];
        }
      });
      this.sampleData[rowIndex] = changedRow;
      const isKept = this.dimensions.every((dim) => {
        return this.columns[dim].isKept(this.sampleData[rowIndex][dim]);
      });
      if (isKept) {
        this.selectedRows.add(rowIndex);
      } else {
        this.selectedRows.delete(rowIndex);
      }
      this.unvalidateColumnInit();
      this.buildPlotArea();
      this.style.applyCssRules();
    }
    drawMainHistograms() {
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").each((dim) => this.columns[dim].drawMainHistogram());
    }
    drawSelectedHistograms() {
      const selected = this.sampleData.filter(
        (_row, i) => this.selectedRows.has(i)
      );
      select_default2(this.bindto + " .plotGroup").selectAll(".dimension").each((dim) => this.columns[dim].drawSelectedHistogram(selected));
    }
    // eslint-disable-next-line max-lines-per-function
    getPlotConfig() {
      const allDimensions = Object.keys(this.sampleData[0]);
      const categorical = allDimensions.map((dim) => {
        if (this.columns[dim]) {
          return this.columns[dim].categorical ? this.columns[dim].categorical.categories : null;
        }
        return null;
      });
      const inputColumns = allDimensions.map((dim) => this.columns[dim] && this.columns[dim].ioType === Column.INPUT);
      const keptColumns = allDimensions.map((dim) => this.dimensions.includes(dim));
      const histoVisibility = allDimensions.map(
        (dim) => this.columns[dim] ? this.columns[dim].histoVisible : false
      );
      const invertedAxes = allDimensions.map((dim) => {
        if (this.columns[dim]) {
          return this.columns[dim].continuous ? this.columns[dim].continuous.invertedAxe : false;
        }
        return false;
      });
      const cutoffs = allDimensions.map(
        (dim) => this.columns[dim] ? this.columns[dim].getCutoffs() : null
      );
      const columnLabels = allDimensions.map(
        (dim) => this.columns[dim] && this.columns[dim].label ? this.columns[dim].label : dim
      );
      return {
        data: [],
        rowLabels: this.rowLabels,
        categorical,
        categoriesRep: this.catEquallySpacedLines ? _ParallelPlot.CATEGORIES_REP_LIST[0] : _ParallelPlot.CATEGORIES_REP_LIST[1],
        arrangeMethod: this.arrangeMethod,
        inputColumns,
        keptColumns,
        histoVisibility,
        invertedAxes,
        cutoffs,
        refRowIndex: this.refRowIndex,
        refColumnDim: this.refColumnDim,
        rotateTitle: this.rotateTitle,
        columnLabels,
        categoricalCS: this.categoricalCsId,
        continuousCS: this.continuousCsId,
        editionMode: this.continuousCsId,
        controlWidgets: this.useControlWidgets,
        cssRules: this.style.cssRules,
        sliderPosition: {
          dimCount: this.visibleDimCount,
          startingDimIndex: this.startingDimIndex
        }
      };
    }
  };

  // src/Typescript/ppHtmlwidget.ts
  HTMLWidgets.widget({
    name: "parallelPlot",
    type: "output",
    // eslint-disable-next-line max-lines-per-function
    factory: function(el, width, height) {
      function js2RIndex(index) {
        return typeof index === "number" ? index + 1 : index;
      }
      function r2JsIndex(index) {
        return typeof index === "number" ? index - 1 : index;
      }
      const parallelPlot = new ParallelPlot(el.id, width, height);
      return {
        // eslint-disable-next-line max-lines-per-function
        renderValue: function(config) {
          document.getElementById(parallelPlot.id()).widget = this;
          if (HTMLWidgets.shinyMode) {
            ["setArrangeMethod", "setCategoriesRep", "setContinuousColorScale", "setRefColumnDim", "setCategoricalColorScale", "setHistoVisibility", "setInvertedAxes", "setCutoffs", "setKeptColumns", "getValue", "highlightRow", "changeRow", "getPlotConfig"].forEach((func) => {
              Shiny.addCustomMessageHandler("parallelPlot:" + func, function(message) {
                const elem = document.getElementById(message.id);
                if (elem) {
                  elem.widget[func](message);
                }
              });
            });
            if (config.eventInputId !== null) {
              parallelPlot.on(ParallelPlot.PLOT_EVENT, function(event) {
                if (event.type === ParallelPlot.EDITION_EVENT || event.type === ParallelPlot.ROW_CLICK_EVENT || event.type === ParallelPlot.HL_ROW_EVENT) {
                  event.value.rowIndex = js2RIndex(event.value.rowIndex);
                }
                if (event.type === ParallelPlot.CUTOFF_EVENT) {
                  event.value.selectedTraces = event.value.selectedTraces.map(js2RIndex);
                }
                Shiny.setInputValue(config.eventInputId, event, { priority: "event" });
              });
            }
          }
          const controlWidgets = config.controlWidgets === null ? !HTMLWidgets.shinyMode : config.controlWidgets;
          const sliderPosition = config.sliderPosition ? {} : null;
          if (config.sliderPosition) {
            if (typeof config.sliderPosition.dimCount === "number") {
              sliderPosition.dimCount = config.sliderPosition.dimCount;
            }
            if (typeof config.sliderPosition.startingDimIndex === "number") {
              sliderPosition.startingDimIndex = r2JsIndex(config.sliderPosition.startingDimIndex);
            }
          }
          parallelPlot.generate({
            data: HTMLWidgets.dataframeToD3(config.data),
            rowLabels: config.rowLabels,
            categorical: config.categorical,
            categoriesRep: config.categoriesRep,
            arrangeMethod: config.arrangeMethod,
            inputColumns: config.inputColumns,
            keptColumns: config.keptColumns,
            histoVisibility: config.histoVisibility,
            invertedAxes: config.invertedAxes,
            cutoffs: config.cutoffs,
            refRowIndex: config.refRowIndex ? r2JsIndex(config.refRowIndex) : void 0,
            refColumnDim: config.refColumnDim,
            rotateTitle: config.rotateTitle,
            columnLabels: config.columnLabels,
            continuousCS: config.continuousCS,
            categoricalCS: config.categoricalCS,
            editionMode: config.editionMode,
            controlWidgets,
            cssRules: config.cssRules,
            sliderPosition
          });
        },
        // End 'renderValue'
        setArrangeMethod: function(params) {
          parallelPlot.setArrangeMethod(params.arrangeMethod);
        },
        setCategoriesRep: function(params) {
          parallelPlot.setCategoriesRep(params.categoriesRep);
        },
        setRefColumnDim: function(params) {
          parallelPlot.setRefColumnDim(params.dim);
        },
        setContinuousColorScale: function(params) {
          parallelPlot.setContinuousColorScale(params.continuousCsId);
        },
        setCategoricalColorScale: function(params) {
          parallelPlot.setCategoricalColorScale(params.categoricalCsId);
        },
        setHistoVisibility: function(params) {
          parallelPlot.setHistoVisibility(params.histoVisibility);
        },
        setInvertedAxes: function(params) {
          parallelPlot.setInvertedAxes(params.invertedAxes);
        },
        setCutoffs: function(params) {
          parallelPlot.setCutoffs(params.cutoffs);
        },
        setKeptColumns: function(params) {
          parallelPlot.setKeptColumns(params.keptColumns);
        },
        getValue: function(params) {
          if (HTMLWidgets.shinyMode) {
            let value = parallelPlot.getValue(params.attrType);
            if (value === null) {
              value = "NULL";
            } else {
              if (params.attrType === ParallelPlot.ST_ATTR_TYPE) {
                value = value.map(js2RIndex);
              }
            }
            Shiny.setInputValue(params.valueInputId, value, { priority: "event" });
          }
        },
        highlightRow: function(params) {
          parallelPlot.highlightRow(r2JsIndex(params.rowIndex), params.newValues);
        },
        changeRow: function(params) {
          if (typeof params.rowIndex === "number") {
            parallelPlot.changeRow(r2JsIndex(params.rowIndex), params.newValues);
          }
        },
        getPlotConfig: function(params) {
          if (HTMLWidgets.shinyMode) {
            const plotConfig = parallelPlot.getPlotConfig();
            if (typeof plotConfig.refRowIndex === "number") {
              plotConfig.refRowIndex = js2RIndex(plotConfig.refRowIndex);
            }
            if (plotConfig.sliderPosition && typeof plotConfig.sliderPosition.startingDimIndex === "number") {
              plotConfig.sliderPosition.startingDimIndex = js2RIndex(plotConfig.sliderPosition.startingDimIndex);
            }
            Shiny.setInputValue(params.configInputId, plotConfig, { priority: "event" });
          }
        },
        resize: function(newWidth, newHeight) {
          parallelPlot.resize(newWidth, newHeight);
        }
      };
    }
    // End 'factory'
  });
})();
