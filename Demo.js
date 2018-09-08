
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    var result = new Array(size);

    for (var i = 0; i < size; i++) {
        result[i] = f(offset + i);
    }

    return result;
}

function initializeFromList(max, ls) {
    var result = new Array(max);

    for (var i = 0; i < max && ls.ctor !== '[]'; i++) {
        result[i] = ls._0;
        ls = ls._1;
    }

    result.length = i;

    return {
        ctor: '_Tuple2',
        _0: result,
        _1: ls
    };
}

function unsafeGet(idx, arr) {
    return arr[idx];
}

function unsafeSet(idx, val, arr) {
    var length = arr.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++) {
        result[i] = arr[i];
    }

    result[idx] = val;
    return result;
}

function push(val, arr) {
    var length = arr.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++) {
        result[i] = arr[i];
    }

    result[length] = val;
    return result;
}

function foldl(f, acc, arr) {
    var length = arr.length;

    for (var i = 0; i < length; i++) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function foldr(f, acc, arr) {
    for (var i = arr.length - 1; i >= 0; i--) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function map(f, arr) {
    var length = arr.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++) {
        result[i] = f(arr[i]);
    }

    return result;
}

function indexedMap(f, offset, arr) {
    var length = arr.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++) {
        result[i] = A2(f, offset + i, arr[i]);
    }

    return result;
}

function slice(from, to, arr) {
    return arr.slice(from, to);
}

function appendN(n, dest, source) {
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length) {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++) {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++) {
        result[i + destLen] = source[i];
    }

    return result;
}

return {
    empty: empty,
    singleton: singleton,
    length: length,
    initialize: F3(initialize),
    initializeFromList: F2(initializeFromList),
    unsafeGet: F2(unsafeGet),
    unsafeSet: F3(unsafeSet),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    map: F2(map),
    indexedMap: F3(indexedMap),
    slice: F3(slice),
    appendN: F3(appendN)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _Skinney$elm_array_exploration$Array_JsArray$appendN = _Skinney$elm_array_exploration$Native_JsArray.appendN;
var _Skinney$elm_array_exploration$Array_JsArray$slice = _Skinney$elm_array_exploration$Native_JsArray.slice;
var _Skinney$elm_array_exploration$Array_JsArray$indexedMap = _Skinney$elm_array_exploration$Native_JsArray.indexedMap;
var _Skinney$elm_array_exploration$Array_JsArray$map = _Skinney$elm_array_exploration$Native_JsArray.map;
var _Skinney$elm_array_exploration$Array_JsArray$foldr = _Skinney$elm_array_exploration$Native_JsArray.foldr;
var _Skinney$elm_array_exploration$Array_JsArray$foldl = _Skinney$elm_array_exploration$Native_JsArray.foldl;
var _Skinney$elm_array_exploration$Array_JsArray$push = _Skinney$elm_array_exploration$Native_JsArray.push;
var _Skinney$elm_array_exploration$Array_JsArray$unsafeSet = _Skinney$elm_array_exploration$Native_JsArray.unsafeSet;
var _Skinney$elm_array_exploration$Array_JsArray$unsafeGet = _Skinney$elm_array_exploration$Native_JsArray.unsafeGet;
var _Skinney$elm_array_exploration$Array_JsArray$initializeFromList = _Skinney$elm_array_exploration$Native_JsArray.initializeFromList;
var _Skinney$elm_array_exploration$Array_JsArray$initialize = _Skinney$elm_array_exploration$Native_JsArray.initialize;
var _Skinney$elm_array_exploration$Array_JsArray$length = _Skinney$elm_array_exploration$Native_JsArray.length;
var _Skinney$elm_array_exploration$Array_JsArray$singleton = _Skinney$elm_array_exploration$Native_JsArray.singleton;
var _Skinney$elm_array_exploration$Array_JsArray$empty = _Skinney$elm_array_exploration$Native_JsArray.empty;
var _Skinney$elm_array_exploration$Array_JsArray$JsArray = function (a) {
	return {ctor: 'JsArray', _0: a};
};

var _Skinney$elm_array_exploration$Array_Hamt$emptyBuilder = {
	tail: _Skinney$elm_array_exploration$Array_JsArray$empty,
	nodeList: {ctor: '[]'},
	nodeListSize: 0
};
var _Skinney$elm_array_exploration$Array_Hamt$translateIndex = F2(
	function (idx, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		var posIndex = (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? (_p2 + idx) : idx;
		return (_elm_lang$core$Native_Utils.cmp(posIndex, 0) < 0) ? 0 : ((_elm_lang$core$Native_Utils.cmp(posIndex, _p2) > 0) ? _p2 : posIndex);
	});
var _Skinney$elm_array_exploration$Array_Hamt$foldl = F3(
	function (f, init, _p3) {
		var _p4 = _p3;
		var helper = F2(
			function (i, acc) {
				var _p5 = i;
				if (_p5.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, acc, _p5._0);
				} else {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, f, acc, _p5._0);
				}
			});
		var acc = A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, init, _p4._2);
		return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, f, acc, _p4._3);
	});
var _Skinney$elm_array_exploration$Array_Hamt$foldr = F3(
	function (f, init, _p6) {
		var _p7 = _p6;
		var acc = A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, f, init, _p7._3);
		var helper = F2(
			function (i, acc) {
				var _p8 = i;
				if (_p8.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p8._0);
				} else {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, f, acc, _p8._0);
				}
			});
		return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p7._2);
	});
var _Skinney$elm_array_exploration$Array_Hamt$toIndexedList = function (_p9) {
	var _p10 = _p9;
	var helper = F2(
		function (n, _p11) {
			var _p12 = _p11;
			var _p13 = _p12._0;
			return {
				ctor: '_Tuple2',
				_0: _p13 - 1,
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _p13, _1: n},
					_1: _p12._1
				}
			};
		});
	return _elm_lang$core$Tuple$second(
		A3(
			_Skinney$elm_array_exploration$Array_Hamt$foldr,
			helper,
			{
				ctor: '_Tuple2',
				_0: _p10._0 - 1,
				_1: {ctor: '[]'}
			},
			_p10));
};
var _Skinney$elm_array_exploration$Array_Hamt$toList = function (arr) {
	return A3(
		_Skinney$elm_array_exploration$Array_Hamt$foldr,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		arr);
};
var _Skinney$elm_array_exploration$Array_Hamt$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var _Skinney$elm_array_exploration$Array_Hamt$length = function (_p14) {
	var _p15 = _p14;
	return _p15._0;
};
var _Skinney$elm_array_exploration$Array_Hamt$isEmpty = function (_p16) {
	var _p17 = _p16;
	return _elm_lang$core$Native_Utils.eq(_p17._0, 0);
};
var _Skinney$elm_array_exploration$Array_Hamt$branchFactor = 32;
var _Skinney$elm_array_exploration$Array_Hamt$shiftStep = _elm_lang$core$Basics$ceiling(
	A2(
		_elm_lang$core$Basics$logBase,
		2,
		_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor)));
var _Skinney$elm_array_exploration$Array_Hamt$bitMask = -1 >>> (32 - _Skinney$elm_array_exploration$Array_Hamt$shiftStep);
var _Skinney$elm_array_exploration$Array_Hamt$getHelp = F3(
	function (shift, idx, tree) {
		getHelp:
		while (true) {
			var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
			var _p18 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			if (_p18.ctor === 'SubTree') {
				var _v10 = shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_v11 = idx,
					_v12 = _p18._0;
				shift = _v10;
				idx = _v11;
				tree = _v12;
				continue getHelp;
			} else {
				return A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, _p18._0);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$get = F2(
	function (idx, _p19) {
		var _p20 = _p19;
		var _p21 = _p20._0;
		return ((_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(idx, _p21) > -1)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.cmp(
			idx,
			_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p21)) > -1) ? _elm_lang$core$Maybe$Just(
			A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, _p20._3)) : _elm_lang$core$Maybe$Just(
			A3(_Skinney$elm_array_exploration$Array_Hamt$getHelp, _p20._1, idx, _p20._2)));
	});
var _Skinney$elm_array_exploration$Array_Hamt$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (treeEnd >>> shift);
			var _p22 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			if (_p22.ctor === 'SubTree') {
				var _v15 = shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_v16 = end,
					_v17 = treeEnd,
					_v18 = _p22._0;
				shift = _v15;
				end = _v16;
				treeEnd = _v17;
				tree = _v18;
				continue fetchNewTail;
			} else {
				return A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, _Skinney$elm_array_exploration$Array_Hamt$bitMask & end, _p22._0);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_elm_lang$core$Native_Utils.cmp(oldShift, newShift) < 1) || _elm_lang$core$Native_Utils.eq(
				_Skinney$elm_array_exploration$Array_JsArray$length(tree),
				0)) {
				return tree;
			} else {
				var _p23 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, 0, tree);
				if (_p23.ctor === 'SubTree') {
					var _v20 = oldShift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
						_v21 = newShift,
						_v22 = _p23._0;
					oldShift = _v20;
					newShift = _v21;
					tree = _v22;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$builderFromArray = function (_p24) {
	var _p25 = _p24;
	var helper = F2(
		function (node, acc) {
			var _p26 = node;
			if (_p26.ctor === 'SubTree') {
				return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, acc, _p26._0);
			} else {
				return {ctor: '::', _0: node, _1: acc};
			}
		});
	return {
		tail: _p25._3,
		nodeList: A3(
			_Skinney$elm_array_exploration$Array_JsArray$foldl,
			helper,
			{ctor: '[]'},
			_p25._2),
		nodeListSize: (_p25._0 / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0
	};
};
var _Skinney$elm_array_exploration$Array_Hamt$Builder = F3(
	function (a, b, c) {
		return {tail: a, nodeList: b, nodeListSize: c};
	});
var _Skinney$elm_array_exploration$Array_Hamt$Array = F4(
	function (a, b, c, d) {
		return {ctor: 'Array', _0: a, _1: b, _2: c, _3: d};
	});
var _Skinney$elm_array_exploration$Array_Hamt$empty = A4(_Skinney$elm_array_exploration$Array_Hamt$Array, 0, _Skinney$elm_array_exploration$Array_Hamt$shiftStep, _Skinney$elm_array_exploration$Array_JsArray$empty, _Skinney$elm_array_exploration$Array_JsArray$empty);
var _Skinney$elm_array_exploration$Array_Hamt$Leaf = function (a) {
	return {ctor: 'Leaf', _0: a};
};
var _Skinney$elm_array_exploration$Array_Hamt$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = _Skinney$elm_array_exploration$Array_JsArray$length(tail);
		var notAppended = (_Skinney$elm_array_exploration$Array_Hamt$branchFactor - _Skinney$elm_array_exploration$Array_JsArray$length(builder.tail)) - tailLen;
		var appended = A3(_Skinney$elm_array_exploration$Array_JsArray$appendN, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, builder.tail, tail);
		return (_elm_lang$core$Native_Utils.cmp(notAppended, 0) < 0) ? {
			tail: A3(_Skinney$elm_array_exploration$Array_JsArray$slice, notAppended, tailLen, tail),
			nodeList: {
				ctor: '::',
				_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(appended),
				_1: builder.nodeList
			},
			nodeListSize: builder.nodeListSize + 1
		} : (_elm_lang$core$Native_Utils.eq(notAppended, 0) ? {
			tail: _Skinney$elm_array_exploration$Array_JsArray$empty,
			nodeList: {
				ctor: '::',
				_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(appended),
				_1: builder.nodeList
			},
			nodeListSize: builder.nodeListSize + 1
		} : {tail: appended, nodeList: builder.nodeList, nodeListSize: builder.nodeListSize});
	});
var _Skinney$elm_array_exploration$Array_Hamt$SubTree = function (a) {
	return {ctor: 'SubTree', _0: a};
};
var _Skinney$elm_array_exploration$Array_Hamt$setHelp = F4(
	function (shift, idx, val, tree) {
		var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
		var _p27 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
		if (_p27.ctor === 'SubTree') {
			var newSub = A4(_Skinney$elm_array_exploration$Array_Hamt$setHelp, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, val, _p27._0);
			return A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				pos,
				_Skinney$elm_array_exploration$Array_Hamt$SubTree(newSub),
				tree);
		} else {
			var newLeaf = A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, val, _p27._0);
			return A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				pos,
				_Skinney$elm_array_exploration$Array_Hamt$Leaf(newLeaf),
				tree);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$set = F3(
	function (idx, val, _p28) {
		var _p29 = _p28;
		var _p33 = _p29._2;
		var _p32 = _p29._3;
		var _p31 = _p29._1;
		var _p30 = _p29._0;
		return ((_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(idx, _p30) > -1)) ? _p29 : ((_elm_lang$core$Native_Utils.cmp(
			idx,
			_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p30)) > -1) ? A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p30,
			_p31,
			_p33,
			A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, val, _p32)) : A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p30,
			_p31,
			A4(_Skinney$elm_array_exploration$Array_Hamt$setHelp, _p31, idx, val, _p33),
			_p32));
	});
var _Skinney$elm_array_exploration$Array_Hamt$insertTailInTree = F4(
	function (shift, idx, tail, tree) {
		var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
		if (_elm_lang$core$Native_Utils.cmp(
			pos,
			_Skinney$elm_array_exploration$Array_JsArray$length(tree)) > -1) {
			if (_elm_lang$core$Native_Utils.eq(shift, 5)) {
				return A2(
					_Skinney$elm_array_exploration$Array_JsArray$push,
					_Skinney$elm_array_exploration$Array_Hamt$Leaf(tail),
					tree);
			} else {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(_Skinney$elm_array_exploration$Array_Hamt$insertTailInTree, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, tail, _Skinney$elm_array_exploration$Array_JsArray$empty));
				return A2(_Skinney$elm_array_exploration$Array_JsArray$push, newSub, tree);
			}
		} else {
			var val = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			var _p34 = val;
			if (_p34.ctor === 'SubTree') {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(_Skinney$elm_array_exploration$Array_Hamt$insertTailInTree, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, tail, _p34._0));
				return A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(
						_Skinney$elm_array_exploration$Array_Hamt$insertTailInTree,
						shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
						idx,
						tail,
						_Skinney$elm_array_exploration$Array_JsArray$singleton(val)));
				return A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$unsafeReplaceTail = F2(
	function (newTail, _p35) {
		var _p36 = _p35;
		var _p39 = _p36._2;
		var _p38 = _p36._1;
		var _p37 = _p36._0;
		var newTailLen = _Skinney$elm_array_exploration$Array_JsArray$length(newTail);
		var originalTailLen = _Skinney$elm_array_exploration$Array_JsArray$length(_p36._3);
		var newArrayLen = _p37 + (newTailLen - originalTailLen);
		if (_elm_lang$core$Native_Utils.eq(newTailLen, _Skinney$elm_array_exploration$Array_Hamt$branchFactor)) {
			var overflow = _elm_lang$core$Native_Utils.cmp(newArrayLen >>> _Skinney$elm_array_exploration$Array_Hamt$shiftStep, 1 << _p38) > 0;
			if (overflow) {
				var newShift = _p38 + _Skinney$elm_array_exploration$Array_Hamt$shiftStep;
				var newTree = A4(
					_Skinney$elm_array_exploration$Array_Hamt$insertTailInTree,
					newShift,
					_p37,
					newTail,
					_Skinney$elm_array_exploration$Array_JsArray$singleton(
						_Skinney$elm_array_exploration$Array_Hamt$SubTree(_p39)));
				return A4(_Skinney$elm_array_exploration$Array_Hamt$Array, newArrayLen, newShift, newTree, _Skinney$elm_array_exploration$Array_JsArray$empty);
			} else {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					newArrayLen,
					_p38,
					A4(_Skinney$elm_array_exploration$Array_Hamt$insertTailInTree, _p38, _p37, newTail, _p39),
					_Skinney$elm_array_exploration$Array_JsArray$empty);
			}
		} else {
			return A4(_Skinney$elm_array_exploration$Array_Hamt$Array, newArrayLen, _p38, _p39, newTail);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$push = F2(
	function (a, _p40) {
		var _p41 = _p40;
		return A2(
			_Skinney$elm_array_exploration$Array_Hamt$unsafeReplaceTail,
			A2(_Skinney$elm_array_exploration$Array_JsArray$push, a, _p41._3),
			_p41);
	});
var _Skinney$elm_array_exploration$Array_Hamt$appendHelpTree = F2(
	function (toAppend, _p42) {
		var _p43 = _p42;
		var _p44 = _p43._3;
		var itemsToAppend = _Skinney$elm_array_exploration$Array_JsArray$length(toAppend);
		var notAppended = (_Skinney$elm_array_exploration$Array_Hamt$branchFactor - _Skinney$elm_array_exploration$Array_JsArray$length(_p44)) - itemsToAppend;
		var appended = A3(_Skinney$elm_array_exploration$Array_JsArray$appendN, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, _p44, toAppend);
		var newArray = A2(_Skinney$elm_array_exploration$Array_Hamt$unsafeReplaceTail, appended, _p43);
		if (_elm_lang$core$Native_Utils.cmp(notAppended, 0) < 0) {
			var nextTail = A3(_Skinney$elm_array_exploration$Array_JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2(_Skinney$elm_array_exploration$Array_Hamt$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$map = F2(
	function (f, _p45) {
		var _p46 = _p45;
		var helper = function (i) {
			var _p47 = i;
			if (_p47.ctor === 'SubTree') {
				return _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A2(_Skinney$elm_array_exploration$Array_JsArray$map, helper, _p47._0));
			} else {
				return _Skinney$elm_array_exploration$Array_Hamt$Leaf(
					A2(_Skinney$elm_array_exploration$Array_JsArray$map, f, _p47._0));
			}
		};
		return A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p46._0,
			_p46._1,
			A2(_Skinney$elm_array_exploration$Array_JsArray$map, helper, _p46._2),
			A2(_Skinney$elm_array_exploration$Array_JsArray$map, f, _p46._3));
	});
var _Skinney$elm_array_exploration$Array_Hamt$toString = function (array) {
	var content = A2(
		_elm_lang$core$String$join,
		',',
		_Skinney$elm_array_exploration$Array_Hamt$toList(
			A2(_Skinney$elm_array_exploration$Array_Hamt$map, _elm_lang$core$Basics$toString, array)));
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'Array [',
		A2(_elm_lang$core$Basics_ops['++'], content, ']'));
};
var _Skinney$elm_array_exploration$Array_Hamt$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (endIdx >>> shift);
		var _p48 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, lastPos, tree);
		if (_p48.ctor === 'SubTree') {
			var newSub = A3(_Skinney$elm_array_exploration$Array_Hamt$sliceTree, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, endIdx, _p48._0);
			return _elm_lang$core$Native_Utils.eq(
				_Skinney$elm_array_exploration$Array_JsArray$length(newSub),
				0) ? A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos, tree) : A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				lastPos,
				_Skinney$elm_array_exploration$Array_Hamt$SubTree(newSub),
				A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos, tree);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$sliceRight = F2(
	function (end, _p49) {
		var _p50 = _p49;
		var _p53 = _p50._2;
		var _p52 = _p50._1;
		var _p51 = _p50._0;
		if (_elm_lang$core$Native_Utils.eq(end, _p51)) {
			return _p50;
		} else {
			if (_elm_lang$core$Native_Utils.cmp(
				end,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p51)) > -1) {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					end,
					_p52,
					_p53,
					A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, _Skinney$elm_array_exploration$Array_Hamt$bitMask & end, _p50._3));
			} else {
				var endIdx = _Skinney$elm_array_exploration$Array_Hamt$tailIndex(end);
				var depth = _elm_lang$core$Basics$floor(
					A2(
						_elm_lang$core$Basics$logBase,
						_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor),
						_elm_lang$core$Basics$toFloat(
							A2(_elm_lang$core$Basics$max, 1, endIdx - 1))));
				var newShift = A2(_elm_lang$core$Basics$max, 5, depth * _Skinney$elm_array_exploration$Array_Hamt$shiftStep);
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					end,
					newShift,
					A3(
						_Skinney$elm_array_exploration$Array_Hamt$hoistTree,
						_p52,
						newShift,
						A3(_Skinney$elm_array_exploration$Array_Hamt$sliceTree, _p52, endIdx, _p53)),
					A4(_Skinney$elm_array_exploration$Array_Hamt$fetchNewTail, _p52, end, endIdx, _p53));
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _p54 = A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, nodes);
			var node = _p54._0;
			var remainingNodes = _p54._1;
			var newAcc = {
				ctor: '::',
				_0: _Skinney$elm_array_exploration$Array_Hamt$SubTree(node),
				_1: acc
			};
			var _p55 = remainingNodes;
			if (_p55.ctor === '[]') {
				return _elm_lang$core$List$reverse(newAcc);
			} else {
				var _v36 = remainingNodes,
					_v37 = newAcc;
				nodes = _v36;
				acc = _v37;
				continue compressNodes;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = _elm_lang$core$Basics$ceiling(
				_elm_lang$core$Basics$toFloat(nodeListSize) / _elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor));
			if (_elm_lang$core$Native_Utils.eq(newNodeSize, 1)) {
				return _elm_lang$core$Tuple$first(
					A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, nodeList));
			} else {
				var _v38 = A2(
					_Skinney$elm_array_exploration$Array_Hamt$compressNodes,
					nodeList,
					{ctor: '[]'}),
					_v39 = newNodeSize;
				nodeList = _v38;
				nodeListSize = _v39;
				continue treeFromBuilder;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (_elm_lang$core$Native_Utils.eq(builder.nodeListSize, 0)) {
			return A4(
				_Skinney$elm_array_exploration$Array_Hamt$Array,
				_Skinney$elm_array_exploration$Array_JsArray$length(builder.tail),
				_Skinney$elm_array_exploration$Array_Hamt$shiftStep,
				_Skinney$elm_array_exploration$Array_JsArray$empty,
				builder.tail);
		} else {
			var correctNodeList = reverseNodeList ? _elm_lang$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(_Skinney$elm_array_exploration$Array_Hamt$treeFromBuilder, correctNodeList, builder.nodeListSize);
			var treeLen = builder.nodeListSize * _Skinney$elm_array_exploration$Array_Hamt$branchFactor;
			var depth = _elm_lang$core$Basics$floor(
				A2(
					_elm_lang$core$Basics$logBase,
					_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor),
					_elm_lang$core$Basics$toFloat(treeLen - 1)));
			return A4(
				_Skinney$elm_array_exploration$Array_Hamt$Array,
				_Skinney$elm_array_exploration$Array_JsArray$length(builder.tail) + treeLen,
				A2(_elm_lang$core$Basics$max, 5, depth * _Skinney$elm_array_exploration$Array_Hamt$shiftStep),
				tree,
				builder.tail);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$initializeHelp = F5(
	function (fn, fromIndex, length, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(fromIndex, 0) < 0) {
				return A2(
					_Skinney$elm_array_exploration$Array_Hamt$builderToArray,
					false,
					{tail: tail, nodeList: nodeList, nodeListSize: (length / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0});
			} else {
				var leaf = _Skinney$elm_array_exploration$Array_Hamt$Leaf(
					A3(_Skinney$elm_array_exploration$Array_JsArray$initialize, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, fromIndex, fn));
				var _v40 = fn,
					_v41 = fromIndex - _Skinney$elm_array_exploration$Array_Hamt$branchFactor,
					_v42 = length,
					_v43 = {ctor: '::', _0: leaf, _1: nodeList},
					_v44 = tail;
				fn = _v40;
				fromIndex = _v41;
				length = _v42;
				nodeList = _v43;
				tail = _v44;
				continue initializeHelp;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$initialize = F2(
	function (length, fn) {
		if (_elm_lang$core$Native_Utils.cmp(length, 0) < 1) {
			return _Skinney$elm_array_exploration$Array_Hamt$empty;
		} else {
			var tailLen = A2(_elm_lang$core$Basics$rem, length, _Skinney$elm_array_exploration$Array_Hamt$branchFactor);
			var tail = A3(_Skinney$elm_array_exploration$Array_JsArray$initialize, tailLen, length - tailLen, fn);
			var initialFromIndex = (length - tailLen) - _Skinney$elm_array_exploration$Array_Hamt$branchFactor;
			return A5(
				_Skinney$elm_array_exploration$Array_Hamt$initializeHelp,
				fn,
				initialFromIndex,
				length,
				{ctor: '[]'},
				tail);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$repeat = F2(
	function (n, e) {
		return A2(
			_Skinney$elm_array_exploration$Array_Hamt$initialize,
			n,
			function (_p56) {
				return e;
			});
	});
var _Skinney$elm_array_exploration$Array_Hamt$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _p57 = A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, list);
			var jsArray = _p57._0;
			var remainingItems = _p57._1;
			if (_elm_lang$core$Native_Utils.cmp(
				_Skinney$elm_array_exploration$Array_JsArray$length(jsArray),
				_Skinney$elm_array_exploration$Array_Hamt$branchFactor) < 0) {
				return A2(
					_Skinney$elm_array_exploration$Array_Hamt$builderToArray,
					true,
					{tail: jsArray, nodeList: nodeList, nodeListSize: nodeListSize});
			} else {
				var _v45 = remainingItems,
					_v46 = {
					ctor: '::',
					_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(jsArray),
					_1: nodeList
				},
					_v47 = nodeListSize + 1;
				list = _v45;
				nodeList = _v46;
				nodeListSize = _v47;
				continue fromListHelp;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$fromList = function (ls) {
	var _p58 = ls;
	if (_p58.ctor === '[]') {
		return _Skinney$elm_array_exploration$Array_Hamt$empty;
	} else {
		return A3(
			_Skinney$elm_array_exploration$Array_Hamt$fromListHelp,
			ls,
			{ctor: '[]'},
			0);
	}
};
var _Skinney$elm_array_exploration$Array_Hamt$filter = F2(
	function (f, arr) {
		var helper = F2(
			function (n, acc) {
				return f(n) ? {ctor: '::', _0: n, _1: acc} : acc;
			});
		return _Skinney$elm_array_exploration$Array_Hamt$fromList(
			A3(
				_Skinney$elm_array_exploration$Array_Hamt$foldr,
				helper,
				{ctor: '[]'},
				arr));
	});
var _Skinney$elm_array_exploration$Array_Hamt$indexedMap = F2(
	function (f, _p59) {
		var _p60 = _p59;
		var initialBuilder = {
			tail: A3(
				_Skinney$elm_array_exploration$Array_JsArray$indexedMap,
				f,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p60._0),
				_p60._3),
			nodeList: {ctor: '[]'},
			nodeListSize: 0
		};
		var helper = F2(
			function (node, builder) {
				var _p61 = node;
				if (_p61.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, builder, _p61._0);
				} else {
					var offset = builder.nodeListSize * _Skinney$elm_array_exploration$Array_Hamt$branchFactor;
					var mappedLeaf = _Skinney$elm_array_exploration$Array_Hamt$Leaf(
						A3(_Skinney$elm_array_exploration$Array_JsArray$indexedMap, f, offset, _p61._0));
					return {
						tail: builder.tail,
						nodeList: {ctor: '::', _0: mappedLeaf, _1: builder.nodeList},
						nodeListSize: builder.nodeListSize + 1
					};
				}
			});
		return A2(
			_Skinney$elm_array_exploration$Array_Hamt$builderToArray,
			true,
			A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, initialBuilder, _p60._2));
	});
var _Skinney$elm_array_exploration$Array_Hamt$append = F2(
	function (_p63, _p62) {
		var _p64 = _p63;
		var _p70 = _p64;
		var _p65 = _p62;
		var _p69 = _p65._2;
		var _p68 = _p65._3;
		if (_elm_lang$core$Native_Utils.cmp(_p65._0, _Skinney$elm_array_exploration$Array_Hamt$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, arr) {
					var _p66 = node;
					if (_p66.ctor === 'SubTree') {
						return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, arr, _p66._0);
					} else {
						return A2(_Skinney$elm_array_exploration$Array_Hamt$appendHelpTree, _p66._0, arr);
					}
				});
			return A2(
				_Skinney$elm_array_exploration$Array_Hamt$appendHelpTree,
				_p68,
				A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, _p70, _p69));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					var _p67 = node;
					if (_p67.ctor === 'SubTree') {
						return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, builder, _p67._0);
					} else {
						return A2(_Skinney$elm_array_exploration$Array_Hamt$appendHelpBuilder, _p67._0, builder);
					}
				});
			var builder = _Skinney$elm_array_exploration$Array_Hamt$builderFromArray(_p70);
			return A2(
				_Skinney$elm_array_exploration$Array_Hamt$builderToArray,
				true,
				A2(
					_Skinney$elm_array_exploration$Array_Hamt$appendHelpBuilder,
					_p68,
					A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, builder, _p69)));
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$sliceLeft = F2(
	function (from, _p71) {
		var _p72 = _p71;
		var _p77 = _p72._3;
		var _p76 = _p72._0;
		if (_elm_lang$core$Native_Utils.eq(from, 0)) {
			return _p72;
		} else {
			if (_elm_lang$core$Native_Utils.cmp(
				from,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p76)) > -1) {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					_p76 - from,
					_Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_Skinney$elm_array_exploration$Array_JsArray$empty,
					A3(
						_Skinney$elm_array_exploration$Array_JsArray$slice,
						from - _Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p76),
						_Skinney$elm_array_exploration$Array_JsArray$length(_p77),
						_p77));
			} else {
				var skipNodes = (from / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						var _p73 = node;
						if (_p73.ctor === 'SubTree') {
							return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p73._0);
						} else {
							return {ctor: '::', _0: _p73._0, _1: acc};
						}
					});
				var leafNodes = A3(
					_Skinney$elm_array_exploration$Array_JsArray$foldr,
					helper,
					{
						ctor: '::',
						_0: _p77,
						_1: {ctor: '[]'}
					},
					_p72._2);
				var nodesToInsert = A2(_elm_lang$core$List$drop, skipNodes, leafNodes);
				var _p74 = nodesToInsert;
				if (_p74.ctor === '[]') {
					return _Skinney$elm_array_exploration$Array_Hamt$empty;
				} else {
					var _p75 = _p74._0;
					var firstSlice = from - (skipNodes * _Skinney$elm_array_exploration$Array_Hamt$branchFactor);
					var initialBuilder = {
						tail: A3(
							_Skinney$elm_array_exploration$Array_JsArray$slice,
							firstSlice,
							_Skinney$elm_array_exploration$Array_JsArray$length(_p75),
							_p75),
						nodeList: {ctor: '[]'},
						nodeListSize: 0
					};
					return A2(
						_Skinney$elm_array_exploration$Array_Hamt$builderToArray,
						true,
						A3(_elm_lang$core$List$foldl, _Skinney$elm_array_exploration$Array_Hamt$appendHelpBuilder, initialBuilder, _p74._1));
				}
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$slice = F3(
	function (from, to, arr) {
		var correctTo = A2(_Skinney$elm_array_exploration$Array_Hamt$translateIndex, to, arr);
		var correctFrom = A2(_Skinney$elm_array_exploration$Array_Hamt$translateIndex, from, arr);
		return (_elm_lang$core$Native_Utils.cmp(correctFrom, correctTo) > 0) ? _Skinney$elm_array_exploration$Array_Hamt$empty : A2(
			_Skinney$elm_array_exploration$Array_Hamt$sliceLeft,
			correctFrom,
			A2(_Skinney$elm_array_exploration$Array_Hamt$sliceRight, correctTo, arr));
	});

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, list) {
		isSubsequenceOf:
		while (true) {
			var _p0 = {ctor: '_Tuple2', _0: subseq, _1: list};
			if (_p0._0.ctor === '[]') {
				return true;
			} else {
				if (_p0._1.ctor === '[]') {
					return false;
				} else {
					var _p1 = _p0._1._1;
					if (_elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0)) {
						var _v1 = _p0._0._1,
							_v2 = _p1;
						subseq = _v1;
						list = _v2;
						continue isSubsequenceOf;
					} else {
						var _v3 = subseq,
							_v4 = _p1;
						subseq = _v3;
						list = _v4;
						continue isSubsequenceOf;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p2 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p2._0.ctor === '[]') {
			return true;
		} else {
			if (_p2._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p2._0._0, _p2._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p2._0._1, _p2._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$isInfixOfHelp = F3(
	function (infixHead, infixTail, list) {
		isInfixOfHelp:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return false;
			} else {
				var _p4 = _p3._1;
				if (_elm_lang$core$Native_Utils.eq(_p3._0, infixHead)) {
					return A2(_elm_community$list_extra$List_Extra$isPrefixOf, infixTail, _p4);
				} else {
					var _v7 = infixHead,
						_v8 = infixTail,
						_v9 = _p4;
					infixHead = _v7;
					infixTail = _v8;
					list = _v9;
					continue isInfixOfHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infixList, list) {
		var _p5 = infixList;
		if (_p5.ctor === '[]') {
			return true;
		} else {
			return A3(_elm_community$list_extra$List_Extra$isInfixOfHelp, _p5._0, _p5._1, list);
		}
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p9,
				_2: _p10
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p9, _1: _p8._0},
						_1: _p8._1,
						_2: _p8._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p11 = xs;
	if (_p11.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p15 = _p11._1;
		var _p14 = _p11._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p14, _1: _p15},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p12) {
					var _p13 = _p12;
					return {
						ctor: '_Tuple2',
						_0: _p13._0,
						_1: {ctor: '::', _0: _p14, _1: _p13._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p15))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p16 = list;
		if (_p16.ctor === '::') {
			var _p17 = _p16._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p17},
				_1: {ctor: '::', _0: _p17, _1: _p16._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitivelyHelp = F4(
	function (result, currentGroup, compare, list) {
		groupWhileTransitivelyHelp:
		while (true) {
			var _p18 = list;
			if (_p18.ctor === '[]') {
				return _elm_lang$core$List$reverse(
					_elm_lang$core$List$isEmpty(currentGroup) ? result : _elm_lang$core$List$reverse(
						{ctor: '::', _0: currentGroup, _1: result}));
			} else {
				if (_p18._1.ctor === '[]') {
					return _elm_lang$core$List$reverse(
						{
							ctor: '::',
							_0: _elm_lang$core$List$reverse(
								{ctor: '::', _0: _p18._0, _1: currentGroup}),
							_1: result
						});
				} else {
					var _p20 = _p18._1;
					var _p19 = _p18._0;
					if (A2(compare, _p19, _p18._1._0)) {
						var _v17 = result,
							_v18 = {ctor: '::', _0: _p19, _1: currentGroup},
							_v19 = compare,
							_v20 = _p20;
						result = _v17;
						currentGroup = _v18;
						compare = _v19;
						list = _v20;
						continue groupWhileTransitivelyHelp;
					} else {
						var _v21 = {
							ctor: '::',
							_0: _elm_lang$core$List$reverse(
								{ctor: '::', _0: _p19, _1: currentGroup}),
							_1: result
						},
							_v22 = {ctor: '[]'},
							_v23 = compare,
							_v24 = _p20;
						result = _v21;
						currentGroup = _v22;
						compare = _v23;
						list = _v24;
						continue groupWhileTransitivelyHelp;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (compare, list) {
		return A4(
			_elm_community$list_extra$List_Extra$groupWhileTransitivelyHelp,
			{ctor: '[]'},
			{ctor: '[]'},
			compare,
			list);
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p21 = m;
				if (_p21.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p21._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p21._0._0) ? _elm_lang$core$Maybe$Just(_p21._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p22) {
			var _p23 = _p22;
			var _p24 = _p23._0;
			return (p(x) && _p23._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p24},
				_1: true
			} : {ctor: '_Tuple2', _0: _p24, _1: false};
		});
	return function (_p25) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p25));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p26 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p26.ctor === '_Tuple2') && (_p26._0.ctor === '::')) && (_p26._1.ctor === '::')) {
				var _p27 = A2(_elm_community$list_extra$List_Extra$splitAt, _p26._0._0, list);
				var head = _p27._0;
				var tail = _p27._1;
				var _v28 = _p26._0._1,
					_v29 = tail,
					_v30 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v28;
				list = _v29;
				accu = _v30;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p28 = f(seed);
		if (_p28.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p28._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p28._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$mapAccumr = F3(
	function (f, acc0, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, _p29) {
					var _p30 = _p29;
					var _p31 = A2(f, _p30._0, x);
					var acc2 = _p31._0;
					var y = _p31._1;
					return {
						ctor: '_Tuple2',
						_0: acc2,
						_1: {ctor: '::', _0: y, _1: _p30._1}
					};
				}),
			{
				ctor: '_Tuple2',
				_0: acc0,
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_community$list_extra$List_Extra$mapAccuml = F3(
	function (f, acc0, list) {
		var _p32 = A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, _p33) {
					var _p34 = _p33;
					var _p35 = A2(f, _p34._0, x);
					var acc2 = _p35._0;
					var y = _p35._1;
					return {
						ctor: '_Tuple2',
						_0: acc2,
						_1: {ctor: '::', _0: y, _1: _p34._1}
					};
				}),
			{
				ctor: '_Tuple2',
				_0: acc0,
				_1: {ctor: '[]'}
			},
			list);
		var accFinal = _p32._0;
		var generatedList = _p32._1;
		return {
			ctor: '_Tuple2',
			_0: accFinal,
			_1: _elm_lang$core$List$reverse(generatedList)
		};
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p36 = xs_;
		if (_p36.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p36._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p36._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p37 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p36._1);
				if (_p37.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p36._0, _p37._0),
						_1: _p37
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p38 = xs_;
		if (_p38.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p39 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p38._1);
			if (_p39.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p38._0, _p39._0),
					_1: _p39
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p40 = xs_;
		if (_p40.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p40._0, _p40._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p41) {
				var _p42 = _p41;
				var _p43 = _p42._0;
				return {
					ctor: '_Tuple2',
					_0: _p43 - 1,
					_1: A3(func, _p43, x, _p42._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p44) {
				var _p45 = _p44;
				var _p46 = _p45._0;
				return {
					ctor: '_Tuple2',
					_0: _p46 + 1,
					_1: A3(func, _p46, x, _p45._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p47 = m;
						if (_p47.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p47._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p48 = m;
						if (_p48.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p48._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$reverseAppend = F2(
	function (list1, list2) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			list2,
			list1);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (acc, list1, list2) {
		interweaveHelp:
		while (true) {
			var _p49 = {ctor: '_Tuple2', _0: list1, _1: list2};
			if (_p49._0.ctor === '::') {
				if (_p49._1.ctor === '::') {
					var _v44 = {
						ctor: '::',
						_0: _p49._1._0,
						_1: {ctor: '::', _0: _p49._0._0, _1: acc}
					},
						_v45 = _p49._0._1,
						_v46 = _p49._1._1;
					acc = _v44;
					list1 = _v45;
					list2 = _v46;
					continue interweaveHelp;
				} else {
					return A2(_elm_community$list_extra$List_Extra$reverseAppend, acc, list1);
				}
			} else {
				return A2(_elm_community$list_extra$List_Extra$reverseAppend, acc, list2);
			}
		}
	});
var _elm_community$list_extra$List_Extra$interweave = _elm_community$list_extra$List_Extra$interweaveHelp(
	{ctor: '[]'});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p50 = xs_;
	if (_p50.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p51) {
			var _p52 = _p51;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p52._0),
				_elm_community$list_extra$List_Extra$permutations(_p52._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p50));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p53 = xs;
	if (_p53.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p54 = _p53._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p54, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p54,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p53._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$rowsLength = function (listOfLists) {
	var _p55 = listOfLists;
	if (_p55.ctor === '[]') {
		return 0;
	} else {
		return _elm_lang$core$List$length(_p55._0);
	}
};
var _elm_community$list_extra$List_Extra$transpose = function (listOfLists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$map2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})),
		A2(
			_elm_lang$core$List$repeat,
			_elm_community$list_extra$List_Extra$rowsLength(listOfLists),
			{ctor: '[]'}),
		listOfLists);
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p56) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p56));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p57) {
				return !pred(_p57);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeIfIndex = function (predicate) {
	return A2(
		_elm_community$list_extra$List_Extra$indexedFoldr,
		F3(
			function (index, item, acc) {
				return predicate(index) ? acc : {ctor: '::', _0: item, _1: acc};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p58 = tail;
			if (_p58.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p58._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p60, _p59) {
				var _p61 = _p60;
				var _p62 = _p59;
				var result = A2(pred, _p61._0, _p62._0);
				var _p63 = result;
				if (_p63.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p61._1, _p62._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p64 = xs;
		if (_p64.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p66 = _p64._1;
			var _p65 = _p64._0;
			return _elm_lang$core$Native_Utils.eq(x, _p65) ? _p66 : {
				ctor: '::',
				_0: _p65,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p66)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, fn, list) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return list;
		} else {
			var tail = A2(_elm_lang$core$List$drop, index, list);
			var head = A2(_elm_lang$core$List$take, index, list);
			var _p67 = tail;
			if (_p67.ctor === '::') {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					head,
					{
						ctor: '::',
						_0: fn(_p67._0),
						_1: _p67._1
					});
			} else {
				return list;
			}
		}
	});
var _elm_community$list_extra$List_Extra$setAt = F2(
	function (index, value) {
		return A2(
			_elm_community$list_extra$List_Extra$updateAt,
			index,
			_elm_lang$core$Basics$always(value));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$count = function (predicate) {
	return A2(
		_elm_lang$core$List$foldl,
		F2(
			function (x, acc) {
				return predicate(x) ? (acc + 1) : acc;
			}),
		0);
};
var _elm_community$list_extra$List_Extra$findIndices = function (predicate) {
	var consIndexIf = F3(
		function (index, x, acc) {
			return predicate(x) ? {ctor: '::', _0: index, _1: acc} : acc;
		});
	return A2(
		_elm_community$list_extra$List_Extra$indexedFoldr,
		consIndexIf,
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			var _p68 = list;
			if (_p68.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				if (predicate(_p68._0)) {
					return _elm_lang$core$Maybe$Just(index);
				} else {
					var _v58 = index + 1,
						_v59 = predicate,
						_v60 = _p68._1;
					index = _v58;
					predicate = _v59;
					list = _v60;
					continue findIndexHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$findIndex = _elm_community$list_extra$List_Extra$findIndexHelp(0);
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p69 = list;
			if (_p69.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p70 = _p69._0;
				if (predicate(_p70)) {
					return _elm_lang$core$Maybe$Just(_p70);
				} else {
					var _v62 = predicate,
						_v63 = _p69._1;
					predicate = _v62;
					list = _v63;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p71) {
		return !A2(_elm_lang$core$List$member, x, _p71);
	};
};
var _elm_community$list_extra$List_Extra$reverseMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$cartesianProduct = function (ll) {
	var _p72 = ll;
	if (_p72.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		return A3(
			_elm_community$list_extra$List_Extra$lift2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p72._0,
			_elm_community$list_extra$List_Extra$cartesianProduct(_p72._1));
	}
};
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			var _p73 = remaining;
			if (_p73.ctor === '[]') {
				return _elm_lang$core$List$reverse(accumulator);
			} else {
				var _p75 = _p73._1;
				var _p74 = _p73._0;
				var computedFirst = f(_p74);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v66 = f,
						_v67 = existing,
						_v68 = _p75,
						_v69 = accumulator;
					f = _v66;
					existing = _v67;
					remaining = _v68;
					accumulator = _v69;
					continue uniqueHelp;
				} else {
					var _v70 = f,
						_v71 = A2(_elm_lang$core$Set$insert, computedFirst, existing),
						_v72 = _p75,
						_v73 = {ctor: '::', _0: _p74, _1: accumulator};
					f = _v70;
					existing = _v71;
					remaining = _v72;
					accumulator = _v73;
					continue uniqueHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A4(
			_elm_community$list_extra$List_Extra$uniqueHelp,
			f,
			_elm_lang$core$Set$empty,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A4(
		_elm_community$list_extra$List_Extra$uniqueHelp,
		_elm_lang$core$Basics$identity,
		_elm_lang$core$Set$empty,
		list,
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p76 = list;
			if (_p76.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p76._0)) {
					var _v75 = predicate,
						_v76 = _p76._1;
					predicate = _v75;
					list = _v76;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p77 = list;
				if (_p77.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p78 = _p77._0;
					if (predicate(_p78)) {
						var _v78 = {ctor: '::', _0: _p78, _1: memo},
							_v79 = _p77._1;
						memo = _v78;
						list = _v79;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p79) {
			return !p(_p79);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p80 = xs_;
		if (_p80.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p82 = _p80._0;
			var _p81 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p82),
				_p80._1);
			var ys = _p81._0;
			var zs = _p81._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p82, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p83) {
				var _p84 = _p83;
				var _p85 = _p84._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p85) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p84._0, _1: _p85};
			});
		var _p86 = ls;
		if (_p86.ctor === '::') {
			if (_p86._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p86._0);
			} else {
				var _p87 = _p86._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p87,
								_1: f(_p87)
							},
							_p86._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p88) {
				var _p89 = _p88;
				var _p90 = _p89._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p90) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p89._0, _1: _p90};
			});
		var _p91 = ls;
		if (_p91.ctor === '::') {
			if (_p91._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p91._0);
			} else {
				var _p92 = _p91._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p92,
								_1: f(_p92)
							},
							_p91._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p93 = xs;
	if (_p93.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p93._0, _1: _p93._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2) || (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0)) {
				return l;
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v86 = index2,
						_v87 = index1,
						_v88 = l;
					index1 = _v86;
					index2 = _v87;
					l = _v88;
					continue swapAt;
				} else {
					var _p94 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
					var part1 = _p94._0;
					var tail1 = _p94._1;
					var _p95 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
					var head2 = _p95._0;
					var tail2 = _p95._1;
					var _p96 = {
						ctor: '_Tuple2',
						_0: _elm_community$list_extra$List_Extra$uncons(head2),
						_1: _elm_community$list_extra$List_Extra$uncons(tail2)
					};
					if (((((_p96.ctor === '_Tuple2') && (_p96._0.ctor === 'Just')) && (_p96._0._0.ctor === '_Tuple2')) && (_p96._1.ctor === 'Just')) && (_p96._1._0.ctor === '_Tuple2')) {
						return _elm_lang$core$List$concat(
							{
								ctor: '::',
								_0: part1,
								_1: {
									ctor: '::',
									_0: {ctor: '::', _0: _p96._1._0._0, _1: _p96._0._0._1},
									_1: {
										ctor: '::',
										_0: {ctor: '::', _0: _p96._0._0._0, _1: _p96._1._0._1},
										_1: {ctor: '[]'}
									}
								}
							});
					} else {
						return l;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$cycleHelp = F3(
	function (acc, n, list) {
		cycleHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) > 0) {
				var _v90 = A2(_elm_community$list_extra$List_Extra$reverseAppend, list, acc),
					_v91 = n - 1,
					_v92 = list;
				acc = _v90;
				n = _v91;
				list = _v92;
				continue cycleHelp;
			} else {
				return acc;
			}
		}
	});
var _elm_community$list_extra$List_Extra$cycle = F2(
	function (len, list) {
		var cycleLength = _elm_lang$core$List$length(list);
		return (_elm_lang$core$Native_Utils.eq(cycleLength, 0) || _elm_lang$core$Native_Utils.eq(cycleLength, len)) ? list : ((_elm_lang$core$Native_Utils.cmp(cycleLength, len) < 0) ? _elm_lang$core$List$reverse(
			A2(
				_elm_community$list_extra$List_Extra$reverseAppend,
				A2(
					_elm_lang$core$List$take,
					A2(_elm_lang$core$Basics$rem, len, cycleLength),
					list),
				A3(
					_elm_community$list_extra$List_Extra$cycleHelp,
					{ctor: '[]'},
					(len / cycleLength) | 0,
					list))) : A2(_elm_lang$core$List$take, len, list));
	});
var _elm_community$list_extra$List_Extra$initialize = F2(
	function (n, f) {
		var step = F2(
			function (i, acc) {
				step:
				while (true) {
					if (_elm_lang$core$Native_Utils.cmp(i, 0) < 0) {
						return acc;
					} else {
						var _v93 = i - 1,
							_v94 = {
							ctor: '::',
							_0: f(i),
							_1: acc
						};
						i = _v93;
						acc = _v94;
						continue step;
					}
				}
			});
		return A2(
			step,
			n - 1,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p97 = f(x);
		if (_p97.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p97._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function (items) {
	var _p98 = items;
	if (_p98.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$List$reverse,
			_elm_lang$core$List$tail(
				_elm_lang$core$List$reverse(_p98)));
	}
};
var _elm_community$list_extra$List_Extra$last = function (items) {
	last:
	while (true) {
		var _p99 = items;
		if (_p99.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			if (_p99._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p99._0);
			} else {
				var _v98 = _p99._1;
				items = _v98;
				continue last;
			}
		}
	}
};

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_community$string_extra$String_Extra$accentRegex = function () {
	var matches = {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: '[à-æ]', _1: 'a'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: '[À-Æ]', _1: 'A'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ç', _1: 'c'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Ç', _1: 'C'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '[è-ë]', _1: 'e'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: '[È-Ë]', _1: 'E'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: '[ì-ï]', _1: 'i'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: '[Ì-Ï]', _1: 'I'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'ñ', _1: 'n'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Ñ', _1: 'N'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: '[ò-ö]', _1: 'o'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: '[Ò-Ö]', _1: 'O'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: '[ù-ü]', _1: 'u'},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: '[Ù-Ü]', _1: 'U'},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'ý', _1: 'y'},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'ÿ', _1: 'y'},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Ý', _1: 'Y'},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	};
	return A2(
		_elm_lang$core$List$map,
		function (_p0) {
			var _p1 = _p0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Regex$regex(_p1._0),
				_1: _p1._1
			};
		},
		matches);
}();
var _elm_community$string_extra$String_Extra$removeAccents = function (string) {
	if (_elm_lang$core$String$isEmpty(string)) {
		return string;
	} else {
		var do_regex_to_remove_acents = function (_p2) {
			var _p3 = _p2;
			return A3(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_p3._0,
				function (_p4) {
					return _p3._1;
				});
		};
		return A3(_elm_lang$core$List$foldl, do_regex_to_remove_acents, string, _elm_community$string_extra$String_Extra$accentRegex);
	}
};
var _elm_community$string_extra$String_Extra$nonEmpty = function (string) {
	return _elm_lang$core$String$isEmpty(string) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(string);
};
var _elm_community$string_extra$String_Extra$replacementCodePoint = 65533;
var _elm_community$string_extra$String_Extra$toCodePoints = function (string) {
	var allCodeUnits = A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Char$toCode,
		_elm_lang$core$String$toList(string));
	var combineAndReverse = F2(
		function (codeUnits, accumulated) {
			combineAndReverse:
			while (true) {
				var _p5 = codeUnits;
				if (_p5.ctor === '[]') {
					return accumulated;
				} else {
					var _p9 = _p5._0;
					var _p8 = _p5._1;
					if ((_elm_lang$core$Native_Utils.cmp(_p9, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 55295) < 1)) {
						var _v3 = _p8,
							_v4 = {ctor: '::', _0: _p9, _1: accumulated};
						codeUnits = _v3;
						accumulated = _v4;
						continue combineAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p9, 55296) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 56319) < 1)) {
							var _p6 = _p8;
							if (_p6.ctor === '[]') {
								return {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
							} else {
								var _p7 = _p6._0;
								if ((_elm_lang$core$Native_Utils.cmp(_p7, 56320) > -1) && (_elm_lang$core$Native_Utils.cmp(_p7, 57343) < 1)) {
									var codePoint = (65536 + ((_p9 - 55296) * 1024)) + (_p7 - 56320);
									var _v6 = _p6._1,
										_v7 = {ctor: '::', _0: codePoint, _1: accumulated};
									codeUnits = _v6;
									accumulated = _v7;
									continue combineAndReverse;
								} else {
									var _v8 = _p8,
										_v9 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
									codeUnits = _v8;
									accumulated = _v9;
									continue combineAndReverse;
								}
							}
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p9, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 65535) < 1)) {
								var _v10 = _p8,
									_v11 = {ctor: '::', _0: _p9, _1: accumulated};
								codeUnits = _v10;
								accumulated = _v11;
								continue combineAndReverse;
							} else {
								var _v12 = _p8,
									_v13 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codeUnits = _v12;
								accumulated = _v13;
								continue combineAndReverse;
							}
						}
					}
				}
			}
		});
	return _elm_lang$core$List$reverse(
		A2(
			combineAndReverse,
			allCodeUnits,
			{ctor: '[]'}));
};
var _elm_community$string_extra$String_Extra$fromCodePoints = function (allCodePoints) {
	var splitAndReverse = F2(
		function (codePoints, accumulated) {
			splitAndReverse:
			while (true) {
				var _p10 = codePoints;
				if (_p10.ctor === '[]') {
					return accumulated;
				} else {
					var _p12 = _p10._1;
					var _p11 = _p10._0;
					if ((_elm_lang$core$Native_Utils.cmp(_p11, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 55295) < 1)) {
						var _v15 = _p12,
							_v16 = {ctor: '::', _0: _p11, _1: accumulated};
						codePoints = _v15;
						accumulated = _v16;
						continue splitAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p11, 65536) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 1114111) < 1)) {
							var subtracted = _p11 - 65536;
							var leading = (subtracted >> 10) + 55296;
							var trailing = (subtracted & 1023) + 56320;
							var _v17 = _p12,
								_v18 = {
								ctor: '::',
								_0: trailing,
								_1: {ctor: '::', _0: leading, _1: accumulated}
							};
							codePoints = _v17;
							accumulated = _v18;
							continue splitAndReverse;
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p11, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 65535) < 1)) {
								var _v19 = _p12,
									_v20 = {ctor: '::', _0: _p11, _1: accumulated};
								codePoints = _v19;
								accumulated = _v20;
								continue splitAndReverse;
							} else {
								var _v21 = _p12,
									_v22 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codePoints = _v21;
								accumulated = _v22;
								continue splitAndReverse;
							}
						}
					}
				}
			}
		});
	var allCodeUnits = _elm_lang$core$List$reverse(
		A2(
			splitAndReverse,
			allCodePoints,
			{ctor: '[]'}));
	return _elm_lang$core$String$fromList(
		A2(_elm_lang$core$List$map, _elm_lang$core$Char$fromCode, allCodeUnits));
};
var _elm_community$string_extra$String_Extra$fromFloat = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$fromInt = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$leftOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				A2(_elm_lang$core$Basics$flip, _elm_lang$core$String$left, string),
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$rightOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p13) {
					return A3(
						_elm_lang$core$Basics$flip,
						_elm_lang$core$String$dropLeft,
						string,
						A2(
							F2(
								function (x, y) {
									return x + y;
								}),
							_elm_lang$core$String$length(pattern),
							_p13));
				},
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$firstResultHelp = F2(
	function ($default, list) {
		firstResultHelp:
		while (true) {
			var _p14 = list;
			if (_p14.ctor === '[]') {
				return $default;
			} else {
				if (_p14._0.ctor === 'Just') {
					return _p14._0._0;
				} else {
					var _v24 = $default,
						_v25 = _p14._1;
					$default = _v24;
					list = _v25;
					continue firstResultHelp;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$firstResult = function (list) {
	return A2(_elm_community$string_extra$String_Extra$firstResultHelp, '', list);
};
var _elm_community$string_extra$String_Extra$leftOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p15) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p15));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'^(.*?)',
							_elm_lang$core$Regex$escape(pattern))),
					string)));
	});
var _elm_community$string_extra$String_Extra$rightOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p16) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p16));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Regex$escape(pattern),
							'(.*)$')),
					string)));
	});
var _elm_community$string_extra$String_Extra$pluralize = F3(
	function (singular, plural, count) {
		return _elm_lang$core$Native_Utils.eq(count, 1) ? A2(_elm_lang$core$Basics_ops['++'], '1 ', singular) : A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(count),
			A2(_elm_lang$core$Basics_ops['++'], ' ', plural));
	});
var _elm_community$string_extra$String_Extra$stripTags = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('<\\/?[^>]+>'),
		_elm_lang$core$Basics$always(''),
		string);
};
var _elm_community$string_extra$String_Extra$toSentenceHelper = F3(
	function (lastPart, sentence, list) {
		toSentenceHelper:
		while (true) {
			var _p17 = list;
			if (_p17.ctor === '[]') {
				return sentence;
			} else {
				if (_p17._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], lastPart, _p17._0));
				} else {
					var _v27 = lastPart,
						_v28 = A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], ', ', _p17._0)),
						_v29 = _p17._1;
					lastPart = _v27;
					sentence = _v28;
					list = _v29;
					continue toSentenceHelper;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$toSentenceBaseCase = function (list) {
	var _p18 = list;
	_v30_2:
	do {
		if (_p18.ctor === '::') {
			if (_p18._1.ctor === '[]') {
				return _p18._0;
			} else {
				if (_p18._1._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_p18._0,
						A2(_elm_lang$core$Basics_ops['++'], ' and ', _p18._1._0));
				} else {
					break _v30_2;
				}
			}
		} else {
			break _v30_2;
		}
	} while(false);
	return '';
};
var _elm_community$string_extra$String_Extra$toSentenceOxford = function (list) {
	var _p19 = list;
	if (((_p19.ctor === '::') && (_p19._1.ctor === '::')) && (_p19._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			', and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p19._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p19._1._0)),
			{ctor: '::', _0: _p19._1._1._0, _1: _p19._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$toSentence = function (list) {
	var _p20 = list;
	if (((_p20.ctor === '::') && (_p20._1.ctor === '::')) && (_p20._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			' and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p20._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p20._1._0)),
			{ctor: '::', _0: _p20._1._1._0, _1: _p20._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$ellipsisWith = F3(
	function (howLong, append, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$left,
				howLong - _elm_lang$core$String$length(append),
				string),
			append);
	});
var _elm_community$string_extra$String_Extra$ellipsis = F2(
	function (howLong, string) {
		return A3(_elm_community$string_extra$String_Extra$ellipsisWith, howLong, '...', string);
	});
var _elm_community$string_extra$String_Extra$countOccurrences = F2(
	function (needle, haystack) {
		return (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(needle),
			0) || _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(haystack),
			0)) ? 0 : _elm_lang$core$List$length(
			A2(_elm_lang$core$String$indexes, needle, haystack));
	});
var _elm_community$string_extra$String_Extra$unindent = function (multilineSting) {
	var isNotWhitespace = function ($char) {
		return (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr(' '))) && (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr('\t')));
	};
	var countLeadingWhitespace = F2(
		function (count, line) {
			countLeadingWhitespace:
			while (true) {
				var _p21 = _elm_lang$core$String$uncons(line);
				if (_p21.ctor === 'Nothing') {
					return count;
				} else {
					var _p23 = _p21._0._1;
					var _p22 = _p21._0._0;
					switch (_p22.valueOf()) {
						case ' ':
							var _v35 = count + 1,
								_v36 = _p23;
							count = _v35;
							line = _v36;
							continue countLeadingWhitespace;
						case '\t':
							var _v37 = count + 1,
								_v38 = _p23;
							count = _v37;
							line = _v38;
							continue countLeadingWhitespace;
						default:
							return count;
					}
				}
			}
		});
	var lines = _elm_lang$core$String$lines(multilineSting);
	var minLead = A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$minimum(
			A2(
				_elm_lang$core$List$map,
				countLeadingWhitespace(0),
				A2(
					_elm_lang$core$List$filter,
					_elm_lang$core$String$any(isNotWhitespace),
					lines))));
	return A2(
		_elm_lang$core$String$join,
		'\n',
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$String$dropLeft(minLead),
			lines));
};
var _elm_community$string_extra$String_Extra$dasherize = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('-'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([A-Z])'),
				function (_p24) {
					return A2(
						_elm_lang$core$String$append,
						'-',
						function (_) {
							return _.match;
						}(_p24));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$underscored = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('_'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([a-z\\d])([A-Z]+)'),
				function (_p25) {
					return A2(
						_elm_lang$core$String$join,
						'_',
						A2(
							_elm_lang$core$List$filterMap,
							_elm_lang$core$Basics$identity,
							function (_) {
								return _.submatches;
							}(_p25)));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$unsurround = F2(
	function (wrap, string) {
		if (A2(_elm_lang$core$String$startsWith, wrap, string) && A2(_elm_lang$core$String$endsWith, wrap, string)) {
			var length = _elm_lang$core$String$length(wrap);
			return A2(
				_elm_lang$core$String$dropRight,
				length,
				A2(_elm_lang$core$String$dropLeft, length, string));
		} else {
			return string;
		}
	});
var _elm_community$string_extra$String_Extra$unquote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$unsurround, '\"', string);
};
var _elm_community$string_extra$String_Extra$surround = F2(
	function (wrap, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			wrap,
			A2(_elm_lang$core$Basics_ops['++'], string, wrap));
	});
var _elm_community$string_extra$String_Extra$quote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$surround, '\"', string);
};
var _elm_community$string_extra$String_Extra$camelize = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('[-_\\s]+(.)?'),
		function (_p26) {
			var _p27 = _p26;
			var _p28 = _p27.submatches;
			if ((_p28.ctor === '::') && (_p28._0.ctor === 'Just')) {
				return _elm_lang$core$String$toUpper(_p28._0._0);
			} else {
				return '';
			}
		},
		_elm_lang$core$String$trim(string));
};
var _elm_community$string_extra$String_Extra$isBlank = function (string) {
	return A2(
		_elm_lang$core$Regex$contains,
		_elm_lang$core$Regex$regex('^\\s*$'),
		string);
};
var _elm_community$string_extra$String_Extra$nonBlank = function (string) {
	return _elm_community$string_extra$String_Extra$isBlank(string) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(string);
};
var _elm_community$string_extra$String_Extra$clean = function (string) {
	return _elm_lang$core$String$trim(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('\\s\\s+'),
			_elm_lang$core$Basics$always(' '),
			string));
};
var _elm_community$string_extra$String_Extra$softBreakRegexp = function (width) {
	return _elm_lang$core$Regex$regex(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'.{1,',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(width),
				'}(\\s+|$)|\\S+?(\\s+|$)')));
};
var _elm_community$string_extra$String_Extra$softEllipsis = F2(
	function (howLong, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$String$append,
			'...',
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([\\.,;:\\s])+$'),
				_elm_lang$core$Basics$always(''),
				A2(
					_elm_lang$core$String$join,
					'',
					A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.match;
						},
						A3(
							_elm_lang$core$Regex$find,
							_elm_lang$core$Regex$AtMost(1),
							_elm_community$string_extra$String_Extra$softBreakRegexp(howLong),
							string)))));
	});
var _elm_community$string_extra$String_Extra$softBreak = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.cmp(width, 0) < 1) ? {ctor: '[]'} : A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.match;
			},
			A3(
				_elm_lang$core$Regex$find,
				_elm_lang$core$Regex$All,
				_elm_community$string_extra$String_Extra$softBreakRegexp(width),
				string));
	});
var _elm_community$string_extra$String_Extra$softWrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$softBreak, width, string));
	});
var _elm_community$string_extra$String_Extra$softWrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$softWrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$breaker = F3(
	function (width, string, acc) {
		breaker:
		while (true) {
			var _p29 = string;
			if (_p29 === '') {
				return _elm_lang$core$List$reverse(acc);
			} else {
				var _v42 = width,
					_v43 = A2(_elm_lang$core$String$dropLeft, width, string),
					_v44 = {
					ctor: '::',
					_0: A3(_elm_lang$core$String$slice, 0, width, string),
					_1: acc
				};
				width = _v42;
				string = _v43;
				acc = _v44;
				continue breaker;
			}
		}
	});
var _elm_community$string_extra$String_Extra$break = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.eq(width, 0) || _elm_lang$core$Native_Utils.eq(string, '')) ? {
			ctor: '::',
			_0: string,
			_1: {ctor: '[]'}
		} : A3(
			_elm_community$string_extra$String_Extra$breaker,
			width,
			string,
			{ctor: '[]'});
	});
var _elm_community$string_extra$String_Extra$wrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$break, width, string));
	});
var _elm_community$string_extra$String_Extra$wrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$wrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$replaceSlice = F4(
	function (substitution, start, end, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A3(_elm_lang$core$String$slice, 0, start, string),
			A2(
				_elm_lang$core$Basics_ops['++'],
				substitution,
				A3(
					_elm_lang$core$String$slice,
					end,
					_elm_lang$core$String$length(string),
					string)));
	});
var _elm_community$string_extra$String_Extra$insertAt = F3(
	function (insert, pos, string) {
		return A4(_elm_community$string_extra$String_Extra$replaceSlice, insert, pos, pos, string);
	});
var _elm_community$string_extra$String_Extra$replace = F3(
	function (search, substitution, string) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(
				_elm_lang$core$Regex$escape(search)),
			function (_p30) {
				return substitution;
			},
			string);
	});
var _elm_community$string_extra$String_Extra$changeCase = F2(
	function (mutator, word) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p31) {
					var _p32 = _p31;
					return A2(
						_elm_lang$core$String$cons,
						mutator(_p32._0),
						_p32._1);
				},
				_elm_lang$core$String$uncons(word)));
	});
var _elm_community$string_extra$String_Extra$toSentenceCase = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toUpper, word);
};
var _elm_community$string_extra$String_Extra$toTitleCase = function (ws) {
	var uppercaseMatch = A3(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('\\w+'),
		function (_p33) {
			return _elm_community$string_extra$String_Extra$toSentenceCase(
				function (_) {
					return _.match;
				}(_p33));
		});
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('^([a-z])|\\s+([a-z])'),
		function (_p34) {
			return uppercaseMatch(
				function (_) {
					return _.match;
				}(_p34));
		},
		ws);
};
var _elm_community$string_extra$String_Extra$classify = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		A3(
			_elm_community$string_extra$String_Extra$replace,
			' ',
			'',
			_elm_community$string_extra$String_Extra$camelize(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('[\\W_]'),
					_elm_lang$core$Basics$always(' '),
					string))));
};
var _elm_community$string_extra$String_Extra$humanize = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		_elm_lang$core$String$toLower(
			_elm_lang$core$String$trim(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('_id$|[-_\\s]+'),
					_elm_lang$core$Basics$always(' '),
					A4(
						_elm_lang$core$Regex$replace,
						_elm_lang$core$Regex$All,
						_elm_lang$core$Regex$regex('[A-Z]'),
						function (_p35) {
							return A2(
								_elm_lang$core$String$append,
								'-',
								function (_) {
									return _.match;
								}(_p35));
						},
						string)))));
};
var _elm_community$string_extra$String_Extra$decapitalize = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toLower, word);
};

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_community$maybe_extra$Maybe_Extra$foldrValues = F2(
	function (item, list) {
		var _p0 = item;
		if (_p0.ctor === 'Nothing') {
			return list;
		} else {
			return {ctor: '::', _0: _p0._0, _1: list};
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$values = A2(
	_elm_lang$core$List$foldr,
	_elm_community$maybe_extra$Maybe_Extra$foldrValues,
	{ctor: '[]'});
var _elm_community$maybe_extra$Maybe_Extra$filter = F2(
	function (f, m) {
		var _p1 = A2(_elm_lang$core$Maybe$map, f, m);
		if ((_p1.ctor === 'Just') && (_p1._0 === true)) {
			return m;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$traverseArray = function (f) {
	var step = F2(
		function (e, acc) {
			var _p2 = f(e);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$Array$push(_p2._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$Array$foldl,
		step,
		_elm_lang$core$Maybe$Just(_elm_lang$core$Array$empty));
};
var _elm_community$maybe_extra$Maybe_Extra$combineArray = _elm_community$maybe_extra$Maybe_Extra$traverseArray(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$traverse = function (f) {
	var step = F2(
		function (e, acc) {
			var _p3 = f(e);
			if (_p3.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(_p3._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$List$foldr,
		step,
		_elm_lang$core$Maybe$Just(
			{ctor: '[]'}));
};
var _elm_community$maybe_extra$Maybe_Extra$combine = _elm_community$maybe_extra$Maybe_Extra$traverse(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$toArray = function (m) {
	var _p4 = m;
	if (_p4.ctor === 'Nothing') {
		return _elm_lang$core$Array$empty;
	} else {
		return A2(_elm_lang$core$Array$repeat, 1, _p4._0);
	}
};
var _elm_community$maybe_extra$Maybe_Extra$toList = function (m) {
	var _p5 = m;
	if (_p5.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p5._0,
			_1: {ctor: '[]'}
		};
	}
};
var _elm_community$maybe_extra$Maybe_Extra$orElse = F2(
	function (ma, mb) {
		var _p6 = mb;
		if (_p6.ctor === 'Nothing') {
			return ma;
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orElseLazy = F2(
	function (fma, mb) {
		var _p7 = mb;
		if (_p7.ctor === 'Nothing') {
			return fma(
				{ctor: '_Tuple0'});
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orLazy = F2(
	function (ma, fmb) {
		var _p8 = ma;
		if (_p8.ctor === 'Nothing') {
			return fmb(
				{ctor: '_Tuple0'});
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$or = F2(
	function (ma, mb) {
		var _p9 = ma;
		if (_p9.ctor === 'Nothing') {
			return mb;
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$prev = _elm_lang$core$Maybe$map2(_elm_lang$core$Basics$always);
var _elm_community$maybe_extra$Maybe_Extra$next = _elm_lang$core$Maybe$map2(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));
var _elm_community$maybe_extra$Maybe_Extra$andMap = _elm_lang$core$Maybe$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _elm_community$maybe_extra$Maybe_Extra$unpack = F3(
	function (d, f, m) {
		var _p10 = m;
		if (_p10.ctor === 'Nothing') {
			return d(
				{ctor: '_Tuple0'});
		} else {
			return f(_p10._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$unwrap = F3(
	function (d, f, m) {
		var _p11 = m;
		if (_p11.ctor === 'Nothing') {
			return d;
		} else {
			return f(_p11._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$isJust = function (m) {
	var _p12 = m;
	if (_p12.ctor === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$isNothing = function (m) {
	var _p13 = m;
	if (_p13.ctor === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$join = function (mx) {
	var _p14 = mx;
	if (_p14.ctor === 'Just') {
		return _p14._0;
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_community$maybe_extra$Maybe_Extra_ops = _elm_community$maybe_extra$Maybe_Extra_ops || {};
_elm_community$maybe_extra$Maybe_Extra_ops['?'] = F2(
	function (mx, x) {
		return A2(_elm_lang$core$Maybe$withDefault, x, mx);
	});

var _SidneyNemzer$elm_text_editor$Position$addLine = F2(
	function (amount, position) {
		return _elm_lang$core$Native_Utils.update(
			position,
			{line: position.line + amount});
	});
var _SidneyNemzer$elm_text_editor$Position$nextLine = _SidneyNemzer$elm_text_editor$Position$addLine(1);
var _SidneyNemzer$elm_text_editor$Position$previousLine = _SidneyNemzer$elm_text_editor$Position$addLine(-1);
var _SidneyNemzer$elm_text_editor$Position$setColumn = F2(
	function (column, position) {
		return _elm_lang$core$Native_Utils.update(
			position,
			{column: column});
	});
var _SidneyNemzer$elm_text_editor$Position$addColumn = F2(
	function (amount, position) {
		return _elm_lang$core$Native_Utils.update(
			position,
			{column: position.column + amount});
	});
var _SidneyNemzer$elm_text_editor$Position$nextColumn = _SidneyNemzer$elm_text_editor$Position$addColumn(1);
var _SidneyNemzer$elm_text_editor$Position$previousColumn = _SidneyNemzer$elm_text_editor$Position$addColumn(-1);
var _SidneyNemzer$elm_text_editor$Position$betweenHelp = F3(
	function (start, end, point) {
		betweenHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(start, end) > 0) {
				var _v0 = end,
					_v1 = start,
					_v2 = point;
				start = _v0;
				end = _v1;
				point = _v2;
				continue betweenHelp;
			} else {
				return (!_elm_lang$core$Native_Utils.eq(start, end)) && ((_elm_lang$core$Native_Utils.cmp(point, start) > -1) && (_elm_lang$core$Native_Utils.cmp(point, end) < 0));
			}
		}
	});
var _SidneyNemzer$elm_text_editor$Position$order = F2(
	function (pos1, pos2) {
		return (_elm_lang$core$Native_Utils.cmp(pos2.line, pos1.line) > 0) ? {ctor: '_Tuple2', _0: pos1, _1: pos2} : ((_elm_lang$core$Native_Utils.eq(pos2.line, pos1.line) && (_elm_lang$core$Native_Utils.cmp(pos2.column, pos1.column) > 0)) ? {ctor: '_Tuple2', _0: pos1, _1: pos2} : {ctor: '_Tuple2', _0: pos2, _1: pos1});
	});
var _SidneyNemzer$elm_text_editor$Position$between = F3(
	function (pos1, pos2, _p0) {
		var _p1 = _p0;
		var _p4 = _p1.line;
		var _p3 = _p1.column;
		var _p2 = A2(_SidneyNemzer$elm_text_editor$Position$order, pos1, pos2);
		var start = _p2._0;
		var end = _p2._1;
		return _elm_lang$core$Native_Utils.eq(start.line, end.line) ? (_elm_lang$core$Native_Utils.eq(_p4, start.line) && A3(_SidneyNemzer$elm_text_editor$Position$betweenHelp, start.column, end.column, _p3)) : (_elm_lang$core$Native_Utils.eq(start.line, _p4) ? (_elm_lang$core$Native_Utils.cmp(_p3, start.column) > -1) : (_elm_lang$core$Native_Utils.eq(end.line, _p4) ? (_elm_lang$core$Native_Utils.cmp(_p3, end.column) < 0) : A3(_SidneyNemzer$elm_text_editor$Position$betweenHelp, start.line, end.line, _p4)));
	});
var _SidneyNemzer$elm_text_editor$Position$Position = F2(
	function (a, b) {
		return {line: a, column: b};
	});

var _SidneyNemzer$elm_text_editor$Util_Array$last = function (array) {
	var length = _Skinney$elm_array_exploration$Array_Hamt$length(array);
	return A2(
		_elm_lang$core$Maybe$map,
		function (a) {
			return {ctor: '_Tuple2', _0: a, _1: length - 1};
		},
		A2(
			_Skinney$elm_array_exploration$Array_Hamt$get,
			0,
			A3(_Skinney$elm_array_exploration$Array_Hamt$slice, -1, length, array)));
};

var _SidneyNemzer$elm_text_editor$Buffer$tuple3MapAll = F2(
	function (fn, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple3',
			_0: fn(_p1._0),
			_1: fn(_p1._1),
			_2: fn(_p1._2)
		};
	});
var _SidneyNemzer$elm_text_editor$Buffer$tuple3CharsPred = function (pred) {
	return _SidneyNemzer$elm_text_editor$Buffer$tuple3MapAll(
		function (_p2) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				false,
				A2(_elm_lang$core$Maybe$map, pred, _p2));
		});
};
var _SidneyNemzer$elm_text_editor$Buffer$stringCharAt = F2(
	function (index, string) {
		return A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$Tuple$first,
			_elm_lang$core$String$uncons(
				A3(_elm_lang$core$String$slice, index, index + 1, string)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$charsAround = F2(
	function (index, string) {
		return {
			ctor: '_Tuple3',
			_0: A2(_SidneyNemzer$elm_text_editor$Buffer$stringCharAt, index - 1, string),
			_1: A2(_SidneyNemzer$elm_text_editor$Buffer$stringCharAt, index, string),
			_2: A2(_SidneyNemzer$elm_text_editor$Buffer$stringCharAt, index + 1, string)
		};
	});
var _SidneyNemzer$elm_text_editor$Buffer$isNonWordChar = function (_p3) {
	return A3(
		_elm_lang$core$Basics$flip,
		_elm_lang$core$String$contains,
		'/\\()\"\':,.;<>~!@#$%^&*|+=[]{}`?-…',
		_elm_lang$core$String$fromChar(_p3));
};
var _SidneyNemzer$elm_text_editor$Buffer$isWhitespace = function (_p4) {
	return A2(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			}),
		'',
		_elm_lang$core$String$trim(
			_elm_lang$core$String$fromChar(_p4)));
};
var _SidneyNemzer$elm_text_editor$Buffer$isWordChar = function ($char) {
	return (!_SidneyNemzer$elm_text_editor$Buffer$isNonWordChar($char)) && (!_SidneyNemzer$elm_text_editor$Buffer$isWhitespace($char));
};
var _SidneyNemzer$elm_text_editor$Buffer$toString = function (_p5) {
	var _p6 = _p5;
	return _p6._0;
};
var _SidneyNemzer$elm_text_editor$Buffer$lines = function (_p7) {
	var _p8 = _p7;
	return A2(_elm_lang$core$String$split, '\n', _p8._0);
};
var _SidneyNemzer$elm_text_editor$Buffer$lineEnd = function (line) {
	return function (_p9) {
		return A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$String$length,
			A2(
				_elm_community$list_extra$List_Extra$getAt,
				line,
				_SidneyNemzer$elm_text_editor$Buffer$lines(_p9)));
	};
};
var _SidneyNemzer$elm_text_editor$Buffer$clampPosition = F3(
	function (direction, buffer, position) {
		var lines_ = _Skinney$elm_array_exploration$Array_Hamt$fromList(
			_SidneyNemzer$elm_text_editor$Buffer$lines(buffer));
		if (_elm_lang$core$Native_Utils.cmp(position.line, 0) < 0) {
			return A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0);
		} else {
			var _p10 = A2(_Skinney$elm_array_exploration$Array_Hamt$get, position.line, lines_);
			if (_p10.ctor === 'Just') {
				var _p13 = _p10._0;
				if (_elm_lang$core$Native_Utils.cmp(
					position.column,
					_elm_lang$core$String$length(_p13)) > 0) {
					var _p11 = direction;
					if (_p11.ctor === 'Forward') {
						return (_elm_lang$core$Native_Utils.cmp(
							position.line,
							_Skinney$elm_array_exploration$Array_Hamt$length(lines_) - 1) < 0) ? A2(_SidneyNemzer$elm_text_editor$Position$Position, position.line + 1, 0) : position;
					} else {
						return A2(
							_SidneyNemzer$elm_text_editor$Position$Position,
							position.line,
							_elm_lang$core$String$length(_p13));
					}
				} else {
					if (_elm_lang$core$Native_Utils.cmp(position.column, 0) < 0) {
						return A2(
							_elm_lang$core$Maybe$withDefault,
							A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0),
							A2(
								_elm_lang$core$Maybe$map,
								function (_p12) {
									return A2(
										_SidneyNemzer$elm_text_editor$Position$Position,
										position.line - 1,
										_elm_lang$core$String$length(_p12));
								},
								A2(_Skinney$elm_array_exploration$Array_Hamt$get, position.line - 1, lines_)));
					} else {
						return position;
					}
				}
			} else {
				var _p14 = _SidneyNemzer$elm_text_editor$Util_Array$last(lines_);
				if (_p14.ctor === 'Just') {
					return A2(
						_SidneyNemzer$elm_text_editor$Position$Position,
						_p14._0._1,
						_elm_lang$core$String$length(_p14._0._0));
				} else {
					return A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0);
				}
			}
		}
	});
var _SidneyNemzer$elm_text_editor$Buffer$end = function (buffer) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0),
		A2(
			_elm_lang$core$Maybe$map,
			function (_p15) {
				var _p16 = _p15;
				return A2(
					_SidneyNemzer$elm_text_editor$Position$Position,
					_p16._1,
					_elm_lang$core$String$length(_p16._0));
			},
			_SidneyNemzer$elm_text_editor$Util_Array$last(
				_Skinney$elm_array_exploration$Array_Hamt$fromList(
					_SidneyNemzer$elm_text_editor$Buffer$lines(buffer)))));
};
var _SidneyNemzer$elm_text_editor$Buffer$indexFromPosition = F2(
	function (buffer, position) {
		return _elm_lang$core$Native_Utils.eq(position.line, 0) ? _elm_lang$core$Maybe$Just(position.column) : A2(
			_elm_lang$core$Maybe$map,
			function (line) {
				return (line + position.column) + 1;
			},
			A2(
				_elm_community$list_extra$List_Extra$getAt,
				position.line - 1,
				A2(_elm_lang$core$String$indexes, '\n', buffer)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$nearWordChar = F2(
	function (position, _p17) {
		var _p18 = _p17;
		var _p21 = _p18._0;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			false,
			A2(
				_elm_lang$core$Maybe$andThen,
				function (index) {
					var currentChar = A2(_SidneyNemzer$elm_text_editor$Buffer$stringCharAt, index, _p21);
					var previousChar = A2(_SidneyNemzer$elm_text_editor$Buffer$stringCharAt, index - 1, _p21);
					return A2(
						_elm_community$maybe_extra$Maybe_Extra$orElseLazy,
						function (_p19) {
							var _p20 = _p19;
							return A2(_elm_lang$core$Maybe$map, _SidneyNemzer$elm_text_editor$Buffer$isWordChar, currentChar);
						},
						A2(_elm_lang$core$Maybe$map, _SidneyNemzer$elm_text_editor$Buffer$isWordChar, previousChar));
				},
				A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p21, position)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$between = F3(
	function (pos1, pos2, _p22) {
		var _p23 = _p22;
		var _p25 = _p23._0;
		var _p24 = A2(_SidneyNemzer$elm_text_editor$Position$order, pos1, pos2);
		var start = _p24._0;
		var end = _p24._1;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A3(
				_elm_lang$core$Maybe$map2,
				F2(
					function (startIndex, endIndex) {
						return A3(_elm_lang$core$String$slice, startIndex, endIndex, _p25);
					}),
				A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p25, start),
				A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p25, end)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$listMapAt = F3(
	function (fn, index, list) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			list,
			A2(
				_elm_lang$core$Maybe$map,
				function (a) {
					return A3(
						_elm_community$list_extra$List_Extra$setAt,
						index,
						fn(a),
						list);
				},
				A2(_elm_community$list_extra$List_Extra$getAt, index, list)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$indentSize = 2;
var _SidneyNemzer$elm_text_editor$Buffer$Buffer = function (a) {
	return {ctor: 'Buffer', _0: a};
};
var _SidneyNemzer$elm_text_editor$Buffer$init = function (content) {
	return _SidneyNemzer$elm_text_editor$Buffer$Buffer(content);
};
var _SidneyNemzer$elm_text_editor$Buffer$insert = F3(
	function (position, string, _p26) {
		var _p27 = _p26;
		var _p28 = _p27._0;
		return _SidneyNemzer$elm_text_editor$Buffer$Buffer(
			A2(
				_elm_lang$core$Maybe$withDefault,
				_p28,
				A2(
					_elm_lang$core$Maybe$map,
					function (index) {
						return A3(_elm_community$string_extra$String_Extra$insertAt, string, index, _p28);
					},
					A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p28, position))));
	});
var _SidneyNemzer$elm_text_editor$Buffer$replace = F4(
	function (pos1, pos2, string, _p29) {
		var _p30 = _p29;
		var _p32 = _p30._0;
		var _p31 = A2(_SidneyNemzer$elm_text_editor$Position$order, pos1, pos2);
		var start = _p31._0;
		var end = _p31._1;
		return _SidneyNemzer$elm_text_editor$Buffer$Buffer(
			A2(
				_elm_lang$core$Maybe$withDefault,
				_p32,
				A3(
					_elm_lang$core$Maybe$map2,
					F2(
						function (startIndex, endIndex) {
							return A2(
								_elm_lang$core$Basics_ops['++'],
								A3(_elm_lang$core$String$slice, 0, startIndex, _p32),
								A2(
									_elm_lang$core$Basics_ops['++'],
									string,
									A2(_elm_lang$core$String$dropLeft, endIndex, _p32)));
						}),
					A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p32, start),
					A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p32, end))));
	});
var _SidneyNemzer$elm_text_editor$Buffer$removeBefore = F2(
	function (position, _p33) {
		var _p34 = _p33;
		var _p35 = _p34._0;
		return _SidneyNemzer$elm_text_editor$Buffer$Buffer(
			A2(
				_elm_lang$core$Maybe$withDefault,
				_p35,
				A2(
					_elm_lang$core$Maybe$map,
					function (index) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							A3(
								_elm_lang$core$String$slice,
								0,
								A2(_elm_lang$core$Basics$max, 0, index - 1),
								_p35),
							A2(_elm_lang$core$String$dropLeft, index, _p35));
					},
					A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p35, position))));
	});
var _SidneyNemzer$elm_text_editor$Buffer$indentFrom = F2(
	function (_p37, _p36) {
		var _p38 = _p37;
		var _p41 = _p38.column;
		var _p39 = _p36;
		var _p40 = _p39._0;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Buffer$Buffer(_p40),
				_1: _p41
			},
			A2(
				_elm_lang$core$Maybe$map,
				function (lineStart) {
					var addIndentSize = _SidneyNemzer$elm_text_editor$Buffer$indentSize - A2(
						_elm_lang$core$Basics_ops['%'],
						_elm_lang$core$String$length(
							A3(_elm_lang$core$String$slice, lineStart, lineStart + _p41, _p40)),
						_SidneyNemzer$elm_text_editor$Buffer$indentSize);
					return {
						ctor: '_Tuple2',
						_0: _SidneyNemzer$elm_text_editor$Buffer$Buffer(
							A2(
								_elm_lang$core$Basics_ops['++'],
								A3(_elm_lang$core$String$slice, 0, lineStart + _p41, _p40),
								A2(
									_elm_lang$core$Basics_ops['++'],
									A2(_elm_lang$core$String$repeat, addIndentSize, ' '),
									A2(_elm_lang$core$String$dropLeft, lineStart + _p41, _p40)))),
						_1: _p41 + addIndentSize
					};
				},
				A2(
					_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition,
					_p40,
					A2(_SidneyNemzer$elm_text_editor$Position$Position, _p38.line, 0))));
	});
var _SidneyNemzer$elm_text_editor$Buffer$indentBetween = F3(
	function (pos1, pos2, buffer) {
		var _p42 = A2(_SidneyNemzer$elm_text_editor$Position$order, pos1, pos2);
		var start = _p42._0;
		var end = _p42._1;
		return A3(
			_elm_lang$core$List$foldl,
			function (line) {
				return function (_p43) {
					return _elm_lang$core$Tuple$first(
						A2(
							_SidneyNemzer$elm_text_editor$Buffer$indentFrom,
							{line: line, column: 0},
							_p43));
				};
			},
			buffer,
			A2(_elm_lang$core$List$range, start.line, end.line));
	});
var _SidneyNemzer$elm_text_editor$Buffer$deindentFrom = F2(
	function (_p45, _p44) {
		var _p46 = _p45;
		var _p49 = _p46.column;
		var _p47 = _p44;
		var _p48 = _p47._0;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Buffer$Buffer(_p48),
				_1: _p49
			},
			A2(
				_elm_lang$core$Maybe$map,
				function (lineStart) {
					var startChars = A3(_elm_lang$core$String$slice, lineStart, lineStart + _SidneyNemzer$elm_text_editor$Buffer$indentSize, _p48);
					var startIndentChars = A3(
						_elm_lang$core$String$foldl,
						F2(
							function ($char, count) {
								return _elm_lang$core$Native_Utils.eq(
									$char,
									_elm_lang$core$Native_Utils.chr(' ')) ? (count + 1) : count;
							}),
						0,
						startChars);
					return {
						ctor: '_Tuple2',
						_0: _SidneyNemzer$elm_text_editor$Buffer$Buffer(
							A2(
								_elm_lang$core$Basics_ops['++'],
								A3(_elm_lang$core$String$slice, 0, lineStart, _p48),
								A2(_elm_lang$core$String$dropLeft, lineStart + startIndentChars, _p48))),
						_1: _p49 - startIndentChars
					};
				},
				A2(
					_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition,
					_p48,
					A2(_SidneyNemzer$elm_text_editor$Position$Position, _p46.line, 0))));
	});
var _SidneyNemzer$elm_text_editor$Buffer$deindentBetween = F3(
	function (pos1, pos2, buffer) {
		var _p50 = A2(_SidneyNemzer$elm_text_editor$Buffer$deindentFrom, pos1, buffer);
		var pos1Buffer = _p50._0;
		var pos1Column = _p50._1;
		var _p51 = A2(_SidneyNemzer$elm_text_editor$Buffer$deindentFrom, pos2, pos1Buffer);
		var pos2Buffer = _p51._0;
		var pos2Column = _p51._1;
		if (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Basics$abs(pos1.line - pos2.line),
			1) > 0) {
			var _p52 = A2(_SidneyNemzer$elm_text_editor$Position$order, pos1, pos2);
			var start = _p52._0;
			var end = _p52._1;
			return {
				ctor: '_Tuple3',
				_0: A3(
					_elm_lang$core$List$foldl,
					function (line) {
						return function (_p53) {
							return _elm_lang$core$Tuple$first(
								A2(
									_SidneyNemzer$elm_text_editor$Buffer$deindentFrom,
									{line: line, column: 0},
									_p53));
						};
					},
					pos2Buffer,
					A2(_elm_lang$core$List$range, start.line + 1, end.line - 1)),
				_1: pos1Column,
				_2: pos2Column
			};
		} else {
			return {ctor: '_Tuple3', _0: pos2Buffer, _1: pos1Column, _2: pos2Column};
		}
	});
var _SidneyNemzer$elm_text_editor$Buffer$NonWord = {ctor: 'NonWord'};
var _SidneyNemzer$elm_text_editor$Buffer$Word = {ctor: 'Word'};
var _SidneyNemzer$elm_text_editor$Buffer$None = {ctor: 'None'};
var _SidneyNemzer$elm_text_editor$Buffer$groupHelp = F5(
	function (direction, consumedNewline, string, position, group) {
		var parts = function () {
			var _p54 = direction;
			if (_p54.ctor === 'Forward') {
				return _elm_lang$core$String$uncons(string);
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$Tuple$mapSecond(_elm_lang$core$String$reverse),
					_elm_lang$core$String$uncons(
						_elm_lang$core$String$reverse(string)));
			}
		}();
		var _p55 = parts;
		if (_p55.ctor === 'Just') {
			var _p59 = _p55._0._1;
			var _p58 = _p55._0._0;
			var nextPosition = function (changeLine) {
				var _p56 = direction;
				if (_p56.ctor === 'Forward') {
					return changeLine ? A2(_SidneyNemzer$elm_text_editor$Position$Position, position.line + 1, 0) : _SidneyNemzer$elm_text_editor$Position$nextColumn(position);
				} else {
					return changeLine ? (A2(_elm_lang$core$String$contains, '\n', _p59) ? A2(
						_SidneyNemzer$elm_text_editor$Position$Position,
						position.line - 1,
						_elm_lang$core$String$length(
							A2(_elm_community$string_extra$String_Extra$rightOfBack, '\n', _p59))) : A2(
						_SidneyNemzer$elm_text_editor$Position$Position,
						position.line - 1,
						_elm_lang$core$String$length(_p59))) : _SidneyNemzer$elm_text_editor$Position$previousColumn(position);
				}
			};
			var next = function (nextConsumedNewline) {
				return A4(
					_SidneyNemzer$elm_text_editor$Buffer$groupHelp,
					direction,
					nextConsumedNewline,
					_p59,
					nextPosition(
						!_elm_lang$core$Native_Utils.eq(consumedNewline, nextConsumedNewline)));
			};
			var _p57 = group;
			switch (_p57.ctor) {
				case 'None':
					return _elm_lang$core$Native_Utils.eq(
						_p58,
						_elm_lang$core$Native_Utils.chr('\n')) ? (consumedNewline ? position : A2(next, true, _SidneyNemzer$elm_text_editor$Buffer$None)) : (_SidneyNemzer$elm_text_editor$Buffer$isWhitespace(_p58) ? A2(next, consumedNewline, _SidneyNemzer$elm_text_editor$Buffer$None) : (_SidneyNemzer$elm_text_editor$Buffer$isNonWordChar(_p58) ? A2(next, consumedNewline, _SidneyNemzer$elm_text_editor$Buffer$NonWord) : A2(next, consumedNewline, _SidneyNemzer$elm_text_editor$Buffer$Word)));
				case 'Word':
					return (!_SidneyNemzer$elm_text_editor$Buffer$isWordChar(_p58)) ? position : A2(next, consumedNewline, _SidneyNemzer$elm_text_editor$Buffer$Word);
				default:
					return _SidneyNemzer$elm_text_editor$Buffer$isNonWordChar(_p58) ? A2(next, consumedNewline, _SidneyNemzer$elm_text_editor$Buffer$NonWord) : position;
			}
		} else {
			return position;
		}
	});
var _SidneyNemzer$elm_text_editor$Buffer$Backward = {ctor: 'Backward'};
var _SidneyNemzer$elm_text_editor$Buffer$groupStart = F2(
	function (position, _p60) {
		var _p61 = _p60;
		var _p62 = _p61._0;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			position,
			A2(
				_elm_lang$core$Maybe$map,
				function (index) {
					return A5(
						_SidneyNemzer$elm_text_editor$Buffer$groupHelp,
						_SidneyNemzer$elm_text_editor$Buffer$Backward,
						false,
						A3(_elm_lang$core$String$slice, 0, index, _p62),
						position,
						_SidneyNemzer$elm_text_editor$Buffer$None);
				},
				A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p62, position)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$Forward = {ctor: 'Forward'};
var _SidneyNemzer$elm_text_editor$Buffer$groupEnd = F2(
	function (position, _p63) {
		var _p64 = _p63;
		var _p65 = _p64._0;
		return A2(
			_elm_lang$core$Maybe$withDefault,
			position,
			A2(
				_elm_lang$core$Maybe$map,
				function (index) {
					return A5(
						_SidneyNemzer$elm_text_editor$Buffer$groupHelp,
						_SidneyNemzer$elm_text_editor$Buffer$Forward,
						false,
						A2(_elm_lang$core$String$dropLeft, index, _p65),
						position,
						_SidneyNemzer$elm_text_editor$Buffer$None);
				},
				A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p65, position)));
	});
var _SidneyNemzer$elm_text_editor$Buffer$groupRange = F2(
	function (position, _p66) {
		var _p67 = _p66;
		var _p71 = _p67._0;
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (index) {
				var chars = A2(_SidneyNemzer$elm_text_editor$Buffer$charsAround, index, _p71);
				var range = function (pred) {
					var _p68 = A2(_SidneyNemzer$elm_text_editor$Buffer$tuple3CharsPred, pred, chars);
					_v24_5:
					do {
						if (_p68.ctor === '_Tuple3') {
							if (_p68._0 === true) {
								if (_p68._1 === true) {
									if (_p68._2 === true) {
										return _elm_lang$core$Maybe$Just(
											{
												ctor: '_Tuple2',
												_0: A2(
													_SidneyNemzer$elm_text_editor$Buffer$groupStart,
													position,
													_SidneyNemzer$elm_text_editor$Buffer$Buffer(_p71)),
												_1: A2(
													_SidneyNemzer$elm_text_editor$Buffer$groupEnd,
													position,
													_SidneyNemzer$elm_text_editor$Buffer$Buffer(_p71))
											});
									} else {
										return _elm_lang$core$Maybe$Just(
											{
												ctor: '_Tuple2',
												_0: A2(
													_SidneyNemzer$elm_text_editor$Buffer$groupStart,
													position,
													_SidneyNemzer$elm_text_editor$Buffer$Buffer(_p71)),
												_1: _SidneyNemzer$elm_text_editor$Position$nextColumn(position)
											});
									}
								} else {
									return _elm_lang$core$Maybe$Just(
										{
											ctor: '_Tuple2',
											_0: A2(
												_SidneyNemzer$elm_text_editor$Buffer$groupStart,
												position,
												_SidneyNemzer$elm_text_editor$Buffer$Buffer(_p71)),
											_1: position
										});
								}
							} else {
								if (_p68._1 === true) {
									if (_p68._2 === true) {
										return _elm_lang$core$Maybe$Just(
											{
												ctor: '_Tuple2',
												_0: position,
												_1: A2(
													_SidneyNemzer$elm_text_editor$Buffer$groupEnd,
													position,
													_SidneyNemzer$elm_text_editor$Buffer$Buffer(_p71))
											});
									} else {
										return _elm_lang$core$Maybe$Just(
											{
												ctor: '_Tuple2',
												_0: position,
												_1: _SidneyNemzer$elm_text_editor$Position$nextColumn(position)
											});
									}
								} else {
									break _v24_5;
								}
							}
						} else {
							break _v24_5;
						}
					} while(false);
					return _elm_lang$core$Maybe$Nothing;
				};
				return A2(
					_elm_community$maybe_extra$Maybe_Extra$orElseLazy,
					function (_p69) {
						var _p70 = _p69;
						return range(_SidneyNemzer$elm_text_editor$Buffer$isNonWordChar);
					},
					range(_SidneyNemzer$elm_text_editor$Buffer$isWordChar));
			},
			A2(_SidneyNemzer$elm_text_editor$Buffer$indexFromPosition, _p71, position));
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _SidneyNemzer$elm_text_editor$Editor_History$State = F3(
	function (a, b, c) {
		return {cursor: a, selection: b, buffer: c};
	});
var _SidneyNemzer$elm_text_editor$Editor_History$InternalHistory = F2(
	function (a, b) {
		return {past: a, future: b};
	});
var _SidneyNemzer$elm_text_editor$Editor_History$History = function (a) {
	return {ctor: 'History', _0: a};
};
var _SidneyNemzer$elm_text_editor$Editor_History$empty = _SidneyNemzer$elm_text_editor$Editor_History$History(
	{
		past: {ctor: '[]'},
		future: {ctor: '[]'}
	});
var _SidneyNemzer$elm_text_editor$Editor_History$push = F2(
	function (entry, _p0) {
		var _p1 = _p0;
		return _SidneyNemzer$elm_text_editor$Editor_History$History(
			{
				past: {ctor: '::', _0: entry, _1: _p1._0.past},
				future: {ctor: '[]'}
			});
	});
var _SidneyNemzer$elm_text_editor$Editor_History$undo = F2(
	function (current, _p2) {
		var _p3 = _p2;
		var _p5 = _p3._0;
		var _p4 = _p5.past;
		if (_p4.ctor === '::') {
			return {
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Editor_History$History(
					{
						past: _p4._1,
						future: {ctor: '::', _0: current, _1: _p5.future}
					}),
				_1: _elm_lang$core$Maybe$Just(_p4._0)
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Editor_History$History(_p5),
				_1: _elm_lang$core$Maybe$Nothing
			};
		}
	});
var _SidneyNemzer$elm_text_editor$Editor_History$redo = F2(
	function (current, _p6) {
		var _p7 = _p6;
		var _p9 = _p7._0;
		var _p8 = _p9.future;
		if (_p8.ctor === '::') {
			return {
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Editor_History$History(
					{
						past: {ctor: '::', _0: current, _1: _p9.past},
						future: _p8._1
					}),
				_1: _elm_lang$core$Maybe$Just(_p8._0)
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _SidneyNemzer$elm_text_editor$Editor_History$History(_p9),
				_1: _elm_lang$core$Maybe$Nothing
			};
		}
	});

var _SidneyNemzer$elm_text_editor$Editor_Model$InternalState = F5(
	function (a, b, c, d, e) {
		return {scrolledLine: a, cursor: b, selection: c, dragging: d, history: e};
	});

var _SidneyNemzer$elm_text_editor$Editor_Update$historyState = F2(
	function (_p0, buffer) {
		var _p1 = _p0;
		return {cursor: _p1.cursor, selection: _p1.selection, buffer: buffer};
	});
var _SidneyNemzer$elm_text_editor$Editor_Update$pushHistory = F3(
	function (oldState, oldBuffer, _p2) {
		var _p3 = _p2;
		var _p5 = _p3._0;
		var _p4 = _p3._1;
		return {
			ctor: '_Tuple3',
			_0: _elm_lang$core$Native_Utils.update(
				_p5,
				{
					history: (!_elm_lang$core$Native_Utils.eq(oldBuffer, _p4)) ? A2(
						_SidneyNemzer$elm_text_editor$Editor_History$push,
						A2(_SidneyNemzer$elm_text_editor$Editor_Update$historyState, oldState, oldBuffer),
						_p5.history) : _p5.history
				}),
			_1: _p4,
			_2: _p3._2
		};
	});
var _SidneyNemzer$elm_text_editor$Editor_Update$autoclose = _elm_lang$core$Dict$fromList(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: '[', _1: ']'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: '{', _1: '}'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: '(', _1: ')'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: '\"', _1: '\"'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '\'', _1: '\''},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: '`', _1: '`'},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	});
var _SidneyNemzer$elm_text_editor$Editor_Update$update = F3(
	function (buffer, msg, state) {
		var _p6 = msg;
		switch (_p6.ctor) {
			case 'MouseDown':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{cursor: _p6._0, dragging: true, selection: _elm_lang$core$Maybe$Nothing}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'MouseOver':
				var _p9 = _p6._0;
				return state.dragging ? {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							selection: function () {
								var _p7 = state.selection;
								if (_p7.ctor === 'Just') {
									var _p8 = _p7._0;
									return _elm_lang$core$Native_Utils.eq(_p8, _p9) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(_p8);
								} else {
									return _elm_lang$core$Native_Utils.eq(_p9, state.cursor) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(state.cursor);
								}
							}(),
							cursor: _p9
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				} : {ctor: '_Tuple3', _0: state, _1: buffer, _2: _elm_lang$core$Platform_Cmd$none};
			case 'MouseUp':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{dragging: false}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorLeft':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p10 = state.selection;
									if (_p10.ctor === 'Just') {
										return _elm_lang$core$Tuple$first(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p10._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								return A3(
									_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
									_SidneyNemzer$elm_text_editor$Buffer$Backward,
									buffer,
									_SidneyNemzer$elm_text_editor$Position$previousColumn(moveFrom));
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorRight':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p11 = state.selection;
									if (_p11.ctor === 'Just') {
										return _elm_lang$core$Tuple$second(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p11._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								return A3(
									_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
									_SidneyNemzer$elm_text_editor$Buffer$Forward,
									buffer,
									_SidneyNemzer$elm_text_editor$Position$nextColumn(moveFrom));
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorUp':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p12 = state.selection;
									if (_p12.ctor === 'Just') {
										return _elm_lang$core$Tuple$first(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p12._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								return A3(
									_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
									_SidneyNemzer$elm_text_editor$Buffer$Backward,
									buffer,
									_SidneyNemzer$elm_text_editor$Position$previousLine(moveFrom));
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorDown':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p13 = state.selection;
									if (_p13.ctor === 'Just') {
										return _elm_lang$core$Tuple$second(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p13._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								return A3(
									_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
									_SidneyNemzer$elm_text_editor$Buffer$Backward,
									buffer,
									_SidneyNemzer$elm_text_editor$Position$nextLine(moveFrom));
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorToLineEnd':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p14 = state.selection;
									if (_p14.ctor === 'Just') {
										return _elm_lang$core$Tuple$second(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p14._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								var _p15 = A2(_SidneyNemzer$elm_text_editor$Buffer$lineEnd, moveFrom.line, buffer);
								if (_p15.ctor === 'Just') {
									return A2(_SidneyNemzer$elm_text_editor$Position$setColumn, _p15._0, state.cursor);
								} else {
									return A3(_SidneyNemzer$elm_text_editor$Buffer$clampPosition, _SidneyNemzer$elm_text_editor$Buffer$Backward, buffer, state.cursor);
								}
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorToLineStart':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: function () {
								var moveFrom = function () {
									var _p16 = state.selection;
									if (_p16.ctor === 'Just') {
										return _elm_lang$core$Tuple$first(
											A2(_SidneyNemzer$elm_text_editor$Position$order, _p16._0, state.cursor));
									} else {
										return state.cursor;
									}
								}();
								return A2(_SidneyNemzer$elm_text_editor$Position$setColumn, 0, moveFrom);
							}(),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorToGroupEnd':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: A2(_SidneyNemzer$elm_text_editor$Buffer$groupEnd, state.cursor, buffer),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'CursorToGroupStart':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: A2(_SidneyNemzer$elm_text_editor$Buffer$groupStart, state.cursor, buffer),
							selection: _elm_lang$core$Maybe$Nothing
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'Insert':
				var _p21 = _p6._0;
				var _p17 = {
					ctor: '_Tuple2',
					_0: state.selection,
					_1: A2(_elm_lang$core$Dict$get, _p21, _SidneyNemzer$elm_text_editor$Editor_Update$autoclose)
				};
				if (_p17._0.ctor === 'Just') {
					if (_p17._1.ctor === 'Just') {
						var _p19 = _p17._0._0;
						var _p18 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p19, state.cursor);
						var start = _p18._0;
						var end = _p18._1;
						var wrapped = A2(
							_elm_lang$core$Basics_ops['++'],
							_p21,
							A2(
								_elm_lang$core$Basics_ops['++'],
								A3(_SidneyNemzer$elm_text_editor$Buffer$between, start, end, buffer),
								_p17._1._0));
						return A3(
							_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
							state,
							buffer,
							{
								ctor: '_Tuple3',
								_0: _elm_lang$core$Native_Utils.update(
									state,
									{
										cursor: _elm_lang$core$Native_Utils.eq(state.cursor.line, start.line) ? _SidneyNemzer$elm_text_editor$Position$nextColumn(state.cursor) : state.cursor,
										selection: _elm_lang$core$Maybe$Just(
											_elm_lang$core$Native_Utils.eq(_p19.line, start.line) ? _SidneyNemzer$elm_text_editor$Position$nextColumn(_p19) : _p19)
									}),
								_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, wrapped, buffer),
								_2: _elm_lang$core$Platform_Cmd$none
							});
					} else {
						var _p20 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p17._0._0, state.cursor);
						var start = _p20._0;
						var end = _p20._1;
						return A3(
							_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
							state,
							buffer,
							{
								ctor: '_Tuple3',
								_0: _elm_lang$core$Native_Utils.update(
									state,
									{
										cursor: _elm_lang$core$Native_Utils.eq(_p21, '\n') ? {line: start.line + 1, column: 0} : _SidneyNemzer$elm_text_editor$Position$nextColumn(start),
										selection: _elm_lang$core$Maybe$Nothing
									}),
								_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, _p21, buffer),
								_2: _elm_lang$core$Platform_Cmd$none
							});
					}
				} else {
					var nearWordChar = A2(_SidneyNemzer$elm_text_editor$Buffer$nearWordChar, state.cursor, buffer);
					var insertString = (!nearWordChar) ? A2(
						_elm_lang$core$Maybe$withDefault,
						_p21,
						A2(
							_elm_lang$core$Maybe$map,
							F2(
								function (x, y) {
									return A2(_elm_lang$core$Basics_ops['++'], x, y);
								})(_p21),
							_p17._1)) : _p21;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: _elm_lang$core$Native_Utils.eq(_p21, '\n') ? {line: state.cursor.line + 1, column: 0} : _SidneyNemzer$elm_text_editor$Position$nextColumn(state.cursor)
								}),
							_1: A3(_SidneyNemzer$elm_text_editor$Buffer$insert, state.cursor, insertString, buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'RemoveCharAfter':
				var _p22 = state.selection;
				if (_p22.ctor === 'Just') {
					var _p23 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p22._0, state.cursor);
					var start = _p23._0;
					var end = _p23._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{cursor: start, selection: _elm_lang$core$Maybe$Nothing}),
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: state,
							_1: A4(
								_SidneyNemzer$elm_text_editor$Buffer$replace,
								state.cursor,
								_SidneyNemzer$elm_text_editor$Position$nextColumn(state.cursor),
								'',
								buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'RemoveCharBefore':
				var _p24 = state.selection;
				if (_p24.ctor === 'Just') {
					var _p25 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p24._0, state.cursor);
					var start = _p25._0;
					var end = _p25._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{cursor: start, selection: _elm_lang$core$Maybe$Nothing}),
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: A3(
										_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
										_SidneyNemzer$elm_text_editor$Buffer$Backward,
										buffer,
										_SidneyNemzer$elm_text_editor$Position$previousColumn(state.cursor))
								}),
							_1: A2(_SidneyNemzer$elm_text_editor$Buffer$removeBefore, state.cursor, buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'RemoveGroupAfter':
				var _p26 = state.selection;
				if (_p26.ctor === 'Just') {
					var _p27 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p26._0, state.cursor);
					var start = _p27._0;
					var end = _p27._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{cursor: start, selection: _elm_lang$core$Maybe$Nothing}),
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					var end = A2(_SidneyNemzer$elm_text_editor$Buffer$groupEnd, state.cursor, buffer);
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: state,
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, state.cursor, end, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'RemoveGroupBefore':
				var _p28 = state.selection;
				if (_p28.ctor === 'Just') {
					var _p29 = A2(_SidneyNemzer$elm_text_editor$Position$order, _p28._0, state.cursor);
					var start = _p29._0;
					var end = _p29._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{cursor: start, selection: _elm_lang$core$Maybe$Nothing}),
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, end, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					var start = A2(_SidneyNemzer$elm_text_editor$Buffer$groupStart, state.cursor, buffer);
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{cursor: start}),
							_1: A4(_SidneyNemzer$elm_text_editor$Buffer$replace, start, state.cursor, '', buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'Indent':
				var _p30 = state.selection;
				if (_p30.ctor === 'Just') {
					var _p31 = _p30._0;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: A2(_SidneyNemzer$elm_text_editor$Position$addColumn, _SidneyNemzer$elm_text_editor$Buffer$indentSize, state.cursor),
									selection: _elm_lang$core$Maybe$Just(
										A2(_SidneyNemzer$elm_text_editor$Position$addColumn, _SidneyNemzer$elm_text_editor$Buffer$indentSize, _p31))
								}),
							_1: A3(_SidneyNemzer$elm_text_editor$Buffer$indentBetween, state.cursor, _p31, buffer),
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					var _p32 = A2(_SidneyNemzer$elm_text_editor$Buffer$indentFrom, state.cursor, buffer);
					var indentedBuffer = _p32._0;
					var indentedColumn = _p32._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: A2(_SidneyNemzer$elm_text_editor$Position$setColumn, indentedColumn, state.cursor)
								}),
							_1: indentedBuffer,
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'Deindent':
				var _p33 = state.selection;
				if (_p33.ctor === 'Just') {
					var _p35 = _p33._0;
					var _p34 = A3(_SidneyNemzer$elm_text_editor$Buffer$deindentBetween, state.cursor, _p35, buffer);
					var deindentedBuffer = _p34._0;
					var cursorColumn = _p34._1;
					var selectionColumn = _p34._2;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: A2(_SidneyNemzer$elm_text_editor$Position$setColumn, cursorColumn, state.cursor),
									selection: _elm_lang$core$Maybe$Just(
										A2(_SidneyNemzer$elm_text_editor$Position$setColumn, selectionColumn, _p35))
								}),
							_1: deindentedBuffer,
							_2: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					var _p36 = A2(_SidneyNemzer$elm_text_editor$Buffer$deindentFrom, state.cursor, buffer);
					var deindentedBuffer = _p36._0;
					var deindentedColumn = _p36._1;
					return A3(
						_SidneyNemzer$elm_text_editor$Editor_Update$pushHistory,
						state,
						buffer,
						{
							ctor: '_Tuple3',
							_0: _elm_lang$core$Native_Utils.update(
								state,
								{
									cursor: A2(_SidneyNemzer$elm_text_editor$Position$setColumn, deindentedColumn, state.cursor)
								}),
							_1: deindentedBuffer,
							_2: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'SelectUp':
				var cursor = A3(
					_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
					_SidneyNemzer$elm_text_editor$Buffer$Backward,
					buffer,
					_SidneyNemzer$elm_text_editor$Position$previousLine(state.cursor));
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectDown':
				var cursor = A3(
					_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
					_SidneyNemzer$elm_text_editor$Buffer$Backward,
					buffer,
					_SidneyNemzer$elm_text_editor$Position$nextLine(state.cursor));
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectLeft':
				var cursor = A3(
					_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
					_SidneyNemzer$elm_text_editor$Buffer$Backward,
					buffer,
					_SidneyNemzer$elm_text_editor$Position$previousColumn(state.cursor));
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectRight':
				var cursor = A3(
					_SidneyNemzer$elm_text_editor$Buffer$clampPosition,
					_SidneyNemzer$elm_text_editor$Buffer$Forward,
					buffer,
					_SidneyNemzer$elm_text_editor$Position$nextColumn(state.cursor));
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectToLineStart':
				var cursor = A2(_SidneyNemzer$elm_text_editor$Position$setColumn, 0, state.cursor);
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectToLineEnd':
				var cursor = A2(
					_SidneyNemzer$elm_text_editor$Position$setColumn,
					A2(
						_elm_lang$core$Maybe$withDefault,
						state.cursor.line,
						A2(_SidneyNemzer$elm_text_editor$Buffer$lineEnd, state.cursor.line, buffer)),
					state.cursor);
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectToGroupStart':
				var cursor = A2(_SidneyNemzer$elm_text_editor$Buffer$groupStart, state.cursor, buffer);
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectToGroupEnd':
				var cursor = A2(_SidneyNemzer$elm_text_editor$Buffer$groupEnd, state.cursor, buffer);
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: cursor,
							selection: _elm_lang$core$Native_Utils.eq(
								state.selection,
								_elm_lang$core$Maybe$Just(cursor)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.eq(state.selection, _elm_lang$core$Maybe$Nothing) && (!_elm_lang$core$Native_Utils.eq(state.cursor, cursor))) ? _elm_lang$core$Maybe$Just(state.cursor) : state.selection)
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectAll':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: _SidneyNemzer$elm_text_editor$Buffer$end(buffer),
							selection: _elm_lang$core$Maybe$Just(
								A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0))
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'SelectGroup':
				var range = A2(_SidneyNemzer$elm_text_editor$Buffer$groupRange, state.cursor, buffer);
				var _p37 = range;
				if (_p37.ctor === 'Just') {
					return {
						ctor: '_Tuple3',
						_0: _elm_lang$core$Native_Utils.update(
							state,
							{
								cursor: _p37._0._1,
								selection: _elm_lang$core$Maybe$Just(_p37._0._0)
							}),
						_1: buffer,
						_2: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple3', _0: state, _1: buffer, _2: _elm_lang$core$Platform_Cmd$none};
				}
			case 'SelectLine':
				return {
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_Utils.update(
						state,
						{
							cursor: A2(
								_elm_lang$core$Maybe$withDefault,
								state.cursor,
								A2(
									_elm_lang$core$Maybe$map,
									function (column) {
										return A2(_SidneyNemzer$elm_text_editor$Position$setColumn, column, state.cursor);
									},
									A2(_SidneyNemzer$elm_text_editor$Buffer$lineEnd, state.cursor.line, buffer))),
							selection: _elm_lang$core$Maybe$Just(
								A2(_SidneyNemzer$elm_text_editor$Position$setColumn, 0, state.cursor))
						}),
					_1: buffer,
					_2: _elm_lang$core$Platform_Cmd$none
				};
			case 'Undo':
				var _p38 = A2(
					_SidneyNemzer$elm_text_editor$Editor_History$undo,
					A2(_SidneyNemzer$elm_text_editor$Editor_Update$historyState, state, buffer),
					state.history);
				if (_p38._1.ctor === 'Just') {
					return {
						ctor: '_Tuple3',
						_0: _elm_lang$core$Native_Utils.update(
							state,
							{cursor: _p38._1._0.cursor, selection: _p38._1._0.selection, history: _p38._0}),
						_1: _p38._1._0.buffer,
						_2: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple3', _0: state, _1: buffer, _2: _elm_lang$core$Platform_Cmd$none};
				}
			default:
				var _p39 = A2(
					_SidneyNemzer$elm_text_editor$Editor_History$redo,
					A2(_SidneyNemzer$elm_text_editor$Editor_Update$historyState, state, buffer),
					state.history);
				if (_p39._1.ctor === 'Just') {
					return {
						ctor: '_Tuple3',
						_0: _elm_lang$core$Native_Utils.update(
							state,
							{cursor: _p39._1._0.cursor, selection: _p39._1._0.selection, history: _p39._0}),
						_1: _p39._1._0.buffer,
						_2: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple3', _0: state, _1: buffer, _2: _elm_lang$core$Platform_Cmd$none};
				}
		}
	});
var _SidneyNemzer$elm_text_editor$Editor_Update$Redo = {ctor: 'Redo'};
var _SidneyNemzer$elm_text_editor$Editor_Update$Undo = {ctor: 'Undo'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectLine = {ctor: 'SelectLine'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectGroup = {ctor: 'SelectGroup'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectAll = {ctor: 'SelectAll'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectToGroupEnd = {ctor: 'SelectToGroupEnd'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectToGroupStart = {ctor: 'SelectToGroupStart'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectToLineEnd = {ctor: 'SelectToLineEnd'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectToLineStart = {ctor: 'SelectToLineStart'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectRight = {ctor: 'SelectRight'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectLeft = {ctor: 'SelectLeft'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectDown = {ctor: 'SelectDown'};
var _SidneyNemzer$elm_text_editor$Editor_Update$SelectUp = {ctor: 'SelectUp'};
var _SidneyNemzer$elm_text_editor$Editor_Update$Deindent = {ctor: 'Deindent'};
var _SidneyNemzer$elm_text_editor$Editor_Update$Indent = {ctor: 'Indent'};
var _SidneyNemzer$elm_text_editor$Editor_Update$RemoveGroupBefore = {ctor: 'RemoveGroupBefore'};
var _SidneyNemzer$elm_text_editor$Editor_Update$RemoveGroupAfter = {ctor: 'RemoveGroupAfter'};
var _SidneyNemzer$elm_text_editor$Editor_Update$RemoveCharBefore = {ctor: 'RemoveCharBefore'};
var _SidneyNemzer$elm_text_editor$Editor_Update$RemoveCharAfter = {ctor: 'RemoveCharAfter'};
var _SidneyNemzer$elm_text_editor$Editor_Update$Insert = function (a) {
	return {ctor: 'Insert', _0: a};
};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorToGroupStart = {ctor: 'CursorToGroupStart'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorToGroupEnd = {ctor: 'CursorToGroupEnd'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorToLineStart = {ctor: 'CursorToLineStart'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorToLineEnd = {ctor: 'CursorToLineEnd'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorDown = {ctor: 'CursorDown'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorUp = {ctor: 'CursorUp'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorRight = {ctor: 'CursorRight'};
var _SidneyNemzer$elm_text_editor$Editor_Update$CursorLeft = {ctor: 'CursorLeft'};
var _SidneyNemzer$elm_text_editor$Editor_Update$MouseUp = {ctor: 'MouseUp'};
var _SidneyNemzer$elm_text_editor$Editor_Update$MouseOver = function (a) {
	return {ctor: 'MouseOver', _0: a};
};
var _SidneyNemzer$elm_text_editor$Editor_Update$MouseDown = function (a) {
	return {ctor: 'MouseDown', _0: a};
};

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _SidneyNemzer$elm_text_editor$Editor_Keymap$keymaps = {
	noModifier: _elm_lang$core$Dict$fromList(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'ArrowUp', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorUp},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ArrowDown', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorDown},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'ArrowLeft', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorLeft},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'ArrowRight', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorRight},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'Backspace', _1: _SidneyNemzer$elm_text_editor$Editor_Update$RemoveCharBefore},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'Delete', _1: _SidneyNemzer$elm_text_editor$Editor_Update$RemoveCharAfter},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'Enter',
										_1: _SidneyNemzer$elm_text_editor$Editor_Update$Insert('\n')
									},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'Home', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorToLineStart},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'End', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorToLineEnd},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'Tab', _1: _SidneyNemzer$elm_text_editor$Editor_Update$Indent},
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}),
	shift: _elm_lang$core$Dict$fromList(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'ArrowUp', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectUp},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ArrowDown', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectDown},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'ArrowLeft', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectLeft},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'ArrowRight', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectRight},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'Tab', _1: _SidneyNemzer$elm_text_editor$Editor_Update$Deindent},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'Home', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectToLineStart},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'End', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectToLineEnd},
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		}),
	control: _elm_lang$core$Dict$fromList(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'ArrowRight', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorToGroupEnd},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ArrowLeft', _1: _SidneyNemzer$elm_text_editor$Editor_Update$CursorToGroupStart},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Backspace', _1: _SidneyNemzer$elm_text_editor$Editor_Update$RemoveGroupBefore},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'Delete', _1: _SidneyNemzer$elm_text_editor$Editor_Update$RemoveGroupAfter},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'a', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectAll},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'd', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectGroup},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'z', _1: _SidneyNemzer$elm_text_editor$Editor_Update$Undo},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'y', _1: _SidneyNemzer$elm_text_editor$Editor_Update$Redo},
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}
		}),
	controlAndShift: _elm_lang$core$Dict$fromList(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'ArrowRight', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectToGroupEnd},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ArrowLeft', _1: _SidneyNemzer$elm_text_editor$Editor_Update$SelectToGroupStart},
				_1: {ctor: '[]'}
			}
		})
};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$Keydown = F5(
	function (a, b, c, d, e) {
		return {$char: a, key: b, ctrl: c, shift: d, alt: e};
	});
var _SidneyNemzer$elm_text_editor$Editor_Keymap$keydownDecoder = A6(
	_elm_lang$core$Json_Decode$map5,
	_SidneyNemzer$elm_text_editor$Editor_Keymap$Keydown,
	A2(
		_elm_lang$core$Json_Decode$map,
		function (key) {
			var _p0 = _elm_lang$core$String$uncons(key);
			if (((_p0.ctor === 'Just') && (_p0._0.ctor === '_Tuple2')) && (_p0._0._1 === '')) {
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$String$fromChar(_p0._0._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		},
		A2(_elm_lang$core$Json_Decode$field, 'key', _elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'key', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'ctrlKey', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'shiftKey', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'altKey', _elm_lang$core$Json_Decode$bool));
var _SidneyNemzer$elm_text_editor$Editor_Keymap$ControlAndShift = {ctor: 'ControlAndShift'};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$Shift = {ctor: 'Shift'};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$Control = {ctor: 'Control'};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$None = {ctor: 'None'};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$modifier = F2(
	function (ctrl, shift) {
		var _p1 = {ctor: '_Tuple2', _0: ctrl, _1: shift};
		if (_p1._0 === true) {
			if (_p1._1 === true) {
				return _SidneyNemzer$elm_text_editor$Editor_Keymap$ControlAndShift;
			} else {
				return _SidneyNemzer$elm_text_editor$Editor_Keymap$Control;
			}
		} else {
			if (_p1._1 === true) {
				return _SidneyNemzer$elm_text_editor$Editor_Keymap$Shift;
			} else {
				return _SidneyNemzer$elm_text_editor$Editor_Keymap$None;
			}
		}
	});
var _SidneyNemzer$elm_text_editor$Editor_Keymap$keyToMsg = function (event) {
	var keyFrom = function (keymap) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_elm_lang$core$Json_Decode$fail('This key does nothing'),
			A2(
				_elm_lang$core$Maybe$map,
				_elm_lang$core$Json_Decode$succeed,
				A2(_elm_lang$core$Dict$get, event.key, keymap)));
	};
	var keyOrCharFrom = function (keymap) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: keyFrom(keymap),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$fail('This key does nothing'),
						A2(
							_elm_lang$core$Maybe$map,
							function (_p2) {
								return _elm_lang$core$Json_Decode$succeed(
									_SidneyNemzer$elm_text_editor$Editor_Update$Insert(_p2));
							},
							event.$char)),
					_1: {ctor: '[]'}
				}
			});
	};
	var _p3 = A2(_SidneyNemzer$elm_text_editor$Editor_Keymap$modifier, event.ctrl, event.shift);
	switch (_p3.ctor) {
		case 'None':
			return keyOrCharFrom(_SidneyNemzer$elm_text_editor$Editor_Keymap$keymaps.noModifier);
		case 'Control':
			return keyFrom(_SidneyNemzer$elm_text_editor$Editor_Keymap$keymaps.control);
		case 'Shift':
			return keyOrCharFrom(_SidneyNemzer$elm_text_editor$Editor_Keymap$keymaps.shift);
		default:
			return keyFrom(_SidneyNemzer$elm_text_editor$Editor_Keymap$keymaps.controlAndShift);
	}
};
var _SidneyNemzer$elm_text_editor$Editor_Keymap$decoder = A2(_elm_lang$core$Json_Decode$andThen, _SidneyNemzer$elm_text_editor$Editor_Keymap$keyToMsg, _SidneyNemzer$elm_text_editor$Editor_Keymap$keydownDecoder);

var _SidneyNemzer$elm_text_editor$Editor_View$onTripleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		A2(
			_elm_lang$core$Json_Decode$andThen,
			function (detail) {
				return (_elm_lang$core$Native_Utils.cmp(detail, 3) > -1) ? _elm_lang$core$Json_Decode$succeed(msg) : _elm_lang$core$Json_Decode$fail('');
			},
			A2(_elm_lang$core$Json_Decode$field, 'detail', _elm_lang$core$Json_Decode$int)));
};
var _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseOver = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'mouseover',
		{preventDefault: false, stopPropagation: true},
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseDown = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'mousedown',
		{preventDefault: false, stopPropagation: true},
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _SidneyNemzer$elm_text_editor$Editor_View$nbsp = _elm_lang$core$Char$fromCode(160);
var _SidneyNemzer$elm_text_editor$Editor_View$ensureNbsp = function ($char) {
	return _elm_lang$core$Native_Utils.eq(
		$char,
		_elm_lang$core$Native_Utils.chr(' ')) ? _SidneyNemzer$elm_text_editor$Editor_View$nbsp : $char;
};
var _SidneyNemzer$elm_text_editor$Editor_View$selected = F3(
	function (cursor, maybeSelection, character) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			false,
			A2(
				_elm_lang$core$Maybe$map,
				function (selection) {
					return A3(_SidneyNemzer$elm_text_editor$Position$between, cursor, selection, character);
				},
				maybeSelection));
	});
var _SidneyNemzer$elm_text_editor$Editor_View$name = 'elm-editor';
var _SidneyNemzer$elm_text_editor$Editor_View$character = F4(
	function (cursor, selection, position, $char) {
		var hasCursor = _elm_lang$core$Native_Utils.eq(cursor, position);
		return A2(
			_elm_lang$html$Html$span,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__character'),
							_1: true
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__character--has-cursor'),
								_1: hasCursor
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__character--selected'),
									_1: A3(_SidneyNemzer$elm_text_editor$Editor_View$selected, cursor, selection, position)
								},
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {
					ctor: '::',
					_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseDown(
						_SidneyNemzer$elm_text_editor$Editor_Update$MouseDown(position)),
					_1: {
						ctor: '::',
						_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseOver(
							_SidneyNemzer$elm_text_editor$Editor_Update$MouseOver(position)),
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(
					_elm_lang$core$String$fromChar(
						_SidneyNemzer$elm_text_editor$Editor_View$ensureNbsp($char))),
				_1: {
					ctor: '::',
					_0: hasCursor ? A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(
								A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-cursor')),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(' '),
							_1: {ctor: '[]'}
						}) : _elm_lang$html$Html$text(''),
					_1: {ctor: '[]'}
				}
			});
	});
var _SidneyNemzer$elm_text_editor$Editor_View$line = F4(
	function (cursor, selection, number, content) {
		var start = A2(_SidneyNemzer$elm_text_editor$Position$Position, number, 0);
		var end = A2(
			_SidneyNemzer$elm_text_editor$Position$Position,
			number,
			_elm_lang$core$String$length(content));
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class(
					A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line')),
				_1: {
					ctor: '::',
					_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseDown(
						_SidneyNemzer$elm_text_editor$Editor_Update$MouseDown(end)),
					_1: {
						ctor: '::',
						_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseOver(
							_SidneyNemzer$elm_text_editor$Editor_Update$MouseOver(end)),
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__number')),
						_1: {
							ctor: '::',
							_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseDown(
								_SidneyNemzer$elm_text_editor$Editor_Update$MouseDown(start)),
							_1: {
								ctor: '::',
								_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseOver(
									_SidneyNemzer$elm_text_editor$Editor_Update$MouseOver(start)),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(number + 1)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(
								A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__gutter-padding')),
							_1: {
								ctor: '::',
								_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseDown(
									_SidneyNemzer$elm_text_editor$Editor_Update$MouseDown(start)),
								_1: {
									ctor: '::',
									_0: _SidneyNemzer$elm_text_editor$Editor_View$captureOnMouseOver(
										_SidneyNemzer$elm_text_editor$Editor_Update$MouseOver(start)),
									_1: {ctor: '[]'}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								_elm_lang$core$String$fromChar(_SidneyNemzer$elm_text_editor$Editor_View$nbsp)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(
									A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__content')),
								_1: {ctor: '[]'}
							},
							A2(
								_elm_lang$core$List$indexedMap,
								function (_p0) {
									return A3(
										_SidneyNemzer$elm_text_editor$Editor_View$character,
										cursor,
										selection,
										A2(_SidneyNemzer$elm_text_editor$Position$Position, number, _p0));
								},
								_elm_lang$core$String$toList(content))),
						_1: {
							ctor: '::',
							_0: (_elm_lang$core$Native_Utils.eq(cursor.line, number) && (_elm_lang$core$Native_Utils.cmp(
								cursor.column,
								_elm_lang$core$String$length(content)) > -1)) ? A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class(
										A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__character')),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class(
											A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-line__character--has-cursor')),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(' '),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$span,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class(
													A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-cursor')),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(' '),
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								}) : _elm_lang$html$Html$text(''),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _SidneyNemzer$elm_text_editor$Editor_View$view = F2(
	function (lines, state) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class(
					A2(_elm_lang$core$Basics_ops['++'], _SidneyNemzer$elm_text_editor$Editor_View$name, '-container')),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$html$Html_Events$onWithOptions,
						'keydown',
						{preventDefault: true, stopPropagation: false},
						_SidneyNemzer$elm_text_editor$Editor_Keymap$decoder),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseUp(_SidneyNemzer$elm_text_editor$Editor_Update$MouseUp),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onDoubleClick(_SidneyNemzer$elm_text_editor$Editor_Update$SelectGroup),
							_1: {
								ctor: '::',
								_0: _SidneyNemzer$elm_text_editor$Editor_View$onTripleClick(_SidneyNemzer$elm_text_editor$Editor_Update$SelectLine),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$tabindex(0),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			A2(
				_elm_lang$core$List$indexedMap,
				A2(_SidneyNemzer$elm_text_editor$Editor_View$line, state.cursor, state.selection),
				lines));
	});

var _SidneyNemzer$elm_text_editor$Editor$view = F2(
	function (buffer, _p0) {
		var _p1 = _p0;
		return A2(
			_SidneyNemzer$elm_text_editor$Editor_View$view,
			_SidneyNemzer$elm_text_editor$Buffer$lines(buffer),
			_p1._0);
	});
var _SidneyNemzer$elm_text_editor$Editor$State = function (a) {
	return {ctor: 'State', _0: a};
};
var _SidneyNemzer$elm_text_editor$Editor$init = _SidneyNemzer$elm_text_editor$Editor$State(
	{
		scrolledLine: 0,
		cursor: A2(_SidneyNemzer$elm_text_editor$Position$Position, 0, 0),
		selection: _elm_lang$core$Maybe$Nothing,
		dragging: false,
		history: _SidneyNemzer$elm_text_editor$Editor_History$empty
	});
var _SidneyNemzer$elm_text_editor$Editor$update = F3(
	function (buffer, msg, _p2) {
		var _p3 = _p2;
		return function (_p4) {
			var _p5 = _p4;
			return {
				ctor: '_Tuple3',
				_0: _SidneyNemzer$elm_text_editor$Editor$State(_p5._0),
				_1: _p5._1,
				_2: _p5._2
			};
		}(
			A3(_SidneyNemzer$elm_text_editor$Editor_Update$update, buffer, msg, _p3._0));
	});

var _SidneyNemzer$elm_text_editor$Editor_Styles$styleText = '\n.elm-editor-container {\n  font-family: monospace;\n  border: 1px solid lightgray;\n  width: 700px;\n  user-select: none;\n}\n\n.elm-editor-container:focus {\n  outline: none;\n}\n\n.elm-editor-line {\n  cursor: text;\n}\n\n.elm-editor-line__number {\n  display: inline-block;\n  width: 20px;\n  padding-right: 5px;\n  text-align: right;\n  background-color: lightgray;\n  cursor: default;\n}\n\n.elm-editor-line__gutter-padding {\n  width: 5px;\n}\n\n.elm-editor-line__character--has-cursor {\n  position: relative;\n}\n\n.elm-editor-line__character--selected {\n  background-color: cornflowerblue;\n  color: white;\n}\n\n.elm-editor-cursor {\n  position: absolute;\n  border-left: 2px solid #adadad;\n  left: 0;\n  height: 100%;\n}\n\n.elm-editor-container:focus .elm-editor-cursor {\n  animation: 1s blink step-start infinite;\n  border-left: 2px solid #333333;\n}\n\n@keyframes blink {\n  from, to {\n    opacity: 0;\n  }\n  50% {\n    opacity: 1;\n  }\n}\n';
var _SidneyNemzer$elm_text_editor$Editor_Styles$style = _elm_lang$html$Html$node('style');
var _SidneyNemzer$elm_text_editor$Editor_Styles$styles = A2(
	_SidneyNemzer$elm_text_editor$Editor_Styles$style,
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$html$Html$text(_SidneyNemzer$elm_text_editor$Editor_Styles$styleText),
		_1: {ctor: '[]'}
	});

var _SidneyNemzer$elm_text_editor$Main$keyDecoder = function (keyToMsg) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		keyToMsg,
		A2(_elm_lang$core$Json_Decode$field, 'key', _elm_lang$core$Json_Decode$string));
};
var _SidneyNemzer$elm_text_editor$Main$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$none;
};
var _SidneyNemzer$elm_text_editor$Main$init = A2(
	_elm_lang$core$Platform_Cmd_ops['!'],
	{
		content: _SidneyNemzer$elm_text_editor$Buffer$init('listMapAt : (a -> a) -> Int -> List a -> List a\nlistMapAt fn index list =\n  List.Extra.getAt index list\n    |> Maybe.map (\\a -> List.Extra.setAt index (fn a) list)\n    |> Maybe.withDefault list'),
		editor: _SidneyNemzer$elm_text_editor$Editor$init,
		lastKeyPress: _elm_lang$core$Maybe$Nothing
	},
	{ctor: '[]'});
var _SidneyNemzer$elm_text_editor$Main$Model = F3(
	function (a, b, c) {
		return {content: a, editor: b, lastKeyPress: c};
	});
var _SidneyNemzer$elm_text_editor$Main$KeyPress = function (a) {
	return {ctor: 'KeyPress', _0: a};
};
var _SidneyNemzer$elm_text_editor$Main$EditorMsg = function (a) {
	return {ctor: 'EditorMsg', _0: a};
};
var _SidneyNemzer$elm_text_editor$Main$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'EditorMsg') {
			var _p1 = A3(_SidneyNemzer$elm_text_editor$Editor$update, model.content, _p0._0, model.editor);
			var editor = _p1._0;
			var content = _p1._1;
			var cmd = _p1._2;
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{editor: editor, content: content}),
				{
					ctor: '::',
					_0: A2(_elm_lang$core$Platform_Cmd$map, _SidneyNemzer$elm_text_editor$Main$EditorMsg, cmd),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						lastKeyPress: _elm_lang$core$Maybe$Just(_p0._0)
					}),
				{ctor: '[]'});
		}
	});
var _SidneyNemzer$elm_text_editor$Main$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html_Events$on,
				'keydown',
				_SidneyNemzer$elm_text_editor$Main$keyDecoder(_SidneyNemzer$elm_text_editor$Main$KeyPress)),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _SidneyNemzer$elm_text_editor$Editor_Styles$styles,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$map,
					_SidneyNemzer$elm_text_editor$Main$EditorMsg,
					A2(_SidneyNemzer$elm_text_editor$Editor$view, model.content, model.editor)),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$details,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$summary,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('Debug'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_elm_lang$core$Basics$toString(model.editor)),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: function () {
										var _p2 = model.lastKeyPress;
										if (_p2.ctor === 'Just') {
											return A2(
												_elm_lang$html$Html$div,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(
														A2(_elm_lang$core$Basics_ops['++'], 'Last key press: ', _p2._0)),
													_1: {ctor: '[]'}
												});
										} else {
											return _elm_lang$html$Html$text('');
										}
									}(),
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _SidneyNemzer$elm_text_editor$Main$main = _elm_lang$html$Html$program(
	{init: _SidneyNemzer$elm_text_editor$Main$init, view: _SidneyNemzer$elm_text_editor$Main$view, update: _SidneyNemzer$elm_text_editor$Main$update, subscriptions: _SidneyNemzer$elm_text_editor$Main$subscriptions})();

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _SidneyNemzer$elm_text_editor$Main$main !== 'undefined') {
    _SidneyNemzer$elm_text_editor$Main$main(Elm['Main'], 'Main', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

