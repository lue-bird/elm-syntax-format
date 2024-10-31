(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
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

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
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


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

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


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = {$: 'GT'};
var $author$project$Top$Normal = {$: 'Normal'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Top$init = function (_v0) {
	return _Utils_Tuple2($author$project$Top$Normal, $elm$core$Platform$Cmd$none);
};
var $author$project$Top$ModelData = F2(
	function (a, b) {
		return {$: 'ModelData', a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $author$project$Top$modelInPort = _Platform_incomingPort(
	'modelInPort',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return $elm$json$Json$Decode$succeed(
						_Utils_Tuple2(_v0, _v1));
				},
				A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)));
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Top$subscriptions = function (model) {
	if (model.$ === 'Error') {
		return $elm$core$Platform$Sub$none;
	} else {
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					$author$project$Top$modelInPort(
					function (_v1) {
						var name = _v1.a;
						var value = _v1.b;
						return A2($author$project$Top$ModelData, name, value);
					})
				]));
	}
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Top$codeOutPort = _Platform_outgoingPort(
	'codeOutPort',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(a),
					$elm$json$Json$Encode$string(b)
				]));
	});
var $elm$core$Debug$log = _Debug_log;
var $stil4m$elm_syntax$Elm$Syntax$Node$Node = F2(
	function (a, b) {
		return {$: 'Node', a: a, b: b};
	});
var $elm$core$Basics$compare = _Utils_compare;
var $stil4m$elm_syntax$Elm$Syntax$Range$compareLocations = F2(
	function (left, right) {
		return (_Utils_cmp(left.row, right.row) < 0) ? $elm$core$Basics$LT : ((_Utils_cmp(left.row, right.row) > 0) ? $elm$core$Basics$GT : A2($elm$core$Basics$compare, left.column, right.column));
	});
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$value = function (_v0) {
	var v = _v0.b;
	return v;
};
var $author$project$ElmSyntaxPrint$commentsAfter = F2(
	function (end, sortedComments) {
		commentsAfter:
		while (true) {
			if (!sortedComments.b) {
				return _List_Nil;
			} else {
				var _v1 = sortedComments.a;
				var headCommentRange = _v1.a;
				var headComment = _v1.b;
				var tailComments = sortedComments.b;
				var _v2 = A2($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations, headCommentRange.start, end);
				switch (_v2.$) {
					case 'LT':
						var $temp$end = end,
							$temp$sortedComments = tailComments;
						end = $temp$end;
						sortedComments = $temp$sortedComments;
						continue commentsAfter;
					case 'GT':
						return A2(
							$elm$core$List$cons,
							headComment,
							A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, tailComments));
					default:
						return A2(
							$elm$core$List$cons,
							headComment,
							A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, tailComments));
				}
			}
		}
	});
var $author$project$ElmSyntaxPrint$commentsInRange = F2(
	function (range, sortedComments) {
		commentsInRange:
		while (true) {
			if (!sortedComments.b) {
				return _List_Nil;
			} else {
				var _v1 = sortedComments.a;
				var headCommentRange = _v1.a;
				var headComment = _v1.b;
				var tailComments = sortedComments.b;
				var _v2 = A2($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations, headCommentRange.start, range.start);
				switch (_v2.$) {
					case 'LT':
						var $temp$range = range,
							$temp$sortedComments = tailComments;
						range = $temp$range;
						sortedComments = $temp$sortedComments;
						continue commentsInRange;
					case 'EQ':
						return A2(
							$elm$core$List$cons,
							headComment,
							A2($author$project$ElmSyntaxPrint$commentsInRange, range, tailComments));
					default:
						var _v3 = A2($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations, headCommentRange.end, range.end);
						switch (_v3.$) {
							case 'GT':
								return _List_Nil;
							case 'LT':
								return A2(
									$elm$core$List$cons,
									headComment,
									A2($author$project$ElmSyntaxPrint$commentsInRange, range, tailComments));
							default:
								return A2(
									$elm$core$List$cons,
									headComment,
									A2($author$project$ElmSyntaxPrint$commentsInRange, range, tailComments));
						}
				}
			}
		}
	});
var $author$project$Print$MultipleLines = {$: 'MultipleLines'};
var $author$project$Print$SingleLine = {$: 'SingleLine'};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $author$project$Print$exactly = function (exactNextString) {
	return function (_v0) {
		return exactNextString;
	};
};
var $author$project$Print$followedBy = F2(
	function (nextPrint, soFarPrint) {
		return function (indent) {
			return _Utils_ap(
				soFarPrint(indent),
				nextPrint(indent));
		};
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $author$project$Print$linebreakIndented = function (state) {
	return '\n' + A2($elm$core$String$repeat, state.indent, ' ');
};
var $elm$core$String$lines = _String_lines;
var $author$project$ElmSyntaxPrint$listDropLastIfIs = F2(
	function (lastElementShouldBeRemoved, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var onlyElement = list.a;
				return lastElementShouldBeRemoved(onlyElement) ? _List_Nil : _List_fromArray(
					[onlyElement]);
			} else {
				var element0 = list.a;
				var _v1 = list.b;
				var element1 = _v1.a;
				var element2Up = _v1.b;
				return A2(
					$elm$core$List$cons,
					element0,
					A2(
						$author$project$ElmSyntaxPrint$listDropLastIfIs,
						lastElementShouldBeRemoved,
						A2($elm$core$List$cons, element1, element2Up)));
			}
		}
	});
var $author$project$Print$empty = $author$project$Print$exactly('');
var $author$project$Print$sequence = function (printSequence) {
	if (!printSequence.b) {
		return $author$project$Print$empty;
	} else {
		var head = printSequence.a;
		var tail = printSequence.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (next, soFar) {
					return A2($author$project$Print$followedBy, next, soFar);
				}),
			head,
			tail);
	}
};
var $author$project$Print$space = $author$project$Print$exactly(' ');
var $elm$core$String$startsWith = _String_startsWith;
var $elm$core$String$trim = _String_trim;
var $elm$core$String$trimLeft = _String_trimLeft;
var $elm$core$String$trimRight = _String_trimRight;
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$String$foldl = _String_foldl;
var $author$project$ElmSyntaxPrint$lineIndentation = function (line) {
	return A3(
		$elm$core$String$foldl,
		F2(
			function (_char, soFar) {
				if (soFar.onlySpaces) {
					if (' ' === _char.valueOf()) {
						return {onlySpaces: true, spaceCount: soFar.spaceCount + 1};
					} else {
						return {onlySpaces: false, spaceCount: soFar.spaceCount};
					}
				} else {
					return soFar;
				}
			}),
		{onlySpaces: true, spaceCount: 0},
		line).spaceCount;
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ElmSyntaxPrint$unindent = function (lines) {
	var nonBlankLines = A2(
		$elm$core$List$filterMap,
		function (line) {
			var _v1 = $elm$core$String$trim(line);
			if (_v1 === '') {
				return $elm$core$Maybe$Nothing;
			} else {
				return $elm$core$Maybe$Just(line);
			}
		},
		lines);
	var _v0 = $elm$core$List$minimum(
		A2($elm$core$List$map, $author$project$ElmSyntaxPrint$lineIndentation, nonBlankLines));
	if (_v0.$ === 'Nothing') {
		return lines;
	} else {
		var minimumIndentation = _v0.a;
		return A2(
			$elm$core$List$map,
			function (line) {
				return A2($elm$core$String$dropLeft, minimumIndentation, line);
			},
			lines);
	}
};
var $author$project$ElmSyntaxPrint$comment = function (syntaxComment) {
	if (syntaxComment === '{--}') {
		return $author$project$Print$exactly('{--}');
	} else {
		var nonDirectlyClosingMultiLineComment = syntaxComment;
		if (A2($elm$core$String$startsWith, '--', nonDirectlyClosingMultiLineComment)) {
			return $author$project$Print$exactly(
				$elm$core$String$trimRight(nonDirectlyClosingMultiLineComment));
		} else {
			var commentContentLines = $elm$core$String$lines(
				A2(
					$elm$core$String$dropRight,
					2,
					A2($elm$core$String$dropLeft, 2, nonDirectlyClosingMultiLineComment)));
			var commentContentNormal = function () {
				if (!commentContentLines.b) {
					return _List_Nil;
				} else {
					var commentContentLine0 = commentContentLines.a;
					var commentContentLine1Up = commentContentLines.b;
					return A2(
						$elm$core$List$map,
						$elm$core$String$trimRight,
						A2(
							$elm$core$List$cons,
							$elm$core$String$trimLeft(commentContentLine0),
							$author$project$ElmSyntaxPrint$unindent(
								A2(
									$author$project$ElmSyntaxPrint$listDropLastIfIs,
									function (line) {
										var _v6 = $elm$core$String$trim(line);
										if (_v6 === '') {
											return true;
										} else {
											return false;
										}
									},
									commentContentLine1Up))));
				}
			}();
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('-}'),
				A2(
					$author$project$Print$followedBy,
					function () {
						if (!commentContentNormal.b) {
							return $author$project$Print$exactly('  ');
						} else {
							if (!commentContentNormal.b.b) {
								var singleLine = commentContentNormal.a;
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly(singleLine),
										$author$project$Print$space));
							} else {
								var firstLine = commentContentNormal.a;
								var _v2 = commentContentNormal.b;
								var secondLine = _v2.a;
								var thirdLineUp = _v2.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$sequence(
										A2(
											$elm$core$List$map,
											function (line) {
												if (line === '') {
													return $author$project$Print$linebreakIndented;
												} else {
													var lineNotEmpty = line;
													return A2(
														$author$project$Print$followedBy,
														$author$project$Print$linebreakIndented,
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$exactly(lineNotEmpty),
															$author$project$Print$exactly('   ')));
												}
											},
											A2($elm$core$List$cons, secondLine, thirdLineUp))),
									function () {
										if (firstLine === '') {
											return $author$project$Print$linebreakIndented;
										} else {
											var lineNotEmpty = firstLine;
											return A2(
												$author$project$Print$followedBy,
												$author$project$Print$linebreakIndented,
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$exactly(lineNotEmpty),
													$author$project$Print$space));
										}
									}());
							}
						}
					}(),
					$author$project$Print$exactly('{-')));
		}
	}
};
var $elm$core$String$contains = _String_contains;
var $author$project$ElmSyntaxPrint$commentCanBePartOfCollapsible = function (syntaxComment) {
	if (syntaxComment === '{--}') {
		return false;
	} else {
		var commentNotDirectlyClosed = syntaxComment;
		return A2($elm$core$String$startsWith, '{-', commentNotDirectlyClosed) && (!A2($elm$core$String$contains, '\n', commentNotDirectlyClosed));
	}
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $author$project$ElmSyntaxPrint$comments = function (syntaxComments) {
	return $author$project$Print$sequence(
		A2(
			$elm$core$List$intersperse,
			$author$project$Print$linebreakIndented,
			A2(
				$elm$core$List$map,
				function (syntaxComment) {
					return $author$project$ElmSyntaxPrint$comment(syntaxComment);
				},
				syntaxComments)));
};
var $author$project$Print$toString = function (print) {
	return print(
		{indent: 0});
};
var $author$project$ElmSyntaxPrint$collapsibleComments = function (commentsToPrint) {
	if (!commentsToPrint.b) {
		return {lineSpread: $author$project$Print$SingleLine, print: $author$project$Print$empty};
	} else {
		var comment0 = commentsToPrint.a;
		var comment1Up = commentsToPrint.b;
		var commentPrints = A2(
			$elm$core$List$map,
			$author$project$ElmSyntaxPrint$comment,
			A2($elm$core$List$cons, comment0, comment1Up));
		return A2(
			$elm$core$List$all,
			function (commentPrint) {
				return $author$project$ElmSyntaxPrint$commentCanBePartOfCollapsible(
					$author$project$Print$toString(commentPrint));
			},
			commentPrints) ? {
			lineSpread: $author$project$Print$SingleLine,
			print: $author$project$Print$sequence(
				A2($elm$core$List$intersperse, $author$project$Print$space, commentPrints))
		} : {
			lineSpread: $author$project$Print$MultipleLines,
			print: $author$project$ElmSyntaxPrint$comments(
				A2($elm$core$List$cons, comment0, comment1Up))
		};
	}
};
var $author$project$Print$linebreak = $author$project$Print$exactly('\n');
var $author$project$ElmSyntaxPrint$moduleLevelComments = function (syntaxComments) {
	if (!syntaxComments.b) {
		return $author$project$Print$empty;
	} else {
		var comment0 = syntaxComments.a;
		var comment1Up = syntaxComments.b;
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$sequence(
				A2(
					$elm$core$List$map,
					function (syntaxComment) {
						if (syntaxComment === '{--}') {
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$comment('{--}'),
									A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
						} else {
							var notEmptyMultiLineComment = syntaxComment;
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								$author$project$ElmSyntaxPrint$comment(notEmptyMultiLineComment));
						}
					},
					comment1Up)),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$linebreak,
				$author$project$ElmSyntaxPrint$comment(comment0)));
	}
};
var $author$project$ElmSyntaxPrint$commentsBetweenDocumentationAndDeclaration = function (syntaxComments) {
	if (!syntaxComments.b) {
		return $author$project$Print$empty;
	} else {
		var comment0 = syntaxComments.a;
		var comment1Up = syntaxComments.b;
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$linebreak,
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$linebreak,
				A2(
					$author$project$Print$followedBy,
					$author$project$ElmSyntaxPrint$moduleLevelComments(
						A2($elm$core$List$cons, comment0, comment1Up)),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)))));
	}
};
var $author$project$Print$lineSpread = function (print) {
	return A2(
		$elm$core$String$contains,
		'\n',
		$author$project$Print$toString(print)) ? $author$project$Print$MultipleLines : $author$project$Print$SingleLine;
};
var $author$project$Print$lineSpreadMerge = F2(
	function (aLineSpread, bLineSpread) {
		if (aLineSpread.$ === 'MultipleLines') {
			return $author$project$Print$MultipleLines;
		} else {
			return bLineSpread;
		}
	});
var $author$project$Print$lineSpreadsCombine = function (lineSpreads) {
	lineSpreadsCombine:
	while (true) {
		if (!lineSpreads.b) {
			return $author$project$Print$SingleLine;
		} else {
			var head = lineSpreads.a;
			var tail = lineSpreads.b;
			if (head.$ === 'MultipleLines') {
				return $author$project$Print$MultipleLines;
			} else {
				var $temp$lineSpreads = tail;
				lineSpreads = $temp$lineSpreads;
				continue lineSpreadsCombine;
			}
		}
	}
};
var $author$project$Print$mapAndLineSpreadsCombine = F2(
	function (elementLineSpread, lineSpreads) {
		mapAndLineSpreadsCombine:
		while (true) {
			if (!lineSpreads.b) {
				return $author$project$Print$SingleLine;
			} else {
				var head = lineSpreads.a;
				var tail = lineSpreads.b;
				var _v1 = elementLineSpread(head);
				if (_v1.$ === 'MultipleLines') {
					return $author$project$Print$MultipleLines;
				} else {
					var $temp$elementLineSpread = elementLineSpread,
						$temp$lineSpreads = tail;
					elementLineSpread = $temp$elementLineSpread;
					lineSpreads = $temp$lineSpreads;
					continue mapAndLineSpreadsCombine;
				}
			}
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$range = function (_v0) {
	var r = _v0.a;
	return r;
};
var $author$project$Print$spaceOrLinebreakIndented = function (lineSpreadToUse) {
	if (lineSpreadToUse.$ === 'SingleLine') {
		return $author$project$Print$space;
	} else {
		return $author$project$Print$linebreakIndented;
	}
};
var $author$project$Print$withIndentAtNextMultipleOf4 = function (print) {
	return function (soFarState) {
		return print(
			{indent: (((soFarState.indent / 4) | 0) * 4) + 4});
	};
};
var $author$project$ElmSyntaxPrint$construct = F3(
	function (printArgumentParenthesizedIfSpaceSeparated, syntaxComments, syntaxConstruct) {
		var commentsBeforeArguments = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (argument, soFar) {
						var commentsBeforeArgument = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{
								end: $stil4m$elm_syntax$Elm$Syntax$Node$range(argument).start,
								start: soFar.previousEnd
							},
							syntaxComments);
						return {
							previousEnd: $stil4m$elm_syntax$Elm$Syntax$Node$range(argument).end,
							resultReverse: A2(
								$elm$core$List$cons,
								function () {
									if (!commentsBeforeArgument.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeArgument.a;
										var comment1Up = commentsBeforeArgument.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								soFar.resultReverse)
						};
					}),
				{previousEnd: syntaxConstruct.fullRange.start, resultReverse: _List_Nil},
				syntaxConstruct._arguments).resultReverse);
		var argumentPrints = A2(
			$elm$core$List$map,
			function (argument) {
				return A2(printArgumentParenthesizedIfSpaceSeparated, syntaxComments, argument);
			},
			syntaxConstruct._arguments);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$Print$lineSpread(syntaxConstruct.start),
					A2(
					$author$project$Print$mapAndLineSpreadsCombine,
					function ($) {
						return $.lineSpread;
					},
					A2($elm$core$List$filterMap, $elm$core$Basics$identity, commentsBeforeArguments)),
					A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, argumentPrints)
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				$author$project$Print$sequence(
					A3(
						$elm$core$List$map2,
						F2(
							function (argumentPrint, maybeCommentsBeforeArgument) {
								return A2(
									$author$project$Print$followedBy,
									argumentPrint,
									A2(
										$author$project$Print$followedBy,
										function () {
											if (maybeCommentsBeforeArgument.$ === 'Nothing') {
												return $author$project$Print$empty;
											} else {
												var commentsBeforeArgument = maybeCommentsBeforeArgument.a;
												return A2(
													$author$project$Print$followedBy,
													$author$project$Print$spaceOrLinebreakIndented(
														A2(
															$author$project$Print$lineSpreadMerge,
															commentsBeforeArgument.lineSpread,
															$author$project$Print$lineSpread(argumentPrint))),
													commentsBeforeArgument.print);
											}
										}(),
										$author$project$Print$spaceOrLinebreakIndented(lineSpread)));
							}),
						argumentPrints,
						commentsBeforeArguments))),
			syntaxConstruct.start);
	});
var $author$project$Print$emptyOrLinebreakIndented = function (lineSpreadToUse) {
	if (lineSpreadToUse.$ === 'SingleLine') {
		return $author$project$Print$empty;
	} else {
		return $author$project$Print$linebreakIndented;
	}
};
var $author$project$ElmSyntaxPrint$lineSpreadInRange = function (range) {
	return _Utils_eq(range.start.row, range.end.row) ? $author$project$Print$SingleLine : $author$project$Print$MultipleLines;
};
var $author$project$Print$withIndentIncreasedBy = F2(
	function (indentationIncrease, print) {
		return function (soFarState) {
			return print(
				{indent: soFarState.indent + indentationIncrease});
		};
	});
var $author$project$ElmSyntaxPrint$invalidNTuple = F3(
	function (printPartNotParenthesized, syntaxComments, syntaxTuple) {
		var lineSpread = $author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxTuple.fullRange);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(')'),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$spaceOrLinebreakIndented(lineSpread),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A2(
							$elm$core$List$intersperse,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly(','),
									$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
							A2(
								$elm$core$List$map,
								function (part) {
									return A2(
										$author$project$Print$withIndentIncreasedBy,
										2,
										A2(printPartNotParenthesized, syntaxComments, part));
								},
								A2(
									$elm$core$List$cons,
									syntaxTuple.part0,
									A2(
										$elm$core$List$cons,
										syntaxTuple.part1,
										A2(
											$elm$core$List$cons,
											syntaxTuple.part2,
											A2($elm$core$List$cons, syntaxTuple.part3, syntaxTuple.part4Up))))))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$space,
						$author$project$Print$exactly('(')))));
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$ElmSyntaxPrint$lineSpreadBetweenNodes = F2(
	function (_v0, _v1) {
		var earlierRange = _v0.a;
		var laterRange = _v1.a;
		return _Utils_eq(earlierRange.start.row, laterRange.end.row) ? $author$project$Print$SingleLine : $author$project$Print$MultipleLines;
	});
var $author$project$ElmSyntaxPrint$lineSpreadInNode = function (_v0) {
	var range = _v0.a;
	return $author$project$ElmSyntaxPrint$lineSpreadInRange(range);
};
var $elm$core$List$map3 = _List_map3;
var $author$project$ElmSyntaxPrint$parenthesized = F3(
	function (printNotParenthesized, syntax, syntaxComments) {
		var notParenthesizedPrint = A2(printNotParenthesized, syntaxComments, syntax.notParenthesized);
		var commentsBeforeInner = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntax.notParenthesized).start,
				start: syntax.fullRange.start
			},
			syntaxComments);
		var commentsBeforeInnerCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(commentsBeforeInner);
		var commentsAfterInner = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: syntax.fullRange.end,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntax.notParenthesized).end
			},
			syntaxComments);
		var commentsAfterInnerCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(commentsAfterInner);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$Print$lineSpread(notParenthesizedPrint),
					commentsBeforeInnerCollapsible.lineSpread,
					commentsAfterInnerCollapsible.lineSpread
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(')'),
			A2(
				$author$project$Print$followedBy,
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$emptyOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$withIndentIncreasedBy,
						1,
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsAfterInner.b) {
									return $author$project$Print$empty;
								} else {
									return A2(
										$author$project$Print$followedBy,
										commentsAfterInnerCollapsible.print,
										$author$project$Print$spaceOrLinebreakIndented(lineSpread));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								notParenthesizedPrint,
								function () {
									if (!commentsBeforeInner.b) {
										return $author$project$Print$empty;
									} else {
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(lineSpread),
											commentsBeforeInnerCollapsible.print);
									}
								}())))),
				$author$project$Print$exactly('(')));
	});
var $author$project$ElmSyntaxPrint$recordLiteral = F3(
	function (fieldSpecific, syntaxComments, syntaxRecord) {
		var _v0 = syntaxRecord.fields;
		if (!_v0.b) {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('}'),
				A2(
					$author$project$Print$followedBy,
					function () {
						var _v1 = A2($author$project$ElmSyntaxPrint$commentsInRange, syntaxRecord.fullRange, syntaxComments);
						if (!_v1.b) {
							return $author$project$Print$empty;
						} else {
							var comment0 = _v1.a;
							var comment1Up = _v1.b;
							var commentsCollapsed = $author$project$ElmSyntaxPrint$collapsibleComments(
								A2($elm$core$List$cons, comment0, comment1Up));
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(commentsCollapsed.lineSpread),
								A2($author$project$Print$withIndentIncreasedBy, 1, commentsCollapsed.print));
						}
					}(),
					$author$project$Print$exactly('{')));
		} else {
			var field0 = _v0.a;
			var field1Up = _v0.b;
			var fieldValuePrints = A2(
				$elm$core$List$map,
				function (_v17) {
					var _v18 = _v17.b;
					var fieldValue = _v18.b;
					return A2(fieldSpecific.printValueNotParenthesized, syntaxComments, fieldValue);
				},
				A2($elm$core$List$cons, field0, field1Up));
			var commentsBeforeFields = A3(
				$elm$core$List$foldl,
				F2(
					function (_v11, soFar) {
						var _v12 = _v11.b;
						var _v13 = _v12.a;
						var fieldNameRange = _v13.a;
						var _v14 = _v12.b;
						var fieldValueRange = _v14.a;
						var commentsBetweenNameAndValue = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: fieldValueRange.start, start: fieldNameRange.start},
							syntaxComments);
						var commentsBeforeName = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: fieldNameRange.start, start: soFar.end},
							syntaxComments);
						return {
							end: fieldValueRange.end,
							reverse: A2(
								$elm$core$List$cons,
								{
									beforeName: function () {
										if (!commentsBeforeName.b) {
											return $elm$core$Maybe$Nothing;
										} else {
											var comment0 = commentsBeforeName.a;
											var comment1Up = commentsBeforeName.b;
											return $elm$core$Maybe$Just(
												$author$project$ElmSyntaxPrint$collapsibleComments(
													A2($elm$core$List$cons, comment0, comment1Up)));
										}
									}(),
									betweenNameAndValue: function () {
										if (!commentsBetweenNameAndValue.b) {
											return $elm$core$Maybe$Nothing;
										} else {
											var comment0 = commentsBetweenNameAndValue.a;
											var comment1Up = commentsBetweenNameAndValue.b;
											return $elm$core$Maybe$Just(
												$author$project$ElmSyntaxPrint$collapsibleComments(
													A2($elm$core$List$cons, comment0, comment1Up)));
										}
									}()
								},
								soFar.reverse)
						};
					}),
				{end: syntaxRecord.fullRange.start, reverse: _List_Nil},
				A2($elm$core$List$cons, field0, field1Up));
			var commentsAfterFields = A2(
				$author$project$ElmSyntaxPrint$commentsInRange,
				{end: syntaxRecord.fullRange.end, start: commentsBeforeFields.end},
				syntaxComments);
			var lineSpread = $author$project$Print$lineSpreadsCombine(
				_List_fromArray(
					[
						$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxRecord.fullRange),
						A2(
						$author$project$Print$mapAndLineSpreadsCombine,
						function (fieldComments) {
							return A2(
								$author$project$Print$lineSpreadMerge,
								function () {
									var _v8 = fieldComments.beforeName;
									if (_v8.$ === 'Nothing') {
										return $author$project$Print$SingleLine;
									} else {
										var commentsBeforeName = _v8.a;
										return commentsBeforeName.lineSpread;
									}
								}(),
								function () {
									var _v9 = fieldComments.betweenNameAndValue;
									if (_v9.$ === 'Nothing') {
										return $author$project$Print$SingleLine;
									} else {
										var commentsBetweenNameAndValue = _v9.a;
										return commentsBetweenNameAndValue.lineSpread;
									}
								}());
						},
						commentsBeforeFields.reverse),
						A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, fieldValuePrints),
						function () {
						if (!commentsAfterFields.b) {
							return $author$project$Print$SingleLine;
						} else {
							return $author$project$Print$MultipleLines;
						}
					}()
					]));
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('}'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsAfterFields.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsAfterFields.a;
								var comment1Up = commentsAfterFields.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$spaceOrLinebreakIndented(lineSpread),
										$author$project$Print$linebreak));
							}
						}(),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$sequence(
								A2(
									$elm$core$List$intersperse,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$exactly(','),
											$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
									A4(
										$elm$core$List$map3,
										F3(
											function (_v2, valuePrint, fieldComments) {
												var _v3 = _v2.b;
												var _v4 = _v3.a;
												var fieldNameRange = _v4.a;
												var fieldName = _v4.b;
												var fieldValue = _v3.b;
												var lineSpreadBetweenNameAndValueNotConsideringComments = A2(
													$author$project$Print$lineSpreadMerge,
													$author$project$ElmSyntaxPrint$lineSpreadInRange(
														{
															end: $stil4m$elm_syntax$Elm$Syntax$Node$range(fieldValue).end,
															start: fieldNameRange.start
														}),
													$author$project$Print$lineSpread(valuePrint));
												return A2(
													$author$project$Print$followedBy,
													A2(
														$author$project$Print$withIndentIncreasedBy,
														2,
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$withIndentAtNextMultipleOf4(
																A2(
																	$author$project$Print$followedBy,
																	valuePrint,
																	function () {
																		var _v6 = fieldComments.betweenNameAndValue;
																		if (_v6.$ === 'Nothing') {
																			return $author$project$Print$spaceOrLinebreakIndented(lineSpreadBetweenNameAndValueNotConsideringComments);
																		} else {
																			var commentsBetweenNameAndValue = _v6.a;
																			return A2(
																				$author$project$Print$followedBy,
																				$author$project$Print$spaceOrLinebreakIndented(
																					A2(
																						$author$project$Print$lineSpreadMerge,
																						commentsBetweenNameAndValue.lineSpread,
																						$author$project$Print$lineSpread(valuePrint))),
																				A2(
																					$author$project$Print$followedBy,
																					commentsBetweenNameAndValue.print,
																					$author$project$Print$spaceOrLinebreakIndented(
																						A2($author$project$Print$lineSpreadMerge, commentsBetweenNameAndValue.lineSpread, lineSpreadBetweenNameAndValueNotConsideringComments))));
																		}
																	}())),
															$author$project$Print$exactly(fieldSpecific.nameValueSeparator))),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$space,
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$exactly(fieldName),
															A2(
																$author$project$Print$withIndentIncreasedBy,
																2,
																function () {
																	var _v5 = fieldComments.beforeName;
																	if (_v5.$ === 'Nothing') {
																		return $author$project$Print$empty;
																	} else {
																		var commentsBeforeName = _v5.a;
																		return A2(
																			$author$project$Print$followedBy,
																			$author$project$Print$spaceOrLinebreakIndented(commentsBeforeName.lineSpread),
																			commentsBeforeName.print);
																	}
																}()))));
											}),
										A2($elm$core$List$cons, field0, field1Up),
										fieldValuePrints,
										$elm$core$List$reverse(commentsBeforeFields.reverse)))),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								$author$project$Print$exactly('{'))))));
		}
	});
var $author$project$ElmSyntaxPrint$qualifiedReference = function (syntaxReference) {
	var _v0 = syntaxReference.qualification;
	if (!_v0.b) {
		return $author$project$Print$exactly(syntaxReference.unqualified);
	} else {
		var modulePartHead = _v0.a;
		var modulePartTail = _v0.b;
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(syntaxReference.unqualified),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('.'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A2(
							$elm$core$List$map,
							function (modulePart) {
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly(modulePart),
									$author$project$Print$exactly('.'));
							},
							modulePartTail)),
					$author$project$Print$exactly(modulePartHead))));
	}
};
var $author$project$ElmSyntaxPrint$referenceConstruct = F3(
	function (printArgumentParenthesizedIfSpaceSeparated, syntaxComments, syntaxReferenceConstruct) {
		return A3(
			$author$project$ElmSyntaxPrint$construct,
			printArgumentParenthesizedIfSpaceSeparated,
			syntaxComments,
			{
				_arguments: syntaxReferenceConstruct._arguments,
				fullRange: syntaxReferenceConstruct.fullRange,
				start: $author$project$ElmSyntaxPrint$qualifiedReference(
					{qualification: syntaxReferenceConstruct.referenceQualification, unqualified: syntaxReferenceConstruct.referenceUnqualified})
			});
	});
var $author$project$ElmSyntaxPrint$triple = F3(
	function (printPartNotParenthesized, syntaxComments, syntaxTriple) {
		var part2Print = A2(printPartNotParenthesized, syntaxComments, syntaxTriple.part2);
		var part1Print = A2(printPartNotParenthesized, syntaxComments, syntaxTriple.part1);
		var part0Print = A2(printPartNotParenthesized, syntaxComments, syntaxTriple.part0);
		var beforePart2Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part2).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part1).end
			},
			syntaxComments);
		var beforePart2CommentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(beforePart2Comments);
		var beforePart1Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part1).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part0).end
			},
			syntaxComments);
		var beforePart1CommentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(beforePart1Comments);
		var beforePart0Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part0).start,
				start: syntaxTriple.fullRange.start
			},
			syntaxComments);
		var beforePart0CommentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(beforePart0Comments);
		var afterPart2Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: syntaxTriple.fullRange.end,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTriple.part2).end
			},
			syntaxComments);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxTriple.fullRange),
					beforePart0CommentsCollapsible.lineSpread,
					beforePart1CommentsCollapsible.lineSpread,
					beforePart2CommentsCollapsible.lineSpread,
					function () {
					if (afterPart2Comments.b) {
						return $author$project$Print$MultipleLines;
					} else {
						return $author$project$Print$SingleLine;
					}
				}()
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(')'),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$spaceOrLinebreakIndented(lineSpread),
				A2(
					$author$project$Print$followedBy,
					A2(
						$author$project$Print$withIndentIncreasedBy,
						2,
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!afterPart2Comments.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = afterPart2Comments.a;
									var comment1Up = afterPart2Comments.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										$author$project$Print$linebreakIndented);
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								part2Print,
								function () {
									if (!beforePart2Comments.b) {
										return $author$project$Print$empty;
									} else {
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(
												A2(
													$author$project$Print$lineSpreadMerge,
													beforePart2CommentsCollapsible.lineSpread,
													$author$project$Print$lineSpread(part2Print))),
											beforePart2CommentsCollapsible.print);
									}
								}()))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$space,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(','),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(lineSpread),
								A2(
									$author$project$Print$followedBy,
									A2(
										$author$project$Print$withIndentIncreasedBy,
										2,
										A2(
											$author$project$Print$followedBy,
											part1Print,
											function () {
												if (!beforePart1Comments.b) {
													return $author$project$Print$empty;
												} else {
													return A2(
														$author$project$Print$followedBy,
														$author$project$Print$spaceOrLinebreakIndented(
															A2(
																$author$project$Print$lineSpreadMerge,
																beforePart1CommentsCollapsible.lineSpread,
																$author$project$Print$lineSpread(part1Print))),
														beforePart1CommentsCollapsible.print);
												}
											}())),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$exactly(','),
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$emptyOrLinebreakIndented(lineSpread),
												A2(
													$author$project$Print$followedBy,
													A2(
														$author$project$Print$withIndentIncreasedBy,
														2,
														A2(
															$author$project$Print$followedBy,
															part0Print,
															function () {
																if (!beforePart0Comments.b) {
																	return $author$project$Print$empty;
																} else {
																	return A2(
																		$author$project$Print$followedBy,
																		$author$project$Print$spaceOrLinebreakIndented(
																			A2(
																				$author$project$Print$lineSpreadMerge,
																				beforePart0CommentsCollapsible.lineSpread,
																				$author$project$Print$lineSpread(part0Print))),
																		beforePart0CommentsCollapsible.print);
																}
															}())),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$space,
														$author$project$Print$exactly('(')))))))))))));
	});
var $author$project$ElmSyntaxPrint$tuple = F3(
	function (printPartNotParenthesized, syntaxComments, syntaxTuple) {
		var part1Print = A2(printPartNotParenthesized, syntaxComments, syntaxTuple.part1);
		var part0Print = A2(printPartNotParenthesized, syntaxComments, syntaxTuple.part0);
		var beforePart1Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTuple.part1).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTuple.part0).end
			},
			syntaxComments);
		var beforePart1CommentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(beforePart1Comments);
		var beforePart0Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTuple.part0).start,
				start: syntaxTuple.fullRange.start
			},
			syntaxComments);
		var beforePart0CommentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(beforePart0Comments);
		var afterPart1Comments = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: syntaxTuple.fullRange.end,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTuple.part1).end
			},
			syntaxComments);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxTuple.fullRange),
					beforePart0CommentsCollapsible.lineSpread,
					beforePart1CommentsCollapsible.lineSpread,
					function () {
					if (afterPart1Comments.b) {
						return $author$project$Print$MultipleLines;
					} else {
						return $author$project$Print$SingleLine;
					}
				}()
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(')'),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$spaceOrLinebreakIndented(lineSpread),
				A2(
					$author$project$Print$followedBy,
					A2(
						$author$project$Print$withIndentIncreasedBy,
						2,
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!afterPart1Comments.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = afterPart1Comments.a;
									var comment1Up = afterPart1Comments.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										$author$project$Print$linebreakIndented);
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								part1Print,
								function () {
									if (!beforePart1Comments.b) {
										return $author$project$Print$empty;
									} else {
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(
												A2(
													$author$project$Print$lineSpreadMerge,
													beforePart1CommentsCollapsible.lineSpread,
													$author$project$Print$lineSpread(part1Print))),
											beforePart1CommentsCollapsible.print);
									}
								}()))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$space,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(','),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(lineSpread),
								A2(
									$author$project$Print$followedBy,
									A2(
										$author$project$Print$withIndentIncreasedBy,
										2,
										A2(
											$author$project$Print$followedBy,
											part0Print,
											function () {
												if (!beforePart0Comments.b) {
													return $author$project$Print$empty;
												} else {
													return A2(
														$author$project$Print$followedBy,
														$author$project$Print$spaceOrLinebreakIndented(
															A2(
																$author$project$Print$lineSpreadMerge,
																beforePart0CommentsCollapsible.lineSpread,
																$author$project$Print$lineSpread(part0Print))),
														beforePart0CommentsCollapsible.print);
												}
											}())),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										$author$project$Print$exactly('(')))))))));
	});
var $author$project$ElmSyntaxPrint$typeFunctionExpand = function (typeNode) {
	if (typeNode.b.$ === 'FunctionTypeAnnotation') {
		var _v1 = typeNode.b;
		var inType = _v1.a;
		var outType = _v1.b;
		return A2(
			$elm$core$List$cons,
			inType,
			$author$project$ElmSyntaxPrint$typeFunctionExpand(outType));
	} else {
		var typeNodeNotFunction = typeNode;
		return _List_fromArray(
			[typeNodeNotFunction]);
	}
};
var $author$project$ElmSyntaxPrint$typeIsSpaceSeparated = function (syntaxType) {
	typeIsSpaceSeparated:
	while (true) {
		switch (syntaxType.$) {
			case 'GenericType':
				return false;
			case 'Typed':
				var _arguments = syntaxType.b;
				if (!_arguments.b) {
					return false;
				} else {
					return true;
				}
			case 'Unit':
				return false;
			case 'Tupled':
				var parts = syntaxType.a;
				if (!parts.b) {
					return false;
				} else {
					if (!parts.b.b) {
						var _v3 = parts.a;
						var inParens = _v3.b;
						var $temp$syntaxType = inParens;
						syntaxType = $temp$syntaxType;
						continue typeIsSpaceSeparated;
					} else {
						if (!parts.b.b.b) {
							var _v4 = parts.b;
							return false;
						} else {
							if (!parts.b.b.b.b) {
								var _v5 = parts.b;
								var _v6 = _v5.b;
								return false;
							} else {
								var _v7 = parts.b;
								var _v8 = _v7.b;
								var _v9 = _v8.b;
								return false;
							}
						}
					}
				}
			case 'Record':
				return false;
			case 'GenericRecord':
				return false;
			default:
				return true;
		}
	}
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation = F2(
	function (a, b) {
		return {$: 'FunctionTypeAnnotation', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord = F2(
	function (a, b) {
		return {$: 'GenericRecord', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType = function (a) {
	return {$: 'GenericType', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record = function (a) {
	return {$: 'Record', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled = function (a) {
	return {$: 'Tupled', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed = F2(
	function (a, b) {
		return {$: 'Typed', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit = {$: 'Unit'};
var $author$project$ElmSyntaxPrint$typeToNotParenthesized = function (_v0) {
	typeToNotParenthesized:
	while (true) {
		var typeRange = _v0.a;
		var syntaxType = _v0.b;
		switch (syntaxType.$) {
			case 'GenericType':
				var name = syntaxType.a;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					typeRange,
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(name));
			case 'Typed':
				var reference = syntaxType.a;
				var _arguments = syntaxType.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					typeRange,
					A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, reference, _arguments));
			case 'Unit':
				return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, typeRange, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit);
			case 'Tupled':
				var parts = syntaxType.a;
				if (!parts.b) {
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						typeRange,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(_List_Nil));
				} else {
					if (!parts.b.b) {
						var inParens = parts.a;
						var $temp$_v0 = inParens;
						_v0 = $temp$_v0;
						continue typeToNotParenthesized;
					} else {
						if (!parts.b.b.b) {
							var part0 = parts.a;
							var _v3 = parts.b;
							var part1 = _v3.a;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								typeRange,
								$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
									_List_fromArray(
										[part0, part1])));
						} else {
							if (!parts.b.b.b.b) {
								var part0 = parts.a;
								var _v4 = parts.b;
								var part1 = _v4.a;
								var _v5 = _v4.b;
								var part2 = _v5.a;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									typeRange,
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
										_List_fromArray(
											[part0, part1, part2])));
							} else {
								var part0 = parts.a;
								var _v6 = parts.b;
								var part1 = _v6.a;
								var _v7 = _v6.b;
								var part2 = _v7.a;
								var _v8 = _v7.b;
								var part3 = _v8.a;
								var part4Up = _v8.b;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									typeRange,
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
										A2(
											$elm$core$List$cons,
											part0,
											A2(
												$elm$core$List$cons,
												part1,
												A2(
													$elm$core$List$cons,
													part2,
													A2($elm$core$List$cons, part3, part4Up))))));
							}
						}
					}
				}
			case 'Record':
				var fields = syntaxType.a;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					typeRange,
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(fields));
			case 'GenericRecord':
				var extendedRecordVariableName = syntaxType.a;
				var additionalFieldsNode = syntaxType.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					typeRange,
					A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, extendedRecordVariableName, additionalFieldsNode));
			default:
				var inType = syntaxType.a;
				var outType = syntaxType.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					typeRange,
					A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, inType, outType));
		}
	}
};
var $author$project$ElmSyntaxPrint$typeToFunction = function (typeNode) {
	var _v0 = $stil4m$elm_syntax$Elm$Syntax$Node$value(
		$author$project$ElmSyntaxPrint$typeToNotParenthesized(typeNode));
	switch (_v0.$) {
		case 'FunctionTypeAnnotation':
			var inType = _v0.a;
			var outType = _v0.b;
			return $elm$core$Maybe$Just(
				{inType: inType, outType: outType});
		case 'GenericType':
			return $elm$core$Maybe$Nothing;
		case 'Typed':
			return $elm$core$Maybe$Nothing;
		case 'Unit':
			return $elm$core$Maybe$Nothing;
		case 'Tupled':
			return $elm$core$Maybe$Nothing;
		case 'Record':
			return $elm$core$Maybe$Nothing;
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ElmSyntaxPrint$typeFunctionNotParenthesized = F2(
	function (syntaxComments, _function) {
		var afterArrowTypes = $author$project$ElmSyntaxPrint$typeFunctionExpand(_function.outType);
		var afterArrowTypesComments = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (_v35, soFar) {
						var afterArrowTypeRange = _v35.a;
						return {
							commentsReverse: A2(
								$elm$core$List$cons,
								A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{end: afterArrowTypeRange.start, start: soFar.end},
									syntaxComments),
								soFar.commentsReverse),
							end: afterArrowTypeRange.end
						};
					}),
				{
					commentsReverse: _List_Nil,
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(_function.inType).end
				},
				afterArrowTypes).commentsReverse);
		var fullLineSpread = A2($elm$core$List$all, $elm$core$List$isEmpty, afterArrowTypesComments) ? A2($author$project$ElmSyntaxPrint$lineSpreadBetweenNodes, _function.inType, _function.outType) : $author$project$Print$MultipleLines;
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$sequence(
				A3(
					$elm$core$List$map2,
					F2(
						function (afterArrowTypeNode, commentsBeforeAfterArrowType) {
							var afterArrowTypePrint = A2($author$project$ElmSyntaxPrint$typeParenthesizedIfParenthesizedFunction, syntaxComments, afterArrowTypeNode);
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										afterArrowTypePrint,
										function () {
											if (!commentsBeforeAfterArrowType.b) {
												return $author$project$Print$spaceOrLinebreakIndented(
													$author$project$ElmSyntaxPrint$lineSpreadInNode(afterArrowTypeNode));
											} else {
												var comment0 = commentsBeforeAfterArrowType.a;
												var comment1Up = commentsBeforeAfterArrowType.b;
												var commentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(
													A2($elm$core$List$cons, comment0, comment1Up));
												var lineSpread = A2(
													$author$project$Print$lineSpreadMerge,
													commentsCollapsible.lineSpread,
													$author$project$Print$lineSpread(afterArrowTypePrint));
												return A2(
													$author$project$Print$followedBy,
													$author$project$Print$spaceOrLinebreakIndented(lineSpread),
													A2(
														$author$project$Print$followedBy,
														commentsCollapsible.print,
														$author$project$Print$spaceOrLinebreakIndented(lineSpread)));
											}
										}())),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly('->'),
									$author$project$Print$spaceOrLinebreakIndented(fullLineSpread)));
						}),
					afterArrowTypes,
					afterArrowTypesComments)),
			A2($author$project$ElmSyntaxPrint$typeParenthesizedIfFunction, syntaxComments, _function.inType));
	});
var $author$project$ElmSyntaxPrint$typeNotParenthesized = F2(
	function (syntaxComments, _v21) {
		typeNotParenthesized:
		while (true) {
			var fullRange = _v21.a;
			var syntaxType = _v21.b;
			switch (syntaxType.$) {
				case 'Unit':
					return $author$project$Print$exactly('()');
				case 'GenericType':
					var name = syntaxType.a;
					return $author$project$Print$exactly(name);
				case 'Typed':
					var _v23 = syntaxType.a;
					var _v24 = _v23.b;
					var referenceQualification = _v24.a;
					var referenceUnqualified = _v24.b;
					var _arguments = syntaxType.b;
					return A3(
						$author$project$ElmSyntaxPrint$referenceConstruct,
						$author$project$ElmSyntaxPrint$typeParenthesizedIfSpaceSeparated,
						syntaxComments,
						{_arguments: _arguments, fullRange: fullRange, referenceQualification: referenceQualification, referenceUnqualified: referenceUnqualified});
				case 'Tupled':
					var parts = syntaxType.a;
					if (!parts.b) {
						return $author$project$Print$exactly('()');
					} else {
						if (!parts.b.b) {
							var inParens = parts.a;
							var commentsBeforeInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).start,
									start: fullRange.start
								},
								syntaxComments);
							var commentsAfterInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: fullRange.end,
									start: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).end
								},
								syntaxComments);
							var _v26 = _Utils_Tuple2(commentsBeforeInParens, commentsAfterInParens);
							if ((!_v26.a.b) && (!_v26.b.b)) {
								var $temp$syntaxComments = syntaxComments,
									$temp$_v21 = inParens;
								syntaxComments = $temp$syntaxComments;
								_v21 = $temp$_v21;
								continue typeNotParenthesized;
							} else {
								return A3(
									$author$project$ElmSyntaxPrint$parenthesized,
									$author$project$ElmSyntaxPrint$typeNotParenthesized,
									{
										fullRange: fullRange,
										notParenthesized: $author$project$ElmSyntaxPrint$typeToNotParenthesized(inParens)
									},
									syntaxComments);
							}
						} else {
							if (!parts.b.b.b) {
								var part0 = parts.a;
								var _v27 = parts.b;
								var part1 = _v27.a;
								return A3(
									$author$project$ElmSyntaxPrint$tuple,
									$author$project$ElmSyntaxPrint$typeNotParenthesized,
									syntaxComments,
									{fullRange: fullRange, part0: part0, part1: part1});
							} else {
								if (!parts.b.b.b.b) {
									var part0 = parts.a;
									var _v28 = parts.b;
									var part1 = _v28.a;
									var _v29 = _v28.b;
									var part2 = _v29.a;
									return A3(
										$author$project$ElmSyntaxPrint$triple,
										$author$project$ElmSyntaxPrint$typeNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2});
								} else {
									var part0 = parts.a;
									var _v30 = parts.b;
									var part1 = _v30.a;
									var _v31 = _v30.b;
									var part2 = _v31.a;
									var _v32 = _v31.b;
									var part3 = _v32.a;
									var part4Up = _v32.b;
									return A3(
										$author$project$ElmSyntaxPrint$invalidNTuple,
										$author$project$ElmSyntaxPrint$typeNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2, part3: part3, part4Up: part4Up});
								}
							}
						}
					}
				case 'Record':
					var fields = syntaxType.a;
					return A3(
						$author$project$ElmSyntaxPrint$recordLiteral,
						{nameValueSeparator: ':', printValueNotParenthesized: $author$project$ElmSyntaxPrint$typeNotParenthesized},
						syntaxComments,
						{fields: fields, fullRange: fullRange});
				case 'GenericRecord':
					var recordVariable = syntaxType.a;
					var _v33 = syntaxType.b;
					var fields = _v33.b;
					return A2(
						$author$project$ElmSyntaxPrint$typeRecordExtension,
						syntaxComments,
						{fields: fields, fullRange: fullRange, recordVariable: recordVariable});
				default:
					var inType = syntaxType.a;
					var outType = syntaxType.b;
					return A2(
						$author$project$ElmSyntaxPrint$typeFunctionNotParenthesized,
						syntaxComments,
						{inType: inType, outType: outType});
			}
		}
	});
var $author$project$ElmSyntaxPrint$typeParenthesized = F2(
	function (syntaxComments, typeNode) {
		return A3(
			$author$project$ElmSyntaxPrint$parenthesized,
			$author$project$ElmSyntaxPrint$typeNotParenthesized,
			{
				fullRange: $stil4m$elm_syntax$Elm$Syntax$Node$range(typeNode),
				notParenthesized: $author$project$ElmSyntaxPrint$typeToNotParenthesized(typeNode)
			},
			syntaxComments);
	});
var $author$project$ElmSyntaxPrint$typeParenthesizedIfFunction = F2(
	function (syntaxComments, typeNode) {
		var _v20 = $author$project$ElmSyntaxPrint$typeToFunction(typeNode);
		if (_v20.$ === 'Just') {
			return A2($author$project$ElmSyntaxPrint$typeParenthesized, syntaxComments, typeNode);
		} else {
			return A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, typeNode);
		}
	});
var $author$project$ElmSyntaxPrint$typeParenthesizedIfParenthesizedFunction = F2(
	function (syntaxComments, typeNode) {
		var _v18 = $stil4m$elm_syntax$Elm$Syntax$Node$value(typeNode);
		if (((_v18.$ === 'Tupled') && _v18.a.b) && (!_v18.a.b.b)) {
			var _v19 = _v18.a;
			var inParens = _v19.a;
			return A2($author$project$ElmSyntaxPrint$typeParenthesizedIfFunction, syntaxComments, inParens);
		} else {
			return A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, typeNode);
		}
	});
var $author$project$ElmSyntaxPrint$typeParenthesizedIfSpaceSeparated = F2(
	function (syntaxComments, typeNode) {
		return $author$project$ElmSyntaxPrint$typeIsSpaceSeparated(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(typeNode)) ? A2($author$project$ElmSyntaxPrint$typeParenthesized, syntaxComments, typeNode) : A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, typeNode);
	});
var $author$project$ElmSyntaxPrint$typeRecordExtension = F2(
	function (syntaxComments, syntaxRecordExtension) {
		var fieldValuePrints = A2(
			$elm$core$List$map,
			function (_v16) {
				var _v17 = _v16.b;
				var fieldValue = _v17.b;
				return A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, fieldValue);
			},
			syntaxRecordExtension.fields);
		var commentsBeforeRecordVariable = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxRecordExtension.recordVariable).start,
				start: syntaxRecordExtension.fullRange.start
			},
			syntaxComments);
		var commentsCollapsibleBeforeRecordVariable = $author$project$ElmSyntaxPrint$collapsibleComments(commentsBeforeRecordVariable);
		var commentsBeforeFields = A3(
			$elm$core$List$foldl,
			F2(
				function (_v10, soFar) {
					var _v11 = _v10.b;
					var _v12 = _v11.a;
					var fieldNameRange = _v12.a;
					var _v13 = _v11.b;
					var fieldValueRange = _v13.a;
					var commentsBetweenNameAndValue = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: fieldValueRange.start, start: fieldNameRange.start},
						syntaxComments);
					var commentsBeforeName = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: fieldNameRange.start, start: soFar.end},
						syntaxComments);
					return {
						end: fieldValueRange.end,
						reverse: A2(
							$elm$core$List$cons,
							{
								beforeName: function () {
									if (!commentsBeforeName.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeName.a;
										var comment1Up = commentsBeforeName.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								betweenNameAndValue: function () {
									if (!commentsBetweenNameAndValue.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBetweenNameAndValue.a;
										var comment1Up = commentsBetweenNameAndValue.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}()
							},
							soFar.reverse)
					};
				}),
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxRecordExtension.recordVariable).end,
				reverse: _List_Nil
			},
			syntaxRecordExtension.fields);
		var commentsAfterFields = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{end: syntaxRecordExtension.fullRange.end, start: commentsBeforeFields.end},
			syntaxComments);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxRecordExtension.fullRange),
					commentsCollapsibleBeforeRecordVariable.lineSpread,
					A2(
					$author$project$Print$mapAndLineSpreadsCombine,
					function (fieldComments) {
						return A2(
							$author$project$Print$lineSpreadMerge,
							function () {
								var _v7 = fieldComments.beforeName;
								if (_v7.$ === 'Nothing') {
									return $author$project$Print$SingleLine;
								} else {
									var commentsBeforeName = _v7.a;
									return commentsBeforeName.lineSpread;
								}
							}(),
							function () {
								var _v8 = fieldComments.betweenNameAndValue;
								if (_v8.$ === 'Nothing') {
									return $author$project$Print$SingleLine;
								} else {
									var commentsBetweenNameAndValue = _v8.a;
									return commentsBetweenNameAndValue.lineSpread;
								}
							}());
					},
					commentsBeforeFields.reverse),
					A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, fieldValuePrints),
					function () {
					if (!commentsAfterFields.b) {
						return $author$project$Print$SingleLine;
					} else {
						return $author$project$Print$MultipleLines;
					}
				}()
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly('}'),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$spaceOrLinebreakIndented(lineSpread),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsAfterFields.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = commentsAfterFields.a;
									var comment1Up = commentsAfterFields.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(lineSpread),
											$author$project$Print$linebreak));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$sequence(
									A2(
										$elm$core$List$intersperse,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$exactly(','),
												$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
										A4(
											$elm$core$List$map3,
											F3(
												function (_v1, valuePrint, fieldComments) {
													var _v2 = _v1.b;
													var _v3 = _v2.a;
													var fieldNameRange = _v3.a;
													var fieldName = _v3.b;
													var fieldValue = _v2.b;
													var lineSpreadBetweenNameAndValueNotConsideringComments = A2(
														$author$project$Print$lineSpreadMerge,
														$author$project$ElmSyntaxPrint$lineSpreadInRange(
															{
																end: $stil4m$elm_syntax$Elm$Syntax$Node$range(fieldValue).end,
																start: fieldNameRange.start
															}),
														$author$project$Print$lineSpread(valuePrint));
													return A2(
														$author$project$Print$followedBy,
														A2(
															$author$project$Print$withIndentIncreasedBy,
															2,
															A2(
																$author$project$Print$followedBy,
																$author$project$Print$withIndentAtNextMultipleOf4(
																	A2(
																		$author$project$Print$followedBy,
																		valuePrint,
																		function () {
																			var _v5 = fieldComments.betweenNameAndValue;
																			if (_v5.$ === 'Nothing') {
																				return $author$project$Print$spaceOrLinebreakIndented(lineSpreadBetweenNameAndValueNotConsideringComments);
																			} else {
																				var commentsBetweenNameAndValue = _v5.a;
																				return A2(
																					$author$project$Print$followedBy,
																					$author$project$Print$spaceOrLinebreakIndented(
																						A2(
																							$author$project$Print$lineSpreadMerge,
																							commentsBetweenNameAndValue.lineSpread,
																							$author$project$Print$lineSpread(valuePrint))),
																					A2(
																						$author$project$Print$followedBy,
																						commentsBetweenNameAndValue.print,
																						$author$project$Print$spaceOrLinebreakIndented(
																							A2($author$project$Print$lineSpreadMerge, commentsBetweenNameAndValue.lineSpread, lineSpreadBetweenNameAndValueNotConsideringComments))));
																			}
																		}())),
																$author$project$Print$exactly(':'))),
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$space,
															A2(
																$author$project$Print$followedBy,
																$author$project$Print$exactly(fieldName),
																A2(
																	$author$project$Print$withIndentIncreasedBy,
																	2,
																	function () {
																		var _v4 = fieldComments.beforeName;
																		if (_v4.$ === 'Nothing') {
																			return $author$project$Print$empty;
																		} else {
																			var commentsBeforeName = _v4.a;
																			return A2(
																				$author$project$Print$followedBy,
																				$author$project$Print$spaceOrLinebreakIndented(commentsBeforeName.lineSpread),
																				commentsBeforeName.print);
																		}
																	}()))));
												}),
											syntaxRecordExtension.fields,
											fieldValuePrints,
											$elm$core$List$reverse(commentsBeforeFields.reverse)))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly('|'),
										$author$project$Print$spaceOrLinebreakIndented(lineSpread)))))),
					A2(
						$author$project$Print$followedBy,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxRecordExtension.recordVariable)),
							A2(
								$author$project$Print$withIndentIncreasedBy,
								2,
								function () {
									if (!commentsBeforeRecordVariable.b) {
										return $author$project$Print$empty;
									} else {
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(commentsCollapsibleBeforeRecordVariable.lineSpread),
											commentsCollapsibleBeforeRecordVariable.print);
									}
								}())),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$space,
							$author$project$Print$exactly('{'))))));
	});
var $author$project$ElmSyntaxPrint$declarationChoiceType = F2(
	function (syntaxComments, syntaxChoiceTypeDeclaration) {
		var parameterPrints = A3(
			$elm$core$List$foldl,
			F2(
				function (parameterPattern, soFar) {
					var parameterPrintedRange = $stil4m$elm_syntax$Elm$Syntax$Node$range(parameterPattern);
					return {
						end: parameterPrintedRange.end,
						reverse: A2(
							$elm$core$List$cons,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$exactly(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(parameterPattern)),
								function () {
									var _v4 = A2(
										$author$project$ElmSyntaxPrint$commentsInRange,
										{end: parameterPrintedRange.start, start: soFar.end},
										syntaxComments);
									if (!_v4.b) {
										return $author$project$Print$empty;
									} else {
										var comment0 = _v4.a;
										var comment1Up = _v4.b;
										var commentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(
											A2($elm$core$List$cons, comment0, comment1Up));
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(commentsCollapsible.lineSpread),
											commentsCollapsible.print);
									}
								}()),
							soFar.reverse)
					};
				}),
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxChoiceTypeDeclaration.name).end,
				reverse: _List_Nil
			},
			syntaxChoiceTypeDeclaration.generics);
		var parametersLineSpread = A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, parameterPrints.reverse);
		var variantPrints = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (_v2, soFar) {
						var variantRange = _v2.a;
						var variant = _v2.b;
						var variantPrint = A3(
							$author$project$ElmSyntaxPrint$construct,
							$author$project$ElmSyntaxPrint$typeParenthesizedIfSpaceSeparated,
							syntaxComments,
							{
								_arguments: variant._arguments,
								fullRange: variantRange,
								start: $author$project$Print$exactly(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(variant.name))
							});
						var commentsVariantPrint = A2(
							$author$project$Print$followedBy,
							variantPrint,
							function () {
								var _v3 = A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{
										end: $stil4m$elm_syntax$Elm$Syntax$Node$range(variant.name).start,
										start: soFar.end
									},
									syntaxComments);
								if (!_v3.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = _v3.a;
									var comment1Up = _v3.b;
									var commentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(
										A2($elm$core$List$cons, comment0, comment1Up));
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$spaceOrLinebreakIndented(
											A2(
												$author$project$Print$lineSpreadMerge,
												commentsCollapsible.lineSpread,
												$author$project$Print$lineSpread(variantPrint))),
										commentsCollapsible.print);
								}
							}());
						return {
							end: variantRange.end,
							prints: A2($elm$core$List$cons, commentsVariantPrint, soFar.prints)
						};
					}),
				{end: parameterPrints.end, prints: _List_Nil},
				syntaxChoiceTypeDeclaration.constructors).prints);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A2(
							$elm$core$List$intersperse,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly('|'),
									$author$project$Print$linebreakIndented)),
							A2(
								$elm$core$List$map,
								function (variantPrint) {
									return A2($author$project$Print$withIndentIncreasedBy, 2, variantPrint);
								},
								variantPrints))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$space,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly('='),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreakIndented,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$withIndentAtNextMultipleOf4(
										$author$project$Print$sequence(
											A2(
												$elm$core$List$map,
												function (parameterPrint) {
													return A2(
														$author$project$Print$followedBy,
														parameterPrint,
														$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread));
												},
												$elm$core$List$reverse(parameterPrints.reverse)))),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly(
											$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxChoiceTypeDeclaration.name)),
										$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread)))))))),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('type'),
				function () {
					var _v0 = syntaxChoiceTypeDeclaration.documentation;
					if (_v0.$ === 'Nothing') {
						return $author$project$Print$empty;
					} else {
						var _v1 = _v0.a;
						var documentationRange = _v1.a;
						var documentation = _v1.b;
						return A2(
							$author$project$Print$followedBy,
							$author$project$ElmSyntaxPrint$commentsBetweenDocumentationAndDeclaration(
								A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{
										end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxChoiceTypeDeclaration.name).start,
										start: documentationRange.start
									},
									syntaxComments)),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								$author$project$Print$exactly(documentation)));
					}
				}()));
	});
var $author$project$ElmSyntaxPrint$unsafeHexDigitIntToString = function (_int) {
	switch (_int) {
		case 0:
			return '0';
		case 1:
			return '1';
		case 2:
			return '2';
		case 3:
			return '3';
		case 4:
			return '4';
		case 5:
			return '5';
		case 6:
			return '6';
		case 7:
			return '7';
		case 8:
			return '8';
		case 9:
			return '9';
		case 10:
			return 'A';
		case 11:
			return 'B';
		case 12:
			return 'C';
		case 13:
			return 'D';
		case 14:
			return 'E';
		default:
			return 'F';
	}
};
var $author$project$ElmSyntaxPrint$intToHexString = function (_int) {
	return (_int < 16) ? $author$project$ElmSyntaxPrint$unsafeHexDigitIntToString(_int) : _Utils_ap(
		$author$project$ElmSyntaxPrint$intToHexString((_int / 16) | 0),
		$author$project$ElmSyntaxPrint$unsafeHexDigitIntToString(_int % 16));
};
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$ElmSyntaxPrint$characterHex = function (character) {
	return $elm$core$String$toUpper(
		$author$project$ElmSyntaxPrint$intToHexString(
			$elm$core$Char$toCode(character)));
};
var $miniBill$elm_unicode$Unicode$LetterLowercase = {$: 'LetterLowercase'};
var $miniBill$elm_unicode$Unicode$LetterModifier = {$: 'LetterModifier'};
var $miniBill$elm_unicode$Unicode$LetterOther = {$: 'LetterOther'};
var $miniBill$elm_unicode$Unicode$LetterTitlecase = {$: 'LetterTitlecase'};
var $miniBill$elm_unicode$Unicode$LetterUppercase = {$: 'LetterUppercase'};
var $miniBill$elm_unicode$Unicode$MarkEnclosing = {$: 'MarkEnclosing'};
var $miniBill$elm_unicode$Unicode$MarkNonSpacing = {$: 'MarkNonSpacing'};
var $miniBill$elm_unicode$Unicode$MarkSpacingCombining = {$: 'MarkSpacingCombining'};
var $miniBill$elm_unicode$Unicode$NumberDecimalDigit = {$: 'NumberDecimalDigit'};
var $miniBill$elm_unicode$Unicode$NumberLetter = {$: 'NumberLetter'};
var $miniBill$elm_unicode$Unicode$NumberOther = {$: 'NumberOther'};
var $miniBill$elm_unicode$Unicode$OtherControl = {$: 'OtherControl'};
var $miniBill$elm_unicode$Unicode$OtherFormat = {$: 'OtherFormat'};
var $miniBill$elm_unicode$Unicode$OtherPrivateUse = {$: 'OtherPrivateUse'};
var $miniBill$elm_unicode$Unicode$OtherSurrogate = {$: 'OtherSurrogate'};
var $miniBill$elm_unicode$Unicode$PunctuationClose = {$: 'PunctuationClose'};
var $miniBill$elm_unicode$Unicode$PunctuationConnector = {$: 'PunctuationConnector'};
var $miniBill$elm_unicode$Unicode$PunctuationDash = {$: 'PunctuationDash'};
var $miniBill$elm_unicode$Unicode$PunctuationFinalQuote = {$: 'PunctuationFinalQuote'};
var $miniBill$elm_unicode$Unicode$PunctuationInitialQuote = {$: 'PunctuationInitialQuote'};
var $miniBill$elm_unicode$Unicode$PunctuationOpen = {$: 'PunctuationOpen'};
var $miniBill$elm_unicode$Unicode$PunctuationOther = {$: 'PunctuationOther'};
var $miniBill$elm_unicode$Unicode$SeparatorLine = {$: 'SeparatorLine'};
var $miniBill$elm_unicode$Unicode$SeparatorParagraph = {$: 'SeparatorParagraph'};
var $miniBill$elm_unicode$Unicode$SeparatorSpace = {$: 'SeparatorSpace'};
var $miniBill$elm_unicode$Unicode$SymbolCurrency = {$: 'SymbolCurrency'};
var $miniBill$elm_unicode$Unicode$SymbolMath = {$: 'SymbolMath'};
var $miniBill$elm_unicode$Unicode$SymbolModifier = {$: 'SymbolModifier'};
var $miniBill$elm_unicode$Unicode$SymbolOther = {$: 'SymbolOther'};
var $elm$core$Basics$modBy = _Basics_modBy;
var $miniBill$elm_unicode$Unicode$getCategory = function (c) {
	var code = $elm$core$Char$toCode(c);
	var e = function (hex) {
		return _Utils_eq(hex, code);
	};
	var l = function (hex) {
		return _Utils_cmp(code, hex) < 0;
	};
	var r = F2(
		function (from, to) {
			return (_Utils_cmp(from, code) < 1) && (_Utils_cmp(code, to) < 1);
		});
	return l(256) ? (l(160) ? (l(59) ? (l(41) ? ((code <= 31) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherControl) : (e(32) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : ((A2(r, 33, 35) || A2(r, 37, 39)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(36) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (e(40) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing))))) : (e(41) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(42) || (e(44) || (A2(r, 46, 47) || e(58)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(43) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(45) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : (A2(r, 48, 57) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))) : (l(94) ? ((e(59) || (A2(r, 63, 64) || e(92))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 60, 62) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (A2(r, 65, 90) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(91) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(93) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing))))) : ((e(94) || e(96)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (e(95) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : (A2(r, 97, 122) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(123) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(124) || e(126)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(125) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 127, 159) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherControl) : $elm$core$Maybe$Nothing))))))))) : (l(177) ? (l(169) ? (e(160) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : ((e(161) || e(167)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 162, 165) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (e(166) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(168) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing))))) : ((e(169) || (e(174) || e(176))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(170) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(171) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : (e(172) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(173) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (e(175) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing))))))) : (l(186) ? (e(177) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 178, 179) || e(185)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((e(180) || e(184)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (e(181) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 182, 183) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (e(186) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(187) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : (A2(r, 188, 190) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(191) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 192, 214) || A2(r, 216, 222)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(215) || e(247)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 223, 246) || A2(r, 248, 255)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))))))) : (l(9084) ? (l(3085) ? (l(1166) ? (l(488) ? (l(356) ? (l(304) ? (l(279) ? (l(266) ? ((e(256) || (e(258) || (e(260) || (e(262) || e(264))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(257) || (e(259) || (e(261) || (e(263) || e(265))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(271) ? ((e(266) || (e(268) || e(270))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(267) || e(269)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(271) || (e(273) || (e(275) || e(277)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(272) || (e(274) || (e(276) || e(278)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(290) ? ((e(279) || (e(281) || (e(283) || (e(285) || (e(287) || e(289)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(280) || (e(282) || (e(284) || (e(286) || e(288))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(296) ? ((e(290) || (e(292) || e(294))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(291) || (e(293) || e(295))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(296) || (e(298) || (e(300) || e(302)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(297) || (e(299) || (e(301) || e(303)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(330) ? (l(316) ? ((e(304) || (e(306) || (e(308) || (e(310) || (e(313) || e(315)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(305) || (e(307) || (e(309) || (A2(r, 311, 312) || e(314))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(321) ? ((e(316) || (e(318) || e(320))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(317) || e(319)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(321) || (e(323) || (e(325) || e(327)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(322) || (e(324) || (e(326) || A2(r, 328, 329)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(342) ? ((e(330) || (e(332) || (e(334) || (e(336) || (e(338) || e(340)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(331) || (e(333) || (e(335) || (e(337) || (e(339) || e(341)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(348) ? ((e(342) || (e(344) || e(346))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(343) || (e(345) || e(347))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(348) || (e(350) || (e(352) || e(354)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(349) || (e(351) || (e(353) || e(355)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))) : (l(424) ? (l(380) ? (l(366) ? ((e(356) || (e(358) || (e(360) || (e(362) || e(364))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(357) || (e(359) || (e(361) || (e(363) || e(365))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(371) ? ((e(366) || (e(368) || e(370))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(367) || e(369)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(371) || (e(373) || (e(375) || e(378)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(372) || (e(374) || (A2(r, 376, 377) || e(379)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(402) ? ((e(380) || (A2(r, 382, 384) || (e(387) || (e(389) || (e(392) || A2(r, 396, 397)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(381) || (A2(r, 385, 386) || (e(388) || (A2(r, 390, 391) || (A2(r, 393, 395) || A2(r, 398, 401)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(414) ? ((e(402) || (e(405) || A2(r, 409, 411))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 403, 404) || (A2(r, 406, 408) || A2(r, 412, 413))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(414) || (e(417) || (e(419) || e(421)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 415, 416) || (e(418) || (e(420) || A2(r, 422, 423)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))) : (l(460) ? (l(440) ? ((e(424) || (A2(r, 426, 427) || (e(429) || (e(432) || (e(436) || e(438)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(425) || (e(428) || (A2(r, 430, 431) || (A2(r, 433, 435) || (e(437) || e(439)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(452) ? ((e(440) || e(444)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 441, 442) || A2(r, 445, 447)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(443) || A2(r, 448, 451)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((e(452) || (e(455) || e(458))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(453) || (e(456) || e(459))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : ((e(454) || e(457)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(472) ? ((e(460) || (e(462) || (e(464) || (e(466) || (e(468) || e(470)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(461) || (e(463) || (e(465) || (e(467) || (e(469) || e(471)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(479) ? ((e(472) || (e(474) || A2(r, 476, 477))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(473) || (e(475) || e(478))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(479) || (e(481) || (e(483) || (e(485) || e(487))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(480) || (e(482) || (e(484) || e(486)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))))) : (l(767) ? (l(540) ? (l(514) ? (l(499) ? ((e(488) || (e(490) || (e(492) || (e(494) || e(497))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(489) || (e(491) || (e(493) || A2(r, 495, 496)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(498) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : $elm$core$Maybe$Nothing))) : (l(506) ? ((e(499) || (e(501) || e(505))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(500) || A2(r, 502, 504)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(506) || (e(508) || (e(510) || e(512)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(507) || (e(509) || (e(511) || e(513)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(526) ? ((e(514) || (e(516) || (e(518) || (e(520) || (e(522) || e(524)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(515) || (e(517) || (e(519) || (e(521) || (e(523) || e(525)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(532) ? ((e(526) || (e(528) || e(530))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(527) || (e(529) || e(531))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(532) || (e(534) || (e(536) || e(538)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(533) || (e(535) || (e(537) || e(539)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(572) ? (l(551) ? ((e(540) || (e(542) || (e(544) || (e(546) || (e(548) || e(550)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(541) || (e(543) || (e(545) || (e(547) || e(549))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(557) ? ((e(551) || (e(553) || e(555))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(552) || (e(554) || e(556))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(557) || (e(559) || (e(561) || A2(r, 563, 569)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(558) || (e(560) || (e(562) || A2(r, 570, 571)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(589) ? ((e(572) || (A2(r, 575, 576) || (e(578) || (e(583) || (e(585) || e(587)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 573, 574) || (e(577) || (A2(r, 579, 582) || (e(584) || (e(586) || e(588)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(709) ? ((e(589) || (A2(r, 591, 659) || A2(r, 661, 687))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(590) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(660) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 688, 705) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 706, 708) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing))))) : ((e(709) || (A2(r, 722, 735) || (A2(r, 741, 747) || (e(749) || A2(r, 751, 766))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : ((A2(r, 710, 721) || (A2(r, 736, 740) || (e(748) || e(750)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing)))))) : (l(1006) ? (l(975) ? (l(893) ? ((e(767) || e(885)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (A2(r, 768, 879) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(880) || (e(882) || e(886))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(881) || (e(883) || (e(887) || A2(r, 891, 892)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(884) || e(890)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))) : (l(903) ? (e(893) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(894) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(895) || e(902)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 900, 901) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing)))) : (e(903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 904, 906) || (e(908) || (A2(r, 910, 911) || (A2(r, 913, 929) || A2(r, 931, 939))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(912) || A2(r, 940, 974)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(992) ? ((e(975) || (A2(r, 978, 980) || (e(984) || (e(986) || (e(988) || e(990)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 976, 977) || (A2(r, 981, 983) || (e(985) || (e(987) || (e(989) || e(991)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(998) ? ((e(992) || (e(994) || e(996))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(993) || (e(995) || e(997))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(998) || (e(1000) || (e(1002) || e(1004)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(999) || (e(1001) || (e(1003) || e(1005)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(1134) ? (l(1120) ? ((e(1006) || (e(1012) || (e(1015) || (A2(r, 1017, 1018) || A2(r, 1021, 1071))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 1007, 1011) || (e(1013) || (e(1016) || (A2(r, 1019, 1020) || A2(r, 1072, 1119))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(1014) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))) : (l(1126) ? ((e(1120) || (e(1122) || e(1124))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1121) || (e(1123) || e(1125))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(1126) || (e(1128) || (e(1130) || e(1132)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1127) || (e(1129) || (e(1131) || e(1133)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(1146) ? ((e(1134) || (e(1136) || (e(1138) || (e(1140) || (e(1142) || e(1144)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1135) || (e(1137) || (e(1139) || (e(1141) || (e(1143) || e(1145)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(1152) ? ((e(1146) || (e(1148) || e(1150))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1147) || (e(1149) || e(1151))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(1152) || (e(1162) || e(1164))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1153) || (e(1163) || e(1165))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(1154) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 1155, 1159) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 1160, 1161) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkEnclosing) : $elm$core$Maybe$Nothing))))))))))) : (l(1756) ? (l(1268) ? (l(1215) ? (l(1189) ? (l(1176) ? ((e(1166) || (e(1168) || (e(1170) || (e(1172) || e(1174))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1167) || (e(1169) || (e(1171) || (e(1173) || e(1175))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(1181) ? ((e(1176) || (e(1178) || e(1180))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1177) || e(1179)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(1181) || (e(1183) || (e(1185) || e(1187)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1182) || (e(1184) || (e(1186) || e(1188)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(1201) ? ((e(1189) || (e(1191) || (e(1193) || (e(1195) || (e(1197) || e(1199)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1190) || (e(1192) || (e(1194) || (e(1196) || (e(1198) || e(1200)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(1207) ? ((e(1201) || (e(1203) || e(1205))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1202) || (e(1204) || e(1206))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(1207) || (e(1209) || (e(1211) || e(1213)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1208) || (e(1210) || (e(1212) || e(1214)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))) : (l(1241) ? (l(1227) ? ((e(1215) || (e(1218) || (e(1220) || (e(1222) || (e(1224) || e(1226)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 1216, 1217) || (e(1219) || (e(1221) || (e(1223) || e(1225))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(1233) ? ((e(1227) || (e(1229) || e(1232))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1228) || A2(r, 1230, 1231)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(1233) || (e(1235) || (e(1237) || e(1239)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1234) || (e(1236) || (e(1238) || e(1240)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(1253) ? ((e(1241) || (e(1243) || (e(1245) || (e(1247) || (e(1249) || e(1251)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1242) || (e(1244) || (e(1246) || (e(1248) || (e(1250) || e(1252)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(1259) ? ((e(1253) || (e(1255) || e(1257))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1254) || (e(1256) || e(1258))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(1259) || (e(1261) || (e(1263) || (e(1265) || e(1267))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1260) || (e(1262) || (e(1264) || e(1266)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))))) : (l(1318) ? (l(1292) ? (l(1279) ? ((e(1268) || (e(1270) || (e(1272) || (e(1274) || (e(1276) || e(1278)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1269) || (e(1271) || (e(1273) || (e(1275) || e(1277))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(1284) ? ((e(1279) || (e(1281) || e(1283))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(1280) || e(1282)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(1284) || (e(1286) || (e(1288) || e(1290)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1285) || (e(1287) || (e(1289) || e(1291)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(1304) ? ((e(1292) || (e(1294) || (e(1296) || (e(1298) || (e(1300) || e(1302)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1293) || (e(1295) || (e(1297) || (e(1299) || (e(1301) || e(1303)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(1310) ? ((e(1304) || (e(1306) || e(1308))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1305) || (e(1307) || e(1309))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(1310) || (e(1312) || (e(1314) || e(1316)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1311) || (e(1313) || (e(1315) || e(1317)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(1478) ? (l(1369) ? ((e(1318) || (e(1320) || (e(1322) || (e(1324) || (e(1326) || A2(r, 1329, 1366)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(1319) || (e(1321) || (e(1323) || (e(1325) || e(1327))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(1424) ? (e(1369) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 1370, 1375) || e(1417)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 1376, 1416) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(1418) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : (A2(r, 1421, 1422) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(1423) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : $elm$core$Maybe$Nothing)))))) : ((A2(r, 1425, 1469) || (e(1471) || (A2(r, 1473, 1474) || A2(r, 1476, 1477)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(1470) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : ((e(1472) || e(1475)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (l(1563) ? ((e(1478) || (A2(r, 1523, 1524) || (A2(r, 1545, 1546) || A2(r, 1548, 1549)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(1479) || A2(r, 1552, 1562)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 1488, 1514) || A2(r, 1519, 1522)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 1536, 1541) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (A2(r, 1542, 1544) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(1547) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (A2(r, 1550, 1551) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))))))) : (l(1631) ? ((e(1563) || A2(r, 1565, 1567)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(1564) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((A2(r, 1568, 1599) || A2(r, 1601, 1610)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(1600) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 1611, 1630) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))) : ((e(1631) || (e(1648) || A2(r, 1750, 1755))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 1632, 1641) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 1642, 1645) || e(1748)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 1646, 1647) || (A2(r, 1649, 1747) || e(1749))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))))) : (l(2556) ? (l(2248) ? (l(2035) ? (l(1790) ? ((e(1756) || (A2(r, 1759, 1764) || (A2(r, 1767, 1768) || A2(r, 1770, 1773)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(1757) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((e(1758) || (e(1769) || e(1789))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 1765, 1766) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 1774, 1775) || A2(r, 1786, 1788)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 1776, 1785) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))) : (l(1839) ? (e(1790) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(1791) || (e(1808) || A2(r, 1810, 1838))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 1792, 1805) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(1807) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (e(1809) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))) : ((e(1839) || (A2(r, 1869, 1957) || (e(1969) || A2(r, 1994, 2026)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 1840, 1866) || (A2(r, 1958, 1968) || A2(r, 2027, 2034))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 1984, 1993) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : (l(2087) ? (l(2045) ? (e(2035) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 2036, 2037) || e(2042)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(2038) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 2039, 2041) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : ((e(2045) || (A2(r, 2070, 2073) || (A2(r, 2075, 2083) || A2(r, 2085, 2086)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2046, 2047) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (A2(r, 2048, 2069) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(2074) || e(2084)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))) : (l(2143) ? ((e(2087) || (A2(r, 2089, 2093) || A2(r, 2137, 2139))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2088) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 2096, 2110) || e(2142)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 2112, 2136) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : ((A2(r, 2144, 2154) || (A2(r, 2160, 2183) || (A2(r, 2185, 2190) || A2(r, 2208, 2247)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(2184) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (A2(r, 2192, 2193) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (A2(r, 2200, 2207) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))))) : (l(2432) ? (l(2368) ? ((e(2248) || (A2(r, 2308, 2361) || e(2365))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(2249) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 2250, 2273) || (A2(r, 2275, 2306) || (e(2362) || e(2364)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2274) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((e(2307) || (e(2363) || A2(r, 2366, 2367))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))) : (l(2391) ? ((e(2368) || (A2(r, 2377, 2380) || A2(r, 2382, 2383))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 2369, 2376) || (e(2381) || A2(r, 2385, 2390))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2384) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((e(2391) || A2(r, 2402, 2403)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 2392, 2401) || A2(r, 2418, 2431)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 2404, 2405) || e(2416)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 2406, 2415) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(2417) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))))) : (l(2502) ? (l(2473) ? ((e(2432) || (A2(r, 2437, 2444) || (A2(r, 2447, 2448) || A2(r, 2451, 2472)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(2433) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2434, 2435) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((A2(r, 2474, 2480) || (e(2482) || (A2(r, 2486, 2489) || e(2493)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(2492) || A2(r, 2497, 2500)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2494, 2496) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : (l(2529) ? ((A2(r, 2503, 2504) || (A2(r, 2507, 2508) || e(2519))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(2509) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(2510) || (A2(r, 2524, 2525) || A2(r, 2527, 2528))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((e(2529) || A2(r, 2544, 2545)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 2530, 2531) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2534, 2543) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 2546, 2547) || e(2555)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (A2(r, 2548, 2553) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(2554) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))))))))) : (l(2820) ? (l(2688) ? (l(2619) ? ((e(2556) || (A2(r, 2565, 2570) || (A2(r, 2575, 2576) || (A2(r, 2579, 2600) || (A2(r, 2602, 2608) || (A2(r, 2610, 2611) || (A2(r, 2613, 2614) || A2(r, 2616, 2617)))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(2557) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(2558) || A2(r, 2561, 2562)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2563) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : (l(2648) ? ((e(2620) || (A2(r, 2625, 2626) || (A2(r, 2631, 2632) || (A2(r, 2635, 2637) || e(2641))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2622, 2624) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)) : ((A2(r, 2649, 2652) || (e(2654) || A2(r, 2674, 2676))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 2662, 2671) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 2672, 2673) || e(2677)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2678) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(2760) ? (l(2737) ? (A2(r, 2689, 2690) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2691) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 2693, 2701) || (A2(r, 2703, 2705) || (A2(r, 2707, 2728) || A2(r, 2730, 2736)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((A2(r, 2738, 2739) || (A2(r, 2741, 2745) || e(2749))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(2748) || (A2(r, 2753, 2757) || e(2759))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2750, 2752) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : (l(2789) ? ((e(2760) || (e(2765) || A2(r, 2786, 2787))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(2761) || A2(r, 2763, 2764)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(2768) || A2(r, 2784, 2785)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : (A2(r, 2790, 2799) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(2800) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(2801) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (e(2809) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 2810, 2815) || e(2817)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2818, 2819) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))))))) : (l(2948) ? (l(2890) ? (l(2875) ? ((A2(r, 2821, 2828) || (A2(r, 2831, 2832) || (A2(r, 2835, 2856) || (A2(r, 2858, 2864) || (A2(r, 2866, 2867) || A2(r, 2869, 2873)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : ((e(2876) || (e(2879) || A2(r, 2881, 2884))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(2877) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(2878) || (e(2880) || A2(r, 2887, 2888))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : (l(2913) ? ((A2(r, 2891, 2892) || e(2903)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(2893) || A2(r, 2901, 2902)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 2908, 2909) || A2(r, 2911, 2912)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((e(2913) || (e(2929) || e(2947))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 2914, 2915) || e(2946)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 2918, 2927) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(2928) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 2930, 2935) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))))) : (l(3017) ? (l(2978) ? ((A2(r, 2949, 2954) || (A2(r, 2958, 2960) || (A2(r, 2962, 2965) || (A2(r, 2969, 2970) || (e(2972) || A2(r, 2974, 2975)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : ((A2(r, 2979, 2980) || (A2(r, 2984, 2986) || A2(r, 2990, 3001))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3006, 3007) || (A2(r, 3009, 3010) || A2(r, 3014, 3016))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(3008) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : (l(3058) ? ((A2(r, 3018, 3020) || e(3031)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(3021) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(3024) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 3046, 3055) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 3056, 3057) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))) : (e(3058) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 3059, 3064) || e(3066)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(3065) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : ((e(3072) || e(3076)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3073, 3075) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 3077, 3084) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))))))))) : (l(7695) ? (l(4881) ? (l(3763) ? (l(3389) ? (l(3217) ? (l(3167) ? ((A2(r, 3086, 3088) || (A2(r, 3090, 3112) || (A2(r, 3114, 3129) || (e(3133) || (A2(r, 3160, 3162) || e(3165)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(3132) || (A2(r, 3134, 3136) || (A2(r, 3142, 3144) || (A2(r, 3146, 3149) || A2(r, 3157, 3158))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3137, 3140) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((A2(r, 3168, 3169) || (e(3200) || (A2(r, 3205, 3212) || A2(r, 3214, 3216)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3170, 3171) || e(3201)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3174, 3183) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((e(3191) || e(3204)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 3192, 3198) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(3199) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 3202, 3203) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))))))) : (l(3284) ? ((A2(r, 3218, 3240) || (A2(r, 3242, 3251) || (A2(r, 3253, 3257) || e(3261)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(3260) || (e(3263) || (e(3270) || A2(r, 3276, 3277)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(3262) || (A2(r, 3264, 3268) || (A2(r, 3271, 3272) || A2(r, 3274, 3275)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : (l(3314) ? (A2(r, 3285, 3286) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 3293, 3294) || (A2(r, 3296, 3297) || e(3313))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 3298, 3299) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3302, 3311) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))) : ((e(3314) || (A2(r, 3332, 3340) || (A2(r, 3342, 3344) || A2(r, 3346, 3386)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(3315) || A2(r, 3330, 3331)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 3328, 3329) || A2(r, 3387, 3388)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(3534) ? (l(3425) ? ((e(3389) || (e(3406) || (A2(r, 3412, 3414) || A2(r, 3423, 3424)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3390, 3392) || (A2(r, 3398, 3400) || (A2(r, 3402, 3404) || e(3415)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 3393, 3396) || e(3405)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(3407) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 3416, 3422) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))) : (l(3457) ? ((e(3425) || A2(r, 3450, 3455)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 3426, 3427) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3430, 3439) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 3440, 3448) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(3449) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))))) : ((e(3457) || e(3530)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3458, 3459) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 3461, 3478) || (A2(r, 3482, 3505) || (A2(r, 3507, 3515) || (e(3517) || A2(r, 3520, 3526))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))) : (l(3653) ? (l(3571) ? ((A2(r, 3535, 3537) || (A2(r, 3544, 3551) || e(3570))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 3538, 3540) || e(3542)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3558, 3567) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))) : (e(3571) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(3572) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 3585, 3632) || (A2(r, 3634, 3635) || A2(r, 3648, 3652))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(3633) || A2(r, 3636, 3642)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(3647) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : $elm$core$Maybe$Nothing)))))) : (l(3715) ? ((e(3653) || A2(r, 3713, 3714)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(3654) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 3655, 3662) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(3663) || A2(r, 3674, 3675)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 3664, 3673) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : ((e(3716) || (A2(r, 3718, 3722) || (A2(r, 3724, 3747) || (e(3749) || (A2(r, 3751, 3760) || e(3762)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(3761) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(4151) ? (l(3898) ? (l(3859) ? ((e(3763) || (e(3773) || (A2(r, 3776, 3780) || (A2(r, 3804, 3807) || e(3840))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3764, 3772) || A2(r, 3784, 3790)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(3782) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 3792, 3801) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 3841, 3843) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 3844, 3858) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(3881) ? ((e(3859) || (A2(r, 3861, 3863) || A2(r, 3866, 3871))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(3860) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 3864, 3865) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 3872, 3880) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))) : (e(3881) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 3882, 3891) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((e(3892) || (e(3894) || e(3896))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(3893) || (e(3895) || e(3897))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(3980) ? (l(3912) ? ((e(3898) || e(3900)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(3899) || e(3901)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 3902, 3903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 3904, 3911) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : ((A2(r, 3913, 3948) || A2(r, 3976, 3979)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3953, 3966) || (A2(r, 3968, 3972) || A2(r, 3974, 3975))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(3967) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(3973) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (l(4047) ? (e(3980) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 3981, 3991) || (A2(r, 3993, 4028) || e(4038))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 4030, 4037) || (A2(r, 4039, 4044) || e(4046))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))) : ((e(4047) || A2(r, 4053, 4056)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 4048, 4052) || A2(r, 4057, 4058)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 4096, 4138) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 4139, 4140) || e(4145)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 4141, 4144) || A2(r, 4146, 4150)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))))) : (l(4238) ? (l(4189) ? ((e(4151) || (A2(r, 4153, 4154) || (A2(r, 4157, 4158) || A2(r, 4184, 4185)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(4152) || (A2(r, 4155, 4156) || A2(r, 4182, 4183))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(4159) || (A2(r, 4176, 4181) || A2(r, 4186, 4188))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 4160, 4169) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 4170, 4175) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (l(4208) ? ((e(4189) || (e(4193) || (A2(r, 4197, 4198) || A2(r, 4206, 4207)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 4190, 4192) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 4194, 4196) || A2(r, 4199, 4205)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((e(4208) || A2(r, 4213, 4225)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 4209, 4212) || (e(4226) || (A2(r, 4229, 4230) || e(4237)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 4227, 4228) || A2(r, 4231, 4236)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))) : (l(4351) ? (l(4255) ? (e(4238) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(4239) || A2(r, 4250, 4252)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 4240, 4249) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(4253) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(4254) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))))) : (e(4255) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 4256, 4293) || (e(4295) || e(4301))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 4304, 4346) || A2(r, 4349, 4350)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(4347) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(4348) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing)))))) : (l(4745) ? (e(4351) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 4352, 4680) || (A2(r, 4682, 4685) || (A2(r, 4688, 4694) || (e(4696) || (A2(r, 4698, 4701) || A2(r, 4704, 4744)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)) : ((A2(r, 4746, 4749) || (A2(r, 4752, 4784) || (A2(r, 4786, 4789) || (A2(r, 4792, 4798) || (e(4800) || (A2(r, 4802, 4805) || (A2(r, 4808, 4822) || A2(r, 4824, 4880)))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))) : (l(6764) ? (l(6143) ? (l(5918) ? (l(5741) ? ((A2(r, 4882, 4885) || (A2(r, 4888, 4954) || (A2(r, 4992, 5007) || A2(r, 5121, 5740)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 4957, 4959) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 4960, 4968) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 4969, 4988) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 5008, 5017) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 5024, 5109) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 5112, 5117) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(5120) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing)))))))) : (l(5791) ? (e(5741) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(5742) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 5743, 5759) || A2(r, 5761, 5786)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(5760) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : (e(5787) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(5788) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))))) : ((A2(r, 5792, 5866) || (A2(r, 5873, 5880) || A2(r, 5888, 5905))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 5867, 5869) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 5870, 5872) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (A2(r, 5906, 5908) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(5909) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))))) : (l(6070) ? ((A2(r, 5919, 5937) || (A2(r, 5952, 5969) || (A2(r, 5984, 5996) || (A2(r, 5998, 6000) || A2(r, 6016, 6067))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 5938, 5939) || (A2(r, 5970, 5971) || (A2(r, 6002, 6003) || A2(r, 6068, 6069)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(5940) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 5941, 5942) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : (l(6102) ? ((e(6070) || (A2(r, 6078, 6085) || A2(r, 6087, 6088))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 6071, 6077) || (e(6086) || A2(r, 6089, 6099))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 6100, 6101) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))) : ((e(6102) || A2(r, 6104, 6106)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(6103) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(6107) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (e(6108) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(6109) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 6112, 6121) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 6128, 6137) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing)))))))))) : (l(6463) ? (l(6278) ? ((A2(r, 6144, 6149) || A2(r, 6151, 6154)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(6150) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : ((A2(r, 6155, 6157) || (e(6159) || e(6277))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(6158) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (A2(r, 6160, 6169) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 6176, 6210) || (A2(r, 6212, 6264) || A2(r, 6272, 6276))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(6211) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))))) : (l(6434) ? ((e(6278) || (e(6313) || A2(r, 6432, 6433))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 6279, 6312) || (e(6314) || (A2(r, 6320, 6389) || A2(r, 6400, 6430)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)) : ((e(6434) || (A2(r, 6439, 6440) || (e(6450) || A2(r, 6457, 6459)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 6435, 6438) || (A2(r, 6441, 6443) || (A2(r, 6448, 6449) || A2(r, 6451, 6456)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : (l(6680) ? ((e(6464) || A2(r, 6622, 6655)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 6468, 6469) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 6470, 6479) || A2(r, 6608, 6617)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 6480, 6509) || (A2(r, 6512, 6516) || (A2(r, 6528, 6571) || (A2(r, 6576, 6601) || A2(r, 6656, 6678))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(6618) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(6679) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(6742) ? ((e(6680) || e(6683)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 6681, 6682) || e(6741)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 6686, 6687) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 6688, 6740) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : ((e(6742) || (A2(r, 6744, 6750) || (e(6752) || (e(6754) || A2(r, 6757, 6763))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(6743) || (e(6753) || A2(r, 6755, 6756))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))))) : (l(7167) ? (l(7001) ? (l(6911) ? ((e(6764) || (A2(r, 6771, 6780) || (e(6783) || (A2(r, 6832, 6845) || A2(r, 6847, 6862))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 6765, 6770) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 6784, 6793) || A2(r, 6800, 6809)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 6816, 6822) || A2(r, 6824, 6829)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(6823) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(6846) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkEnclosing) : $elm$core$Maybe$Nothing)))))) : (l(6970) ? ((A2(r, 6912, 6915) || (e(6964) || A2(r, 6966, 6969))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(6916) || e(6965)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 6917, 6963) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((e(6970) || (e(6972) || e(6978))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(6971) || (A2(r, 6973, 6977) || A2(r, 6979, 6980))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 6981, 6988) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 6992, 7000) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))) : (l(7081) ? (l(7039) ? (e(7001) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 7002, 7008) || A2(r, 7037, 7038)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 7009, 7018) || A2(r, 7028, 7036)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 7019, 7027) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : ((A2(r, 7040, 7041) || (A2(r, 7074, 7077) || e(7080))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(7042) || (e(7073) || A2(r, 7078, 7079))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 7043, 7072) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (l(7142) ? ((e(7081) || A2(r, 7083, 7085)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(7082) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 7086, 7087) || A2(r, 7098, 7141)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 7088, 7097) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))) : ((e(7142) || (A2(r, 7144, 7145) || (e(7149) || A2(r, 7151, 7153)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(7143) || (A2(r, 7146, 7148) || (e(7150) || A2(r, 7154, 7155)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 7164, 7166) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(7412) ? (l(7293) ? ((e(7167) || A2(r, 7227, 7231)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 7168, 7203) || (A2(r, 7245, 7247) || A2(r, 7258, 7287))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 7204, 7211) || A2(r, 7220, 7221)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 7212, 7219) || A2(r, 7222, 7223)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 7232, 7241) || A2(r, 7248, 7257)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 7288, 7292) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing)))))) : (l(7378) ? (e(7293) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 7294, 7295) || A2(r, 7360, 7367)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 7296, 7304) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 7312, 7354) || A2(r, 7357, 7359)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 7376, 7377) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))) : ((e(7378) || (A2(r, 7380, 7392) || (A2(r, 7394, 7400) || e(7405)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(7379) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(7393) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 7401, 7404) || A2(r, 7406, 7411)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))) : (l(7680) ? ((e(7412) || (A2(r, 7416, 7417) || A2(r, 7616, 7679))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 7413, 7414) || e(7418)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(7415) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 7424, 7467) || (A2(r, 7531, 7543) || A2(r, 7545, 7578))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 7468, 7530) || (e(7544) || A2(r, 7579, 7615))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))) : (l(7686) ? ((e(7680) || (e(7682) || e(7684))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7681) || (e(7683) || e(7685))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(7686) || (e(7688) || (e(7690) || (e(7692) || e(7694))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7687) || (e(7689) || (e(7691) || e(7693)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))))) : (l(7904) ? (l(7794) ? (l(7743) ? (l(7718) ? (l(7705) ? ((e(7695) || (e(7697) || (e(7699) || (e(7701) || e(7703))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7696) || (e(7698) || (e(7700) || (e(7702) || e(7704))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(7710) ? ((e(7705) || (e(7707) || e(7709))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7706) || e(7708)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7710) || (e(7712) || (e(7714) || e(7716)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7711) || (e(7713) || (e(7715) || e(7717)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(7729) ? ((e(7718) || (e(7720) || (e(7722) || (e(7724) || (e(7726) || e(7728)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7719) || (e(7721) || (e(7723) || (e(7725) || e(7727))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(7735) ? ((e(7729) || (e(7731) || e(7733))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7730) || (e(7732) || e(7734))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7735) || (e(7737) || (e(7739) || e(7741)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7736) || (e(7738) || (e(7740) || e(7742)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))) : (l(7767) ? (l(7754) ? ((e(7743) || (e(7745) || (e(7747) || (e(7749) || (e(7751) || e(7753)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7744) || (e(7746) || (e(7748) || (e(7750) || e(7752))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(7759) ? ((e(7754) || (e(7756) || e(7758))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7755) || e(7757)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(7759) || (e(7761) || (e(7763) || e(7765)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7760) || (e(7762) || (e(7764) || e(7766)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(7779) ? ((e(7767) || (e(7769) || (e(7771) || (e(7773) || (e(7775) || e(7777)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7768) || (e(7770) || (e(7772) || (e(7774) || (e(7776) || e(7778)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(7785) ? ((e(7779) || (e(7781) || e(7783))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7780) || (e(7782) || e(7784))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7785) || (e(7787) || (e(7789) || (e(7791) || e(7793))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7786) || (e(7788) || (e(7790) || e(7792)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))))) : (l(7852) ? (l(7818) ? (l(7805) ? ((e(7794) || (e(7796) || (e(7798) || (e(7800) || (e(7802) || e(7804)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7795) || (e(7797) || (e(7799) || (e(7801) || e(7803))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(7810) ? ((e(7805) || (e(7807) || e(7809))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7806) || e(7808)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7810) || (e(7812) || (e(7814) || e(7816)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7811) || (e(7813) || (e(7815) || e(7817)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(7838) ? ((e(7818) || (e(7820) || (e(7822) || (e(7824) || (e(7826) || e(7828)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7819) || (e(7821) || (e(7823) || (e(7825) || (e(7827) || A2(r, 7829, 7837)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(7844) ? ((e(7838) || (e(7840) || e(7842))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7839) || (e(7841) || e(7843))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(7844) || (e(7846) || (e(7848) || e(7850)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7845) || (e(7847) || (e(7849) || e(7851)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(7877) ? (l(7863) ? ((e(7852) || (e(7854) || (e(7856) || (e(7858) || (e(7860) || e(7862)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7853) || (e(7855) || (e(7857) || (e(7859) || e(7861))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(7869) ? ((e(7863) || (e(7865) || e(7867))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7864) || (e(7866) || e(7868))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7869) || (e(7871) || (e(7873) || e(7875)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7870) || (e(7872) || (e(7874) || e(7876)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(7889) ? ((e(7877) || (e(7879) || (e(7881) || (e(7883) || (e(7885) || e(7887)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7878) || (e(7880) || (e(7882) || (e(7884) || (e(7886) || e(7888)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(7895) ? ((e(7889) || (e(7891) || e(7893))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7890) || (e(7892) || e(7894))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7895) || (e(7897) || (e(7899) || (e(7901) || e(7903))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7896) || (e(7898) || (e(7900) || e(7902)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))))) : (l(8273) ? (l(8103) ? (l(7928) ? (l(7915) ? ((e(7904) || (e(7906) || (e(7908) || (e(7910) || (e(7912) || e(7914)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7905) || (e(7907) || (e(7909) || (e(7911) || e(7913))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(7920) ? ((e(7915) || (e(7917) || e(7919))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7916) || e(7918)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(7920) || (e(7922) || (e(7924) || e(7926)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7921) || (e(7923) || (e(7925) || e(7927)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(7983) ? (l(7933) ? ((e(7928) || (e(7930) || e(7932))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(7929) || e(7931)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(7933) || (A2(r, 7935, 7943) || (A2(r, 7952, 7957) || A2(r, 7968, 7975)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(7934) || (A2(r, 7944, 7951) || (A2(r, 7960, 7965) || A2(r, 7976, 7982)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : (l(8039) ? ((e(7983) || (A2(r, 7992, 7999) || (A2(r, 8008, 8013) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && A2(r, 8025, 8031))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 7984, 7991) || (A2(r, 8000, 8005) || (A2(r, 8016, 8023) || A2(r, 8032, 8038)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(8039) || (A2(r, 8048, 8061) || (A2(r, 8064, 8071) || (A2(r, 8080, 8087) || A2(r, 8096, 8102))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 8040, 8047) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 8072, 8079) || A2(r, 8088, 8095)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : $elm$core$Maybe$Nothing)))))) : (l(8191) ? (l(8140) ? ((e(8103) || (A2(r, 8112, 8116) || (A2(r, 8118, 8119) || (e(8126) || (A2(r, 8130, 8132) || A2(r, 8134, 8135)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 8104, 8111) || e(8124)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : ((A2(r, 8120, 8123) || A2(r, 8136, 8139)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(8125) || A2(r, 8127, 8129)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing)))) : (l(8167) ? (e(8140) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : ((A2(r, 8141, 8143) || A2(r, 8157, 8159)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : ((A2(r, 8144, 8147) || (A2(r, 8150, 8151) || A2(r, 8160, 8166))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 8152, 8155) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : ((e(8167) || (A2(r, 8178, 8180) || A2(r, 8182, 8183))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 8168, 8172) || A2(r, 8184, 8187)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 8173, 8175) || A2(r, 8189, 8190)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (e(8188) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterTitlecase) : $elm$core$Maybe$Nothing)))))) : (l(8232) ? (A2(r, 8192, 8202) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : (A2(r, 8203, 8207) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (A2(r, 8208, 8213) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : ((A2(r, 8214, 8215) || A2(r, 8224, 8231)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(8216) || (A2(r, 8219, 8220) || e(8223))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : ((e(8217) || e(8221)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : ((e(8218) || e(8222)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing))))))) : (l(8250) ? (e(8232) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorLine) : (e(8233) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorParagraph) : (A2(r, 8234, 8238) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (e(8239) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : (A2(r, 8240, 8248) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(8249) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : $elm$core$Maybe$Nothing)))))) : (e(8250) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : ((A2(r, 8251, 8254) || (A2(r, 8257, 8259) || A2(r, 8263, 8272))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 8255, 8256) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : (e(8260) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(8261) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(8262) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))))))))) : (l(8495) ? (l(8420) ? (l(8316) ? ((e(8273) || (e(8275) || A2(r, 8277, 8286))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(8274) || A2(r, 8314, 8315)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(8276) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : (e(8287) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : ((A2(r, 8288, 8292) || A2(r, 8294, 8303)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((e(8304) || A2(r, 8308, 8313)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(8305) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))))) : (l(8333) ? ((e(8316) || A2(r, 8330, 8332)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(8317) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(8318) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (e(8319) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 8320, 8329) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))) : (e(8333) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(8334) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 8336, 8348) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 8352, 8384) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : ((A2(r, 8400, 8412) || e(8417)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 8413, 8416) || A2(r, 8418, 8419)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkEnclosing) : $elm$core$Maybe$Nothing)))))))) : (l(8468) ? (e(8420) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkEnclosing) : (A2(r, 8421, 8432) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 8448, 8449) || (A2(r, 8451, 8454) || A2(r, 8456, 8457))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(8450) || (e(8455) || (A2(r, 8459, 8461) || A2(r, 8464, 8466)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(8458) || (A2(r, 8462, 8463) || e(8467))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(8484) ? ((e(8468) || (A2(r, 8470, 8471) || A2(r, 8478, 8483))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(8469) || A2(r, 8473, 8477)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(8472) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))) : ((e(8484) || (e(8486) || (e(8488) || A2(r, 8490, 8493)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(8485) || (e(8487) || (e(8489) || e(8494)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))))) : (l(8603) ? (l(8523) ? ((e(8495) || (e(8500) || (e(8505) || (A2(r, 8508, 8509) || A2(r, 8518, 8521))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 8496, 8499) || (A2(r, 8510, 8511) || e(8517))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 8501, 8504) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 8506, 8507) || e(8522)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 8512, 8516) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))))) : (l(8579) ? (e(8523) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 8524, 8525) || e(8527)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(8526) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 8528, 8543) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 8544, 8578) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : $elm$core$Maybe$Nothing))))) : (e(8579) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(8580) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 8581, 8584) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (e(8585) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 8586, 8587) || A2(r, 8597, 8601)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 8592, 8596) || e(8602)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing)))))))) : (l(8659) ? (l(8613) ? ((e(8603) || (e(8608) || e(8611))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 8604, 8607) || (A2(r, 8609, 8610) || e(8612))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)) : ((e(8613) || (A2(r, 8615, 8621) || (A2(r, 8623, 8653) || A2(r, 8656, 8657)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(8614) || (e(8622) || (A2(r, 8654, 8655) || e(8658)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))) : (l(8969) ? ((e(8659) || (A2(r, 8661, 8691) || A2(r, 8960, 8967))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(8660) || A2(r, 8692, 8959)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(8968) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing))) : ((e(8969) || (e(8971) || e(9002))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(8970) || e(9001)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((A2(r, 8972, 8991) || (A2(r, 8994, 9000) || A2(r, 9003, 9083))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 8992, 8993) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing)))))))))))) : (l(65103) ? (l(42587) ? (l(11483) ? (l(11370) ? (l(10223) ? (l(10092) ? (l(9654) ? ((e(9084) || (A2(r, 9115, 9139) || A2(r, 9180, 9185))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 9085, 9114) || (A2(r, 9140, 9179) || (A2(r, 9186, 9254) || (A2(r, 9280, 9290) || (A2(r, 9372, 9449) || A2(r, 9472, 9653)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 9312, 9371) || A2(r, 9450, 9471)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))) : (l(9727) ? ((e(9654) || (A2(r, 9656, 9664) || A2(r, 9666, 9719))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(9655) || (e(9665) || A2(r, 9720, 9726))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing)) : ((e(9727) || e(9839)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 9728, 9838) || A2(r, 9840, 10087)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(10088) || e(10090)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(10089) || e(10091)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))))) : (l(10175) ? ((e(10092) || (e(10094) || (e(10096) || (e(10098) || e(10100))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(10093) || (e(10095) || (e(10097) || (e(10099) || e(10101))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 10102, 10131) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 10132, 10174) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : (l(10215) ? (e(10175) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 10176, 10180) || A2(r, 10183, 10213)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((e(10181) || e(10214)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(10182) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))) : ((e(10215) || (e(10217) || (e(10219) || e(10221)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(10216) || (e(10218) || (e(10220) || e(10222)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing))))) : (l(10647) ? (l(10634) ? ((e(10223) || (e(10628) || (e(10630) || e(10632)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((A2(r, 10224, 10239) || A2(r, 10496, 10626)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (A2(r, 10240, 10495) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(10627) || (e(10629) || (e(10631) || e(10633)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing)))) : (l(10639) ? ((e(10634) || (e(10636) || e(10638))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(10635) || e(10637)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing)) : ((e(10639) || (e(10641) || (e(10643) || e(10645)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(10640) || (e(10642) || (e(10644) || e(10646)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))) : (l(11076) ? (l(10714) ? ((e(10647) || e(10712)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(10648) || e(10713)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 10649, 10711) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))) : ((e(10714) || e(10748)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(10715) || e(10749)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((A2(r, 10716, 10747) || (A2(r, 10750, 11007) || A2(r, 11056, 11075))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (A2(r, 11008, 11055) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))))) : (l(11311) ? ((e(11076) || A2(r, 11079, 11084)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 11077, 11078) || (A2(r, 11085, 11123) || (A2(r, 11126, 11157) || A2(r, 11159, 11263)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 11264, 11310) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : ((e(11311) || (e(11360) || (A2(r, 11362, 11364) || (e(11367) || e(11369))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 11312, 11359) || (e(11361) || (A2(r, 11365, 11366) || e(11368)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))) : (l(11431) ? (l(11405) ? (l(11392) ? ((e(11370) || (e(11372) || (e(11377) || (A2(r, 11379, 11380) || A2(r, 11382, 11387))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11371) || (A2(r, 11373, 11376) || (e(11378) || (e(11381) || A2(r, 11390, 11391))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 11388, 11389) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))) : (l(11397) ? ((e(11392) || (e(11394) || e(11396))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11393) || e(11395)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(11397) || (e(11399) || (e(11401) || e(11403)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11398) || (e(11400) || (e(11402) || e(11404)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))) : (l(11417) ? ((e(11405) || (e(11407) || (e(11409) || (e(11411) || (e(11413) || e(11415)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11406) || (e(11408) || (e(11410) || (e(11412) || (e(11414) || e(11416)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(11423) ? ((e(11417) || (e(11419) || e(11421))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11418) || (e(11420) || e(11422))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(11423) || (e(11425) || (e(11427) || e(11429)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11424) || (e(11426) || (e(11428) || e(11430)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))) : (l(11456) ? (l(11442) ? ((e(11431) || (e(11433) || (e(11435) || (e(11437) || (e(11439) || e(11441)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11432) || (e(11434) || (e(11436) || (e(11438) || e(11440))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(11448) ? ((e(11442) || (e(11444) || e(11446))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11443) || (e(11445) || e(11447))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(11448) || (e(11450) || (e(11452) || e(11454)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11449) || (e(11451) || (e(11453) || e(11455)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(11468) ? ((e(11456) || (e(11458) || (e(11460) || (e(11462) || (e(11464) || e(11466)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11457) || (e(11459) || (e(11461) || (e(11463) || (e(11465) || e(11467)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(11474) ? ((e(11468) || (e(11470) || e(11472))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11469) || (e(11471) || e(11473))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(11474) || (e(11476) || (e(11478) || (e(11480) || e(11482))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(11475) || (e(11477) || (e(11479) || e(11481)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))))) : (l(12300) ? (l(11799) ? (l(11630) ? (l(11500) ? ((e(11483) || (e(11485) || (e(11487) || (e(11489) || A2(r, 11491, 11492))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11484) || (e(11486) || (e(11488) || (e(11490) || e(11499))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 11493, 11498) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))) : (l(11512) ? ((e(11500) || (e(11502) || e(11507))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(11501) || e(11506)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 11503, 11505) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((A2(r, 11513, 11516) || A2(r, 11518, 11519)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(11517) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 11520, 11557) || (e(11559) || e(11565))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 11568, 11623) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))) : (l(11743) ? (e(11631) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(11632) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(11647) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 11648, 11670) || (A2(r, 11680, 11686) || (A2(r, 11688, 11694) || (A2(r, 11696, 11702) || (A2(r, 11704, 11710) || (A2(r, 11712, 11718) || (A2(r, 11720, 11726) || (A2(r, 11728, 11734) || A2(r, 11736, 11742))))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (l(11781) ? (A2(r, 11744, 11775) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 11776, 11777) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(11778) || e(11780)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : (e(11779) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : $elm$core$Maybe$Nothing)))) : ((e(11781) || (e(11786) || e(11789))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : ((A2(r, 11782, 11784) || (e(11787) || A2(r, 11790, 11798))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(11785) || e(11788)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : $elm$core$Maybe$Nothing)))))) : (l(11842) ? (l(11812) ? ((e(11799) || e(11802)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : ((A2(r, 11800, 11801) || (e(11803) || A2(r, 11806, 11807))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(11804) || e(11808)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationInitialQuote) : ((e(11805) || e(11809)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationFinalQuote) : (e(11810) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(11811) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))))) : (l(11817) ? ((e(11812) || (e(11814) || e(11816))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(11813) || e(11815)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)) : (e(11817) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((A2(r, 11818, 11822) || (A2(r, 11824, 11833) || (A2(r, 11836, 11839) || e(11841)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(11823) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 11834, 11835) || e(11840)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing)))))) : (l(11903) ? (l(11862) ? ((e(11842) || e(11861)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((A2(r, 11843, 11855) || A2(r, 11858, 11860)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 11856, 11857) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))) : ((e(11862) || (e(11864) || (e(11866) || e(11868)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(11863) || (e(11865) || e(11867))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(11869) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing)))) : (l(12292) ? ((A2(r, 11904, 11929) || (A2(r, 11931, 12019) || (A2(r, 12032, 12245) || A2(r, 12272, 12287)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(12288) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SeparatorSpace) : (A2(r, 12289, 12291) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))) : (e(12292) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(12293) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(12294) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(12295) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : ((e(12296) || e(12298)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(12297) || e(12299)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)))))))))) : (l(12841) ? (l(12343) ? (l(12312) ? ((e(12300) || (e(12302) || (e(12304) || (e(12308) || e(12310))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(12301) || (e(12303) || (e(12305) || (e(12309) || e(12311))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 12306, 12307) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))) : (l(12319) ? ((e(12312) || (e(12314) || e(12317))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(12313) || (e(12315) || e(12318))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (e(12316) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing))) : (e(12319) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(12320) || e(12342)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 12321, 12329) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (A2(r, 12330, 12333) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 12334, 12335) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(12336) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : (A2(r, 12337, 12341) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))))))) : (l(12538) ? (l(12352) ? ((e(12343) || A2(r, 12350, 12351)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 12344, 12346) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (e(12347) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(12348) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(12349) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : ((A2(r, 12353, 12438) || (e(12447) || A2(r, 12449, 12537))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 12441, 12442) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 12443, 12444) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (A2(r, 12445, 12446) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(12448) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing)))))) : (l(12689) ? ((e(12538) || (e(12543) || (A2(r, 12549, 12591) || A2(r, 12593, 12686)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(12539) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 12540, 12542) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(12688) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : ((e(12689) || (A2(r, 12694, 12703) || (A2(r, 12736, 12771) || (e(12783) || A2(r, 12800, 12830))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 12690, 12693) || A2(r, 12832, 12840)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 12704, 12735) || A2(r, 12784, 12799)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))) : (l(42560) ? (l(19967) ? ((e(12841) || (A2(r, 12872, 12879) || (A2(r, 12881, 12895) || (A2(r, 12928, 12937) || A2(r, 12977, 12991))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 12842, 12871) || (e(12880) || (A2(r, 12896, 12927) || (A2(r, 12938, 12976) || (A2(r, 12992, 13311) || A2(r, 19904, 19966)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 13312, 19903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : (l(42237) ? ((e(19967) || A2(r, 42128, 42182)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 19968, 40980) || (A2(r, 40982, 42124) || A2(r, 42192, 42231))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(40981) || A2(r, 42232, 42236)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))) : ((e(42237) || e(42508)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 42238, 42239) || A2(r, 42509, 42511)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 42240, 42507) || (A2(r, 42512, 42527) || A2(r, 42538, 42539))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 42528, 42537) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))) : (l(42572) ? ((e(42560) || (e(42562) || (e(42564) || (e(42566) || (e(42568) || e(42570)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42561) || (e(42563) || (e(42565) || (e(42567) || (e(42569) || e(42571)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(42578) ? ((e(42572) || (e(42574) || e(42576))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42573) || (e(42575) || e(42577))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(42578) || (e(42580) || (e(42582) || (e(42584) || e(42586))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42579) || (e(42581) || (e(42583) || e(42585)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))))) : (l(42945) ? (l(42824) ? (l(42646) ? (l(42621) ? (l(42598) ? ((e(42587) || (e(42589) || (e(42591) || (e(42593) || (e(42595) || e(42597)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42588) || (e(42590) || (e(42592) || (e(42594) || e(42596))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(42603) ? ((e(42598) || (e(42600) || e(42602))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42599) || e(42601)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(42603) || e(42605)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(42604) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(42606) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(42607) || A2(r, 42612, 42620)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 42608, 42610) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkEnclosing) : (e(42611) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))))) : (l(42632) ? (e(42621) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(42622) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(42623) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((e(42624) || (e(42626) || (e(42628) || e(42630)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42625) || (e(42627) || (e(42629) || e(42631)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(42638) ? ((e(42632) || (e(42634) || e(42636))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42633) || (e(42635) || e(42637))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(42638) || (e(42640) || (e(42642) || e(42644)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42639) || (e(42641) || (e(42643) || e(42645)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(42795) ? (l(42751) ? ((e(42646) || (e(42648) || e(42650))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42647) || (e(42649) || e(42651))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 42652, 42653) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 42654, 42655) || A2(r, 42736, 42737)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 42656, 42725) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 42726, 42735) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (A2(r, 42738, 42743) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))))) : ((A2(r, 42752, 42774) || A2(r, 42784, 42785)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (A2(r, 42775, 42783) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((e(42786) || (e(42788) || (e(42790) || (e(42792) || e(42794))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42787) || (e(42789) || (e(42791) || e(42793)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(42809) ? ((e(42795) || (e(42797) || (A2(r, 42799, 42801) || (e(42803) || (e(42805) || e(42807)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42796) || (e(42798) || (e(42802) || (e(42804) || (e(42806) || e(42808)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(42815) ? ((e(42809) || (e(42811) || e(42813))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42810) || (e(42812) || e(42814))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(42815) || (e(42817) || (e(42819) || (e(42821) || e(42823))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42816) || (e(42818) || (e(42820) || e(42822)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))))) : (l(42882) ? (l(42848) ? (l(42835) ? ((e(42824) || (e(42826) || (e(42828) || (e(42830) || (e(42832) || e(42834)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42825) || (e(42827) || (e(42829) || (e(42831) || e(42833))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(42840) ? ((e(42835) || (e(42837) || e(42839))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42836) || e(42838)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((e(42840) || (e(42842) || (e(42844) || e(42846)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42841) || (e(42843) || (e(42845) || e(42847)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(42860) ? ((e(42848) || (e(42850) || (e(42852) || (e(42854) || (e(42856) || e(42858)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42849) || (e(42851) || (e(42853) || (e(42855) || (e(42857) || e(42859)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(42873) ? ((e(42860) || e(42862)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42861) || (e(42863) || A2(r, 42865, 42872))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(42864) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))) : ((e(42873) || (e(42875) || (A2(r, 42877, 42878) || e(42880)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42874) || (e(42876) || (e(42879) || e(42881)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))) : (l(42910) ? (l(42894) ? ((e(42882) || (e(42884) || (e(42886) || (e(42891) || e(42893))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42883) || (e(42885) || (e(42887) || e(42892)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(42888) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 42889, 42890) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing)))) : (l(42902) ? ((e(42894) || (e(42897) || A2(r, 42899, 42901))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(42895) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(42896) || e(42898)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : ((e(42902) || (e(42904) || (e(42906) || e(42908)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42903) || (e(42905) || (e(42907) || e(42909)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(42926) ? (l(42915) ? ((e(42910) || (e(42912) || e(42914))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42911) || e(42913)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(42915) || (e(42917) || (e(42919) || e(42921)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42916) || (e(42918) || (e(42920) || A2(r, 42922, 42925)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : (l(42936) ? ((e(42926) || (A2(r, 42928, 42932) || e(42934))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42927) || (e(42933) || e(42935))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : ((e(42936) || (e(42938) || (e(42940) || (e(42942) || e(42944))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((e(42937) || (e(42939) || (e(42941) || e(42943)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))))) : (l(43697) ? (l(43273) ? (l(43042) ? (l(42993) ? ((e(42945) || (e(42947) || (e(42952) || (e(42954) || (e(42967) || (e(42969) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && A2(r, 42961, 42965)))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(42946) || (A2(r, 42948, 42951) || (e(42953) || (e(42960) || (e(42966) || e(42968)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : (l(43002) ? ((A2(r, 42994, 42996) || A2(r, 43000, 43001)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(42997) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(42998) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(42999) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (e(43002) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 43003, 43009) || (A2(r, 43011, 43013) || (A2(r, 43015, 43018) || A2(r, 43020, 43041)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(43010) || (e(43014) || e(43019))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))) : (l(43137) ? (l(43055) ? (e(43042) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43043, 43044) || e(43047)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 43045, 43046) || e(43052)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 43048, 43051) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : (A2(r, 43056, 43061) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 43062, 43063) || e(43065)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(43064) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (A2(r, 43072, 43123) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 43124, 43127) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(43136) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))))) : (l(43249) ? ((e(43137) || A2(r, 43188, 43203)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 43138, 43187) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43204, 43205) || A2(r, 43232, 43248)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 43214, 43215) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 43216, 43225) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : ((e(43249) || e(43263)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 43250, 43255) || (e(43259) || A2(r, 43261, 43262))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43256, 43258) || e(43260)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 43264, 43272) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))))) : (l(43494) ? (l(43442) ? (e(43273) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 43274, 43301) || (A2(r, 43312, 43334) || (A2(r, 43360, 43388) || A2(r, 43396, 43441)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43302, 43309) || (A2(r, 43335, 43345) || A2(r, 43392, 43394))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 43310, 43311) || e(43359)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 43346, 43347) || e(43395)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))) : (l(43453) ? (e(43442) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(43443) || (A2(r, 43446, 43449) || e(43452))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 43444, 43445) || A2(r, 43450, 43451)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((e(43453) || e(43493)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 43454, 43456) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 43457, 43469) || A2(r, 43486, 43487)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(43471) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 43472, 43481) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 43488, 43492) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))))) : (l(43595) ? (l(43566) ? (e(43494) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 43495, 43503) || (A2(r, 43514, 43518) || A2(r, 43520, 43560))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 43504, 43513) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 43561, 43565) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : ((e(43566) || (A2(r, 43569, 43570) || (A2(r, 43573, 43574) || e(43587)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 43567, 43568) || A2(r, 43571, 43572)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 43584, 43586) || A2(r, 43588, 43594)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (l(43632) ? ((e(43595) || A2(r, 43616, 43631)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(43596) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(43597) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 43600, 43609) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 43612, 43615) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (e(43632) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 43633, 43638) || (e(43642) || A2(r, 43646, 43695))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 43639, 43641) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(43643) || e(43645)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(43644) || e(43696)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))))))) : (l(64274) ? (l(43815) ? (l(43743) ? ((e(43697) || (A2(r, 43701, 43702) || (A2(r, 43705, 43709) || (e(43712) || (e(43714) || A2(r, 43739, 43740)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43698, 43700) || (A2(r, 43703, 43704) || (A2(r, 43710, 43711) || e(43713)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(43741) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(43742) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : (l(43762) ? ((e(43743) || A2(r, 43760, 43761)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 43744, 43754) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(43755) || A2(r, 43758, 43759)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 43756, 43757) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : ((e(43762) || (A2(r, 43777, 43782) || (A2(r, 43785, 43790) || (A2(r, 43793, 43798) || A2(r, 43808, 43814))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 43763, 43764) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(43765) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(43766) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(44008) ? ((A2(r, 43816, 43822) || A2(r, 43968, 44002)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 43824, 43866) || (A2(r, 43872, 43880) || A2(r, 43888, 43967))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(43867) || A2(r, 43882, 43883)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : ((A2(r, 43868, 43871) || e(43881)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 44003, 44004) || A2(r, 44006, 44007)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(44005) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : (l(55215) ? ((e(44008) || e(44013)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 44009, 44010) || e(44012)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(44011) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 44016, 44025) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 44032, 55203) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))) : ((A2(r, 55216, 55238) || (A2(r, 55243, 55291) || (A2(r, 63744, 64109) || A2(r, 64112, 64217)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 55296, 57343) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherSurrogate) : (A2(r, 57344, 63743) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherPrivateUse) : (A2(r, 64256, 64262) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))))) : (l(65048) ? (l(64466) ? (A2(r, 64275, 64279) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(64285) || (A2(r, 64287, 64296) || (A2(r, 64298, 64310) || (A2(r, 64312, 64316) || (e(64318) || (A2(r, 64320, 64321) || (A2(r, 64323, 64324) || A2(r, 64326, 64433)))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(64286) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(64297) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (A2(r, 64434, 64450) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing))))) : (l(64974) ? ((A2(r, 64467, 64829) || (A2(r, 64848, 64911) || A2(r, 64914, 64967))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(64830) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (e(64831) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (A2(r, 64832, 64847) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : ((e(64975) || A2(r, 65021, 65023)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 65008, 65019) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(65020) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : (A2(r, 65024, 65039) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 65040, 65046) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(65047) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing)))))))) : (l(65083) ? ((e(65048) || (e(65078) || (e(65080) || e(65082)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(65049) || e(65072)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 65056, 65071) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 65073, 65074) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : (A2(r, 65075, 65076) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : ((e(65077) || (e(65079) || e(65081))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : $elm$core$Maybe$Nothing)))))) : (l(65089) ? ((e(65083) || (e(65085) || e(65087))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(65084) || (e(65086) || e(65088))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing)) : ((e(65089) || (e(65091) || e(65095))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(65090) || (e(65092) || e(65096))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((A2(r, 65093, 65094) || A2(r, 65097, 65100)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 65101, 65102) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : $elm$core$Maybe$Nothing))))))))))) : (l(71996) ? (l(69404) ? (l(66421) ? (l(65378) ? (l(65288) ? (l(65121) ? (e(65103) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : ((A2(r, 65104, 65106) || (A2(r, 65108, 65111) || A2(r, 65119, 65120))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(65112) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : ((e(65113) || (e(65115) || e(65117))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(65114) || (e(65116) || e(65118))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : $elm$core$Maybe$Nothing))))) : (l(65129) ? ((e(65121) || e(65128)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(65122) || A2(r, 65124, 65126)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(65123) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing))) : ((e(65129) || e(65284)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : ((A2(r, 65130, 65131) || (A2(r, 65281, 65283) || A2(r, 65285, 65287))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 65136, 65140) || A2(r, 65142, 65276)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(65279) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : $elm$core$Maybe$Nothing)))))) : (l(65339) ? (e(65288) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(65289) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(65290) || (e(65292) || (A2(r, 65294, 65295) || (A2(r, 65306, 65307) || A2(r, 65311, 65312))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(65291) || A2(r, 65308, 65310)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(65293) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : (A2(r, 65296, 65305) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 65313, 65338) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))))))) : (l(65370) ? (e(65339) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(65340) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(65341) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : ((e(65342) || e(65344)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : (e(65343) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationConnector) : (A2(r, 65345, 65369) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))))) : (e(65370) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((e(65371) || e(65375)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : ((e(65372) || e(65374)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((e(65373) || e(65376)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (e(65377) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))))) : (l(65598) ? (l(65505) ? (l(65437) ? (e(65378) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOpen) : (e(65379) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationClose) : (A2(r, 65380, 65381) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 65382, 65391) || A2(r, 65393, 65436)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(65392) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))) : ((e(65437) || (A2(r, 65440, 65470) || (A2(r, 65474, 65479) || (A2(r, 65482, 65487) || (A2(r, 65490, 65495) || A2(r, 65498, 65500)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 65438, 65439) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(65504) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : $elm$core$Maybe$Nothing)))) : (l(65516) ? ((e(65505) || A2(r, 65509, 65510)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : ((e(65506) || A2(r, 65513, 65515)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : (e(65507) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : ((e(65508) || e(65512)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : (e(65516) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 65517, 65518) || A2(r, 65532, 65533)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 65529, 65531) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((A2(r, 65536, 65547) || (A2(r, 65549, 65574) || (A2(r, 65576, 65594) || A2(r, 65596, 65597)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))) : (l(65999) ? (l(65855) ? ((A2(r, 65599, 65613) || (A2(r, 65616, 65629) || A2(r, 65664, 65786))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 65792, 65794) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 65799, 65843) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 65847, 65854) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : ((e(65855) || (A2(r, 65913, 65929) || (A2(r, 65932, 65934) || (A2(r, 65936, 65948) || e(65952))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 65856, 65908) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : ((A2(r, 65909, 65912) || A2(r, 65930, 65931)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing)))) : (l(66303) ? (A2(r, 66000, 66044) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(66045) || e(66272)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 66176, 66204) || A2(r, 66208, 66256)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 66273, 66299) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing)))) : ((A2(r, 66304, 66335) || (A2(r, 66349, 66368) || (A2(r, 66370, 66377) || A2(r, 66384, 66420)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 66336, 66339) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((e(66369) || e(66378)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : $elm$core$Maybe$Nothing))))))) : (l(67902) ? (l(67071) ? (l(66735) ? ((e(66421) || (A2(r, 66432, 66461) || (A2(r, 66464, 66499) || (A2(r, 66504, 66511) || A2(r, 66640, 66717))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 66422, 66426) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(66463) || e(66512)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 66513, 66517) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : (A2(r, 66560, 66599) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 66600, 66639) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 66720, 66729) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))))) : (l(66939) ? ((A2(r, 66736, 66771) || A2(r, 66928, 66938)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 66776, 66811) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 66816, 66855) || A2(r, 66864, 66915)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(66927) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : ((A2(r, 66940, 66954) || (A2(r, 66956, 66962) || A2(r, 66964, 66965))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 66967, 66977) || (A2(r, 66979, 66993) || (A2(r, 66995, 67001) || A2(r, 67003, 67004)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (l(67671) ? ((A2(r, 67072, 67382) || (A2(r, 67392, 67413) || (A2(r, 67424, 67431) || (A2(r, 67584, 67589) || (e(67592) || (A2(r, 67594, 67637) || (A2(r, 67639, 67640) || (e(67644) || A2(r, 67647, 67669))))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 67456, 67461) || (A2(r, 67463, 67504) || A2(r, 67506, 67514))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing)) : (l(67807) ? (e(67671) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 67672, 67679) || (A2(r, 67705, 67711) || A2(r, 67751, 67759))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 67680, 67702) || A2(r, 67712, 67742)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 67703, 67704) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : ((A2(r, 67808, 67826) || (A2(r, 67828, 67829) || (A2(r, 67840, 67861) || A2(r, 67872, 67897)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 67835, 67839) || A2(r, 67862, 67867)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(67871) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(68324) ? (l(68120) ? (e(67903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 67968, 68023) || (A2(r, 68030, 68031) || (e(68096) || (A2(r, 68112, 68115) || A2(r, 68117, 68119))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 68028, 68029) || (A2(r, 68032, 68047) || A2(r, 68050, 68095))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 68097, 68099) || (A2(r, 68101, 68102) || A2(r, 68108, 68111))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : (l(68220) ? ((A2(r, 68121, 68149) || A2(r, 68192, 68219)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 68152, 68154) || e(68159)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 68160, 68168) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 68176, 68184) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : ((e(68220) || (A2(r, 68224, 68252) || (A2(r, 68288, 68295) || A2(r, 68297, 68323)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 68221, 68222) || A2(r, 68253, 68255)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (e(68223) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(68296) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))))) : (l(68607) ? (l(68415) ? ((e(68324) || A2(r, 68352, 68405)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 68325, 68326) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 68331, 68335) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 68336, 68342) || A2(r, 68409, 68414)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : ((e(68415) || A2(r, 68505, 68508)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 68416, 68437) || (A2(r, 68448, 68466) || A2(r, 68480, 68497))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 68440, 68447) || (A2(r, 68472, 68479) || A2(r, 68521, 68527))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing)))) : (l(69215) ? ((A2(r, 68608, 68680) || A2(r, 68864, 68899)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 68736, 68786) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 68800, 68850) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 68858, 68863) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 68900, 68903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 68912, 68921) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))) : (A2(r, 69216, 69246) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 69248, 69289) || (A2(r, 69296, 69297) || A2(r, 69376, 69403))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 69291, 69292) || A2(r, 69373, 69375)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(69293) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationDash) : $elm$core$Maybe$Nothing))))))))) : (l(70452) ? (l(70002) ? (l(69758) ? (l(69599) ? ((e(69404) || (e(69415) || (A2(r, 69424, 69445) || (A2(r, 69488, 69505) || A2(r, 69552, 69572))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 69405, 69414) || (A2(r, 69457, 69460) || A2(r, 69573, 69579))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 69446, 69456) || A2(r, 69506, 69509)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 69461, 69465) || A2(r, 69510, 69513)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : (l(69702) ? ((A2(r, 69600, 69622) || A2(r, 69635, 69687)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(69632) || e(69634)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(69633) || A2(r, 69688, 69701)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((e(69702) || (e(69744) || A2(r, 69747, 69748))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 69703, 69709) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 69714, 69733) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 69734, 69743) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 69745, 69746) || e(69749)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))) : (l(69839) ? ((A2(r, 69759, 69761) || (A2(r, 69811, 69814) || (A2(r, 69817, 69818) || e(69826)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(69762) || (A2(r, 69808, 69810) || A2(r, 69815, 69816))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 69763, 69807) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 69819, 69820) || A2(r, 69822, 69825)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(69821) || e(69837)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : $elm$core$Maybe$Nothing))))) : (l(69932) ? ((A2(r, 69840, 69864) || A2(r, 69891, 69926)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 69872, 69881) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : ((A2(r, 69888, 69890) || A2(r, 69927, 69931)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((e(69932) || A2(r, 69957, 69958)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 69933, 69940) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 69942, 69951) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 69952, 69955) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(69956) || (e(69959) || A2(r, 69968, 70001))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))))))) : (l(70193) ? (l(70092) ? (l(70018) ? ((e(70002) || e(70006)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(70003) || A2(r, 70016, 70017)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70004, 70005) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))) : ((e(70018) || (A2(r, 70067, 70069) || A2(r, 70079, 70080))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 70019, 70066) || A2(r, 70081, 70084)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 70070, 70078) || A2(r, 70089, 70091)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70085, 70088) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : (l(70107) ? ((e(70092) || e(70095)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(70093) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(70094) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 70096, 70105) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(70106) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))) : ((e(70107) || A2(r, 70109, 70111)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(70108) || (A2(r, 70144, 70161) || A2(r, 70163, 70187))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 70113, 70132) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 70188, 70190) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 70191, 70192) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))))) : (l(70302) ? (l(70205) ? ((e(70193) || (e(70196) || A2(r, 70198, 70199))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 70194, 70195) || e(70197)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 70200, 70204) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))) : (e(70205) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(70206) || e(70209)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 70207, 70208) || (A2(r, 70272, 70278) || (e(70280) || (A2(r, 70282, 70285) || A2(r, 70287, 70301))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (l(70399) ? ((A2(r, 70303, 70312) || A2(r, 70320, 70366)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(70313) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(70367) || A2(r, 70371, 70378)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70368, 70370) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 70384, 70393) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : (A2(r, 70400, 70401) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70402, 70403) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 70405, 70412) || (A2(r, 70415, 70416) || (A2(r, 70419, 70440) || (A2(r, 70442, 70448) || A2(r, 70450, 70451))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))) : (l(71167) ? (l(70748) ? (l(70501) ? ((A2(r, 70453, 70457) || (e(70461) || (e(70480) || A2(r, 70493, 70497)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 70459, 70460) || e(70464)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 70462, 70463) || (A2(r, 70465, 70468) || (A2(r, 70471, 70472) || (A2(r, 70475, 70477) || (e(70487) || A2(r, 70498, 70499)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : (l(70721) ? ((A2(r, 70502, 70508) || (A2(r, 70512, 70516) || A2(r, 70712, 70719))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70656, 70708) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 70709, 70711) || e(70720)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((e(70721) || e(70725)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 70722, 70724) || e(70726)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 70727, 70730) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 70731, 70735) || A2(r, 70746, 70747)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 70736, 70745) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))))) : (l(70853) ? (l(70840) ? (e(70749) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(70750) || A2(r, 70835, 70839)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 70751, 70753) || A2(r, 70784, 70831)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 70832, 70834) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))) : ((e(70840) || (e(70842) || (A2(r, 70847, 70848) || A2(r, 70850, 70851)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(70841) || (A2(r, 70843, 70846) || e(70849))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(70852) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (l(71095) ? ((e(70853) || (e(70855) || A2(r, 71040, 71086))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(70854) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 70864, 70873) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 71087, 71089) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 71090, 71093) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))) : ((A2(r, 71096, 71099) || e(71102)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 71100, 71101) || (A2(r, 71103, 71104) || A2(r, 71132, 71133))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 71105, 71127) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 71128, 71131) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))) : (l(71457) ? (l(71338) ? ((A2(r, 71168, 71215) || (e(71236) || A2(r, 71296, 71337))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 71216, 71218) || (A2(r, 71227, 71228) || e(71230))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 71219, 71226) || (e(71229) || A2(r, 71231, 71232))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 71233, 71235) || A2(r, 71264, 71276)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 71248, 71257) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : (l(71350) ? (e(71338) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(71339) || (e(71341) || A2(r, 71344, 71349))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(71340) || A2(r, 71342, 71343)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((e(71350) || e(71456)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(71351) || A2(r, 71453, 71455)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(71352) || A2(r, 71424, 71450)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(71353) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 71360, 71369) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))))) : (l(71736) ? ((e(71457) || (e(71462) || A2(r, 71724, 71726))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 71458, 71461) || (A2(r, 71463, 71467) || A2(r, 71727, 71735))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 71472, 71481) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 71482, 71483) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 71484, 71486) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(71487) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 71488, 71494) || A2(r, 71680, 71723)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))))) : (l(71934) ? (e(71736) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 71737, 71738) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(71739) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 71840, 71871) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 71872, 71903) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 71904, 71913) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 71914, 71922) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))))) : ((A2(r, 71935, 71942) || (e(71945) || (A2(r, 71948, 71955) || (A2(r, 71957, 71958) || A2(r, 71960, 71983))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 71984, 71989) || A2(r, 71991, 71992)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (e(71995) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))))))) : (l(119893) ? (l(73647) ? (l(72767) ? (l(72242) ? (l(72144) ? ((e(71996) || (e(71998) || e(72003))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(71997) || (e(72000) || e(72002))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(71999) || (e(72001) || (A2(r, 72096, 72103) || A2(r, 72106, 72143)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 72004, 72006) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 72016, 72025) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : (l(72160) ? (e(72144) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 72145, 72147) || A2(r, 72156, 72159)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 72148, 72151) || A2(r, 72154, 72155)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((e(72160) || A2(r, 72193, 72202)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(72161) || (e(72163) || (e(72192) || A2(r, 72203, 72241)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(72162) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(72164) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))))) : (l(72342) ? (l(72262) ? ((e(72242) || e(72250)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 72243, 72248) || A2(r, 72251, 72254)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(72249) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 72255, 72261) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))) : (e(72262) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(72263) || (A2(r, 72273, 72278) || (A2(r, 72281, 72283) || A2(r, 72330, 72341)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(72272) || A2(r, 72284, 72329)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 72279, 72280) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))))) : (l(72447) ? ((e(72342) || A2(r, 72344, 72345)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(72343) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 72346, 72348) || A2(r, 72350, 72354)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((e(72349) || A2(r, 72368, 72440)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)))) : (A2(r, 72448, 72457) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 72704, 72712) || A2(r, 72714, 72750)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(72751) || e(72766)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 72752, 72758) || A2(r, 72760, 72765)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))))))) : (l(73065) ? (l(72884) ? ((e(72767) || (A2(r, 72850, 72871) || (A2(r, 72874, 72880) || A2(r, 72882, 72883)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(72768) || A2(r, 72818, 72847)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 72769, 72773) || A2(r, 72816, 72817)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 72784, 72793) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 72794, 72812) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((e(72873) || e(72881)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing)))))) : (l(73019) ? (e(72884) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 72885, 72886) || (A2(r, 73009, 73014) || e(73018))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 72960, 72966) || (A2(r, 72968, 72969) || A2(r, 72971, 73008))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))) : ((A2(r, 73020, 73021) || (A2(r, 73023, 73029) || e(73031))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(73030) || (A2(r, 73056, 73061) || A2(r, 73063, 73064))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 73040, 73049) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))))) : (l(73471) ? (l(73110) ? (A2(r, 73066, 73097) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 73098, 73102) || A2(r, 73107, 73108)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 73104, 73105) || e(73109)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((e(73110) || A2(r, 73461, 73462)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((e(73111) || A2(r, 73459, 73460)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(73112) || A2(r, 73440, 73458)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 73120, 73129) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 73463, 73464) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(73525) ? (A2(r, 73472, 73473) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(73474) || (A2(r, 73476, 73488) || A2(r, 73490, 73523))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(73475) || e(73524)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : $elm$core$Maybe$Nothing))) : ((e(73525) || (A2(r, 73534, 73535) || e(73537))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 73526, 73530) || (e(73536) || e(73538))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 73539, 73551) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 73552, 73561) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))))))) : (l(94178) ? (l(92879) ? (l(77823) ? ((e(73648) || (A2(r, 73728, 74649) || (A2(r, 74880, 75075) || A2(r, 77712, 77808)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 73664, 73684) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((A2(r, 73685, 73692) || A2(r, 73697, 73713)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 73693, 73696) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : ((e(73727) || (A2(r, 74864, 74868) || A2(r, 77809, 77810))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 74752, 74862) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberLetter) : $elm$core$Maybe$Nothing)))))) : ((A2(r, 77824, 78895) || (A2(r, 78913, 78918) || (A2(r, 82944, 83526) || (A2(r, 92160, 92728) || (A2(r, 92736, 92766) || A2(r, 92784, 92862)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 78896, 78911) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : ((e(78912) || A2(r, 78919, 78933)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 92768, 92777) || A2(r, 92864, 92873)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 92782, 92783) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing)))))) : (l(93026) ? ((A2(r, 92880, 92909) || A2(r, 92928, 92975)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 92912, 92916) || A2(r, 92976, 92982)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((e(92917) || (A2(r, 92983, 92987) || e(92996))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 92988, 92991) || e(92997)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 92992, 92995) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 93008, 93017) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 93019, 93025) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))))) : (l(93951) ? ((A2(r, 93027, 93047) || A2(r, 93053, 93071)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 93760, 93791) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 93792, 93823) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 93824, 93846) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 93847, 93850) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : $elm$core$Maybe$Nothing))))) : ((A2(r, 93952, 94026) || e(94032)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(94031) || A2(r, 94095, 94098)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 94033, 94087) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 94099, 94111) || A2(r, 94176, 94177)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))))))) : (l(118607) ? (l(110932) ? (l(101631) ? (e(94178) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (e(94179) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (e(94180) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 94192, 94193) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : ((A2(r, 94208, 100343) || A2(r, 100352, 101589)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing))))) : ((A2(r, 101632, 101640) || (A2(r, 110592, 110882) || (e(110898) || A2(r, 110928, 110930)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 110576, 110579) || (A2(r, 110581, 110587) || A2(r, 110589, 110590))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : $elm$core$Maybe$Nothing))) : (l(113807) ? ((e(110933) || (A2(r, 110948, 110951) || (A2(r, 110960, 111355) || (A2(r, 113664, 113770) || (A2(r, 113776, 113788) || A2(r, 113792, 113800)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : (A2(r, 113808, 113817) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(113820) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 113821, 113822) || (A2(r, 118528, 118573) || A2(r, 118576, 118598))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(113823) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 113824, 113827) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : $elm$core$Maybe$Nothing))))))) : (l(119209) ? (l(119145) ? ((A2(r, 118608, 118723) || (A2(r, 118784, 119029) || (A2(r, 119040, 119078) || A2(r, 119081, 119140)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 119141, 119142) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 119143, 119144) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing))) : ((e(119145) || (A2(r, 119163, 119170) || A2(r, 119173, 119179))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 119146, 119148) || (A2(r, 119171, 119172) || A2(r, 119180, 119208))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 119149, 119154) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkSpacingCombining) : (A2(r, 119155, 119162) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : $elm$core$Maybe$Nothing))))) : (l(119519) ? ((e(119209) || (A2(r, 119214, 119274) || (A2(r, 119296, 119361) || e(119365)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 119210, 119213) || A2(r, 119362, 119364)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 119488, 119507) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))) : ((A2(r, 119520, 119539) || A2(r, 119648, 119672)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 119552, 119638) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((A2(r, 119808, 119833) || A2(r, 119860, 119885)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 119834, 119859) || A2(r, 119886, 119892)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing))))))))) : (l(124111) ? (l(120629) ? (l(120137) ? (l(120004) ? ((A2(r, 119894, 119911) || (A2(r, 119938, 119963) || (A2(r, 119990, 119993) || (e(119995) || A2(r, 119997, 120003))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 119912, 119937) || (e(119964) || (A2(r, 119966, 119967) || (e(119970) || (A2(r, 119973, 119974) || (A2(r, 119977, 119980) || A2(r, 119982, 119989))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)) : ((A2(r, 120005, 120015) || (A2(r, 120042, 120067) || A2(r, 120094, 120119))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 120016, 120041) || (A2(r, 120068, 120069) || (A2(r, 120071, 120074) || (A2(r, 120077, 120084) || (A2(r, 120086, 120092) || (A2(r, 120120, 120121) || (A2(r, 120123, 120126) || (A2(r, 120128, 120132) || e(120134))))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : (l(120431) ? ((A2(r, 120138, 120144) || (A2(r, 120172, 120197) || (A2(r, 120224, 120249) || (A2(r, 120276, 120301) || (A2(r, 120328, 120353) || A2(r, 120380, 120405)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : ((A2(r, 120146, 120171) || (A2(r, 120198, 120223) || (A2(r, 120250, 120275) || (A2(r, 120302, 120327) || (A2(r, 120354, 120379) || A2(r, 120406, 120430)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)) : (l(120539) ? ((e(120431) || (A2(r, 120458, 120485) || A2(r, 120514, 120538))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 120432, 120457) || A2(r, 120488, 120512)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (e(120513) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : $elm$core$Maybe$Nothing))) : ((e(120539) || (e(120571) || e(120597))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 120540, 120545) || (A2(r, 120572, 120596) || A2(r, 120598, 120603))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 120546, 120570) || A2(r, 120604, 120628)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))))) : (l(121478) ? (l(120771) ? ((e(120629) || (e(120655) || (e(120687) || (e(120713) || e(120745))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 120630, 120654) || (A2(r, 120656, 120661) || (A2(r, 120688, 120712) || (A2(r, 120714, 120719) || A2(r, 120746, 120770))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 120662, 120686) || A2(r, 120720, 120744)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing))) : (l(121398) ? (e(120771) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 120772, 120777) || e(120779)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (e(120778) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 120782, 120831) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 120832, 121343) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 121344, 121397) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))))) : ((e(121398) || (A2(r, 121403, 121452) || (e(121461) || e(121476)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : ((A2(r, 121399, 121402) || (A2(r, 121453, 121460) || (A2(r, 121462, 121475) || e(121477)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))) : (l(122927) ? (l(122634) ? (e(121478) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 121479, 121483) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : ((A2(r, 121499, 121503) || A2(r, 121505, 121519)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 122624, 122633) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : $elm$core$Maybe$Nothing)))) : (e(122634) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((A2(r, 122635, 122654) || A2(r, 122661, 122666)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : ((A2(r, 122880, 122886) || (A2(r, 122888, 122904) || (A2(r, 122907, 122913) || (A2(r, 122915, 122916) || A2(r, 122918, 122922))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : $elm$core$Maybe$Nothing)))) : (l(123214) ? ((A2(r, 122928, 122989) || A2(r, 123191, 123197)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((e(123023) || A2(r, 123184, 123190)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 123136, 123180) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 123200, 123209) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing)))) : ((e(123214) || (A2(r, 123536, 123565) || A2(r, 123584, 123627))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(123215) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : ((e(123566) || A2(r, 123628, 123631)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 123632, 123641) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (e(123647) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : $elm$core$Maybe$Nothing))))))))) : (l(127135) ? (l(126463) ? (l(125217) ? ((A2(r, 124112, 124138) || (A2(r, 124896, 124902) || (A2(r, 124904, 124907) || (A2(r, 124909, 124910) || (A2(r, 124912, 124926) || A2(r, 124928, 125124)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (e(124139) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : ((A2(r, 124140, 124143) || A2(r, 125136, 125142)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 124144, 124153) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 125127, 125135) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : (A2(r, 125184, 125216) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : $elm$core$Maybe$Nothing)))))) : (l(126123) ? (e(125217) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterUppercase) : (A2(r, 125218, 125251) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterLowercase) : (A2(r, 125252, 125258) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (e(125259) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterModifier) : (A2(r, 125264, 125273) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : (A2(r, 125278, 125279) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$PunctuationOther) : (A2(r, 126065, 126122) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing))))))) : ((e(126123) || (A2(r, 126125, 126127) || (A2(r, 126129, 126132) || (A2(r, 126209, 126253) || A2(r, 126255, 126269))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : ((e(126124) || e(126254)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (e(126128) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolCurrency) : $elm$core$Maybe$Nothing))))) : (l(126566) ? (l(126515) ? ((A2(r, 126464, 126467) || (A2(r, 126469, 126495) || (A2(r, 126497, 126498) || (e(126500) || (e(126503) || A2(r, 126505, 126514)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : ((A2(r, 126516, 126519) || (e(126530) || (A2(r, 126541, 126543) || (A2(r, 126545, 126546) || (e(126548) || (A2(r, 126561, 126562) || (e(126564) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && (A2(r, 126521, 126523) || (A2(r, 126535, 126539) || A2(r, 126551, 126559))))))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing)) : (l(126602) ? ((A2(r, 126567, 126570) || (A2(r, 126572, 126578) || (A2(r, 126580, 126583) || (A2(r, 126585, 126588) || (e(126590) || A2(r, 126592, 126601)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : ((A2(r, 126603, 126619) || (A2(r, 126625, 126627) || (A2(r, 126629, 126633) || A2(r, 126635, 126651)))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : (A2(r, 126704, 126705) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolMath) : ((A2(r, 126976, 127019) || A2(r, 127024, 127123)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing)))))) : (l(129199) ? (l(127994) ? ((A2(r, 127136, 127150) || (A2(r, 127153, 127167) || (A2(r, 127169, 127183) || (A2(r, 127185, 127221) || (A2(r, 127245, 127405) || (A2(r, 127462, 127490) || (A2(r, 127504, 127547) || (A2(r, 127552, 127560) || (A2(r, 127568, 127569) || (A2(r, 127584, 127589) || A2(r, 127744, 127993))))))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 127232, 127244) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberOther) : $elm$core$Maybe$Nothing)) : (l(128991) ? ((e(127994) || (A2(r, 128000, 128727) || (A2(r, 128732, 128748) || (A2(r, 128752, 128764) || (A2(r, 128768, 128886) || A2(r, 128891, 128985)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 127995, 127999) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolModifier) : $elm$core$Maybe$Nothing)) : ((A2(r, 128992, 129003) || (e(129008) || (A2(r, 129024, 129035) || (A2(r, 129040, 129095) || (A2(r, 129104, 129113) || (A2(r, 129120, 129159) || A2(r, 129168, 129197))))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing))) : (l(131071) ? (l(129726) ? ((A2(r, 129200, 129201) || (A2(r, 129280, 129619) || (A2(r, 129632, 129645) || (A2(r, 129648, 129660) || (A2(r, 129664, 129672) || A2(r, 129680, 129725)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : $elm$core$Maybe$Nothing) : ((A2(r, 129727, 129733) || (A2(r, 129742, 129755) || (A2(r, 129760, 129768) || (A2(r, 129776, 129784) || (A2(r, 129792, 129938) || A2(r, 129940, 129994)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$SymbolOther) : (A2(r, 130032, 130041) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$NumberDecimalDigit) : $elm$core$Maybe$Nothing))) : (l(194559) ? ((A2(r, 131072, 173791) || (A2(r, 173824, 177977) || (A2(r, 177984, 178205) || (A2(r, 178208, 183969) || (A2(r, 183984, 191456) || A2(r, 191472, 192093)))))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : $elm$core$Maybe$Nothing) : ((A2(r, 194560, 195101) || (A2(r, 196608, 201546) || A2(r, 201552, 205743))) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$LetterOther) : ((e(917505) || A2(r, 917536, 917631)) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherFormat) : (A2(r, 917760, 917999) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$MarkNonSpacing) : (A2(r, 983040, 1114109) ? $elm$core$Maybe$Just($miniBill$elm_unicode$Unicode$OtherPrivateUse) : $elm$core$Maybe$Nothing)))))))))))));
};
var $author$project$ElmSyntaxPrint$characterIsPrint = function (character) {
	var _v0 = $miniBill$elm_unicode$Unicode$getCategory(character);
	if (_v0.$ === 'Nothing') {
		return false;
	} else {
		var category = _v0.a;
		switch (category.$) {
			case 'SeparatorLine':
				return true;
			case 'SeparatorParagraph':
				return true;
			case 'OtherControl':
				return true;
			case 'OtherFormat':
				return true;
			case 'OtherSurrogate':
				return true;
			case 'OtherPrivateUse':
				return true;
			case 'OtherNotAssigned':
				return true;
			case 'LetterUppercase':
				return false;
			case 'LetterLowercase':
				return false;
			case 'LetterTitlecase':
				return false;
			case 'MarkNonSpacing':
				return false;
			case 'MarkSpacingCombining':
				return false;
			case 'MarkEnclosing':
				return false;
			case 'NumberDecimalDigit':
				return false;
			case 'NumberLetter':
				return false;
			case 'NumberOther':
				return false;
			case 'SeparatorSpace':
				return false;
			case 'LetterModifier':
				return false;
			case 'LetterOther':
				return false;
			case 'PunctuationConnector':
				return false;
			case 'PunctuationDash':
				return false;
			case 'PunctuationOpen':
				return false;
			case 'PunctuationClose':
				return false;
			case 'PunctuationInitialQuote':
				return false;
			case 'PunctuationFinalQuote':
				return false;
			case 'PunctuationOther':
				return false;
			case 'SymbolMath':
				return false;
			case 'SymbolCurrency':
				return false;
			case 'SymbolModifier':
				return false;
			default:
				return false;
		}
	}
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $author$project$ElmSyntaxPrint$quotedCharToEscaped = function (character) {
	switch (character.valueOf()) {
		case '\'':
			return '\\\'';
		case '\\':
			return '\\\\';
		case '\t':
			return '\\t';
		case '\n':
			return '\\n';
		case '\u000D':
			return '\\u{000D}';
		default:
			var otherCharacter = character;
			return $author$project$ElmSyntaxPrint$characterIsPrint(otherCharacter) ? ('\\u{' + ($author$project$ElmSyntaxPrint$characterHex(otherCharacter) + '}')) : $elm$core$String$fromChar(otherCharacter);
	}
};
var $author$project$ElmSyntaxPrint$charLiteral = function (charContent) {
	return A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly('\''),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(
				$author$project$ElmSyntaxPrint$quotedCharToEscaped(charContent)),
			$author$project$Print$exactly('\'')));
};
var $author$project$ElmSyntaxPrint$declarationSignature = F2(
	function (syntaxComments, signature) {
		var typePrint = A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, signature.typeAnnotation);
		var rangeBetweenNameAndType = {
			end: $stil4m$elm_syntax$Elm$Syntax$Node$range(signature.typeAnnotation).start,
			start: $stil4m$elm_syntax$Elm$Syntax$Node$range(signature.name).end
		};
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					typePrint,
					function () {
						var _v0 = A2($author$project$ElmSyntaxPrint$commentsInRange, rangeBetweenNameAndType, syntaxComments);
						if (!_v0.b) {
							return $author$project$Print$spaceOrLinebreakIndented(
								$author$project$Print$lineSpread(typePrint));
						} else {
							var comment0 = _v0.a;
							var comment1Up = _v0.b;
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreakIndented,
								A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)),
									$author$project$Print$linebreakIndented));
						}
					}())),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(':'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$space,
					$author$project$Print$exactly(
						$stil4m$elm_syntax$Elm$Syntax$Node$value(signature.name)))));
	});
var $author$project$ElmSyntaxPrint$expressionGlsl = function (glslContent) {
	return A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly('|]'),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$sequence(
				A2(
					$elm$core$List$intersperse,
					$author$project$Print$linebreak,
					A2(
						$elm$core$List$map,
						$author$project$Print$exactly,
						$elm$core$String$lines(glslContent)))),
			$author$project$Print$exactly('[glsl|')));
};
var $author$project$ElmSyntaxPrint$expressionIsSpaceSeparated = function (syntaxExpression) {
	expressionIsSpaceSeparated:
	while (true) {
		switch (syntaxExpression.$) {
			case 'UnitExpr':
				return false;
			case 'Application':
				var application = syntaxExpression.a;
				if (!application.b) {
					return false;
				} else {
					if (!application.b.b) {
						var _v2 = application.a;
						var notActuallyApplied = _v2.b;
						var $temp$syntaxExpression = notActuallyApplied;
						syntaxExpression = $temp$syntaxExpression;
						continue expressionIsSpaceSeparated;
					} else {
						var _v3 = application.b;
						return true;
					}
				}
			case 'OperatorApplication':
				return true;
			case 'FunctionOrValue':
				return false;
			case 'IfBlock':
				return true;
			case 'PrefixOperator':
				return false;
			case 'Operator':
				return false;
			case 'Integer':
				return false;
			case 'Hex':
				return false;
			case 'Floatable':
				return false;
			case 'Negation':
				return false;
			case 'Literal':
				return false;
			case 'CharLiteral':
				return false;
			case 'TupledExpression':
				var parts = syntaxExpression.a;
				if (!parts.b) {
					return false;
				} else {
					if (!parts.b.b) {
						var _v5 = parts.a;
						var inParens = _v5.b;
						var $temp$syntaxExpression = inParens;
						syntaxExpression = $temp$syntaxExpression;
						continue expressionIsSpaceSeparated;
					} else {
						if (!parts.b.b.b) {
							var _v6 = parts.b;
							return false;
						} else {
							if (!parts.b.b.b.b) {
								var _v7 = parts.b;
								var _v8 = _v7.b;
								return false;
							} else {
								var _v9 = parts.b;
								var _v10 = _v9.b;
								var _v11 = _v10.b;
								return false;
							}
						}
					}
				}
			case 'ParenthesizedExpression':
				var _v12 = syntaxExpression.a;
				var inParens = _v12.b;
				var $temp$syntaxExpression = inParens;
				syntaxExpression = $temp$syntaxExpression;
				continue expressionIsSpaceSeparated;
			case 'LetExpression':
				return true;
			case 'CaseExpression':
				return true;
			case 'LambdaExpression':
				return true;
			case 'RecordExpr':
				return false;
			case 'ListExpr':
				return false;
			case 'RecordAccess':
				return false;
			case 'RecordAccessFunction':
				return false;
			case 'RecordUpdateExpression':
				return false;
			default:
				return false;
		}
	}
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression = function (a) {
	return {$: 'TupledExpression', a: a};
};
var $author$project$ElmSyntaxPrint$expressionToNotParenthesized = function (_v0) {
	var fullRange = _v0.a;
	var syntaxExpression = _v0.b;
	switch (syntaxExpression.$) {
		case 'ParenthesizedExpression':
			var inParens = syntaxExpression.a;
			return $author$project$ElmSyntaxPrint$expressionToNotParenthesized(inParens);
		case 'TupledExpression':
			var parts = syntaxExpression.a;
			if (!parts.b) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fullRange,
					$stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(_List_Nil));
			} else {
				if (!parts.b.b) {
					var inParens = parts.a;
					return $author$project$ElmSyntaxPrint$expressionToNotParenthesized(inParens);
				} else {
					if (!parts.b.b.b) {
						var part0 = parts.a;
						var _v3 = parts.b;
						var part1 = _v3.a;
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							fullRange,
							$stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
								_List_fromArray(
									[part0, part1])));
					} else {
						if (!parts.b.b.b.b) {
							var part0 = parts.a;
							var _v4 = parts.b;
							var part1 = _v4.a;
							var _v5 = _v4.b;
							var part2 = _v5.a;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								fullRange,
								$stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
									_List_fromArray(
										[part0, part1, part2])));
						} else {
							var part0 = parts.a;
							var _v6 = parts.b;
							var part1 = _v6.a;
							var _v7 = _v6.b;
							var part2 = _v7.a;
							var _v8 = _v7.b;
							var part3 = _v8.a;
							var part4Up = _v8.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								fullRange,
								$stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
									A2(
										$elm$core$List$cons,
										part0,
										A2(
											$elm$core$List$cons,
											part1,
											A2(
												$elm$core$List$cons,
												part2,
												A2($elm$core$List$cons, part3, part4Up))))));
						}
					}
				}
			}
		default:
			var syntaxExpressionNotParenthesized = syntaxExpression;
			return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, syntaxExpressionNotParenthesized);
	}
};
var $author$project$ElmSyntaxPrint$expressionIsSpaceSeparatedExceptApplication = function (expressionNode) {
	if ($author$project$ElmSyntaxPrint$expressionIsSpaceSeparated(
		$stil4m$elm_syntax$Elm$Syntax$Node$value(expressionNode))) {
		var _v0 = $author$project$ElmSyntaxPrint$expressionToNotParenthesized(expressionNode);
		if (_v0.b.$ === 'Application') {
			return false;
		} else {
			return true;
		}
	} else {
		return false;
	}
};
var $author$project$ElmSyntaxPrint$expressionOperationExpand = F3(
	function (left, operator, right) {
		var rightExpanded = function () {
			if (right.b.$ === 'OperatorApplication') {
				var _v3 = right.b;
				var rightOperator = _v3.a;
				var rightLeft = _v3.c;
				var rightRight = _v3.d;
				var rightOperationExpanded = A3($author$project$ElmSyntaxPrint$expressionOperationExpand, rightLeft, rightOperator, rightRight);
				return {
					beforeRightestOperatorExpressionChain: A2(
						$elm$core$List$cons,
						{expression: rightOperationExpanded.leftest, operator: operator},
						rightOperationExpanded.beforeRightestOperatorExpressionChain),
					rightestExpression: rightOperationExpanded.rightestExpression,
					rightestOperator: rightOperationExpanded.rightestOperator
				};
			} else {
				var rightNotOperation = right;
				return {beforeRightestOperatorExpressionChain: _List_Nil, rightestExpression: rightNotOperation, rightestOperator: operator};
			}
		}();
		if (left.b.$ === 'OperatorApplication') {
			var _v1 = left.b;
			var leftOperator = _v1.a;
			var leftLeft = _v1.c;
			var leftRight = _v1.d;
			var leftOperationExpanded = A3($author$project$ElmSyntaxPrint$expressionOperationExpand, leftLeft, leftOperator, leftRight);
			return {
				beforeRightestOperatorExpressionChain: _Utils_ap(
					leftOperationExpanded.beforeRightestOperatorExpressionChain,
					A2(
						$elm$core$List$cons,
						{expression: leftOperationExpanded.rightestExpression, operator: leftOperationExpanded.rightestOperator},
						rightExpanded.beforeRightestOperatorExpressionChain)),
				leftest: leftOperationExpanded.leftest,
				rightestExpression: rightExpanded.rightestExpression,
				rightestOperator: rightExpanded.rightestOperator
			};
		} else {
			var leftNotOperation = left;
			return {beforeRightestOperatorExpressionChain: rightExpanded.beforeRightestOperatorExpressionChain, leftest: leftNotOperation, rightestExpression: rightExpanded.rightestExpression, rightestOperator: rightExpanded.rightestOperator};
		}
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$Basics$truncate = _Basics_truncate;
var $author$project$ElmSyntaxPrint$floatLiteral = function (_float) {
	return _Utils_eq(_float | 0, _float) ? A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly('.0'),
		$author$project$Print$exactly(
			$elm$core$String$fromFloat(_float))) : $author$project$Print$exactly(
		$elm$core$String$fromFloat(_float));
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $author$project$ElmSyntaxPrint$stringResizePadLeftWith0s = F2(
	function (length, unpaddedString) {
		return (_Utils_cmp(
			length,
			$elm$core$String$length(unpaddedString)) < 0) ? A2($elm$core$String$left, length, unpaddedString) : _Utils_ap(
			A2(
				$elm$core$String$repeat,
				length - $elm$core$String$length(unpaddedString),
				'0'),
			unpaddedString);
	});
var $author$project$ElmSyntaxPrint$hexLiteral = function (_int) {
	var maybeSignPrint = (_int < 0) ? $author$project$Print$exactly('-') : $author$project$Print$empty;
	var intAbs = $elm$core$Basics$abs(_int);
	var digitCountToPrint = (intAbs <= 255) ? 2 : ((intAbs <= 65535) ? 4 : ((intAbs <= 4294967295) ? 8 : 16));
	return A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly(
			A2(
				$author$project$ElmSyntaxPrint$stringResizePadLeftWith0s,
				digitCountToPrint,
				$author$project$ElmSyntaxPrint$intToHexString(_int))),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly('0x'),
			maybeSignPrint));
};
var $author$project$ElmSyntaxPrint$intLiteral = function (_int) {
	return $author$project$Print$exactly(
		$elm$core$String$fromInt(_int));
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern = {$: 'AllPattern'};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern = F2(
	function (a, b) {
		return {$: 'AsPattern', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern = function (a) {
	return {$: 'CharPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$FloatPattern = function (a) {
	return {$: 'FloatPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern = function (a) {
	return {$: 'HexPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern = function (a) {
	return {$: 'IntPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern = function (a) {
	return {$: 'ListPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern = F2(
	function (a, b) {
		return {$: 'NamedPattern', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern = function (a) {
	return {$: 'ParenthesizedPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern = function (a) {
	return {$: 'RecordPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern = function (a) {
	return {$: 'StringPattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern = function (a) {
	return {$: 'TuplePattern', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern = {$: 'UnitPattern'};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern = function (a) {
	return {$: 'VarPattern', a: a};
};
var $author$project$ElmSyntaxPrint$patternConsExpand = function (_v0) {
	var fulRange = _v0.a;
	var syntaxPattern = _v0.b;
	switch (syntaxPattern.$) {
		case 'UnConsPattern':
			var headPattern = syntaxPattern.a;
			var tailPattern = syntaxPattern.b;
			return A2(
				$elm$core$List$cons,
				headPattern,
				$author$project$ElmSyntaxPrint$patternConsExpand(tailPattern));
		case 'AllPattern':
			return _List_fromArray(
				[
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fulRange, $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern)
				]);
		case 'UnitPattern':
			return _List_fromArray(
				[
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fulRange, $stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern)
				]);
		case 'CharPattern':
			var _char = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern(_char))
				]);
		case 'StringPattern':
			var string = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern(string))
				]);
		case 'IntPattern':
			var _int = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern(_int))
				]);
		case 'HexPattern':
			var _int = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern(_int))
				]);
		case 'FloatPattern':
			var _float = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$FloatPattern(_float))
				]);
		case 'TuplePattern':
			var parts = syntaxPattern.a;
			if (!parts.b) {
				return _List_fromArray(
					[
						A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						fulRange,
						$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(_List_Nil))
					]);
			} else {
				if (!parts.b.b) {
					var inParens = parts.a;
					return _List_fromArray(
						[
							A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							fulRange,
							$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
								_List_fromArray(
									[inParens])))
						]);
				} else {
					if (!parts.b.b.b) {
						var part0 = parts.a;
						var _v3 = parts.b;
						var part1 = _v3.a;
						return _List_fromArray(
							[
								A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								fulRange,
								$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
									_List_fromArray(
										[part0, part1])))
							]);
					} else {
						if (!parts.b.b.b.b) {
							var part0 = parts.a;
							var _v4 = parts.b;
							var part1 = _v4.a;
							var _v5 = _v4.b;
							var part2 = _v5.a;
							return _List_fromArray(
								[
									A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									fulRange,
									$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
										_List_fromArray(
											[part0, part1, part2])))
								]);
						} else {
							var part0 = parts.a;
							var _v6 = parts.b;
							var part1 = _v6.a;
							var _v7 = _v6.b;
							var part2 = _v7.a;
							var _v8 = _v7.b;
							var part3 = _v8.a;
							var part4Up = _v8.b;
							return _List_fromArray(
								[
									A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									fulRange,
									$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
										A2(
											$elm$core$List$cons,
											part0,
											A2(
												$elm$core$List$cons,
												part1,
												A2(
													$elm$core$List$cons,
													part2,
													A2($elm$core$List$cons, part3, part4Up))))))
								]);
						}
					}
				}
			}
		case 'RecordPattern':
			var fields = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern(fields))
				]);
		case 'ListPattern':
			var elements = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern(elements))
				]);
		case 'VarPattern':
			var variableName = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(variableName))
				]);
		case 'NamedPattern':
			var reference = syntaxPattern.a;
			var parameters = syntaxPattern.b;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					A2($stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern, reference, parameters))
				]);
		case 'AsPattern':
			var aliasedPattern = syntaxPattern.a;
			var aliasName = syntaxPattern.b;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					A2($stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern, aliasedPattern, aliasName))
				]);
		default:
			var inParens = syntaxPattern.a;
			return _List_fromArray(
				[
					A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fulRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern(inParens))
				]);
	}
};
var $author$project$ElmSyntaxPrint$patternIsSpaceSeparated = function (syntaxPattern) {
	patternIsSpaceSeparated:
	while (true) {
		switch (syntaxPattern.$) {
			case 'AllPattern':
				return false;
			case 'UnitPattern':
				return false;
			case 'VarPattern':
				return false;
			case 'CharPattern':
				return false;
			case 'StringPattern':
				return false;
			case 'IntPattern':
				return false;
			case 'HexPattern':
				return false;
			case 'FloatPattern':
				return false;
			case 'ParenthesizedPattern':
				var _v1 = syntaxPattern.a;
				var inParens = _v1.b;
				var $temp$syntaxPattern = inParens;
				syntaxPattern = $temp$syntaxPattern;
				continue patternIsSpaceSeparated;
			case 'TuplePattern':
				var parts = syntaxPattern.a;
				if (!parts.b) {
					return false;
				} else {
					if (!parts.b.b) {
						var _v3 = parts.a;
						var inParens = _v3.b;
						var $temp$syntaxPattern = inParens;
						syntaxPattern = $temp$syntaxPattern;
						continue patternIsSpaceSeparated;
					} else {
						if (!parts.b.b.b) {
							var _v4 = parts.b;
							return false;
						} else {
							if (!parts.b.b.b.b) {
								var _v5 = parts.b;
								var _v6 = _v5.b;
								return false;
							} else {
								var _v7 = parts.b;
								var _v8 = _v7.b;
								var _v9 = _v8.b;
								return false;
							}
						}
					}
				}
			case 'RecordPattern':
				return false;
			case 'UnConsPattern':
				return true;
			case 'ListPattern':
				return false;
			case 'NamedPattern':
				var argumentPatterns = syntaxPattern.b;
				if (!argumentPatterns.b) {
					return false;
				} else {
					return true;
				}
			default:
				return true;
		}
	}
};
var $author$project$ElmSyntaxPrint$patternRecord = F2(
	function (syntaxComments, syntaxRecord) {
		var _v0 = syntaxRecord.fields;
		if (!_v0.b) {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('}'),
				A2(
					$author$project$Print$followedBy,
					function () {
						var _v1 = A2($author$project$ElmSyntaxPrint$commentsInRange, syntaxRecord.fullRange, syntaxComments);
						if (!_v1.b) {
							return $author$project$Print$empty;
						} else {
							var comment0 = _v1.a;
							var comment1Up = _v1.b;
							var commentsCollapsed = $author$project$ElmSyntaxPrint$collapsibleComments(
								A2($elm$core$List$cons, comment0, comment1Up));
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(commentsCollapsed.lineSpread),
								A2($author$project$Print$withIndentIncreasedBy, 1, commentsCollapsed.print));
						}
					}(),
					$author$project$Print$exactly('{')));
		} else {
			var field0 = _v0.a;
			var field1Up = _v0.b;
			var commentsBeforeFields = A3(
				$elm$core$List$foldl,
				F2(
					function (_v5, soFar) {
						var elementRange = _v5.a;
						return {
							end: elementRange.end,
							reverse: A2(
								$elm$core$List$cons,
								A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{end: elementRange.start, start: soFar.end},
									syntaxComments),
								soFar.reverse)
						};
					}),
				{end: syntaxRecord.fullRange.start, reverse: _List_Nil},
				A2($elm$core$List$cons, field0, field1Up));
			var commentsAfterFields = A2(
				$author$project$ElmSyntaxPrint$commentsInRange,
				{end: syntaxRecord.fullRange.end, start: commentsBeforeFields.end},
				syntaxComments);
			var lineSpread = (A2($elm$core$List$all, $elm$core$List$isEmpty, commentsBeforeFields.reverse) && $elm$core$List$isEmpty(commentsAfterFields)) ? $author$project$Print$SingleLine : $author$project$Print$MultipleLines;
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('}'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsAfterFields.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsAfterFields.a;
								var comment1Up = commentsAfterFields.b;
								return A2(
									$author$project$Print$withIndentIncreasedBy,
									2,
									A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										$author$project$Print$spaceOrLinebreakIndented(lineSpread)));
							}
						}(),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$sequence(
								A2(
									$elm$core$List$intersperse,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$exactly(','),
											$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
									A3(
										$elm$core$List$map2,
										F2(
											function (_v2, commentsBeforeField) {
												var fieldName = _v2.b;
												return A2(
													$author$project$Print$withIndentIncreasedBy,
													2,
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$exactly(fieldName),
														function () {
															if (!commentsBeforeField.b) {
																return $author$project$Print$empty;
															} else {
																var comment0 = commentsBeforeField.a;
																var comment1Up = commentsBeforeField.b;
																return A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$linebreakIndented,
																	$author$project$ElmSyntaxPrint$comments(
																		A2($elm$core$List$cons, comment0, comment1Up)));
															}
														}()));
											}),
										A2($elm$core$List$cons, field0, field1Up),
										$elm$core$List$reverse(commentsBeforeFields.reverse)))),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								$author$project$Print$exactly('{'))))));
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Pattern$UnConsPattern = F2(
	function (a, b) {
		return {$: 'UnConsPattern', a: a, b: b};
	});
var $author$project$ElmSyntaxPrint$patternToNotParenthesized = function (_v0) {
	var fullRange = _v0.a;
	var syntaxPattern = _v0.b;
	switch (syntaxPattern.$) {
		case 'ParenthesizedPattern':
			var inParens = syntaxPattern.a;
			return inParens;
		case 'TuplePattern':
			var parts = syntaxPattern.a;
			if (!parts.b) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fullRange,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(_List_Nil));
			} else {
				if (!parts.b.b) {
					var inParens = parts.a;
					return inParens;
				} else {
					if (!parts.b.b.b) {
						var part0 = parts.a;
						var _v3 = parts.b;
						var part1 = _v3.a;
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							fullRange,
							$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
								_List_fromArray(
									[part0, part1])));
					} else {
						if (!parts.b.b.b.b) {
							var part0 = parts.a;
							var _v4 = parts.b;
							var part1 = _v4.a;
							var _v5 = _v4.b;
							var part2 = _v5.a;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								fullRange,
								$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
									_List_fromArray(
										[part0, part1, part2])));
						} else {
							var part0 = parts.a;
							var _v6 = parts.b;
							var part1 = _v6.a;
							var _v7 = _v6.b;
							var part2 = _v7.a;
							var _v8 = _v7.b;
							var part3 = _v8.a;
							var part4Up = _v8.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								fullRange,
								$stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
									A2(
										$elm$core$List$cons,
										part0,
										A2(
											$elm$core$List$cons,
											part1,
											A2(
												$elm$core$List$cons,
												part2,
												A2($elm$core$List$cons, part3, part4Up))))));
						}
					}
				}
			}
		case 'AllPattern':
			return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern);
		case 'UnitPattern':
			return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, $stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern);
		case 'VarPattern':
			var name = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(name));
		case 'CharPattern':
			var _char = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern(_char));
		case 'StringPattern':
			var string = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern(string));
		case 'IntPattern':
			var _int = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern(_int));
		case 'HexPattern':
			var _int = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern(_int));
		case 'FloatPattern':
			var _float = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$FloatPattern(_float));
		case 'RecordPattern':
			var fields = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern(fields));
		case 'UnConsPattern':
			var headPattern = syntaxPattern.a;
			var tailPattern = syntaxPattern.b;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				A2($stil4m$elm_syntax$Elm$Syntax$Pattern$UnConsPattern, headPattern, tailPattern));
		case 'ListPattern':
			var elementPatterns = syntaxPattern.a;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				$stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern(elementPatterns));
		case 'NamedPattern':
			var syntaxQualifiedNameRef = syntaxPattern.a;
			var argumentPatterns = syntaxPattern.b;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				A2($stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern, syntaxQualifiedNameRef, argumentPatterns));
		default:
			var aliasedPattern = syntaxPattern.a;
			var aliasNameNode = syntaxPattern.b;
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				fullRange,
				A2($stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern, aliasedPattern, aliasNameNode));
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$ElmSyntaxPrint$quotedStringCharToEscaped = function (character) {
	switch (character.valueOf()) {
		case '\"':
			return '\\\"';
		case '\\':
			return '\\\\';
		case '\t':
			return '\\t';
		case '\n':
			return '\\n';
		case '\u000D':
			return '\\u{000D}';
		default:
			var otherCharacter = character;
			return $author$project$ElmSyntaxPrint$characterIsPrint(otherCharacter) ? ('\\u{' + ($author$project$ElmSyntaxPrint$characterHex(otherCharacter) + '}')) : $elm$core$String$fromChar(otherCharacter);
	}
};
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$ElmSyntaxPrint$stringAlterAfterFirstBeforeLastChar = F2(
	function (alterAfterFirstBeforeLastChar, string) {
		return _Utils_ap(
			A3($elm$core$String$slice, 0, 1, string),
			_Utils_ap(
				alterAfterFirstBeforeLastChar(
					A3(
						$elm$core$String$slice,
						1,
						$elm$core$String$length(string) - 1,
						string)),
				A3(
					$elm$core$String$slice,
					$elm$core$String$length(string) - 1,
					$elm$core$String$length(string),
					string)));
	});
var $author$project$ElmSyntaxPrint$stringLiteral = function (_v0) {
	var range = _v0.a;
	var stringContent = _v0.b;
	var stringContentEscaped = A3(
		$elm$core$String$foldl,
		F2(
			function (contentChar, soFar) {
				return _Utils_ap(
					soFar,
					$author$project$ElmSyntaxPrint$quotedStringCharToEscaped(contentChar));
			}),
		'',
		stringContent);
	var wasProbablyTripleDoubleQuoteOriginally = (!_Utils_eq(range.start.row, range.end.row)) || (((range.end.column - range.start.column) - $elm$core$String$length(stringContentEscaped)) !== 2);
	return wasProbablyTripleDoubleQuoteOriginally ? A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly('\"\"\"'),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$sequence(
				A2(
					$elm$core$List$intersperse,
					$author$project$Print$linebreak,
					A2(
						$elm$core$List$map,
						$author$project$Print$exactly,
						$elm$core$String$lines(
							A2(
								$author$project$ElmSyntaxPrint$stringAlterAfterFirstBeforeLastChar,
								A2($elm$core$String$replace, '\\\"', '\"'),
								A3(
									$elm$core$String$replace,
									'\\u{000D}',
									'\u000D',
									A3(
										$elm$core$String$replace,
										'\\r',
										'\u000D',
										A3($elm$core$String$replace, '\\n', '\n', stringContentEscaped)))))))),
			$author$project$Print$exactly('\"\"\"'))) : A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly('\"'),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(stringContentEscaped),
			$author$project$Print$exactly('\"')));
};
var $author$project$ElmSyntaxPrint$patternAs = F2(
	function (syntaxComments, syntaxAs) {
		var commentsBeforeAliasName = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxAs.aliasNameNode).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxAs.aliasedPattern).end
			},
			syntaxComments);
		var commentsCollapsibleBeforeAliasName = $author$project$ElmSyntaxPrint$collapsibleComments(commentsBeforeAliasName);
		var aliasedPatternPrint = A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, syntaxAs.aliasedPattern);
		var lineSpread = A2(
			$author$project$Print$lineSpreadMerge,
			$author$project$Print$lineSpread(aliasedPatternPrint),
			commentsCollapsibleBeforeAliasName.lineSpread);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly(
						$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxAs.aliasNameNode)),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsBeforeAliasName.b) {
								return $author$project$Print$empty;
							} else {
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$spaceOrLinebreakIndented(commentsCollapsibleBeforeAliasName.lineSpread),
									commentsCollapsibleBeforeAliasName.print);
							}
						}(),
						$author$project$Print$spaceOrLinebreakIndented(lineSpread)))),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('as'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(lineSpread),
					aliasedPatternPrint)));
	});
var $author$project$ElmSyntaxPrint$patternCons = F2(
	function (syntaxComments, syntaxCons) {
		var tailPatterns = $author$project$ElmSyntaxPrint$patternConsExpand(syntaxCons.tail);
		var tailPatternsCommentsBefore = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (_v18, soFar) {
						var tailPatternRange = _v18.a;
						var commentsBeforeTailPattern = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: tailPatternRange.start, start: soFar.end},
							syntaxComments);
						return {
							end: tailPatternRange.end,
							reverse: A2(
								$elm$core$List$cons,
								function () {
									if (!commentsBeforeTailPattern.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeTailPattern.a;
										var comment1Up = commentsBeforeTailPattern.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								soFar.reverse)
						};
					}),
				{
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCons.head).end,
					reverse: _List_Nil
				},
				tailPatterns).reverse);
		var tailPatternPrints = A2(
			$elm$core$List$map,
			function (tailPattern) {
				return A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, tailPattern);
			},
			tailPatterns);
		var headPrint = A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, syntaxCons.head);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$Print$lineSpread(headPrint),
					A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, tailPatternPrints),
					A2(
					$author$project$Print$mapAndLineSpreadsCombine,
					function ($) {
						return $.lineSpread;
					},
					A2($elm$core$List$filterMap, $elm$core$Basics$identity, tailPatternsCommentsBefore))
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A2(
							$elm$core$List$intersperse,
							$author$project$Print$spaceOrLinebreakIndented(lineSpread),
							A3(
								$elm$core$List$map2,
								F2(
									function (tailPatternPrint, maybeCommentsBefore) {
										return A2(
											$author$project$Print$followedBy,
											A2(
												$author$project$Print$withIndentIncreasedBy,
												3,
												A2(
													$author$project$Print$followedBy,
													tailPatternPrint,
													function () {
														if (maybeCommentsBefore.$ === 'Nothing') {
															return $author$project$Print$empty;
														} else {
															var commentsBefore = maybeCommentsBefore.a;
															return A2(
																$author$project$Print$followedBy,
																$author$project$Print$spaceOrLinebreakIndented(
																	A2(
																		$author$project$Print$lineSpreadMerge,
																		commentsBefore.lineSpread,
																		$author$project$Print$lineSpread(tailPatternPrint))),
																commentsBefore.print);
														}
													}())),
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$space,
												$author$project$Print$exactly('::')));
									}),
								tailPatternPrints,
								tailPatternsCommentsBefore))),
					$author$project$Print$spaceOrLinebreakIndented(lineSpread))),
			headPrint);
	});
var $author$project$ElmSyntaxPrint$patternList = F2(
	function (syntaxComments, syntaxList) {
		var _v11 = syntaxList.elements;
		if (!_v11.b) {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(']'),
				A2(
					$author$project$Print$followedBy,
					function () {
						var _v12 = A2($author$project$ElmSyntaxPrint$commentsInRange, syntaxList.fullRange, syntaxComments);
						if (!_v12.b) {
							return $author$project$Print$empty;
						} else {
							var comment0 = _v12.a;
							var comment1Up = _v12.b;
							var commentsCollapsed = $author$project$ElmSyntaxPrint$collapsibleComments(
								A2($elm$core$List$cons, comment0, comment1Up));
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(commentsCollapsed.lineSpread),
								A2($author$project$Print$withIndentIncreasedBy, 1, commentsCollapsed.print));
						}
					}(),
					$author$project$Print$exactly('[')));
		} else {
			var element0 = _v11.a;
			var element1Up = _v11.b;
			var elementPrints = A2(
				$elm$core$List$map,
				function (element) {
					return A2($author$project$ElmSyntaxPrint$patternNotParenthesized, syntaxComments, element);
				},
				A2($elm$core$List$cons, element0, element1Up));
			var commentsBeforeElements = A3(
				$elm$core$List$foldl,
				F2(
					function (_v15, soFar) {
						var elementRange = _v15.a;
						var commentsBeforeElement = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: elementRange.start, start: soFar.end},
							syntaxComments);
						return {
							end: elementRange.end,
							reverse: A2(
								$elm$core$List$cons,
								function () {
									if (!commentsBeforeElement.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeElement.a;
										var comment1Up = commentsBeforeElement.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								soFar.reverse)
						};
					}),
				{end: syntaxList.fullRange.start, reverse: _List_Nil},
				A2($elm$core$List$cons, element0, element1Up));
			var lineSpread = $author$project$Print$lineSpreadsCombine(
				_List_fromArray(
					[
						$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxList.fullRange),
						A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, elementPrints),
						A2(
						$author$project$Print$mapAndLineSpreadsCombine,
						function ($) {
							return $.lineSpread;
						},
						A2($elm$core$List$filterMap, $elm$core$Basics$identity, commentsBeforeElements.reverse))
					]));
			var commentsAfterElements = A2(
				$author$project$ElmSyntaxPrint$commentsInRange,
				{end: syntaxList.fullRange.end, start: commentsBeforeElements.end},
				syntaxComments);
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(']'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsAfterElements.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsAfterElements.a;
								var comment1Up = commentsAfterElements.b;
								return A2(
									$author$project$Print$withIndentIncreasedBy,
									2,
									A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										$author$project$Print$spaceOrLinebreakIndented(lineSpread)));
							}
						}(),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$sequence(
								A2(
									$elm$core$List$intersperse,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$exactly(','),
											$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
									A3(
										$elm$core$List$map2,
										F2(
											function (elementPrint, maybeCommentsBeforeElement) {
												return A2(
													$author$project$Print$withIndentIncreasedBy,
													2,
													A2(
														$author$project$Print$followedBy,
														elementPrint,
														function () {
															if (maybeCommentsBeforeElement.$ === 'Nothing') {
																return $author$project$Print$empty;
															} else {
																var commentsBeforeElement = maybeCommentsBeforeElement.a;
																return A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$spaceOrLinebreakIndented(
																		A2(
																			$author$project$Print$lineSpreadMerge,
																			commentsBeforeElement.lineSpread,
																			$author$project$Print$lineSpread(elementPrint))),
																	commentsBeforeElement.print);
															}
														}()));
											}),
										elementPrints,
										$elm$core$List$reverse(commentsBeforeElements.reverse)))),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								$author$project$Print$exactly('['))))));
		}
	});
var $author$project$ElmSyntaxPrint$patternNotParenthesized = F2(
	function (syntaxComments, _v0) {
		patternNotParenthesized:
		while (true) {
			var fullRange = _v0.a;
			var syntaxPattern = _v0.b;
			switch (syntaxPattern.$) {
				case 'AllPattern':
					return $author$project$Print$exactly('_');
				case 'UnitPattern':
					return $author$project$Print$exactly('()');
				case 'VarPattern':
					var name = syntaxPattern.a;
					return $author$project$Print$exactly(name);
				case 'CharPattern':
					var _char = syntaxPattern.a;
					return $author$project$ElmSyntaxPrint$charLiteral(_char);
				case 'StringPattern':
					var string = syntaxPattern.a;
					return $author$project$ElmSyntaxPrint$stringLiteral(
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, string));
				case 'IntPattern':
					var _int = syntaxPattern.a;
					return $author$project$ElmSyntaxPrint$intLiteral(_int);
				case 'HexPattern':
					var _int = syntaxPattern.a;
					return $author$project$ElmSyntaxPrint$hexLiteral(_int);
				case 'FloatPattern':
					var _float = syntaxPattern.a;
					return $author$project$Print$exactly(
						$elm$core$String$fromFloat(_float));
				case 'ParenthesizedPattern':
					var inParens = syntaxPattern.a;
					var commentsBeforeInParens = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{
							end: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).start,
							start: fullRange.start
						},
						syntaxComments);
					var commentsAfterInParens = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{
							end: fullRange.end,
							start: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).end
						},
						syntaxComments);
					var _v2 = _Utils_Tuple2(commentsBeforeInParens, commentsAfterInParens);
					if ((!_v2.a.b) && (!_v2.b.b)) {
						var $temp$syntaxComments = syntaxComments,
							$temp$_v0 = inParens;
						syntaxComments = $temp$syntaxComments;
						_v0 = $temp$_v0;
						continue patternNotParenthesized;
					} else {
						return A3(
							$author$project$ElmSyntaxPrint$parenthesized,
							$author$project$ElmSyntaxPrint$patternNotParenthesized,
							{
								fullRange: fullRange,
								notParenthesized: $author$project$ElmSyntaxPrint$patternToNotParenthesized(inParens)
							},
							syntaxComments);
					}
				case 'TuplePattern':
					var parts = syntaxPattern.a;
					if (!parts.b) {
						return $author$project$Print$exactly('()');
					} else {
						if (!parts.b.b) {
							var inParens = parts.a;
							var commentsBeforeInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).start,
									start: fullRange.start
								},
								syntaxComments);
							var commentsAfterInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: fullRange.end,
									start: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).end
								},
								syntaxComments);
							var _v7 = _Utils_Tuple2(commentsBeforeInParens, commentsAfterInParens);
							if ((!_v7.a.b) && (!_v7.b.b)) {
								var $temp$syntaxComments = syntaxComments,
									$temp$_v0 = inParens;
								syntaxComments = $temp$syntaxComments;
								_v0 = $temp$_v0;
								continue patternNotParenthesized;
							} else {
								return A3(
									$author$project$ElmSyntaxPrint$parenthesized,
									$author$project$ElmSyntaxPrint$patternNotParenthesized,
									{
										fullRange: fullRange,
										notParenthesized: $author$project$ElmSyntaxPrint$patternToNotParenthesized(inParens)
									},
									syntaxComments);
							}
						} else {
							if (!parts.b.b.b) {
								var part0 = parts.a;
								var _v4 = parts.b;
								var part1 = _v4.a;
								return A3(
									$author$project$ElmSyntaxPrint$tuple,
									$author$project$ElmSyntaxPrint$patternNotParenthesized,
									syntaxComments,
									{fullRange: fullRange, part0: part0, part1: part1});
							} else {
								if (!parts.b.b.b.b) {
									var part0 = parts.a;
									var _v5 = parts.b;
									var part1 = _v5.a;
									var _v6 = _v5.b;
									var part2 = _v6.a;
									return A3(
										$author$project$ElmSyntaxPrint$triple,
										$author$project$ElmSyntaxPrint$patternNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2});
								} else {
									var part0 = parts.a;
									var _v8 = parts.b;
									var part1 = _v8.a;
									var _v9 = _v8.b;
									var part2 = _v9.a;
									var _v10 = _v9.b;
									var part3 = _v10.a;
									var part4Up = _v10.b;
									return A3(
										$author$project$ElmSyntaxPrint$invalidNTuple,
										$author$project$ElmSyntaxPrint$patternNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2, part3: part3, part4Up: part4Up});
								}
							}
						}
					}
				case 'RecordPattern':
					var fields = syntaxPattern.a;
					return A2(
						$author$project$ElmSyntaxPrint$patternRecord,
						syntaxComments,
						{fields: fields, fullRange: fullRange});
				case 'UnConsPattern':
					var headPattern = syntaxPattern.a;
					var tailPattern = syntaxPattern.b;
					return A2(
						$author$project$ElmSyntaxPrint$patternCons,
						syntaxComments,
						{head: headPattern, tail: tailPattern});
				case 'ListPattern':
					var elementPatterns = syntaxPattern.a;
					return A2(
						$author$project$ElmSyntaxPrint$patternList,
						syntaxComments,
						{elements: elementPatterns, fullRange: fullRange});
				case 'NamedPattern':
					var syntaxQualifiedNameRef = syntaxPattern.a;
					var argumentPatterns = syntaxPattern.b;
					return A3(
						$author$project$ElmSyntaxPrint$referenceConstruct,
						$author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated,
						syntaxComments,
						{_arguments: argumentPatterns, fullRange: fullRange, referenceQualification: syntaxQualifiedNameRef.moduleName, referenceUnqualified: syntaxQualifiedNameRef.name});
				default:
					var aliasedPattern = syntaxPattern.a;
					var aliasNameNode = syntaxPattern.b;
					return A2(
						$author$project$ElmSyntaxPrint$patternAs,
						syntaxComments,
						{aliasNameNode: aliasNameNode, aliasedPattern: aliasedPattern});
			}
		}
	});
var $author$project$ElmSyntaxPrint$patternParenthesized = F2(
	function (syntaxComments, patternNode) {
		return A3(
			$author$project$ElmSyntaxPrint$parenthesized,
			$author$project$ElmSyntaxPrint$patternNotParenthesized,
			{
				fullRange: $stil4m$elm_syntax$Elm$Syntax$Node$range(patternNode),
				notParenthesized: $author$project$ElmSyntaxPrint$patternToNotParenthesized(patternNode)
			},
			syntaxComments);
	});
var $author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated = F2(
	function (syntaxComments, syntaxPattern) {
		return $author$project$ElmSyntaxPrint$patternIsSpaceSeparated(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxPattern)) ? A2($author$project$ElmSyntaxPrint$patternParenthesized, syntaxComments, syntaxPattern) : A2($author$project$ElmSyntaxPrint$patternNotParenthesized, syntaxComments, syntaxPattern);
	});
var $author$project$ElmSyntaxPrint$case_ = F2(
	function (syntaxComments, _v70) {
		var casePattern = _v70.a;
		var caseResult = _v70.b;
		var patternPrint = A2($author$project$ElmSyntaxPrint$patternNotParenthesized, syntaxComments, casePattern);
		var commentsBeforeExpression = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(caseResult).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(casePattern).end
			},
			syntaxComments);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, caseResult),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsBeforeExpression.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsBeforeExpression.a;
								var comment1Up = commentsBeforeExpression.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$linebreakIndented,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)));
							}
						}(),
						$author$project$Print$linebreakIndented))),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('->'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(
						$author$project$Print$lineSpread(patternPrint)),
					patternPrint)));
	});
var $author$project$ElmSyntaxPrint$declarationExpressionImplementation = F2(
	function (syntaxComments, implementation) {
		var parameterPrints = A2(
			$elm$core$List$map,
			function (parameterPattern) {
				return A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, parameterPattern);
			},
			implementation._arguments);
		var parameterCommentsBefore = A3(
			$elm$core$List$foldl,
			F2(
				function (parameterPattern, soFar) {
					var parameterRange = $stil4m$elm_syntax$Elm$Syntax$Node$range(parameterPattern);
					var commentsBeforeParameter = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: parameterRange.start, start: soFar.end},
						syntaxComments);
					return {
						end: parameterRange.end,
						reverse: A2(
							$elm$core$List$cons,
							function () {
								if (!commentsBeforeParameter.b) {
									return $elm$core$Maybe$Nothing;
								} else {
									var comment0 = commentsBeforeParameter.a;
									var comment1Up = commentsBeforeParameter.b;
									return $elm$core$Maybe$Just(
										$author$project$ElmSyntaxPrint$collapsibleComments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							soFar.reverse)
					};
				}),
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(implementation.name).end,
				reverse: _List_Nil
			},
			implementation._arguments);
		var parametersLineSpread = A2(
			$author$project$Print$lineSpreadMerge,
			A2(
				$author$project$Print$mapAndLineSpreadsCombine,
				function ($) {
					return $.lineSpread;
				},
				A2($elm$core$List$filterMap, $elm$core$Basics$identity, parameterCommentsBefore.reverse)),
			A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, parameterPrints));
		var commentsBetweenParametersAndResult = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(implementation.expression).start,
				start: parameterCommentsBefore.end
			},
			syntaxComments);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, implementation.expression),
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsBetweenParametersAndResult.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = commentsBetweenParametersAndResult.a;
									var comment1Up = commentsBetweenParametersAndResult.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreakIndented,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							$author$project$Print$linebreakIndented)),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('='),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread),
							$author$project$Print$sequence(
								A3(
									$elm$core$List$map2,
									F2(
										function (parameterPrint, maybeCommentsBefore) {
											return A2(
												$author$project$Print$followedBy,
												parameterPrint,
												A2(
													$author$project$Print$followedBy,
													function () {
														if (maybeCommentsBefore.$ === 'Nothing') {
															return $author$project$Print$empty;
														} else {
															var commentsBefore = maybeCommentsBefore.a;
															return A2(
																$author$project$Print$followedBy,
																$author$project$Print$spaceOrLinebreakIndented(
																	A2(
																		$author$project$Print$lineSpreadMerge,
																		commentsBefore.lineSpread,
																		$author$project$Print$lineSpread(parameterPrint))),
																commentsBefore.print);
														}
													}(),
													$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread)));
										}),
									parameterPrints,
									$elm$core$List$reverse(parameterCommentsBefore.reverse))))))),
			$author$project$Print$exactly(
				$stil4m$elm_syntax$Elm$Syntax$Node$value(implementation.name)));
	});
var $author$project$ElmSyntaxPrint$expressionCall = F2(
	function (syntaxComments, syntaxCall) {
		var commentsBeforeArgument0 = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCall.argument0).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCall.applied).end
			},
			syntaxComments);
		var collapsibleCommentsBeforeArgument0 = $author$project$ElmSyntaxPrint$collapsibleComments(commentsBeforeArgument0);
		var argument1UpPrints = A2(
			$elm$core$List$map,
			function (argument) {
				return A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated, syntaxComments, argument);
			},
			syntaxCall.argument1Up);
		var argument1UpCommentsBefore = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (argument, soFar) {
						var commentsBeforeArgument = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{
								end: $stil4m$elm_syntax$Elm$Syntax$Node$range(argument).start,
								start: soFar.previousEnd
							},
							syntaxComments);
						return {
							previousEnd: $stil4m$elm_syntax$Elm$Syntax$Node$range(argument).end,
							resultReverse: A2(
								$elm$core$List$cons,
								function () {
									if (!commentsBeforeArgument.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeArgument.a;
										var comment1Up = commentsBeforeArgument.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								soFar.resultReverse)
						};
					}),
				{
					previousEnd: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCall.argument0).end,
					resultReverse: _List_Nil
				},
				syntaxCall.argument1Up).resultReverse);
		var argument0Print = A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated, syntaxComments, syntaxCall.argument0);
		var appliedPrint = A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated, syntaxComments, syntaxCall.applied);
		var argument0LineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$Print$lineSpread(appliedPrint),
					A2($author$project$ElmSyntaxPrint$lineSpreadBetweenNodes, syntaxCall.applied, syntaxCall.argument0),
					collapsibleCommentsBeforeArgument0.lineSpread,
					$author$project$Print$lineSpread(argument0Print)
				]));
		var argument1UpLineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxCall.fullRange),
					argument0LineSpread,
					A2(
					$author$project$Print$mapAndLineSpreadsCombine,
					function ($) {
						return $.lineSpread;
					},
					A2($elm$core$List$filterMap, $elm$core$Basics$identity, argument1UpCommentsBefore)),
					A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, argument1UpPrints)
				]));
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A3(
							$elm$core$List$map2,
							F2(
								function (argumentPrint, maybeCommentsBeforeArgument) {
									return A2(
										$author$project$Print$followedBy,
										argumentPrint,
										A2(
											$author$project$Print$followedBy,
											function () {
												if (maybeCommentsBeforeArgument.$ === 'Nothing') {
													return $author$project$Print$empty;
												} else {
													var commentsBeforeArgument = maybeCommentsBeforeArgument.a;
													return A2(
														$author$project$Print$followedBy,
														$author$project$Print$spaceOrLinebreakIndented(
															A2(
																$author$project$Print$lineSpreadMerge,
																commentsBeforeArgument.lineSpread,
																$author$project$Print$lineSpread(argumentPrint))),
														commentsBeforeArgument.print);
												}
											}(),
											$author$project$Print$spaceOrLinebreakIndented(argument1UpLineSpread)));
								}),
							argument1UpPrints,
							argument1UpCommentsBefore)),
					A2(
						$author$project$Print$followedBy,
						argument0Print,
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsBeforeArgument0.b) {
									return $author$project$Print$empty;
								} else {
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$spaceOrLinebreakIndented(
											A2(
												$author$project$Print$lineSpreadMerge,
												collapsibleCommentsBeforeArgument0.lineSpread,
												$author$project$Print$lineSpread(argument0Print))),
										collapsibleCommentsBeforeArgument0.print);
								}
							}(),
							$author$project$Print$spaceOrLinebreakIndented(argument0LineSpread))))),
			appliedPrint);
	});
var $author$project$ElmSyntaxPrint$expressionCaseOf = F2(
	function (syntaxComments, syntaxCaseOf) {
		var commentsBeforeCasedExpression = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCaseOf.expression).start,
				start: syntaxCaseOf.fullRange.start
			},
			syntaxComments);
		var casedExpressionLineSpread = function () {
			if (commentsBeforeCasedExpression.b) {
				return $author$project$Print$MultipleLines;
			} else {
				return $author$project$ElmSyntaxPrint$lineSpreadInNode(syntaxCaseOf.expression);
			}
		}();
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$sequence(
						A2(
							$elm$core$List$intersperse,
							A2($author$project$Print$followedBy, $author$project$Print$linebreakIndented, $author$project$Print$linebreak),
							$elm$core$List$reverse(
								A3(
									$elm$core$List$foldl,
									F2(
										function (_v61, soFar) {
											var casePattern = _v61.a;
											var caseResult = _v61.b;
											var commentsBeforeCasePattern = A2(
												$author$project$ElmSyntaxPrint$commentsInRange,
												{
													end: $stil4m$elm_syntax$Elm$Syntax$Node$range(casePattern).start,
													start: soFar.end
												},
												syntaxComments);
											var commentsAndCasePrint = A2(
												$author$project$Print$followedBy,
												A2(
													$author$project$ElmSyntaxPrint$case_,
													syntaxComments,
													_Utils_Tuple2(casePattern, caseResult)),
												function () {
													if (!commentsBeforeCasePattern.b) {
														return $author$project$Print$empty;
													} else {
														var comment0 = commentsBeforeCasePattern.a;
														var comment1Up = commentsBeforeCasePattern.b;
														return A2(
															$author$project$Print$followedBy,
															$author$project$Print$linebreakIndented,
															$author$project$ElmSyntaxPrint$comments(
																A2($elm$core$List$cons, comment0, comment1Up)));
													}
												}());
											return {
												end: $stil4m$elm_syntax$Elm$Syntax$Node$range(caseResult).end,
												reverse: A2($elm$core$List$cons, commentsAndCasePrint, soFar.reverse)
											};
										}),
									{
										end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxCaseOf.expression).end,
										reverse: _List_Nil
									},
									syntaxCaseOf.cases).reverse))),
					$author$project$Print$linebreakIndented)),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('of'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(casedExpressionLineSpread),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$withIndentAtNextMultipleOf4(
							A2(
								$author$project$Print$followedBy,
								A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxCaseOf.expression),
								A2(
									$author$project$Print$followedBy,
									function () {
										if (!commentsBeforeCasedExpression.b) {
											return $author$project$Print$empty;
										} else {
											var comment0 = commentsBeforeCasedExpression.a;
											var comment1Up = commentsBeforeCasedExpression.b;
											return A2(
												$author$project$Print$followedBy,
												$author$project$Print$linebreakIndented,
												$author$project$ElmSyntaxPrint$comments(
													A2($elm$core$List$cons, comment0, comment1Up)));
										}
									}(),
									$author$project$Print$spaceOrLinebreakIndented(casedExpressionLineSpread)))),
						$author$project$Print$exactly('case')))));
	});
var $author$project$ElmSyntaxPrint$expressionIfThenElse = F2(
	function (syntaxComments, syntaxIfThenElse) {
		var onTruePrint = A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxIfThenElse.onTrue);
		var onFalseNotParenthesized = $author$project$ElmSyntaxPrint$expressionToNotParenthesized(syntaxIfThenElse.onFalse);
		var conditionPrint = A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxIfThenElse.condition);
		var commentsBeforeOnTrue = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.onTrue).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.condition).end
			},
			syntaxComments);
		var commentsBeforeOnFalseNotParenthesizedInParens = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(onFalseNotParenthesized).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.onFalse).start
			},
			syntaxComments);
		var commentsBeforeOnFalse = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.onFalse).start,
				start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.onTrue).end
			},
			syntaxComments);
		var commentsBeforeCondition = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxIfThenElse.condition).start,
				start: syntaxIfThenElse.fullRange.start
			},
			syntaxComments);
		var conditionLineSpread = function () {
			if (commentsBeforeCondition.b) {
				return $author$project$Print$MultipleLines;
			} else {
				return A2(
					$author$project$Print$lineSpreadMerge,
					syntaxIfThenElse.conditionLineSpreadMinimum,
					$author$project$Print$lineSpread(conditionPrint));
			}
		}();
		return A2(
			$author$project$Print$followedBy,
			function () {
				var _v54 = _Utils_Tuple2(commentsBeforeOnFalseNotParenthesizedInParens, onFalseNotParenthesized);
				if ((!_v54.a.b) && (_v54.b.b.$ === 'IfBlock')) {
					var _v55 = _v54.b;
					var onFalseNotParenthesizedRange = _v55.a;
					var _v56 = _v55.b;
					var onFalseCondition = _v56.a;
					var onFalseOnTrue = _v56.b;
					var onFalseOnFalse = _v56.c;
					if (!commentsBeforeOnFalse.b) {
						return A2(
							$author$project$Print$followedBy,
							A2(
								$author$project$ElmSyntaxPrint$expressionIfThenElse,
								syntaxComments,
								{condition: onFalseCondition, conditionLineSpreadMinimum: $author$project$Print$SingleLine, fullRange: onFalseNotParenthesizedRange, onFalse: onFalseOnFalse, onTrue: onFalseOnTrue}),
							$author$project$Print$space);
					} else {
						var comment0 = commentsBeforeOnFalse.a;
						var comment1Up = commentsBeforeOnFalse.b;
						return A2(
							$author$project$Print$followedBy,
							A2(
								$author$project$ElmSyntaxPrint$expressionIfThenElse,
								syntaxComments,
								{condition: onFalseCondition, conditionLineSpreadMinimum: $author$project$Print$MultipleLines, fullRange: onFalseNotParenthesizedRange, onFalse: onFalseOnFalse, onTrue: onFalseOnTrue}),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreakIndented,
								A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)),
									$author$project$Print$linebreakIndented)));
					}
				} else {
					return $author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxIfThenElse.onFalse),
							A2(
								$author$project$Print$followedBy,
								function () {
									if (!commentsBeforeOnFalse.b) {
										return $author$project$Print$empty;
									} else {
										var comment0 = commentsBeforeOnFalse.a;
										var comment1Up = commentsBeforeOnFalse.b;
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$linebreakIndented,
											$author$project$ElmSyntaxPrint$comments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								$author$project$Print$linebreakIndented)));
				}
			}(),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('else'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$linebreakIndented,
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$withIndentAtNextMultipleOf4(
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								A2(
									$author$project$Print$followedBy,
									onTruePrint,
									A2(
										$author$project$Print$followedBy,
										function () {
											if (!commentsBeforeOnTrue.b) {
												return $author$project$Print$empty;
											} else {
												var comment0 = commentsBeforeOnTrue.a;
												var comment1Up = commentsBeforeOnTrue.b;
												return A2(
													$author$project$Print$followedBy,
													$author$project$Print$linebreakIndented,
													$author$project$ElmSyntaxPrint$comments(
														A2($elm$core$List$cons, comment0, comment1Up)));
											}
										}(),
										$author$project$Print$linebreakIndented)))),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly('then'),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$spaceOrLinebreakIndented(conditionLineSpread),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$withIndentAtNextMultipleOf4(
										A2(
											$author$project$Print$followedBy,
											conditionPrint,
											A2(
												$author$project$Print$followedBy,
												function () {
													if (!commentsBeforeCondition.b) {
														return $author$project$Print$empty;
													} else {
														var comment0 = commentsBeforeCondition.a;
														var comment1Up = commentsBeforeCondition.b;
														return A2(
															$author$project$Print$followedBy,
															$author$project$Print$linebreakIndented,
															$author$project$ElmSyntaxPrint$comments(
																A2($elm$core$List$cons, comment0, comment1Up)));
													}
												}(),
												$author$project$Print$spaceOrLinebreakIndented(conditionLineSpread)))),
									$author$project$Print$exactly('if'))))))));
	});
var $author$project$ElmSyntaxPrint$expressionLambda = F2(
	function (syntaxComments, _v48) {
		var fullRange = _v48.a;
		var syntaxLambda = _v48.b;
		var parameterPrints = A2(
			$elm$core$List$map,
			function (parameterPattern) {
				return A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, parameterPattern);
			},
			syntaxLambda.args);
		var parameterCommentsBefore = A3(
			$elm$core$List$foldl,
			F2(
				function (parameterPattern, soFar) {
					var parameterRange = $stil4m$elm_syntax$Elm$Syntax$Node$range(parameterPattern);
					var commentsBeforeParameter = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: parameterRange.start, start: soFar.end},
						syntaxComments);
					return {
						end: parameterRange.end,
						reverse: A2(
							$elm$core$List$cons,
							function () {
								if (!commentsBeforeParameter.b) {
									return $elm$core$Maybe$Nothing;
								} else {
									var comment0 = commentsBeforeParameter.a;
									var comment1Up = commentsBeforeParameter.b;
									return $elm$core$Maybe$Just(
										$author$project$ElmSyntaxPrint$collapsibleComments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							soFar.reverse)
					};
				}),
			{end: fullRange.start, reverse: _List_Nil},
			syntaxLambda.args);
		var parametersLineSpread = A2(
			$author$project$Print$lineSpreadMerge,
			A2(
				$author$project$Print$mapAndLineSpreadsCombine,
				function ($) {
					return $.lineSpread;
				},
				A2($elm$core$List$filterMap, $elm$core$Basics$identity, parameterCommentsBefore.reverse)),
			A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, parameterPrints));
		var commentsBeforeResult = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxLambda.expression).start,
				start: parameterCommentsBefore.end
			},
			syntaxComments);
		return A2(
			$author$project$Print$followedBy,
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$withIndentAtNextMultipleOf4(
					A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxLambda.expression),
						function () {
							if (!commentsBeforeResult.b) {
								return $author$project$Print$spaceOrLinebreakIndented(
									A2(
										$author$project$Print$lineSpreadMerge,
										$author$project$ElmSyntaxPrint$lineSpreadInRange(fullRange),
										parametersLineSpread));
							} else {
								var comment0 = commentsBeforeResult.a;
								var comment1Up = commentsBeforeResult.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$linebreakIndented,
									A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										$author$project$Print$linebreakIndented));
							}
						}())),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('->'),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread),
						A2(
							$author$project$Print$withIndentIncreasedBy,
							1,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$sequence(
									A2(
										$elm$core$List$intersperse,
										$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread),
										A3(
											$elm$core$List$map2,
											F2(
												function (parameterPrint, maybeCommentsBefore) {
													return A2(
														$author$project$Print$followedBy,
														parameterPrint,
														function () {
															if (maybeCommentsBefore.$ === 'Nothing') {
																return $author$project$Print$empty;
															} else {
																var commentsBefore = maybeCommentsBefore.a;
																return A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$spaceOrLinebreakIndented(
																		A2(
																			$author$project$Print$lineSpreadMerge,
																			commentsBefore.lineSpread,
																			$author$project$Print$lineSpread(parameterPrint))),
																	commentsBefore.print);
															}
														}());
												}),
											parameterPrints,
											$elm$core$List$reverse(parameterCommentsBefore.reverse)))),
								$author$project$Print$emptyOrLinebreakIndented(parametersLineSpread)))))),
			$author$project$Print$exactly('\\'));
	});
var $author$project$ElmSyntaxPrint$expressionLetDeclaration = F2(
	function (syntaxComments, letDeclaration) {
		if (letDeclaration.$ === 'LetFunction') {
			var letDeclarationExpression = letDeclaration.a;
			return A2(
				$author$project$Print$followedBy,
				A2(
					$author$project$ElmSyntaxPrint$declarationExpressionImplementation,
					syntaxComments,
					$stil4m$elm_syntax$Elm$Syntax$Node$value(letDeclarationExpression.declaration)),
				function () {
					var _v44 = letDeclarationExpression.signature;
					if (_v44.$ === 'Nothing') {
						return $author$project$Print$empty;
					} else {
						var _v45 = _v44.a;
						var signatureRange = _v45.a;
						var signature = _v45.b;
						var commentsBetweenSignatureAndImplementationName = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{
								end: $stil4m$elm_syntax$Elm$Syntax$Node$range(letDeclarationExpression.declaration).start,
								start: signatureRange.end
							},
							syntaxComments);
						return A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreakIndented,
							A2(
								$author$project$Print$followedBy,
								function () {
									if (!commentsBetweenSignatureAndImplementationName.b) {
										return $author$project$Print$empty;
									} else {
										var comment0 = commentsBetweenSignatureAndImplementationName.a;
										var comment1Up = commentsBetweenSignatureAndImplementationName.b;
										return A2(
											$author$project$Print$followedBy,
											$author$project$ElmSyntaxPrint$comments(
												A2($elm$core$List$cons, comment0, comment1Up)),
											$author$project$Print$linebreakIndented);
									}
								}(),
								A2($author$project$ElmSyntaxPrint$declarationSignature, syntaxComments, signature)));
					}
				}());
		} else {
			var destructuringPattern = letDeclaration.a;
			var destructuredExpression = letDeclaration.b;
			var commentsBeforeDestructuredExpression = A2(
				$author$project$ElmSyntaxPrint$commentsInRange,
				{
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(destructuredExpression).start,
					start: $stil4m$elm_syntax$Elm$Syntax$Node$range(destructuringPattern).end
				},
				syntaxComments);
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$withIndentAtNextMultipleOf4(
					A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, destructuredExpression),
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsBeforeDestructuredExpression.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = commentsBeforeDestructuredExpression.a;
									var comment1Up = commentsBeforeDestructuredExpression.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreakIndented,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							$author$project$Print$linebreakIndented))),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('='),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$space,
						A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, destructuringPattern))));
		}
	});
var $author$project$ElmSyntaxPrint$expressionLetIn = F2(
	function (syntaxComments, syntaxLetIn) {
		var letDeclarationPrints = A3(
			$elm$core$List$foldl,
			F2(
				function (_v41, soFar) {
					var letDeclarationRange = _v41.a;
					var letDeclaration = _v41.b;
					var commentsBefore = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: letDeclarationRange.start, start: soFar.end},
						syntaxComments);
					var letDeclarationPrint = A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$expressionLetDeclaration, syntaxComments, letDeclaration),
						function () {
							if (!commentsBefore.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsBefore.a;
								var comment1Up = commentsBefore.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$Print$linebreakIndented,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)));
							}
						}());
					return {
						end: letDeclarationRange.end,
						printsReverse: A2($elm$core$List$cons, letDeclarationPrint, soFar.printsReverse)
					};
				}),
			{end: syntaxLetIn.fullRange.start, printsReverse: _List_Nil},
			A2($elm$core$List$cons, syntaxLetIn.letDeclaration0, syntaxLetIn.letDeclaration1Up));
		var commentsBeforeResult = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxLetIn.result).start,
				start: letDeclarationPrints.end
			},
			syntaxComments);
		return A2(
			$author$project$Print$followedBy,
			A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, syntaxLetIn.result),
			A2(
				$author$project$Print$followedBy,
				function () {
					if (!commentsBeforeResult.b) {
						return $author$project$Print$empty;
					} else {
						var comment0 = commentsBeforeResult.a;
						var comment1Up = commentsBeforeResult.b;
						return A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreakIndented,
							$author$project$ElmSyntaxPrint$comments(
								A2($elm$core$List$cons, comment0, comment1Up)));
					}
				}(),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$linebreakIndented,
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('in'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreakIndented,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$sequence(
											A2(
												$elm$core$List$intersperse,
												A2($author$project$Print$followedBy, $author$project$Print$linebreakIndented, $author$project$Print$linebreak),
												$elm$core$List$reverse(letDeclarationPrints.printsReverse))),
										$author$project$Print$linebreakIndented)),
								$author$project$Print$exactly('let')))))));
	});
var $author$project$ElmSyntaxPrint$expressionList = F2(
	function (syntaxComments, syntaxList) {
		var _v34 = syntaxList.elements;
		if (!_v34.b) {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(']'),
				A2(
					$author$project$Print$followedBy,
					function () {
						var _v35 = A2($author$project$ElmSyntaxPrint$commentsInRange, syntaxList.fullRange, syntaxComments);
						if (!_v35.b) {
							return $author$project$Print$empty;
						} else {
							var comment0 = _v35.a;
							var comment1Up = _v35.b;
							var commentsCollapsed = $author$project$ElmSyntaxPrint$collapsibleComments(
								A2($elm$core$List$cons, comment0, comment1Up));
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(commentsCollapsed.lineSpread),
								A2($author$project$Print$withIndentIncreasedBy, 1, commentsCollapsed.print));
						}
					}(),
					$author$project$Print$exactly('[')));
		} else {
			var element0 = _v34.a;
			var element1Up = _v34.b;
			var elementPrints = A2(
				$elm$core$List$map,
				function (element) {
					return A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, element);
				},
				A2($elm$core$List$cons, element0, element1Up));
			var commentsBeforeElements = A3(
				$elm$core$List$foldl,
				F2(
					function (_v38, soFar) {
						var elementRange = _v38.a;
						var commentsBeforeElement = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: elementRange.start, start: soFar.end},
							syntaxComments);
						return {
							end: elementRange.end,
							reverse: A2(
								$elm$core$List$cons,
								function () {
									if (!commentsBeforeElement.b) {
										return $elm$core$Maybe$Nothing;
									} else {
										var comment0 = commentsBeforeElement.a;
										var comment1Up = commentsBeforeElement.b;
										return $elm$core$Maybe$Just(
											$author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}(),
								soFar.reverse)
						};
					}),
				{end: syntaxList.fullRange.start, reverse: _List_Nil},
				A2($elm$core$List$cons, element0, element1Up));
			var lineSpread = $author$project$Print$lineSpreadsCombine(
				_List_fromArray(
					[
						$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxList.fullRange),
						A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, elementPrints),
						A2(
						$author$project$Print$mapAndLineSpreadsCombine,
						function ($) {
							return $.lineSpread;
						},
						A2($elm$core$List$filterMap, $elm$core$Basics$identity, commentsBeforeElements.reverse))
					]));
			var commentsAfterElements = A2(
				$author$project$ElmSyntaxPrint$commentsInRange,
				{end: syntaxList.fullRange.end, start: commentsBeforeElements.end},
				syntaxComments);
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(']'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$spaceOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$followedBy,
						function () {
							if (!commentsAfterElements.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = commentsAfterElements.a;
								var comment1Up = commentsAfterElements.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$comments(
										A2($elm$core$List$cons, comment0, comment1Up)),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$spaceOrLinebreakIndented(lineSpread),
										$author$project$Print$linebreak));
							}
						}(),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$sequence(
								A2(
									$elm$core$List$intersperse,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$space,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$exactly(','),
											$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
									A3(
										$elm$core$List$map2,
										F2(
											function (elementPrint, maybeCommentsBeforeElement) {
												return A2(
													$author$project$Print$withIndentIncreasedBy,
													2,
													A2(
														$author$project$Print$followedBy,
														elementPrint,
														function () {
															if (maybeCommentsBeforeElement.$ === 'Nothing') {
																return $author$project$Print$empty;
															} else {
																var commentsBeforeElement = maybeCommentsBeforeElement.a;
																return A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$spaceOrLinebreakIndented(
																		A2(
																			$author$project$Print$lineSpreadMerge,
																			commentsBeforeElement.lineSpread,
																			$author$project$Print$lineSpread(elementPrint))),
																	commentsBeforeElement.print);
															}
														}()));
											}),
										elementPrints,
										$elm$core$List$reverse(commentsBeforeElements.reverse)))),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								$author$project$Print$exactly('['))))));
		}
	});
var $author$project$ElmSyntaxPrint$expressionNotParenthesized = F2(
	function (syntaxComments, _v19) {
		expressionNotParenthesized:
		while (true) {
			var fullRange = _v19.a;
			var syntaxExpression = _v19.b;
			switch (syntaxExpression.$) {
				case 'UnitExpr':
					return $author$project$Print$exactly('()');
				case 'Application':
					var application = syntaxExpression.a;
					if (!application.b) {
						return $author$project$Print$empty;
					} else {
						if (!application.b.b) {
							var notAppliedAfterAll = application.a;
							var $temp$syntaxComments = syntaxComments,
								$temp$_v19 = notAppliedAfterAll;
							syntaxComments = $temp$syntaxComments;
							_v19 = $temp$_v19;
							continue expressionNotParenthesized;
						} else {
							var applied = application.a;
							var _v22 = application.b;
							var argument0 = _v22.a;
							var argument1Up = _v22.b;
							return A2(
								$author$project$ElmSyntaxPrint$expressionCall,
								syntaxComments,
								{applied: applied, argument0: argument0, argument1Up: argument1Up, fullRange: fullRange});
						}
					}
				case 'OperatorApplication':
					var operator = syntaxExpression.a;
					var left = syntaxExpression.c;
					var right = syntaxExpression.d;
					return A2(
						$author$project$ElmSyntaxPrint$expressionOperation,
						syntaxComments,
						{fullRange: fullRange, left: left, operator: operator, right: right});
				case 'FunctionOrValue':
					var qualification = syntaxExpression.a;
					var unqualified = syntaxExpression.b;
					return $author$project$ElmSyntaxPrint$qualifiedReference(
						{qualification: qualification, unqualified: unqualified});
				case 'IfBlock':
					var condition = syntaxExpression.a;
					var onTrue = syntaxExpression.b;
					var onFalse = syntaxExpression.c;
					return A2(
						$author$project$ElmSyntaxPrint$expressionIfThenElse,
						syntaxComments,
						{condition: condition, conditionLineSpreadMinimum: $author$project$Print$SingleLine, fullRange: fullRange, onFalse: onFalse, onTrue: onTrue});
				case 'PrefixOperator':
					var operatorSymbol = syntaxExpression.a;
					return A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly(')'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(operatorSymbol),
							$author$project$Print$exactly('(')));
				case 'Operator':
					var operatorSymbol = syntaxExpression.a;
					return $author$project$Print$exactly(operatorSymbol);
				case 'Integer':
					var _int = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$intLiteral(_int);
				case 'Hex':
					var _int = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$hexLiteral(_int);
				case 'Floatable':
					var _float = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$floatLiteral(_float);
				case 'Negation':
					var negated = syntaxExpression.a;
					return A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated, syntaxComments, negated),
						$author$project$Print$exactly('-'));
				case 'Literal':
					var string = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$stringLiteral(
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, string));
				case 'CharLiteral':
					var _char = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$charLiteral(_char);
				case 'TupledExpression':
					var parts = syntaxExpression.a;
					if (!parts.b) {
						return $author$project$Print$exactly('()');
					} else {
						if (!parts.b.b) {
							var inParens = parts.a;
							var commentsBeforeInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).start,
									start: fullRange.start
								},
								syntaxComments);
							var commentsAfterInParens = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{
									end: fullRange.end,
									start: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).end
								},
								syntaxComments);
							var _v24 = _Utils_Tuple2(commentsBeforeInParens, commentsAfterInParens);
							if ((!_v24.a.b) && (!_v24.b.b)) {
								var $temp$syntaxComments = syntaxComments,
									$temp$_v19 = inParens;
								syntaxComments = $temp$syntaxComments;
								_v19 = $temp$_v19;
								continue expressionNotParenthesized;
							} else {
								return A3(
									$author$project$ElmSyntaxPrint$parenthesized,
									$author$project$ElmSyntaxPrint$expressionNotParenthesized,
									{
										fullRange: fullRange,
										notParenthesized: $author$project$ElmSyntaxPrint$expressionToNotParenthesized(inParens)
									},
									syntaxComments);
							}
						} else {
							if (!parts.b.b.b) {
								var part0 = parts.a;
								var _v25 = parts.b;
								var part1 = _v25.a;
								return A3(
									$author$project$ElmSyntaxPrint$tuple,
									$author$project$ElmSyntaxPrint$expressionNotParenthesized,
									syntaxComments,
									{fullRange: fullRange, part0: part0, part1: part1});
							} else {
								if (!parts.b.b.b.b) {
									var part0 = parts.a;
									var _v26 = parts.b;
									var part1 = _v26.a;
									var _v27 = _v26.b;
									var part2 = _v27.a;
									return A3(
										$author$project$ElmSyntaxPrint$triple,
										$author$project$ElmSyntaxPrint$expressionNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2});
								} else {
									var part0 = parts.a;
									var _v28 = parts.b;
									var part1 = _v28.a;
									var _v29 = _v28.b;
									var part2 = _v29.a;
									var _v30 = _v29.b;
									var part3 = _v30.a;
									var part4Up = _v30.b;
									return A3(
										$author$project$ElmSyntaxPrint$invalidNTuple,
										$author$project$ElmSyntaxPrint$expressionNotParenthesized,
										syntaxComments,
										{fullRange: fullRange, part0: part0, part1: part1, part2: part2, part3: part3, part4Up: part4Up});
								}
							}
						}
					}
				case 'ParenthesizedExpression':
					var inParens = syntaxExpression.a;
					var commentsBeforeInParens = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{
							end: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).start,
							start: fullRange.start
						},
						syntaxComments);
					var commentsAfterInParens = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{
							end: fullRange.end,
							start: $stil4m$elm_syntax$Elm$Syntax$Node$range(inParens).end
						},
						syntaxComments);
					var _v31 = _Utils_Tuple2(commentsBeforeInParens, commentsAfterInParens);
					if ((!_v31.a.b) && (!_v31.b.b)) {
						var $temp$syntaxComments = syntaxComments,
							$temp$_v19 = inParens;
						syntaxComments = $temp$syntaxComments;
						_v19 = $temp$_v19;
						continue expressionNotParenthesized;
					} else {
						return A3(
							$author$project$ElmSyntaxPrint$parenthesized,
							$author$project$ElmSyntaxPrint$expressionNotParenthesized,
							{
								fullRange: fullRange,
								notParenthesized: $author$project$ElmSyntaxPrint$expressionToNotParenthesized(inParens)
							},
							syntaxComments);
					}
				case 'LetExpression':
					var syntaxLetIn = syntaxExpression.a;
					var _v32 = syntaxLetIn.declarations;
					if (!_v32.b) {
						var $temp$syntaxComments = syntaxComments,
							$temp$_v19 = syntaxLetIn.expression;
						syntaxComments = $temp$syntaxComments;
						_v19 = $temp$_v19;
						continue expressionNotParenthesized;
					} else {
						var letDeclaration0 = _v32.a;
						var letDeclaration1Up = _v32.b;
						return A2(
							$author$project$ElmSyntaxPrint$expressionLetIn,
							syntaxComments,
							{fullRange: fullRange, letDeclaration0: letDeclaration0, letDeclaration1Up: letDeclaration1Up, result: syntaxLetIn.expression});
					}
				case 'CaseExpression':
					var syntaxCaseOf = syntaxExpression.a;
					return A2(
						$author$project$ElmSyntaxPrint$expressionCaseOf,
						syntaxComments,
						{cases: syntaxCaseOf.cases, expression: syntaxCaseOf.expression, fullRange: fullRange});
				case 'LambdaExpression':
					var syntaxLambda = syntaxExpression.a;
					return A2(
						$author$project$ElmSyntaxPrint$expressionLambda,
						syntaxComments,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fullRange, syntaxLambda));
				case 'RecordExpr':
					var fields = syntaxExpression.a;
					return A3(
						$author$project$ElmSyntaxPrint$recordLiteral,
						{nameValueSeparator: '=', printValueNotParenthesized: $author$project$ElmSyntaxPrint$expressionNotParenthesized},
						syntaxComments,
						{fields: fields, fullRange: fullRange});
				case 'ListExpr':
					var elements = syntaxExpression.a;
					return A2(
						$author$project$ElmSyntaxPrint$expressionList,
						syntaxComments,
						{elements: elements, fullRange: fullRange});
				case 'RecordAccess':
					var syntaxRecord = syntaxExpression.a;
					var _v33 = syntaxExpression.b;
					var accessedFieldName = _v33.b;
					return A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly(accessedFieldName),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly('.'),
							A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated, syntaxComments, syntaxRecord)));
				case 'RecordAccessFunction':
					var dotFieldName = syntaxExpression.a;
					return A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly(
							A3($elm$core$String$replace, '.', '', dotFieldName)),
						$author$project$Print$exactly('.'));
				case 'RecordUpdateExpression':
					var recordVariableNode = syntaxExpression.a;
					var fields = syntaxExpression.b;
					return A2(
						$author$project$ElmSyntaxPrint$expressionRecordUpdate,
						syntaxComments,
						{fields: fields, fullRange: fullRange, recordVariable: recordVariableNode});
				default:
					var glsl = syntaxExpression.a;
					return $author$project$ElmSyntaxPrint$expressionGlsl(glsl);
			}
		}
	});
var $author$project$ElmSyntaxPrint$expressionOperation = F2(
	function (syntaxComments, syntaxOperation) {
		var operationExpanded = A3($author$project$ElmSyntaxPrint$expressionOperationExpand, syntaxOperation.left, syntaxOperation.operator, syntaxOperation.right);
		var leftestPrint = A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparatedExceptApplication, syntaxComments, operationExpanded.leftest);
		var beforeRightestComments = A3(
			$elm$core$List$foldl,
			F2(
				function (operatorAndExpressionBeforeRightest, soFar) {
					var expressionRange = $stil4m$elm_syntax$Elm$Syntax$Node$range(operatorAndExpressionBeforeRightest.expression);
					var commentsBefore = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: expressionRange.start, start: soFar.end},
						syntaxComments);
					return {
						commentsReverse: A2(
							$elm$core$List$cons,
							function () {
								if (!commentsBefore.b) {
									return $elm$core$Maybe$Nothing;
								} else {
									var comment0 = commentsBefore.a;
									var comment1Up = commentsBefore.b;
									return $elm$core$Maybe$Just(
										$author$project$ElmSyntaxPrint$collapsibleComments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							soFar.commentsReverse),
						end: expressionRange.end
					};
				}),
			{
				commentsReverse: _List_Nil,
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(operationExpanded.leftest).end
			},
			operationExpanded.beforeRightestOperatorExpressionChain);
		var beforeRightestOperatorExpressionChainWithPreviousLineSpread = A3(
			$elm$core$List$foldl,
			F2(
				function (operatorExpression, soFar) {
					var expressionPrint = A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparatedExceptApplication, syntaxComments, operatorExpression.expression);
					return {
						previousLineSpread: $author$project$Print$lineSpread(expressionPrint),
						rightToLeft: A2(
							$elm$core$List$cons,
							{commentsBeforeExpression: operatorExpression.commentsBeforeExpression, expression: operatorExpression.expression, expressionPrint: expressionPrint, operator: operatorExpression.operator, previousLineSpread: soFar.previousLineSpread},
							soFar.rightToLeft)
					};
				}),
			{
				previousLineSpread: $author$project$Print$lineSpread(leftestPrint),
				rightToLeft: _List_Nil
			},
			A3(
				$elm$core$List$map2,
				F2(
					function (operatorExpression, commentsBeforeExpression) {
						return {commentsBeforeExpression: commentsBeforeExpression, expression: operatorExpression.expression, operator: operatorExpression.operator};
					}),
				operationExpanded.beforeRightestOperatorExpressionChain,
				$elm$core$List$reverse(beforeRightestComments.commentsReverse)));
		var commentsBeforeRightestExpression = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(operationExpanded.rightestExpression).start,
				start: beforeRightestComments.end
			},
			syntaxComments);
		var commentsCollapsibleBeforeRightestExpression = $author$project$ElmSyntaxPrint$collapsibleComments(commentsBeforeRightestExpression);
		var lineSpread = $author$project$Print$lineSpreadsCombine(
			_List_fromArray(
				[
					$author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxOperation.fullRange),
					A2(
					$author$project$Print$mapAndLineSpreadsCombine,
					function ($) {
						return $.lineSpread;
					},
					A2($elm$core$List$filterMap, $elm$core$Basics$identity, beforeRightestComments.commentsReverse)),
					commentsCollapsibleBeforeRightestExpression.lineSpread
				]));
		var rightestOperatorExpressionPrint = function () {
			var _v15 = operationExpanded.rightestOperator;
			if (_v15 === '<|') {
				var expressionPrint = A2($author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda, syntaxComments, operationExpanded.rightestExpression);
				return A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							expressionPrint,
							A2(
								$author$project$Print$followedBy,
								function () {
									if (!commentsBeforeRightestExpression.b) {
										return $author$project$Print$empty;
									} else {
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(
												A2(
													$author$project$Print$lineSpreadMerge,
													commentsCollapsibleBeforeRightestExpression.lineSpread,
													$author$project$Print$lineSpread(expressionPrint))),
											commentsCollapsibleBeforeRightestExpression.print);
									}
								}(),
								$author$project$Print$spaceOrLinebreakIndented(lineSpread)))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('<|'),
						$author$project$Print$spaceOrLinebreakIndented(beforeRightestOperatorExpressionChainWithPreviousLineSpread.previousLineSpread)));
			} else {
				var nonApLOperator = _v15;
				var expressionPrint = $author$project$ElmSyntaxPrint$expressionIsSpaceSeparatedExceptApplication(operationExpanded.rightestExpression) ? A2(
					$author$project$Print$withIndentIncreasedBy,
					$elm$core$String$length(nonApLOperator) + 1,
					A2($author$project$ElmSyntaxPrint$expressionParenthesized, syntaxComments, operationExpanded.rightestExpression)) : A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, operationExpanded.rightestExpression);
				return $author$project$Print$withIndentAtNextMultipleOf4(
					A2(
						$author$project$Print$followedBy,
						expressionPrint,
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsBeforeRightestExpression.b) {
									return $author$project$Print$empty;
								} else {
									return A2(
										$author$project$Print$withIndentIncreasedBy,
										$elm$core$String$length(nonApLOperator) + 1,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(
												A2(
													$author$project$Print$lineSpreadMerge,
													commentsCollapsibleBeforeRightestExpression.lineSpread,
													$author$project$Print$lineSpread(expressionPrint))),
											commentsCollapsibleBeforeRightestExpression.print));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$space,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly(nonApLOperator),
									$author$project$Print$spaceOrLinebreakIndented(lineSpread))))));
			}
		}();
		return A2(
			$author$project$Print$followedBy,
			A3(
				$elm$core$List$foldl,
				F2(
					function (operatorExpression, chainRightPrint) {
						var _v12 = operatorExpression.operator;
						if (_v12 === '<|') {
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										chainRightPrint,
										A2(
											$author$project$Print$followedBy,
											operatorExpression.expressionPrint,
											A2(
												$author$project$Print$followedBy,
												function () {
													var _v13 = operatorExpression.commentsBeforeExpression;
													if (_v13.$ === 'Nothing') {
														return $author$project$Print$empty;
													} else {
														var commentsBeforeExpression = _v13.a;
														return A2(
															$author$project$Print$followedBy,
															$author$project$Print$spaceOrLinebreakIndented(
																A2(
																	$author$project$Print$lineSpreadMerge,
																	commentsBeforeExpression.lineSpread,
																	$author$project$Print$lineSpread(operatorExpression.expressionPrint))),
															commentsBeforeExpression.print);
													}
												}(),
												$author$project$Print$spaceOrLinebreakIndented(lineSpread))))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly('<|'),
									$author$project$Print$spaceOrLinebreakIndented(operatorExpression.previousLineSpread)));
						} else {
							var nonApLOperator = _v12;
							return A2(
								$author$project$Print$followedBy,
								chainRightPrint,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$expressionIsSpaceSeparatedExceptApplication(operatorExpression.expression) ? A2(
											$author$project$Print$withIndentIncreasedBy,
											$elm$core$String$length(nonApLOperator) + 1,
											operatorExpression.expressionPrint) : operatorExpression.expressionPrint,
										A2(
											$author$project$Print$followedBy,
											function () {
												var _v14 = operatorExpression.commentsBeforeExpression;
												if (_v14.$ === 'Nothing') {
													return $author$project$Print$empty;
												} else {
													var commentsBeforeExpression = _v14.a;
													return A2(
														$author$project$Print$withIndentIncreasedBy,
														$elm$core$String$length(nonApLOperator) + 1,
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$spaceOrLinebreakIndented(
																A2(
																	$author$project$Print$lineSpreadMerge,
																	commentsBeforeExpression.lineSpread,
																	$author$project$Print$lineSpread(operatorExpression.expressionPrint))),
															commentsBeforeExpression.print));
												}
											}(),
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$space,
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$exactly(nonApLOperator),
													$author$project$Print$spaceOrLinebreakIndented(lineSpread)))))));
						}
					}),
				rightestOperatorExpressionPrint,
				beforeRightestOperatorExpressionChainWithPreviousLineSpread.rightToLeft),
			leftestPrint);
	});
var $author$project$ElmSyntaxPrint$expressionParenthesized = F2(
	function (syntaxComments, expressionNode) {
		return A3(
			$author$project$ElmSyntaxPrint$parenthesized,
			$author$project$ElmSyntaxPrint$expressionNotParenthesized,
			{
				fullRange: $stil4m$elm_syntax$Elm$Syntax$Node$range(expressionNode),
				notParenthesized: $author$project$ElmSyntaxPrint$expressionToNotParenthesized(expressionNode)
			},
			syntaxComments);
	});
var $author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparated = F2(
	function (syntaxComments, expressionNode) {
		return $author$project$ElmSyntaxPrint$expressionIsSpaceSeparated(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(expressionNode)) ? A2($author$project$ElmSyntaxPrint$expressionParenthesized, syntaxComments, expressionNode) : A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, expressionNode);
	});
var $author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparatedExceptApplication = F2(
	function (syntaxComments, expressionNode) {
		return $author$project$ElmSyntaxPrint$expressionIsSpaceSeparatedExceptApplication(expressionNode) ? A2($author$project$ElmSyntaxPrint$expressionParenthesized, syntaxComments, expressionNode) : A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, expressionNode);
	});
var $author$project$ElmSyntaxPrint$expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda = F2(
	function (syntaxComments, expressionNode) {
		if ($author$project$ElmSyntaxPrint$expressionIsSpaceSeparated(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(expressionNode))) {
			var _v11 = $stil4m$elm_syntax$Elm$Syntax$Node$value(
				$author$project$ElmSyntaxPrint$expressionToNotParenthesized(expressionNode));
			switch (_v11.$) {
				case 'Application':
					return A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, expressionNode);
				case 'LambdaExpression':
					return A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, expressionNode);
				default:
					return A2($author$project$ElmSyntaxPrint$expressionParenthesized, syntaxComments, expressionNode);
			}
		} else {
			return A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, expressionNode);
		}
	});
var $author$project$ElmSyntaxPrint$expressionRecordUpdate = F2(
	function (syntaxComments, syntaxRecordUpdate) {
		var commentsBeforeRecordVariable = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxRecordUpdate.recordVariable).start,
				start: syntaxRecordUpdate.fullRange.start
			},
			syntaxComments);
		var commentsBeforeFields = A3(
			$elm$core$List$foldl,
			F2(
				function (_v7, soFar) {
					var _v8 = _v7.b;
					var _v9 = _v8.a;
					var fieldNameRange = _v9.a;
					var _v10 = _v8.b;
					var fieldValueRange = _v10.a;
					return {
						end: fieldValueRange.end,
						reverse: A2(
							$elm$core$List$cons,
							{
								beforeName: A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{end: fieldNameRange.start, start: soFar.end},
									syntaxComments),
								betweenNameAndValue: A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{end: fieldValueRange.start, start: fieldNameRange.start},
									syntaxComments)
							},
							soFar.reverse)
					};
				}),
			{
				end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxRecordUpdate.recordVariable).end,
				reverse: _List_Nil
			},
			syntaxRecordUpdate.fields);
		var commentsAfterFields = A2(
			$author$project$ElmSyntaxPrint$commentsInRange,
			{end: syntaxRecordUpdate.fullRange.end, start: commentsBeforeFields.end},
			syntaxComments);
		var lineSpread = ($elm$core$List$isEmpty(commentsBeforeRecordVariable) && (A2(
			$elm$core$List$all,
			function (fieldComments) {
				return $elm$core$List$isEmpty(fieldComments.beforeName) && $elm$core$List$isEmpty(fieldComments.betweenNameAndValue);
			},
			commentsBeforeFields.reverse) && $elm$core$List$isEmpty(commentsAfterFields))) ? $author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxRecordUpdate.fullRange) : $author$project$Print$MultipleLines;
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly('}'),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$spaceOrLinebreakIndented(lineSpread),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsAfterFields.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = commentsAfterFields.a;
									var comment1Up = commentsAfterFields.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$spaceOrLinebreakIndented(lineSpread),
											$author$project$Print$linebreak));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$sequence(
									A2(
										$elm$core$List$intersperse,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$exactly(','),
												$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
										A3(
											$elm$core$List$map2,
											F2(
												function (_v1, fieldComments) {
													var _v2 = _v1.b;
													var _v3 = _v2.a;
													var fieldNameRange = _v3.a;
													var fieldName = _v3.b;
													var fieldValue = _v2.b;
													return A2(
														$author$project$Print$followedBy,
														$author$project$Print$withIndentAtNextMultipleOf4(
															A2(
																$author$project$Print$followedBy,
																A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, syntaxComments, fieldValue),
																function () {
																	var _v5 = fieldComments.betweenNameAndValue;
																	if (!_v5.b) {
																		return $author$project$Print$spaceOrLinebreakIndented(
																			$author$project$ElmSyntaxPrint$lineSpreadInRange(
																				{
																					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(fieldValue).end,
																					start: fieldNameRange.start
																				}));
																	} else {
																		var comment0 = _v5.a;
																		var comment1Up = _v5.b;
																		return A2(
																			$author$project$Print$followedBy,
																			$author$project$Print$linebreakIndented,
																			A2(
																				$author$project$Print$followedBy,
																				$author$project$ElmSyntaxPrint$comments(
																					A2($elm$core$List$cons, comment0, comment1Up)),
																				$author$project$Print$linebreakIndented));
																	}
																}())),
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$exactly('='),
															A2(
																$author$project$Print$followedBy,
																$author$project$Print$space,
																A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$exactly(fieldName),
																	A2(
																		$author$project$Print$withIndentIncreasedBy,
																		2,
																		function () {
																			var _v4 = fieldComments.beforeName;
																			if (!_v4.b) {
																				return $author$project$Print$empty;
																			} else {
																				var comment0 = _v4.a;
																				var comment1Up = _v4.b;
																				return A2(
																					$author$project$Print$followedBy,
																					$author$project$Print$linebreakIndented,
																					$author$project$ElmSyntaxPrint$comments(
																						A2($elm$core$List$cons, comment0, comment1Up)));
																			}
																		}())))));
												}),
											syntaxRecordUpdate.fields,
											$elm$core$List$reverse(commentsBeforeFields.reverse)))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly('|'),
										$author$project$Print$spaceOrLinebreakIndented(lineSpread)))))),
					A2(
						$author$project$Print$followedBy,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxRecordUpdate.recordVariable)),
							A2(
								$author$project$Print$withIndentIncreasedBy,
								2,
								function () {
									if (!commentsBeforeRecordVariable.b) {
										return $author$project$Print$empty;
									} else {
										var comment0 = commentsBeforeRecordVariable.a;
										var comment1Up = commentsBeforeRecordVariable.b;
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$linebreakIndented,
											$author$project$ElmSyntaxPrint$comments(
												A2($elm$core$List$cons, comment0, comment1Up)));
									}
								}())),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$space,
							$author$project$Print$exactly('{'))))));
	});
var $author$project$ElmSyntaxPrint$declarationDestructuring = F3(
	function (syntaxComments, destructuringPattern, destructuringExpression) {
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					A2($author$project$ElmSyntaxPrint$expressionNotParenthesized, _List_Nil, destructuringExpression),
					$author$project$Print$linebreakIndented)),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('='),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$space,
					A2($author$project$ElmSyntaxPrint$patternParenthesizedIfSpaceSeparated, syntaxComments, destructuringPattern))));
	});
var $author$project$ElmSyntaxPrint$declarationExpression = F2(
	function (syntaxComments, syntaxExpressionDeclaration) {
		return A2(
			$author$project$Print$followedBy,
			A2(
				$author$project$ElmSyntaxPrint$declarationExpressionImplementation,
				syntaxComments,
				$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxExpressionDeclaration.declaration)),
			A2(
				$author$project$Print$followedBy,
				function () {
					var _v4 = syntaxExpressionDeclaration.signature;
					if (_v4.$ === 'Nothing') {
						return $author$project$Print$empty;
					} else {
						var _v5 = _v4.a;
						var signatureRange = _v5.a;
						var signature = _v5.b;
						var commentsBetweenSignatureAndImplementationName = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{
								end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxExpressionDeclaration.declaration).start,
								start: signatureRange.end
							},
							syntaxComments);
						return A2(
							$author$project$Print$followedBy,
							function () {
								if (!commentsBetweenSignatureAndImplementationName.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = commentsBetweenSignatureAndImplementationName.a;
									var comment1Up = commentsBetweenSignatureAndImplementationName.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreak,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$linebreak,
											A2(
												$author$project$Print$followedBy,
												$author$project$ElmSyntaxPrint$moduleLevelComments(
													A2($elm$core$List$cons, comment0, comment1Up)),
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$linebreak,
													A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)))));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								A2($author$project$ElmSyntaxPrint$declarationSignature, syntaxComments, signature)));
					}
				}(),
				function () {
					var _v0 = syntaxExpressionDeclaration.documentation;
					if (_v0.$ === 'Nothing') {
						return $author$project$Print$empty;
					} else {
						var _v1 = _v0.a;
						var documentationRange = _v1.a;
						var documentation = _v1.b;
						return A2(
							$author$project$Print$followedBy,
							$author$project$ElmSyntaxPrint$commentsBetweenDocumentationAndDeclaration(
								A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{
										end: function () {
											var _v2 = syntaxExpressionDeclaration.signature;
											if (_v2.$ === 'Nothing') {
												return $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxExpressionDeclaration.declaration).start;
											} else {
												var _v3 = _v2.a;
												var signatureRange = _v3.a;
												return signatureRange.start;
											}
										}(),
										start: documentationRange.start
									},
									syntaxComments)),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$linebreak,
								$author$project$Print$exactly(documentation)));
					}
				}()));
	});
var $author$project$ElmSyntaxPrint$infixDirection = function (syntaxInfixDirection) {
	switch (syntaxInfixDirection.$) {
		case 'Left':
			return $author$project$Print$exactly('left ');
		case 'Right':
			return $author$project$Print$exactly('right');
		default:
			return $author$project$Print$exactly('non  ');
	}
};
var $author$project$ElmSyntaxPrint$declarationInfix = function (syntaxInfixDeclaration) {
	return A2(
		$author$project$Print$followedBy,
		$author$project$Print$exactly(
			$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxInfixDeclaration._function)),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$space,
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('='),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$space,
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly(')'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$exactly(
								$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxInfixDeclaration.operator)),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$exactly('('),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly(
											$elm$core$String$fromInt(
												$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxInfixDeclaration.precedence))),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											A2(
												$author$project$Print$followedBy,
												$author$project$ElmSyntaxPrint$infixDirection(
													$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxInfixDeclaration.direction)),
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$space,
													$author$project$Print$exactly('infix')))))))))))));
};
var $author$project$ElmSyntaxPrint$declarationPort = F2(
	function (syntaxComments, signature) {
		return A2(
			$author$project$Print$followedBy,
			A2($author$project$ElmSyntaxPrint$declarationSignature, syntaxComments.comments, signature),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$space,
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('port'),
					function () {
						var _v0 = syntaxComments.documentationComment;
						if (_v0.$ === 'Nothing') {
							return $author$project$Print$empty;
						} else {
							var _v1 = _v0.a;
							var documentationRange = _v1.a;
							var documentation = _v1.b;
							return A2(
								$author$project$Print$followedBy,
								$author$project$ElmSyntaxPrint$commentsBetweenDocumentationAndDeclaration(
									A2(
										$author$project$ElmSyntaxPrint$commentsInRange,
										{
											end: $stil4m$elm_syntax$Elm$Syntax$Node$range(signature.name).start,
											start: documentationRange.start
										},
										syntaxComments.comments)),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$linebreak,
									$author$project$Print$exactly(documentation)));
						}
					}())));
	});
var $author$project$ElmSyntaxPrint$listFilledLast = function (_v0) {
	listFilledLast:
	while (true) {
		var head = _v0.a;
		var tail = _v0.b;
		if (!tail.b) {
			return head;
		} else {
			var tailHead = tail.a;
			var tailTail = tail.b;
			var $temp$_v0 = _Utils_Tuple2(tailHead, tailTail);
			_v0 = $temp$_v0;
			continue listFilledLast;
		}
	}
};
var $author$project$ElmSyntaxPrint$declarationTypeAlias = F2(
	function (syntaxComments, syntaxTypeAliasDeclaration) {
		var rangeBetweenParametersAndType = function () {
			var _v4 = syntaxTypeAliasDeclaration.generics;
			if (!_v4.b) {
				return {
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTypeAliasDeclaration.typeAnnotation).start,
					start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTypeAliasDeclaration.name).end
				};
			} else {
				var parameter0 = _v4.a;
				var parameter1Up = _v4.b;
				return {
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTypeAliasDeclaration.typeAnnotation).start,
					start: $stil4m$elm_syntax$Elm$Syntax$Node$range(
						$author$project$ElmSyntaxPrint$listFilledLast(
							_Utils_Tuple2(parameter0, parameter1Up))).end
				};
			}
		}();
		var parameterPrints = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (parameterName, soFar) {
						var parameterPrintedRange = $stil4m$elm_syntax$Elm$Syntax$Node$range(parameterName);
						return {
							end: parameterPrintedRange.end,
							prints: A2(
								$elm$core$List$cons,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly(
										$stil4m$elm_syntax$Elm$Syntax$Node$value(parameterName)),
									function () {
										var _v3 = A2(
											$author$project$ElmSyntaxPrint$commentsInRange,
											{end: parameterPrintedRange.start, start: soFar.end},
											syntaxComments);
										if (!_v3.b) {
											return $author$project$Print$empty;
										} else {
											var comment0 = _v3.a;
											var comment1Up = _v3.b;
											var commentsCollapsible = $author$project$ElmSyntaxPrint$collapsibleComments(
												A2($elm$core$List$cons, comment0, comment1Up));
											return A2(
												$author$project$Print$followedBy,
												$author$project$Print$spaceOrLinebreakIndented(commentsCollapsible.lineSpread),
												commentsCollapsible.print);
										}
									}()),
								soFar.prints)
						};
					}),
				{
					end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTypeAliasDeclaration.name).end,
					prints: _List_Nil
				},
				syntaxTypeAliasDeclaration.generics).prints);
		var parametersLineSpread = A2($author$project$Print$mapAndLineSpreadsCombine, $author$project$Print$lineSpread, parameterPrints);
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$withIndentAtNextMultipleOf4(
				A2(
					$author$project$Print$followedBy,
					A2(
						$author$project$Print$followedBy,
						A2($author$project$ElmSyntaxPrint$typeNotParenthesized, syntaxComments, syntaxTypeAliasDeclaration.typeAnnotation),
						A2(
							$author$project$Print$followedBy,
							function () {
								var _v2 = A2($author$project$ElmSyntaxPrint$commentsInRange, rangeBetweenParametersAndType, syntaxComments);
								if (!_v2.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = _v2.a;
									var comment1Up = _v2.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreakIndented,
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							$author$project$Print$linebreakIndented)),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('='),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									$author$project$Print$sequence(
										A2(
											$elm$core$List$map,
											function (parameterPrint) {
												return A2(
													$author$project$Print$followedBy,
													parameterPrint,
													$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread));
											},
											parameterPrints))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly(
										$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxTypeAliasDeclaration.name)),
									$author$project$Print$spaceOrLinebreakIndented(parametersLineSpread))))))),
			A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly('alias'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$space,
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('type'),
						function () {
							var _v0 = syntaxTypeAliasDeclaration.documentation;
							if (_v0.$ === 'Nothing') {
								return $author$project$Print$empty;
							} else {
								var _v1 = _v0.a;
								var documentationRange = _v1.a;
								var documentation = _v1.b;
								return A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$commentsBetweenDocumentationAndDeclaration(
										A2(
											$author$project$ElmSyntaxPrint$commentsInRange,
											{
												end: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxTypeAliasDeclaration.name).start,
												start: documentationRange.start
											},
											syntaxComments)),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreak,
										$author$project$Print$exactly(documentation)));
							}
						}()))));
	});
var $author$project$ElmSyntaxPrint$declaration = F2(
	function (syntaxComments, syntaxDeclaration) {
		switch (syntaxDeclaration.$) {
			case 'FunctionDeclaration':
				var syntaxExpressionDeclaration = syntaxDeclaration.a;
				return A2($author$project$ElmSyntaxPrint$declarationExpression, syntaxComments.comments, syntaxExpressionDeclaration);
			case 'AliasDeclaration':
				var syntaxTypeAliasDeclaration = syntaxDeclaration.a;
				return A2($author$project$ElmSyntaxPrint$declarationTypeAlias, syntaxComments.comments, syntaxTypeAliasDeclaration);
			case 'CustomTypeDeclaration':
				var syntaxChoiceTypeDeclaration = syntaxDeclaration.a;
				return A2($author$project$ElmSyntaxPrint$declarationChoiceType, syntaxComments.comments, syntaxChoiceTypeDeclaration);
			case 'PortDeclaration':
				var signature = syntaxDeclaration.a;
				return A2(
					$author$project$ElmSyntaxPrint$declarationPort,
					{comments: syntaxComments.comments, documentationComment: syntaxComments.portDocumentationComment},
					signature);
			case 'InfixDeclaration':
				var syntaxInfixDeclaration = syntaxDeclaration.a;
				return $author$project$ElmSyntaxPrint$declarationInfix(syntaxInfixDeclaration);
			default:
				var destructuringPattern = syntaxDeclaration.a;
				var destructuringExpression = syntaxDeclaration.b;
				return A3($author$project$ElmSyntaxPrint$declarationDestructuring, syntaxComments.comments, destructuringPattern, destructuringExpression);
		}
	});
var $author$project$ElmSyntaxPrint$firstCommentInRange = F2(
	function (range, sortedComments) {
		firstCommentInRange:
		while (true) {
			if (!sortedComments.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var _v1 = sortedComments.a;
				var headCommentRange = _v1.a;
				var headComment = _v1.b;
				var tailComments = sortedComments.b;
				var _v2 = A2($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations, headCommentRange.start, range.start);
				switch (_v2.$) {
					case 'LT':
						var $temp$range = range,
							$temp$sortedComments = tailComments;
						range = $temp$range;
						sortedComments = $temp$sortedComments;
						continue firstCommentInRange;
					case 'EQ':
						return $elm$core$Maybe$Just(
							A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, headCommentRange, headComment));
					default:
						var _v3 = A2($stil4m$elm_syntax$Elm$Syntax$Range$compareLocations, headCommentRange.end, range.end);
						switch (_v3.$) {
							case 'GT':
								return $elm$core$Maybe$Nothing;
							case 'LT':
								return $elm$core$Maybe$Just(
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, headCommentRange, headComment));
							default:
								return $elm$core$Maybe$Just(
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, headCommentRange, headComment));
						}
				}
			}
		}
	});
var $author$project$ElmSyntaxPrint$linebreaksFollowedByDeclaration = F2(
	function (syntaxComments, syntaxDeclaration) {
		switch (syntaxDeclaration.$) {
			case 'FunctionDeclaration':
				var syntaxExpressionDeclaration = syntaxDeclaration.a;
				return A2(
					$author$project$Print$followedBy,
					A2($author$project$ElmSyntaxPrint$declarationExpression, syntaxComments.comments, syntaxExpressionDeclaration),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
			case 'AliasDeclaration':
				var syntaxTypeAliasDeclaration = syntaxDeclaration.a;
				return A2(
					$author$project$Print$followedBy,
					A2($author$project$ElmSyntaxPrint$declarationTypeAlias, syntaxComments.comments, syntaxTypeAliasDeclaration),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
			case 'CustomTypeDeclaration':
				var syntaxChoiceTypeDeclaration = syntaxDeclaration.a;
				return A2(
					$author$project$Print$followedBy,
					A2($author$project$ElmSyntaxPrint$declarationChoiceType, syntaxComments.comments, syntaxChoiceTypeDeclaration),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
			case 'PortDeclaration':
				var signature = syntaxDeclaration.a;
				return A2(
					$author$project$Print$followedBy,
					A2(
						$author$project$ElmSyntaxPrint$declarationPort,
						{comments: syntaxComments.comments, documentationComment: syntaxComments.portDocumentationComment},
						signature),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
			case 'InfixDeclaration':
				var syntaxInfixDeclaration = syntaxDeclaration.a;
				return A2(
					$author$project$Print$followedBy,
					$author$project$ElmSyntaxPrint$declarationInfix(syntaxInfixDeclaration),
					$author$project$Print$linebreak);
			default:
				var destructuringPattern = syntaxDeclaration.a;
				var destructuringExpression = syntaxDeclaration.b;
				return A2(
					$author$project$Print$followedBy,
					A3($author$project$ElmSyntaxPrint$declarationDestructuring, syntaxComments.comments, destructuringPattern, destructuringExpression),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
		}
	});
var $author$project$ElmSyntaxPrint$moduleLevelCommentsBeforeDeclaration = function (syntaxComments) {
	return A2(
		$author$project$Print$followedBy,
		function () {
			var _v0 = $author$project$ElmSyntaxPrint$listFilledLast(
				_Utils_Tuple2(syntaxComments.comment0, syntaxComments.comment1Up));
			if (_v0 === '{--}') {
				return $author$project$Print$empty;
			} else {
				return A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak);
			}
		}(),
		A2(
			$author$project$Print$followedBy,
			$author$project$ElmSyntaxPrint$moduleLevelComments(
				A2($elm$core$List$cons, syntaxComments.comment0, syntaxComments.comment1Up)),
			$author$project$Print$linebreak));
};
var $author$project$ElmSyntaxPrint$declarations = F2(
	function (context, syntaxDeclarations) {
		if (!syntaxDeclarations.b) {
			return $author$project$Print$empty;
		} else {
			var _v1 = syntaxDeclarations.a;
			var declaration0Range = _v1.a;
			var declaration0 = _v1.b;
			var declarations1Up = syntaxDeclarations.b;
			return A2(
				$author$project$Print$followedBy,
				A3(
					$elm$core$List$foldl,
					F2(
						function (_v3, soFar) {
							var declarationRange = _v3.a;
							var syntaxDeclaration = _v3.b;
							var maybeDeclarationPortDocumentationComment = function () {
								switch (syntaxDeclaration.$) {
									case 'PortDeclaration':
										return A2(
											$author$project$ElmSyntaxPrint$firstCommentInRange,
											{end: declarationRange.start, start: soFar.previousRange.end},
											context.portDocumentationComments);
									case 'FunctionDeclaration':
										return $elm$core$Maybe$Nothing;
									case 'AliasDeclaration':
										return $elm$core$Maybe$Nothing;
									case 'CustomTypeDeclaration':
										return $elm$core$Maybe$Nothing;
									case 'InfixDeclaration':
										return $elm$core$Maybe$Nothing;
									default:
										return $elm$core$Maybe$Nothing;
								}
							}();
							return {
								previousRange: declarationRange,
								print: A2(
									$author$project$Print$followedBy,
									function () {
										var _v4 = A2(
											$author$project$ElmSyntaxPrint$commentsInRange,
											{end: declarationRange.start, start: soFar.previousRange.end},
											context.comments);
										if (_v4.b) {
											var comment0 = _v4.a;
											var comment1Up = _v4.b;
											return A2(
												$author$project$Print$followedBy,
												A2(
													$author$project$ElmSyntaxPrint$declaration,
													{comments: context.comments, portDocumentationComment: maybeDeclarationPortDocumentationComment},
													syntaxDeclaration),
												A2(
													$author$project$Print$followedBy,
													$author$project$ElmSyntaxPrint$moduleLevelCommentsBeforeDeclaration(
														{comment0: comment0, comment1Up: comment1Up}),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$linebreak,
														A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak))));
										} else {
											return A2(
												$author$project$ElmSyntaxPrint$linebreaksFollowedByDeclaration,
												{comments: context.comments, portDocumentationComment: maybeDeclarationPortDocumentationComment},
												syntaxDeclaration);
										}
									}(),
									soFar.print)
							};
						}),
					{previousRange: declaration0Range, print: $author$project$Print$empty},
					declarations1Up).print,
				A2(
					$author$project$ElmSyntaxPrint$declaration,
					{
						comments: context.comments,
						portDocumentationComment: function () {
							switch (declaration0.$) {
								case 'PortDeclaration':
									return A2(
										$author$project$ElmSyntaxPrint$firstCommentInRange,
										{end: declaration0Range.start, start: context.previousEnd},
										context.portDocumentationComments);
								case 'FunctionDeclaration':
									return $elm$core$Maybe$Nothing;
								case 'AliasDeclaration':
									return $elm$core$Maybe$Nothing;
								case 'CustomTypeDeclaration':
									return $elm$core$Maybe$Nothing;
								case 'InfixDeclaration':
									return $elm$core$Maybe$Nothing;
								default:
									return $elm$core$Maybe$Nothing;
							}
						}()
					},
					declaration0));
		}
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$ElmSyntaxPrint$expose = function (syntaxExpose) {
	switch (syntaxExpose.$) {
		case 'InfixExpose':
			var operatorSymbol = syntaxExpose.a;
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(')'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly(operatorSymbol),
					$author$project$Print$exactly('(')));
		case 'FunctionExpose':
			var name = syntaxExpose.a;
			return $author$project$Print$exactly(name);
		case 'TypeOrAliasExpose':
			var name = syntaxExpose.a;
			return $author$project$Print$exactly(name);
		default:
			var syntaxExposeType = syntaxExpose.a;
			var _v1 = syntaxExposeType.open;
			if (_v1.$ === 'Nothing') {
				return $author$project$Print$exactly(syntaxExposeType.name);
			} else {
				return A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('(..)'),
					$author$project$Print$exactly(syntaxExposeType.name));
			}
	}
};
var $author$project$ElmSyntaxPrint$exposingMulti = F2(
	function (syntaxComments, syntaxExposing) {
		var containedComments = A2($author$project$ElmSyntaxPrint$commentsInRange, syntaxExposing.fullRange, syntaxComments);
		var lineSpread = function () {
			if (containedComments.b) {
				return $author$project$Print$MultipleLines;
			} else {
				return $author$project$ElmSyntaxPrint$lineSpreadInRange(syntaxExposing.fullRange);
			}
		}();
		return A2(
			$author$project$Print$followedBy,
			$author$project$Print$exactly(')'),
			A2(
				$author$project$Print$followedBy,
				function () {
					if (!containedComments.b) {
						return $author$project$Print$empty;
					} else {
						var comment0 = containedComments.a;
						var comment1Up = containedComments.b;
						return A2(
							$author$project$Print$followedBy,
							$author$project$Print$emptyOrLinebreakIndented(lineSpread),
							$author$project$ElmSyntaxPrint$comments(
								A2($elm$core$List$cons, comment0, comment1Up)));
					}
				}(),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$emptyOrLinebreakIndented(lineSpread),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$sequence(
							A2(
								$elm$core$List$intersperse,
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly(','),
										$author$project$Print$emptyOrLinebreakIndented(lineSpread))),
								A2(
									$elm$core$List$map,
									function (_v1) {
										var syntaxExpose = _v1.b;
										return $author$project$ElmSyntaxPrint$expose(syntaxExpose);
									},
									A2($elm$core$List$cons, syntaxExposing.expose0, syntaxExposing.expose1Up)))),
						A2(
							$author$project$Print$followedBy,
							function () {
								if (lineSpread.$ === 'SingleLine') {
									return $author$project$Print$empty;
								} else {
									return $author$project$Print$space;
								}
							}(),
							$author$project$Print$exactly('('))))));
	});
var $author$project$ElmSyntaxPrint$importExposing = F2(
	function (syntaxComments, _v0) {
		var exposingRange = _v0.a;
		var syntaxExposing = _v0.b;
		if (syntaxExposing.$ === 'All') {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(')'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('..'),
					$author$project$Print$exactly('(')));
		} else {
			var exposingSet = syntaxExposing.a;
			if (!exposingSet.b) {
				return $author$project$Print$exactly('()');
			} else {
				var expose0 = exposingSet.a;
				var expose1Up = exposingSet.b;
				return A2(
					$author$project$ElmSyntaxPrint$exposingMulti,
					syntaxComments,
					{expose0: expose0, expose1Up: expose1Up, fullRange: exposingRange});
			}
		}
	});
var $author$project$ElmSyntaxPrint$moduleName = function (syntaxModuleName) {
	return $author$project$Print$exactly(
		A2($elm$core$String$join, '.', syntaxModuleName));
};
var $author$project$ElmSyntaxPrint$import_ = F2(
	function (syntaxComments, _v0) {
		var incorrectImportRange = _v0.a;
		var syntaxImport = _v0.b;
		var importRange = function () {
			var _v12 = syntaxImport.exposingList;
			if (_v12.$ === 'Nothing') {
				return incorrectImportRange;
			} else {
				var _v13 = _v12.a;
				var syntaxExposingRange = _v13.a;
				return {end: syntaxExposingRange.end, start: incorrectImportRange.start};
			}
		}();
		var _v1 = syntaxImport.moduleName;
		var moduleNameRange = _v1.a;
		var syntaxModuleName = _v1.b;
		return A2(
			$author$project$Print$followedBy,
			function () {
				var _v8 = syntaxImport.exposingList;
				if (_v8.$ === 'Nothing') {
					return $author$project$Print$empty;
				} else {
					var syntaxExposing = _v8.a;
					var exposingPrint = A2($author$project$ElmSyntaxPrint$importExposing, syntaxComments, syntaxExposing);
					var exposingPartStart = function () {
						var _v10 = syntaxImport.moduleAlias;
						if (_v10.$ === 'Nothing') {
							return moduleNameRange.end;
						} else {
							var _v11 = _v10.a;
							var moduleAliasRange = _v11.a;
							return moduleAliasRange.end;
						}
					}();
					var _v9 = A2(
						$author$project$ElmSyntaxPrint$commentsInRange,
						{end: importRange.end, start: exposingPartStart},
						syntaxComments);
					if (!_v9.b) {
						var lineSpread = $author$project$Print$lineSpread(exposingPrint);
						return $author$project$Print$withIndentAtNextMultipleOf4(
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										exposingPrint,
										$author$project$Print$spaceOrLinebreakIndented(lineSpread))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly('exposing'),
									$author$project$Print$spaceOrLinebreakIndented(lineSpread))));
					} else {
						var exposingComment0 = _v9.a;
						var exposingComment1Up = _v9.b;
						return $author$project$Print$withIndentAtNextMultipleOf4(
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$withIndentAtNextMultipleOf4(
									A2(
										$author$project$Print$followedBy,
										exposingPrint,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$linebreakIndented,
											A2(
												$author$project$Print$followedBy,
												$author$project$ElmSyntaxPrint$comments(
													A2($elm$core$List$cons, exposingComment0, exposingComment1Up)),
												$author$project$Print$linebreakIndented)))),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$exactly('exposing'),
									$author$project$Print$linebreakIndented)));
					}
				}
			}(),
			A2(
				$author$project$Print$followedBy,
				function () {
					var _v2 = syntaxImport.moduleAlias;
					if (_v2.$ === 'Nothing') {
						var _v3 = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: moduleNameRange.start, start: importRange.start},
							syntaxComments);
						if (!_v3.b) {
							return A2(
								$author$project$Print$followedBy,
								$author$project$ElmSyntaxPrint$moduleName(syntaxModuleName),
								$author$project$Print$space);
						} else {
							var comment0 = _v3.a;
							var comment1Up = _v3.b;
							return $author$project$Print$withIndentAtNextMultipleOf4(
								A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$moduleName(syntaxModuleName),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreakIndented,
										A2(
											$author$project$Print$followedBy,
											$author$project$ElmSyntaxPrint$comments(
												A2($elm$core$List$cons, comment0, comment1Up)),
											$author$project$Print$linebreakIndented))));
						}
					} else {
						var _v4 = _v2.a;
						var moduleAliasRange = _v4.a;
						var moduleAlias = _v4.b;
						var _v5 = A2(
							$author$project$ElmSyntaxPrint$commentsInRange,
							{end: moduleAliasRange.start, start: moduleNameRange.end},
							syntaxComments);
						if (!_v5.b) {
							return A2(
								$author$project$Print$followedBy,
								$author$project$ElmSyntaxPrint$moduleName(moduleAlias),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly('as'),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											function () {
												var _v6 = A2(
													$author$project$ElmSyntaxPrint$commentsInRange,
													{end: moduleNameRange.start, start: importRange.start},
													syntaxComments);
												if (!_v6.b) {
													return A2(
														$author$project$Print$followedBy,
														$author$project$ElmSyntaxPrint$moduleName(syntaxModuleName),
														$author$project$Print$space);
												} else {
													var moduleNameComment0 = _v6.a;
													var moduleNameComment1Up = _v6.b;
													return $author$project$Print$withIndentAtNextMultipleOf4(
														A2(
															$author$project$Print$followedBy,
															$author$project$ElmSyntaxPrint$moduleName(syntaxModuleName),
															A2(
																$author$project$Print$followedBy,
																$author$project$Print$linebreakIndented,
																A2(
																	$author$project$Print$followedBy,
																	$author$project$ElmSyntaxPrint$comments(
																		A2($elm$core$List$cons, moduleNameComment0, moduleNameComment1Up)),
																	$author$project$Print$linebreakIndented))));
												}
											}()))));
						} else {
							var aliasComment0 = _v5.a;
							var aliasComment1Up = _v5.b;
							return $author$project$Print$withIndentAtNextMultipleOf4(
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$withIndentAtNextMultipleOf4(
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$withIndentAtNextMultipleOf4(
												A2(
													$author$project$Print$followedBy,
													$author$project$ElmSyntaxPrint$moduleName(moduleAlias),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$linebreakIndented,
														A2(
															$author$project$Print$followedBy,
															$author$project$ElmSyntaxPrint$comments(
																A2($elm$core$List$cons, aliasComment0, aliasComment1Up)),
															$author$project$Print$linebreakIndented)))),
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$exactly('as'),
												$author$project$Print$linebreakIndented))),
									A2(
										$author$project$Print$followedBy,
										function () {
											var _v7 = A2(
												$author$project$ElmSyntaxPrint$commentsInRange,
												{end: moduleNameRange.start, start: importRange.start},
												syntaxComments);
											if (!_v7.b) {
												return $author$project$ElmSyntaxPrint$moduleName(syntaxModuleName);
											} else {
												var moduleNameComment0 = _v7.a;
												var moduleNameComment1Up = _v7.b;
												return A2(
													$author$project$Print$followedBy,
													$author$project$ElmSyntaxPrint$moduleName(syntaxModuleName),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$linebreakIndented,
														$author$project$ElmSyntaxPrint$comments(
															A2($elm$core$List$cons, moduleNameComment0, moduleNameComment1Up))));
											}
										}(),
										$author$project$Print$linebreakIndented)));
						}
					}
				}(),
				$author$project$Print$exactly('import')));
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$All = function (a) {
	return {$: 'All', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit = function (a) {
	return {$: 'Explicit', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Range$empty = {
	end: {column: 0, row: 0},
	start: {column: 0, row: 0}
};
var $stil4m$elm_syntax$Elm$Syntax$Node$empty = function (a) {
	return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$empty, a);
};
var $author$project$ElmSyntaxPrint$exposeCompare = F2(
	function (a, b) {
		switch (a.$) {
			case 'InfixExpose':
				var aOperatorSymbol = a.a;
				switch (b.$) {
					case 'InfixExpose':
						var bOperatorSymbol = b.a;
						return A2($elm$core$Basics$compare, aOperatorSymbol, bOperatorSymbol);
					case 'FunctionExpose':
						return $elm$core$Basics$LT;
					case 'TypeOrAliasExpose':
						return $elm$core$Basics$LT;
					default:
						return $elm$core$Basics$LT;
				}
			case 'FunctionExpose':
				var aName = a.a;
				switch (b.$) {
					case 'InfixExpose':
						return $elm$core$Basics$GT;
					case 'FunctionExpose':
						var bName = b.a;
						return A2($elm$core$Basics$compare, aName, bName);
					case 'TypeOrAliasExpose':
						return $elm$core$Basics$GT;
					default:
						return $elm$core$Basics$GT;
				}
			case 'TypeOrAliasExpose':
				var aName = a.a;
				switch (b.$) {
					case 'InfixExpose':
						return $elm$core$Basics$GT;
					case 'FunctionExpose':
						return $elm$core$Basics$LT;
					case 'TypeOrAliasExpose':
						var bName = b.a;
						return A2($elm$core$Basics$compare, aName, bName);
					default:
						var bTypeExpose = b.a;
						return A2($elm$core$Basics$compare, aName, bTypeExpose.name);
				}
			default:
				var aTypeExpose = a.a;
				switch (b.$) {
					case 'InfixExpose':
						return $elm$core$Basics$GT;
					case 'FunctionExpose':
						return $elm$core$Basics$LT;
					case 'TypeOrAliasExpose':
						var bName = b.a;
						return A2($elm$core$Basics$compare, aTypeExpose.name, bName);
					default:
						var bTypeExpose = b.a;
						return A2($elm$core$Basics$compare, aTypeExpose.name, bTypeExpose.name);
				}
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose = function (a) {
	return {$: 'TypeExpose', a: a};
};
var $author$project$ElmSyntaxPrint$exposeMerge = F2(
	function (a, b) {
		switch (a.$) {
			case 'TypeExpose':
				var aTypeExpose = a.a;
				switch (b.$) {
					case 'TypeExpose':
						var bTypeExpose = b.a;
						return $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
							{
								name: aTypeExpose.name,
								open: function () {
									var _v2 = aTypeExpose.open;
									if (_v2.$ === 'Just') {
										var openRange = _v2.a;
										return $elm$core$Maybe$Just(openRange);
									} else {
										return bTypeExpose.open;
									}
								}()
							});
					case 'InfixExpose':
						return $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(aTypeExpose);
					case 'FunctionExpose':
						return $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(aTypeExpose);
					default:
						return $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(aTypeExpose);
				}
			case 'InfixExpose':
				return b;
			case 'FunctionExpose':
				return b;
			default:
				return b;
		}
	});
var $author$project$ElmSyntaxPrint$exposesCombine = function (syntaxExposes) {
	exposesCombine:
	while (true) {
		if (!syntaxExposes.b) {
			return _List_Nil;
		} else {
			if (!syntaxExposes.b.b) {
				var onlyExpose = syntaxExposes.a;
				return _List_fromArray(
					[onlyExpose]);
			} else {
				var expose0 = syntaxExposes.a;
				var _v1 = syntaxExposes.b;
				var expose1 = _v1.a;
				var expose2Up = _v1.b;
				var _v2 = A2($author$project$ElmSyntaxPrint$exposeCompare, expose0, expose1);
				switch (_v2.$) {
					case 'EQ':
						var $temp$syntaxExposes = A2(
							$elm$core$List$cons,
							A2($author$project$ElmSyntaxPrint$exposeMerge, expose0, expose1),
							expose2Up);
						syntaxExposes = $temp$syntaxExposes;
						continue exposesCombine;
					case 'LT':
						return A2(
							$elm$core$List$cons,
							expose0,
							$author$project$ElmSyntaxPrint$exposesCombine(
								A2($elm$core$List$cons, expose1, expose2Up)));
					default:
						return A2(
							$elm$core$List$cons,
							expose0,
							$author$project$ElmSyntaxPrint$exposesCombine(
								A2($elm$core$List$cons, expose1, expose2Up)));
				}
			}
		}
	}
};
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$ElmSyntaxPrint$exposeListToNormal = function (syntaxExposeList) {
	return A2(
		$elm$core$List$map,
		$stil4m$elm_syntax$Elm$Syntax$Node$empty,
		$author$project$ElmSyntaxPrint$exposesCombine(
			A2(
				$elm$core$List$sortWith,
				$author$project$ElmSyntaxPrint$exposeCompare,
				A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$value, syntaxExposeList))));
};
var $author$project$ElmSyntaxPrint$exposingToNormal = function (syntaxExposing) {
	if (syntaxExposing.$ === 'All') {
		var allRange = syntaxExposing.a;
		return $stil4m$elm_syntax$Elm$Syntax$Exposing$All(allRange);
	} else {
		var exposeSet = syntaxExposing.a;
		return $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
			$author$project$ElmSyntaxPrint$exposeListToNormal(exposeSet));
	}
};
var $author$project$ElmSyntaxPrint$importToNormal = function (syntaxImport) {
	return {
		exposingList: function () {
			var _v0 = syntaxImport.exposingList;
			if (_v0.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var _v1 = _v0.a;
				var exposingRange = _v1.a;
				var syntaxExposing = _v1.b;
				return $elm$core$Maybe$Just(
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						exposingRange,
						$author$project$ElmSyntaxPrint$exposingToNormal(syntaxExposing)));
			}
		}(),
		moduleAlias: syntaxImport.moduleAlias,
		moduleName: syntaxImport.moduleName
	};
};
var $author$project$ElmSyntaxPrint$exposingCombine = F2(
	function (a, b) {
		if (a.$ === 'Just') {
			if (a.a.b.$ === 'All') {
				var _v1 = a.a;
				var exposingAllRange = _v1.a;
				var allRange = _v1.b.a;
				return $elm$core$Maybe$Just(
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						exposingAllRange,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$All(allRange)));
			} else {
				var _v2 = a.a;
				var earlierExposingExplicitRange = _v2.a;
				var earlierExposeSet = _v2.b.a;
				return $elm$core$Maybe$Just(
					function () {
						if (b.$ === 'Just') {
							if (b.a.b.$ === 'All') {
								var _v4 = b.a;
								var exposingAllRange = _v4.a;
								var allRange = _v4.b.a;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									exposingAllRange,
									$stil4m$elm_syntax$Elm$Syntax$Exposing$All(allRange));
							} else {
								var _v5 = b.a;
								var laterExposingExplicitRange = _v5.a;
								var laterExposeSet = _v5.b.a;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									function () {
										var _v6 = $author$project$ElmSyntaxPrint$lineSpreadInRange(earlierExposingExplicitRange);
										if (_v6.$ === 'MultipleLines') {
											return earlierExposingExplicitRange;
										} else {
											return laterExposingExplicitRange;
										}
									}(),
									$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
										$author$project$ElmSyntaxPrint$exposeListToNormal(
											_Utils_ap(earlierExposeSet, laterExposeSet))));
							}
						} else {
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								earlierExposingExplicitRange,
								$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(earlierExposeSet));
						}
					}());
			}
		} else {
			return b;
		}
	});
var $author$project$ElmSyntaxPrint$importsMerge = F2(
	function (earlier, later) {
		return {
			exposingList: A2($author$project$ElmSyntaxPrint$exposingCombine, earlier.exposingList, later.exposingList),
			moduleAlias: function () {
				var _v0 = earlier.moduleAlias;
				if (_v0.$ === 'Just') {
					var alias = _v0.a;
					return $elm$core$Maybe$Just(alias);
				} else {
					return later.moduleAlias;
				}
			}(),
			moduleName: later.moduleName
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$map = F2(
	function (f, _v0) {
		var r = _v0.a;
		var a = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			r,
			f(a));
	});
var $author$project$ElmSyntaxPrint$importsCombine = function (syntaxImports) {
	importsCombine:
	while (true) {
		if (!syntaxImports.b) {
			return _List_Nil;
		} else {
			if (!syntaxImports.b.b) {
				var onlyImport = syntaxImports.a;
				return _List_fromArray(
					[
						A2($stil4m$elm_syntax$Elm$Syntax$Node$map, $author$project$ElmSyntaxPrint$importToNormal, onlyImport)
					]);
			} else {
				var _v1 = syntaxImports.a;
				var import0Range = _v1.a;
				var import0 = _v1.b;
				var _v2 = syntaxImports.b;
				var _v3 = _v2.a;
				var import1Range = _v3.a;
				var import1 = _v3.b;
				var import2Up = _v2.b;
				if (_Utils_eq(
					$stil4m$elm_syntax$Elm$Syntax$Node$value(import0.moduleName),
					$stil4m$elm_syntax$Elm$Syntax$Node$value(import1.moduleName))) {
					var $temp$syntaxImports = A2(
						$elm$core$List$cons,
						A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							import1Range,
							A2($author$project$ElmSyntaxPrint$importsMerge, import0, import1)),
						import2Up);
					syntaxImports = $temp$syntaxImports;
					continue importsCombine;
				} else {
					return A2(
						$elm$core$List$cons,
						A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							import0Range,
							$author$project$ElmSyntaxPrint$importToNormal(import0)),
						$author$project$ElmSyntaxPrint$importsCombine(
							A2(
								$elm$core$List$cons,
								A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, import1Range, import1),
								import2Up)));
				}
			}
		}
	}
};
var $author$project$ElmSyntaxPrint$imports = F2(
	function (syntaxComments, syntaxImports) {
		if (!syntaxImports.b) {
			return $author$project$Print$empty;
		} else {
			var _v1 = syntaxImports.a;
			var import0Range = _v1.a;
			var import0 = _v1.b;
			var imports1Up = syntaxImports.b;
			var commentsBetweenImports = A3(
				$elm$core$List$foldl,
				F2(
					function (_v5, soFar) {
						var importRange = _v5.a;
						return {
							comments: _Utils_ap(
								soFar.comments,
								A2(
									$author$project$ElmSyntaxPrint$commentsInRange,
									{end: importRange.start, start: soFar.previousImportRange.end},
									syntaxComments)),
							previousImportRange: importRange
						};
					}),
				{comments: _List_Nil, previousImportRange: import0Range},
				A2(
					$elm$core$List$cons,
					A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, import0Range, import0),
					imports1Up)).comments;
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$sequence(
					A2(
						$elm$core$List$intersperse,
						$author$project$Print$linebreak,
						A2(
							$elm$core$List$map,
							function (syntaxImport) {
								return A2($author$project$ElmSyntaxPrint$import_, syntaxComments, syntaxImport);
							},
							$author$project$ElmSyntaxPrint$importsCombine(
								A2(
									$elm$core$List$sortWith,
									F2(
										function (_v3, _v4) {
											var a = _v3.b;
											var b = _v4.b;
											return A2(
												$elm$core$Basics$compare,
												$stil4m$elm_syntax$Elm$Syntax$Node$value(a.moduleName),
												$stil4m$elm_syntax$Elm$Syntax$Node$value(b.moduleName));
										}),
									A2(
										$elm$core$List$cons,
										A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, import0Range, import0),
										imports1Up)))))),
				function () {
					if (!commentsBetweenImports.b) {
						return $author$project$Print$empty;
					} else {
						var comment0 = commentsBetweenImports.a;
						var comment1Up = commentsBetweenImports.b;
						return A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreak,
							$author$project$ElmSyntaxPrint$moduleLevelComments(
								A2($elm$core$List$cons, comment0, comment1Up)));
					}
				}());
		}
	});
var $author$project$ElmSyntaxPrint$moduleDocumentationBeforeCutOffLine = F2(
	function (cutOffLine, allComments) {
		moduleDocumentationBeforeCutOffLine:
		while (true) {
			if (!allComments.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var headComment = allComments.a;
				var restOfComments = allComments.b;
				var _v1 = headComment;
				var range = _v1.a;
				var content = _v1.b;
				if (_Utils_cmp(range.start.row, cutOffLine) > 0) {
					return $elm$core$Maybe$Nothing;
				} else {
					if (A2($elm$core$String$startsWith, '{-|', content)) {
						return $elm$core$Maybe$Just(headComment);
					} else {
						var $temp$cutOffLine = cutOffLine,
							$temp$allComments = restOfComments;
						cutOffLine = $temp$cutOffLine;
						allComments = $temp$allComments;
						continue moduleDocumentationBeforeCutOffLine;
					}
				}
			}
		}
	});
var $author$project$ElmSyntaxPrint$moduleDocumentation = function (ast) {
	var cutOffLine = function () {
		var _v0 = ast.imports;
		if (_v0.b) {
			var _v1 = _v0.a;
			var firstImportRange = _v1.a;
			return firstImportRange.start.row;
		} else {
			var _v2 = ast.declarations;
			if (_v2.b) {
				var _v3 = _v2.a;
				var firstDeclarationRange = _v3.a;
				return firstDeclarationRange.start.row;
			} else {
				return 0;
			}
		}
	}();
	return A2($author$project$ElmSyntaxPrint$moduleDocumentationBeforeCutOffLine, cutOffLine, ast.comments);
};
var $author$project$ElmSyntaxPrint$moduleDocumentationParse = function (moduleDocumentationContent) {
	var parsed = A3(
		$elm$core$List$foldl,
		F2(
			function (line, soFar) {
				return A2($elm$core$String$startsWith, '@docs ', line) ? {
					finishedBlocks: A2(
						$elm$core$List$cons,
						{
							atDocsLine: A2(
								$elm$core$List$map,
								$elm$core$String$trim,
								A2(
									$elm$core$String$split,
									',',
									A3(
										$elm$core$String$slice,
										6,
										$elm$core$String$length(line),
										line))),
							rawBefore: soFar.rawSinceAtDocs
						},
						soFar.finishedBlocks),
					rawSinceAtDocs: ''
				} : {finishedBlocks: soFar.finishedBlocks, rawSinceAtDocs: soFar.rawSinceAtDocs + '\n'};
			}),
		{finishedBlocks: _List_Nil, rawSinceAtDocs: ''},
		$elm$core$String$lines(moduleDocumentationContent));
	return {
		rawAfterAtDocsLines: parsed.rawSinceAtDocs,
		whileAtDocsLines: $elm$core$List$reverse(parsed.finishedBlocks)
	};
};
var $author$project$ElmSyntaxPrint$exposeToAtDocsString = function (syntaxExpose) {
	switch (syntaxExpose.$) {
		case 'InfixExpose':
			var operatorSymbol = syntaxExpose.a;
			return '(' + (operatorSymbol + ')');
		case 'FunctionExpose':
			var name = syntaxExpose.a;
			return name;
		case 'TypeOrAliasExpose':
			var name = syntaxExpose.a;
			return name;
		default:
			var choiceTypeExpose = syntaxExpose.a;
			return choiceTypeExpose.name;
	}
};
var $author$project$ElmSyntaxPrint$listFirstJustMap = F2(
	function (elementToMaybe, list) {
		listFirstJustMap:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var head = list.a;
				var tail = list.b;
				var _v1 = elementToMaybe(head);
				if (_v1.$ === 'Nothing') {
					var $temp$elementToMaybe = elementToMaybe,
						$temp$list = tail;
					elementToMaybe = $temp$elementToMaybe;
					list = $temp$list;
					continue listFirstJustMap;
				} else {
					var b = _v1.a;
					return $elm$core$Maybe$Just(b);
				}
			}
		}
	});
var $author$project$ElmSyntaxPrint$atDocsLineToExposesAndRemaining = F2(
	function (atDocsLine, remainingExposes) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (exposeAsAtDocsString, soFar) {
					var toExposeReferencedByAtDocsString = function (ex) {
						return _Utils_eq(
							$author$project$ElmSyntaxPrint$exposeToAtDocsString(ex),
							exposeAsAtDocsString) ? $elm$core$Maybe$Just(ex) : $elm$core$Maybe$Nothing;
					};
					var _v0 = A2($author$project$ElmSyntaxPrint$listFirstJustMap, toExposeReferencedByAtDocsString, soFar.remainingExposes);
					if (_v0.$ === 'Nothing') {
						return soFar;
					} else {
						var exposeReferencedByAtDocsString = _v0.a;
						return {
							exposes: A2($elm$core$List$cons, exposeReferencedByAtDocsString, soFar.exposes),
							remainingExposes: A2(
								$elm$core$List$filter,
								function (ex) {
									return !_Utils_eq(ex, exposeReferencedByAtDocsString);
								},
								soFar.remainingExposes)
						};
					}
				}),
			{exposes: _List_Nil, remainingExposes: remainingExposes},
			atDocsLine);
	});
var $author$project$ElmSyntaxPrint$moduleExposing = F2(
	function (context, _v0) {
		var exposingRange = _v0.a;
		var syntaxExposing = _v0.b;
		if (syntaxExposing.$ === 'All') {
			return A2(
				$author$project$Print$followedBy,
				$author$project$Print$exactly(')'),
				A2(
					$author$project$Print$followedBy,
					$author$project$Print$exactly('..'),
					$author$project$Print$exactly('(')));
		} else {
			var exposingSet = syntaxExposing.a;
			var _v2 = $author$project$ElmSyntaxPrint$exposeListToNormal(exposingSet);
			if (!_v2.b) {
				return $author$project$Print$exactly('()');
			} else {
				if (!_v2.b.b) {
					var _v3 = _v2.a;
					var onlySyntaxExpose = _v3.b;
					var containedComments = A2($author$project$ElmSyntaxPrint$commentsInRange, exposingRange, context.comments);
					var lineSpread = function () {
						if (containedComments.b) {
							return $author$project$Print$MultipleLines;
						} else {
							return $author$project$Print$SingleLine;
						}
					}();
					return A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly(')'),
						A2(
							$author$project$Print$followedBy,
							function () {
								if (!containedComments.b) {
									return $author$project$Print$empty;
								} else {
									var comment0 = containedComments.a;
									var comment1Up = containedComments.b;
									return A2(
										$author$project$Print$followedBy,
										$author$project$Print$emptyOrLinebreakIndented(lineSpread),
										$author$project$ElmSyntaxPrint$comments(
											A2($elm$core$List$cons, comment0, comment1Up)));
								}
							}(),
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$emptyOrLinebreakIndented(lineSpread),
								A2(
									$author$project$Print$followedBy,
									$author$project$ElmSyntaxPrint$expose(onlySyntaxExpose),
									A2(
										$author$project$Print$followedBy,
										function () {
											if (lineSpread.$ === 'SingleLine') {
												return $author$project$Print$empty;
											} else {
												return $author$project$Print$space;
											}
										}(),
										$author$project$Print$exactly('('))))));
				} else {
					var expose0 = _v2.a;
					var _v7 = _v2.b;
					var expose1 = _v7.a;
					var expose2Up = _v7.b;
					var _v8 = context.atDocsLines;
					if (_v8.b) {
						var atDocsLine0 = _v8.a;
						var atDocsLine1Up = _v8.b;
						var atDocsExposeLines = A3(
							$elm$core$List$foldr,
							F2(
								function (atDocsLine, soFar) {
									var atDocsExposeLine = A2($author$project$ElmSyntaxPrint$atDocsLineToExposesAndRemaining, atDocsLine, soFar.remainingExposes);
									return {
										atDocsExposeLines: A2($elm$core$List$cons, atDocsExposeLine.exposes, soFar.atDocsExposeLines),
										remainingExposes: atDocsExposeLine.remainingExposes
									};
								}),
							{
								atDocsExposeLines: _List_Nil,
								remainingExposes: A2(
									$elm$core$List$map,
									$stil4m$elm_syntax$Elm$Syntax$Node$value,
									$author$project$ElmSyntaxPrint$exposeListToNormal(
										A2(
											$elm$core$List$cons,
											expose0,
											A2($elm$core$List$cons, expose1, expose2Up))))
							},
							A2($elm$core$List$cons, atDocsLine0, atDocsLine1Up));
						var _v9 = A2(
							$elm$core$List$filter,
							function (line) {
								if (!line.b) {
									return false;
								} else {
									return true;
								}
							},
							atDocsExposeLines.atDocsExposeLines);
						if (!_v9.b) {
							return A2(
								$author$project$ElmSyntaxPrint$exposingMulti,
								context.comments,
								{
									expose0: expose0,
									expose1Up: A2($elm$core$List$cons, expose1, expose2Up),
									fullRange: exposingRange
								});
						} else {
							var atDocsExposeLine0 = _v9.a;
							var atDocsExposeLine1Up = _v9.b;
							return A2(
								$author$project$Print$followedBy,
								$author$project$Print$exactly(')'),
								A2(
									$author$project$Print$followedBy,
									function () {
										var _v11 = atDocsExposeLines.remainingExposes;
										if (!_v11.b) {
											return $author$project$Print$empty;
										} else {
											var remainingExpose0 = _v11.a;
											var remainingExpose1Up = _v11.b;
											return A2(
												$author$project$Print$followedBy,
												$author$project$Print$linebreakIndented,
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$sequence(
														A2(
															$elm$core$List$intersperse,
															A2(
																$author$project$Print$followedBy,
																$author$project$Print$space,
																$author$project$Print$exactly(',')),
															A2(
																$elm$core$List$map,
																$author$project$ElmSyntaxPrint$expose,
																A2($elm$core$List$cons, remainingExpose0, remainingExpose1Up)))),
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$space,
														$author$project$Print$exactly(','))));
										}
									}(),
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$linebreakIndented,
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$sequence(
												A2(
													$elm$core$List$intersperse,
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$space,
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$exactly(','),
															$author$project$Print$linebreakIndented)),
													A2(
														$elm$core$List$map,
														function (atDocsLine) {
															return $author$project$Print$sequence(
																A2(
																	$elm$core$List$intersperse,
																	A2(
																		$author$project$Print$followedBy,
																		$author$project$Print$space,
																		$author$project$Print$exactly(',')),
																	A2($elm$core$List$map, $author$project$ElmSyntaxPrint$expose, atDocsLine)));
														},
														A2($elm$core$List$cons, atDocsExposeLine0, atDocsExposeLine1Up)))),
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$space,
												$author$project$Print$exactly('('))))));
						}
					} else {
						return A2(
							$author$project$ElmSyntaxPrint$exposingMulti,
							context.comments,
							{
								expose0: expose0,
								expose1Up: A2($elm$core$List$cons, expose1, expose2Up),
								fullRange: exposingRange
							});
					}
				}
			}
		}
	});
var $author$project$ElmSyntaxPrint$moduleHeader = F2(
	function (context, syntaxModuleHeader) {
		switch (syntaxModuleHeader.$) {
			case 'NormalModule':
				var defaultModuleData = syntaxModuleHeader.a;
				var exposingPrint = A2($author$project$ElmSyntaxPrint$moduleExposing, context, defaultModuleData.exposingList);
				var lineSpread = $author$project$Print$lineSpread(exposingPrint);
				return A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							exposingPrint,
							$author$project$Print$spaceOrLinebreakIndented(lineSpread))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('exposing'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$space,
							A2(
								$author$project$Print$followedBy,
								$author$project$ElmSyntaxPrint$moduleName(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(defaultModuleData.moduleName)),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									$author$project$Print$exactly('module'))))));
			case 'PortModule':
				var defaultModuleData = syntaxModuleHeader.a;
				var exposingPrint = A2($author$project$ElmSyntaxPrint$moduleExposing, context, defaultModuleData.exposingList);
				var lineSpread = $author$project$Print$lineSpread(exposingPrint);
				return A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							exposingPrint,
							$author$project$Print$spaceOrLinebreakIndented(lineSpread))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('exposing'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$space,
							A2(
								$author$project$Print$followedBy,
								$author$project$ElmSyntaxPrint$moduleName(
									$stil4m$elm_syntax$Elm$Syntax$Node$value(defaultModuleData.moduleName)),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$exactly('module'),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											$author$project$Print$exactly('port'))))))));
			default:
				var effectModuleData = syntaxModuleHeader.a;
				var exposingPrint = A2($author$project$ElmSyntaxPrint$moduleExposing, context, effectModuleData.exposingList);
				var lineSpread = $author$project$Print$lineSpread(exposingPrint);
				return A2(
					$author$project$Print$followedBy,
					$author$project$Print$withIndentAtNextMultipleOf4(
						A2(
							$author$project$Print$followedBy,
							exposingPrint,
							$author$project$Print$spaceOrLinebreakIndented(lineSpread))),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$exactly('exposing'),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$space,
							A2(
								$author$project$Print$followedBy,
								$author$project$Print$exactly('}'),
								A2(
									$author$project$Print$followedBy,
									$author$project$Print$space,
									A2(
										$author$project$Print$followedBy,
										$author$project$Print$sequence(
											A2(
												$elm$core$List$intersperse,
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$space,
													$author$project$Print$exactly(',')),
												A2(
													$elm$core$List$filterMap,
													$elm$core$Basics$identity,
													_List_fromArray(
														[
															function () {
															var _v1 = effectModuleData.command;
															if (_v1.$ === 'Nothing') {
																return $elm$core$Maybe$Nothing;
															} else {
																var _v2 = _v1.a;
																var name = _v2.b;
																return $elm$core$Maybe$Just(
																	A2(
																		$author$project$Print$followedBy,
																		$author$project$Print$exactly(name),
																		A2(
																			$author$project$Print$followedBy,
																			$author$project$Print$space,
																			A2(
																				$author$project$Print$followedBy,
																				$author$project$Print$exactly('='),
																				A2(
																					$author$project$Print$followedBy,
																					$author$project$Print$space,
																					$author$project$Print$exactly('command'))))));
															}
														}(),
															function () {
															var _v3 = effectModuleData.subscription;
															if (_v3.$ === 'Nothing') {
																return $elm$core$Maybe$Nothing;
															} else {
																var _v4 = _v3.a;
																var name = _v4.b;
																return $elm$core$Maybe$Just(
																	A2(
																		$author$project$Print$followedBy,
																		$author$project$Print$exactly(name),
																		A2(
																			$author$project$Print$followedBy,
																			$author$project$Print$space,
																			A2(
																				$author$project$Print$followedBy,
																				$author$project$Print$exactly('='),
																				A2(
																					$author$project$Print$followedBy,
																					$author$project$Print$space,
																					$author$project$Print$exactly('subscription'))))));
															}
														}()
														])))),
										A2(
											$author$project$Print$followedBy,
											$author$project$Print$space,
											A2(
												$author$project$Print$followedBy,
												$author$project$Print$exactly('{'),
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$space,
													A2(
														$author$project$Print$followedBy,
														$author$project$Print$exactly('where'),
														A2(
															$author$project$Print$followedBy,
															$author$project$Print$space,
															A2(
																$author$project$Print$followedBy,
																$author$project$ElmSyntaxPrint$moduleName(
																	$stil4m$elm_syntax$Elm$Syntax$Node$value(effectModuleData.moduleName)),
																A2(
																	$author$project$Print$followedBy,
																	$author$project$Print$space,
																	A2(
																		$author$project$Print$followedBy,
																		$author$project$Print$exactly('module'),
																		A2(
																			$author$project$Print$followedBy,
																			$author$project$Print$space,
																			$author$project$Print$exactly('effect'))))))))))))))));
		}
	});
var $author$project$ElmSyntaxPrint$splitOffPortDocumentationComments = function (commentsAndPortDocumentationComments) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (commentOrPortDocumentationComments, soFar) {
				return A2(
					$elm$core$String$startsWith,
					'{-|',
					$stil4m$elm_syntax$Elm$Syntax$Node$value(commentOrPortDocumentationComments)) ? {
					comments: soFar.comments,
					portDocumentationComments: A2($elm$core$List$cons, commentOrPortDocumentationComments, soFar.portDocumentationComments)
				} : {
					comments: A2($elm$core$List$cons, commentOrPortDocumentationComments, soFar.comments),
					portDocumentationComments: soFar.portDocumentationComments
				};
			}),
		{comments: _List_Nil, portDocumentationComments: _List_Nil},
		commentsAndPortDocumentationComments);
};
var $author$project$ElmSyntaxPrint$module_ = function (syntaxModule) {
	var maybeModuleDocumentation = $author$project$ElmSyntaxPrint$moduleDocumentation(syntaxModule);
	var lastSyntaxLocationBeforeDeclarations = function () {
		var _v13 = syntaxModule.imports;
		if (_v13.b) {
			var _v14 = _v13.a;
			var firstImportRange = _v14.a;
			return firstImportRange.end;
		} else {
			return $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxModule.moduleDefinition).end;
		}
	}();
	var commentsAndPortDocumentationComments = $author$project$ElmSyntaxPrint$splitOffPortDocumentationComments(
		function () {
			if (maybeModuleDocumentation.$ === 'Nothing') {
				return syntaxModule.comments;
			} else {
				var syntaxModuleDocumentation = maybeModuleDocumentation.a;
				return A2(
					$elm$core$List$filter,
					function (c) {
						return !_Utils_eq(c, syntaxModuleDocumentation);
					},
					syntaxModule.comments);
			}
		}());
	var atDocsLines = function () {
		if (maybeModuleDocumentation.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var _v11 = maybeModuleDocumentation.a;
			var syntaxModuleDocumentation = _v11.b;
			return A2(
				$elm$core$List$map,
				function ($) {
					return $.atDocsLine;
				},
				$author$project$ElmSyntaxPrint$moduleDocumentationParse(syntaxModuleDocumentation).whileAtDocsLines);
		}
	}();
	return A2(
		$author$project$Print$followedBy,
		function () {
			var _v8 = syntaxModule.declarations;
			if (!_v8.b) {
				return $author$project$Print$empty;
			} else {
				var declaration0 = _v8.a;
				var declaration1Up = _v8.b;
				var _v9 = A2(
					$author$project$ElmSyntaxPrint$commentsAfter,
					$stil4m$elm_syntax$Elm$Syntax$Node$range(
						$author$project$ElmSyntaxPrint$listFilledLast(
							_Utils_Tuple2(declaration0, declaration1Up))).end,
					commentsAndPortDocumentationComments.comments);
				if (!_v9.b) {
					return $author$project$Print$empty;
				} else {
					var comment0 = _v9.a;
					var comment1Up = _v9.b;
					return A2(
						$author$project$Print$followedBy,
						$author$project$ElmSyntaxPrint$moduleLevelComments(
							A2($elm$core$List$cons, comment0, comment1Up)),
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreak,
							A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak)));
				}
			}
		}(),
		A2(
			$author$project$Print$followedBy,
			$author$project$Print$linebreak,
			A2(
				$author$project$Print$followedBy,
				A2(
					$author$project$ElmSyntaxPrint$declarations,
					{comments: commentsAndPortDocumentationComments.comments, portDocumentationComments: commentsAndPortDocumentationComments.portDocumentationComments, previousEnd: lastSyntaxLocationBeforeDeclarations},
					syntaxModule.declarations),
				A2(
					$author$project$Print$followedBy,
					function () {
						var _v5 = syntaxModule.declarations;
						if (!_v5.b) {
							return $author$project$Print$empty;
						} else {
							var _v6 = _v5.a;
							var declaration0Range = _v6.a;
							var _v7 = A2(
								$author$project$ElmSyntaxPrint$commentsInRange,
								{end: declaration0Range.start, start: lastSyntaxLocationBeforeDeclarations},
								commentsAndPortDocumentationComments.comments);
							if (!_v7.b) {
								return $author$project$Print$empty;
							} else {
								var comment0 = _v7.a;
								var comment1Up = _v7.b;
								return $author$project$ElmSyntaxPrint$moduleLevelCommentsBeforeDeclaration(
									{comment0: comment0, comment1Up: comment1Up});
							}
						}
					}(),
					A2(
						$author$project$Print$followedBy,
						$author$project$Print$linebreak,
						A2(
							$author$project$Print$followedBy,
							$author$project$Print$linebreak,
							A2(
								$author$project$Print$followedBy,
								function () {
									var _v2 = syntaxModule.imports;
									if (!_v2.b) {
										return $author$project$Print$empty;
									} else {
										var _v3 = _v2.a;
										var import0Range = _v3.a;
										var import0 = _v3.b;
										var import1Up = _v2.b;
										return A2(
											$author$project$Print$followedBy,
											$author$project$Print$linebreak,
											A2(
												$author$project$Print$followedBy,
												A2(
													$author$project$ElmSyntaxPrint$imports,
													commentsAndPortDocumentationComments.comments,
													A2(
														$elm$core$List$cons,
														A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, import0Range, import0),
														import1Up)),
												A2(
													$author$project$Print$followedBy,
													$author$project$Print$linebreak,
													function () {
														var _v4 = A2(
															$author$project$ElmSyntaxPrint$commentsInRange,
															{
																end: import0Range.start,
																start: $stil4m$elm_syntax$Elm$Syntax$Node$range(syntaxModule.moduleDefinition).end
															},
															commentsAndPortDocumentationComments.comments);
														if (!_v4.b) {
															return $author$project$Print$linebreak;
														} else {
															var comment0 = _v4.a;
															var comment1Up = _v4.b;
															return A2(
																$author$project$Print$followedBy,
																$author$project$ElmSyntaxPrint$moduleLevelComments(
																	A2($elm$core$List$cons, comment0, comment1Up)),
																A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak));
														}
													}())));
									}
								}(),
								A2(
									$author$project$Print$followedBy,
									function () {
										if (maybeModuleDocumentation.$ === 'Nothing') {
											return $author$project$Print$empty;
										} else {
											var _v1 = maybeModuleDocumentation.a;
											var moduleDocumentationAsString = _v1.b;
											return A2(
												$author$project$Print$followedBy,
												$author$project$Print$exactly(moduleDocumentationAsString),
												A2($author$project$Print$followedBy, $author$project$Print$linebreak, $author$project$Print$linebreak));
										}
									}(),
									A2(
										$author$project$ElmSyntaxPrint$moduleHeader,
										{atDocsLines: atDocsLines, comments: commentsAndPortDocumentationComments.comments},
										$stil4m$elm_syntax$Elm$Syntax$Node$value(syntaxModule.moduleDefinition))))))))));
};
var $stil4m$elm_syntax$Rope$empty = $elm$core$Maybe$Nothing;
var $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration = function (a) {
	return {$: 'AliasDeclaration', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration = function (a) {
	return {$: 'CustomTypeDeclaration', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration = function (a) {
	return {$: 'FunctionDeclaration', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration = function (a) {
	return {$: 'PortDeclaration', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Signature$Signature = F2(
	function (name, typeAnnotation) {
		return {name: name, typeAnnotation: typeAnnotation};
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$combine = F3(
	function (f, a, b) {
		var start = a.a.start;
		var end = b.a.end;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			{end: end, start: start},
			A2(f, a, b));
	});
var $stil4m$elm_syntax$ParserFast$Done = function (a) {
	return {$: 'Done', a: a};
};
var $stil4m$elm_syntax$ParserFast$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $stil4m$elm_syntax$ParserFast$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$ExpectingAnyChar = F2(
	function (a, b) {
		return {$: 'ExpectingAnyChar', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$Good = F2(
	function (a, b) {
		return {$: 'Good', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$core$String$any = _String_any;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $stil4m$elm_syntax$Char$Extra$isUtf16Surrogate = function (c) {
	return $elm$core$Basics$isNaN(
		$elm$core$Char$toCode(c));
};
var $stil4m$elm_syntax$ParserFast$charStringIsUtf16HighSurrogate = function (charString) {
	return A2($elm$core$String$any, $stil4m$elm_syntax$Char$Extra$isUtf16Surrogate, charString);
};
var $stil4m$elm_syntax$ParserFast$charOrEnd = F2(
	function (offset, string) {
		var actualChar = A3($elm$core$String$slice, offset, offset + 1, string);
		switch (actualChar) {
			case '\n':
				return -2;
			case '':
				return -1;
			default:
				return $stil4m$elm_syntax$ParserFast$charStringIsUtf16HighSurrogate(actualChar) ? (offset + 2) : (offset + 1);
		}
	});
var $stil4m$elm_syntax$ParserFast$skipWhileHelp = F6(
	function (isGood, offset, row, col, src, indent) {
		skipWhileHelp:
		while (true) {
			var actualChar = A3($elm$core$String$slice, offset, offset + 1, src);
			if (A2($elm$core$String$any, isGood, actualChar)) {
				if (actualChar === '\n') {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileHelp;
				}
			} else {
				if ($stil4m$elm_syntax$ParserFast$charStringIsUtf16HighSurrogate(actualChar) && A2(
					$elm$core$String$any,
					isGood,
					A3($elm$core$String$slice, offset, offset + 2, src))) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 2,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileHelp;
				} else {
					return {col: col, indent: indent, offset: offset, row: row, src: src};
				}
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$anyCharFollowedByWhileMap = F2(
	function (consumedStringToRes, afterFirstIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var firstOffset = A2($stil4m$elm_syntax$ParserFast$charOrEnd, s.offset, s.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingAnyChar, s.row, s.col));
				} else {
					var s1 = _Utils_eq(firstOffset, -2) ? A6($stil4m$elm_syntax$ParserFast$skipWhileHelp, afterFirstIsOkay, s.offset + 1, s.row + 1, 1, s.src, s.indent) : A6($stil4m$elm_syntax$ParserFast$skipWhileHelp, afterFirstIsOkay, firstOffset, s.row, s.col + 1, s.src, s.indent);
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						consumedStringToRes(
							A3($elm$core$String$slice, s.offset, s1.offset, s.src)),
						s1);
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$loopHelp = F5(
	function (committedSoFar, state, element, reduce, s0) {
		loopHelp:
		while (true) {
			var parseElement = element.a;
			var _v0 = parseElement(s0);
			if (_v0.$ === 'Good') {
				var step = _v0.a;
				var s1 = _v0.b;
				var _v1 = A2(reduce, step, state);
				if (_v1.$ === 'Loop') {
					var newState = _v1.a;
					var $temp$committedSoFar = true,
						$temp$state = newState,
						$temp$element = element,
						$temp$reduce = reduce,
						$temp$s0 = s1;
					committedSoFar = $temp$committedSoFar;
					state = $temp$state;
					element = $temp$element;
					reduce = $temp$reduce;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = _v1.a;
					return A2($stil4m$elm_syntax$ParserFast$Good, result, s1);
				}
			} else {
				var elementCommitted = _v0.a;
				var x = _v0.b;
				return A2($stil4m$elm_syntax$ParserFast$Bad, committedSoFar || elementCommitted, x);
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$loop = F3(
	function (state, element, reduce) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				return A5($stil4m$elm_syntax$ParserFast$loopHelp, false, state, element, reduce, s);
			});
	});
var $stil4m$elm_syntax$ParserFast$map2WithRange = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var committed = _v2.a;
					var x = _v2.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v2.a;
					var s1 = _v2.b;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var x = _v3.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v3.a;
						var s2 = _v3.b;
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							A3(
								func,
								{
									end: {column: s2.col, row: s2.row},
									start: {column: s0.col, row: s0.row}
								},
								a,
								b),
							s2);
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$ExpectingOneOf = F3(
	function (a, b, c) {
		return {$: 'ExpectingOneOf', a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$ParserFast$oneOf2 = F2(
	function (_v0, _v1) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v2 = attemptFirst(s);
				if (_v2.$ === 'Good') {
					var firstGood = _v2;
					return firstGood;
				} else {
					var firstBad = _v2;
					var firstCommitted = firstBad.a;
					var firstX = firstBad.b;
					if (firstCommitted) {
						return firstBad;
					} else {
						var _v3 = attemptSecond(s);
						if (_v3.$ === 'Good') {
							var secondGood = _v3;
							return secondGood;
						} else {
							var secondBad = _v3;
							var secondCommitted = secondBad.a;
							var secondX = secondBad.b;
							return secondCommitted ? secondBad : A2(
								$stil4m$elm_syntax$ParserFast$Bad,
								false,
								A3($stil4m$elm_syntax$ParserFast$ExpectingOneOf, firstX, secondX, _List_Nil));
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$oneOf3 = F3(
	function (_v0, _v1, _v2) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		var attemptThird = _v2.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v3 = attemptFirst(s);
				if (_v3.$ === 'Good') {
					var firstGood = _v3;
					return firstGood;
				} else {
					var firstBad = _v3;
					var firstCommitted = firstBad.a;
					var firstX = firstBad.b;
					if (firstCommitted) {
						return firstBad;
					} else {
						var _v4 = attemptSecond(s);
						if (_v4.$ === 'Good') {
							var secondGood = _v4;
							return secondGood;
						} else {
							var secondBad = _v4;
							var secondCommitted = secondBad.a;
							var secondX = secondBad.b;
							if (secondCommitted) {
								return secondBad;
							} else {
								var _v5 = attemptThird(s);
								if (_v5.$ === 'Good') {
									var thirdGood = _v5;
									return thirdGood;
								} else {
									var thirdBad = _v5;
									var thirdCommitted = thirdBad.a;
									var thirdX = thirdBad.b;
									return thirdCommitted ? thirdBad : A2(
										$stil4m$elm_syntax$ParserFast$Bad,
										false,
										A3(
											$stil4m$elm_syntax$ParserFast$ExpectingOneOf,
											firstX,
											secondX,
											_List_fromArray(
												[thirdX])));
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$ExpectingSymbol = F3(
	function (a, b, c) {
		return {$: 'ExpectingSymbol', a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$ParserFast$symbol = F2(
	function (str, res) {
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + strLength;
				return _Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					str + '') ? A2(
					$stil4m$elm_syntax$ParserFast$Good,
					res,
					{col: s.col + strLength, indent: s.indent, offset: newOffset, row: s.row, src: s.src}) : A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s.row, s.col, str));
			});
	});
var $stil4m$elm_syntax$ParserFast$pStepCommit = function (pStep) {
	if (pStep.$ === 'Good') {
		var good = pStep;
		return good;
	} else {
		var x = pStep.b;
		return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
	}
};
var $stil4m$elm_syntax$ParserFast$symbolFollowedBy = F2(
	function (str, _v0) {
		var parseNext = _v0.a;
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + strLength;
				return _Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					str + '') ? $stil4m$elm_syntax$ParserFast$pStepCommit(
					parseNext(
						{col: s.col + strLength, indent: s.indent, offset: newOffset, row: s.row, src: s.src})) : A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s.row, s.col, str));
			});
	});
var $stil4m$elm_syntax$ParserFast$while = function (isGood) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileHelp, isGood, s0.offset, s0.row, s0.col, s0.src, s0.indent);
			return A2(
				$stil4m$elm_syntax$ParserFast$Good,
				A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
				s1);
		});
};
var $stil4m$elm_syntax$ParserFast$nestableMultiCommentMapWithRange = F3(
	function (rangeContentToRes, _v0, _v1) {
		var openChar = _v0.a;
		var openTail = _v0.b;
		var closeChar = _v1.a;
		var closeTail = _v1.b;
		var open = A2($elm$core$String$cons, openChar, openTail);
		var isNotRelevant = function (_char) {
			return (!_Utils_eq(_char, openChar)) && ((!_Utils_eq(_char, closeChar)) && (!$stil4m$elm_syntax$Char$Extra$isUtf16Surrogate(_char)));
		};
		var close = A2($elm$core$String$cons, closeChar, closeTail);
		return A3(
			$stil4m$elm_syntax$ParserFast$map2WithRange,
			F3(
				function (range, afterOpen, contentAfterAfterOpen) {
					return A2(
						rangeContentToRes,
						range,
						_Utils_ap(
							open,
							_Utils_ap(
								afterOpen,
								_Utils_ap(contentAfterAfterOpen, close))));
				}),
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				open,
				$stil4m$elm_syntax$ParserFast$while(isNotRelevant)),
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				A2($stil4m$elm_syntax$ParserFast$symbol, close, ''),
				A3(
					$stil4m$elm_syntax$ParserFast$loop,
					_Utils_Tuple2('', 1),
					A3(
						$stil4m$elm_syntax$ParserFast$oneOf3,
						A2(
							$stil4m$elm_syntax$ParserFast$symbol,
							close,
							_Utils_Tuple2(close, -1)),
						A2(
							$stil4m$elm_syntax$ParserFast$symbol,
							open,
							_Utils_Tuple2(open, 1)),
						A2(
							$stil4m$elm_syntax$ParserFast$anyCharFollowedByWhileMap,
							function (consumed) {
								return _Utils_Tuple2(consumed, 0);
							},
							isNotRelevant)),
					F2(
						function (_v2, _v3) {
							var toAppend = _v2.a;
							var nestingChange = _v2.b;
							var soFarContent = _v3.a;
							var soFarNesting = _v3.b;
							var newNesting = soFarNesting + nestingChange;
							return (!newNesting) ? $stil4m$elm_syntax$ParserFast$Done(soFarContent) : $stil4m$elm_syntax$ParserFast$Loop(
								_Utils_Tuple2(soFarContent + (toAppend + ''), newNesting));
						}))));
	});
var $stil4m$elm_syntax$Elm$Parser$Comments$multiLineCommentNoCheck = A3(
	$stil4m$elm_syntax$ParserFast$nestableMultiCommentMapWithRange,
	$stil4m$elm_syntax$Elm$Syntax$Node$Node,
	_Utils_Tuple2(
		_Utils_chr('{'),
		'-'),
	_Utils_Tuple2(
		_Utils_chr('-'),
		'}'));
var $stil4m$elm_syntax$Elm$Parser$Comments$declarationDocumentation = $stil4m$elm_syntax$Elm$Parser$Comments$multiLineCommentNoCheck;
var $stil4m$elm_syntax$Rope$Branch2 = F2(
	function (a, b) {
		return {$: 'Branch2', a: a, b: b};
	});
var $stil4m$elm_syntax$Rope$filledPrependTo = F2(
	function (right, leftLikelyFilled) {
		if (right.$ === 'Nothing') {
			return $elm$core$Maybe$Just(leftLikelyFilled);
		} else {
			var rightLikelyFilled = right.a;
			return $elm$core$Maybe$Just(
				A2($stil4m$elm_syntax$Rope$Branch2, leftLikelyFilled, rightLikelyFilled));
		}
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$FunctionDeclarationAfterDocumentation = function (a) {
	return {$: 'FunctionDeclarationAfterDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Application = function (a) {
	return {$: 'Application', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression = function (a) {
	return {$: 'CaseExpression', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expression$ExtendRightByOperation = function (a) {
	return {$: 'ExtendRightByOperation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expression$FieldsFirstValue = function (a) {
	return {$: 'FieldsFirstValue', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock = F3(
	function (a, b, c) {
		return {$: 'IfBlock', a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression = function (a) {
	return {$: 'LambdaExpression', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Infix$Left = {$: 'Left'};
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring = F2(
	function (a, b) {
		return {$: 'LetDestructuring', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression = function (a) {
	return {$: 'LetExpression', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction = function (a) {
	return {$: 'LetFunction', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr = function (a) {
	return {$: 'ListExpr', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Negation = function (a) {
	return {$: 'Negation', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Infix$Non = {$: 'Non'};
var $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression = function (a) {
	return {$: 'ParenthesizedExpression', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess = F2(
	function (a, b) {
		return {$: 'RecordAccess', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr = function (a) {
	return {$: 'RecordExpr', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression = F2(
	function (a, b) {
		return {$: 'RecordUpdateExpression', a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$RecordUpdateFirstSetter = function (a) {
	return {$: 'RecordUpdateFirstSetter', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Infix$Right = {$: 'Right'};
var $stil4m$elm_syntax$Elm$Parser$Expression$TupledParenthesizedFollowedByRecordAccesses = function (a) {
	return {$: 'TupledParenthesizedFollowedByRecordAccesses', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expression$TupledTwoOrThree = function (a) {
	return {$: 'TupledTwoOrThree', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr = {$: 'UnitExpr'};
var $stil4m$elm_syntax$Elm$Syntax$Expression$PrefixOperator = function (a) {
	return {$: 'PrefixOperator', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$isAllowedOperatorToken = function (operatorCandidateToValidate) {
	switch (operatorCandidateToValidate) {
		case '==':
			return true;
		case '/=':
			return true;
		case '::':
			return true;
		case '++':
			return true;
		case '+':
			return true;
		case '*':
			return true;
		case '<|':
			return true;
		case '|>':
			return true;
		case '||':
			return true;
		case '<=':
			return true;
		case '>=':
			return true;
		case '|=':
			return true;
		case '|.':
			return true;
		case '//':
			return true;
		case '</>':
			return true;
		case '<?>':
			return true;
		case '^':
			return true;
		case '<<':
			return true;
		case '>>':
			return true;
		case '<':
			return true;
		case '>':
			return true;
		case '/':
			return true;
		case '&&':
			return true;
		case '-':
			return true;
		default:
			return false;
	}
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$isOperatorSymbolChar = function (c) {
	switch (c.valueOf()) {
		case '+':
			return true;
		case '-':
			return true;
		case '/':
			return true;
		case '*':
			return true;
		case '=':
			return true;
		case '.':
			return true;
		case '<':
			return true;
		case '>':
			return true;
		case ':':
			return true;
		case '&':
			return true;
		case '|':
			return true;
		case '^':
			return true;
		case '?':
			return true;
		default:
			return false;
	}
};
var $stil4m$elm_syntax$ParserFast$ExpectingStringSatisfyingPredicate = F2(
	function (a, b) {
		return {$: 'ExpectingStringSatisfyingPredicate', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakAnd2PartUtf16Help = F3(
	function (isGood, offset, src) {
		skipWhileWithoutLinebreakAnd2PartUtf16Help:
		while (true) {
			if (A2(
				$elm$core$String$any,
				isGood,
				A3($elm$core$String$slice, offset, offset + 1, src))) {
				var $temp$isGood = isGood,
					$temp$offset = offset + 1,
					$temp$src = src;
				isGood = $temp$isGood;
				offset = $temp$offset;
				src = $temp$src;
				continue skipWhileWithoutLinebreakAnd2PartUtf16Help;
			} else {
				return offset;
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol = F4(
	function (whileRangeAndContentToRes, whileCharIsOkay, whileResultIsOkay, mandatoryFinalSymbol) {
		var mandatoryFinalSymbolLength = $elm$core$String$length(mandatoryFinalSymbol);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var s1Offset = A3($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakAnd2PartUtf16Help, whileCharIsOkay, s0.offset, s0.src);
				var whileContent = A3($elm$core$String$slice, s0.offset, s1Offset, s0.src);
				if (_Utils_eq(
					A3($elm$core$String$slice, s1Offset, s1Offset + mandatoryFinalSymbolLength, s0.src),
					mandatoryFinalSymbol + '') && whileResultIsOkay(whileContent)) {
					var s1Column = s0.col + (s1Offset - s0.offset);
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A2(
							whileRangeAndContentToRes,
							{
								end: {column: s1Column, row: s0.row},
								start: {column: s0.col, row: s0.row}
							},
							whileContent),
						{col: s1Column + mandatoryFinalSymbolLength, indent: s0.indent, offset: s1Offset + mandatoryFinalSymbolLength, row: s0.row, src: s0.src});
				} else {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingStringSatisfyingPredicate, s0.row, s0.col + 1));
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$allowedPrefixOperatorFollowedByClosingParensOneOf = A4(
	$stil4m$elm_syntax$ParserFast$whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol,
	F2(
		function (operatorRange, operator) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					{
						end: {column: operatorRange.end.column + 1, row: operatorRange.end.row},
						start: {column: operatorRange.start.column - 1, row: operatorRange.start.row}
					},
					$stil4m$elm_syntax$Elm$Syntax$Expression$PrefixOperator(operator))
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$isOperatorSymbolChar,
	$stil4m$elm_syntax$Elm$Parser$Tokens$isAllowedOperatorToken,
	')');
var $stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication = F4(
	function (a, b, c, d) {
		return {$: 'OperatorApplication', a: a, b: b, c: c, d: d};
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$applyExtensionRight = F2(
	function (_v0, leftNode) {
		var operation = _v0.a;
		var leftRange = leftNode.a;
		var rightExpressionNode = operation.expression;
		var rightExpressionRange = rightExpressionNode.a;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			{end: rightExpressionRange.end, start: leftRange.start},
			A4($stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication, operation.symbol, operation.direction, leftNode, rightExpressionNode));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$CharLiteral = function (a) {
	return {$: 'CharLiteral', a: a};
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $stil4m$elm_syntax$ParserFast$anyChar = $stil4m$elm_syntax$ParserFast$Parser(
	function (s) {
		var newOffset = A2($stil4m$elm_syntax$ParserFast$charOrEnd, s.offset, s.src);
		if (_Utils_eq(newOffset, -1)) {
			return A2(
				$stil4m$elm_syntax$ParserFast$Bad,
				false,
				A2($stil4m$elm_syntax$ParserFast$ExpectingAnyChar, s.row, s.col));
		} else {
			if (_Utils_eq(newOffset, -2)) {
				return A2(
					$stil4m$elm_syntax$ParserFast$Good,
					_Utils_chr('\n'),
					{col: 1, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src});
			} else {
				var _v0 = $elm$core$String$toList(
					A3($elm$core$String$slice, s.offset, newOffset, s.src));
				if (!_v0.b) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingAnyChar, s.row, s.col));
				} else {
					var c = _v0.a;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						c,
						{col: s.col + 1, indent: s.indent, offset: newOffset, row: s.row, src: s.src});
				}
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$followedBySymbol = F2(
	function (str, _v0) {
		var parsePrevious = _v0.a;
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parsePrevious(s0);
				if (_v1.$ === 'Good') {
					var res = _v1.a;
					var s1 = _v1.b;
					var newOffset = s1.offset + strLength;
					return _Utils_eq(
						A3($elm$core$String$slice, s1.offset, newOffset, s1.src),
						str + '') ? A2(
						$stil4m$elm_syntax$ParserFast$Good,
						res,
						{col: s1.col + strLength, indent: s1.indent, offset: newOffset, row: s1.row, src: s1.src}) : A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						true,
						A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s1.row, s1.col, str));
				} else {
					var bad = _v1;
					return bad;
				}
			});
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $stil4m$elm_syntax$Elm$Parser$Tokens$charToHex = function (c) {
	switch (c.valueOf()) {
		case '0':
			return 0;
		case '1':
			return 1;
		case '2':
			return 2;
		case '3':
			return 3;
		case '4':
			return 4;
		case '5':
			return 5;
		case '6':
			return 6;
		case '7':
			return 7;
		case '8':
			return 8;
		case '9':
			return 9;
		case 'a':
			return 10;
		case 'b':
			return 11;
		case 'c':
			return 12;
		case 'd':
			return 13;
		case 'e':
			return 14;
		case 'f':
			return 15;
		case 'A':
			return 10;
		case 'B':
			return 11;
		case 'C':
			return 12;
		case 'D':
			return 13;
		case 'E':
			return 14;
		default:
			return 15;
	}
};
var $elm$core$Basics$pow = _Basics_pow;
var $stil4m$elm_syntax$Elm$Parser$Tokens$hexStringToInt = function (string) {
	return A3(
		$elm$core$String$foldr,
		F2(
			function (c, soFar) {
				return {
					exponent: soFar.exponent + 1,
					result: soFar.result + (A2($elm$core$Basics$pow, 16, soFar.exponent) * $stil4m$elm_syntax$Elm$Parser$Tokens$charToHex(c))
				};
			}),
		{exponent: 0, result: 0},
		string).result;
};
var $stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate = F2(
	function (a, b) {
		return {$: 'ExpectingCharSatisfyingPredicate', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak = F3(
	function (predicate, offset, string) {
		var actualChar = A3($elm$core$String$slice, offset, offset + 1, string);
		return A2($elm$core$String$any, predicate, actualChar) ? (offset + 1) : (($stil4m$elm_syntax$ParserFast$charStringIsUtf16HighSurrogate(actualChar) && A2(
			$elm$core$String$any,
			predicate,
			A3($elm$core$String$slice, offset, offset + 2, string))) ? (offset + 2) : (-1));
	});
var $stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp = F6(
	function (isGood, offset, row, col, src, indent) {
		skipWhileWithoutLinebreakHelp:
		while (true) {
			var actualChar = A3($elm$core$String$slice, offset, offset + 1, src);
			if (A2($elm$core$String$any, isGood, actualChar)) {
				var $temp$isGood = isGood,
					$temp$offset = offset + 1,
					$temp$row = row,
					$temp$col = col + 1,
					$temp$src = src,
					$temp$indent = indent;
				isGood = $temp$isGood;
				offset = $temp$offset;
				row = $temp$row;
				col = $temp$col;
				src = $temp$src;
				indent = $temp$indent;
				continue skipWhileWithoutLinebreakHelp;
			} else {
				if ($stil4m$elm_syntax$ParserFast$charStringIsUtf16HighSurrogate(actualChar) && A2(
					$elm$core$String$any,
					isGood,
					A3($elm$core$String$slice, offset, offset + 2, src))) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 2,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileWithoutLinebreakHelp;
				} else {
					return {col: col, indent: indent, offset: offset, row: row, src: src};
				}
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$ifFollowedByWhileMapWithoutLinebreak = F3(
	function (consumedStringToRes, firstIsOkay, afterFirstIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var firstOffset = A3($stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak, firstIsOkay, s0.offset, s0.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate, s0.row, s0.col));
				} else {
					var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, afterFirstIsOkay, firstOffset, s0.row, s0.col + 1, s0.src, s0.indent);
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						consumedStringToRes(
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src)),
						s1);
				}
			});
	});
var $elm$core$Char$isHexDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return ((48 <= code) && (code <= 57)) || (((65 <= code) && (code <= 70)) || ((97 <= code) && (code <= 102)));
};
var $stil4m$elm_syntax$ParserFast$oneOf7 = F7(
	function (_v0, _v1, _v2, _v3, _v4, _v5, _v6) {
		var attempt0 = _v0.a;
		var attempt1 = _v1.a;
		var attempt2 = _v2.a;
		var attempt3 = _v3.a;
		var attempt4 = _v4.a;
		var attempt5 = _v5.a;
		var attempt6 = _v6.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v7 = attempt0(s);
				if (_v7.$ === 'Good') {
					var good = _v7;
					return good;
				} else {
					var bad0 = _v7;
					var committed0 = bad0.a;
					var x0 = bad0.b;
					if (committed0) {
						return bad0;
					} else {
						var _v8 = attempt1(s);
						if (_v8.$ === 'Good') {
							var good = _v8;
							return good;
						} else {
							var bad1 = _v8;
							var committed1 = bad1.a;
							var x1 = bad1.b;
							if (committed1) {
								return bad1;
							} else {
								var _v9 = attempt2(s);
								if (_v9.$ === 'Good') {
									var good = _v9;
									return good;
								} else {
									var bad2 = _v9;
									var committed2 = bad2.a;
									var x2 = bad2.b;
									if (committed2) {
										return bad2;
									} else {
										var _v10 = attempt3(s);
										if (_v10.$ === 'Good') {
											var good = _v10;
											return good;
										} else {
											var bad3 = _v10;
											var committed3 = bad3.a;
											var x3 = bad3.b;
											if (committed3) {
												return bad3;
											} else {
												var _v11 = attempt4(s);
												if (_v11.$ === 'Good') {
													var good = _v11;
													return good;
												} else {
													var bad4 = _v11;
													var committed4 = bad4.a;
													var x4 = bad4.b;
													if (committed4) {
														return bad4;
													} else {
														var _v12 = attempt5(s);
														if (_v12.$ === 'Good') {
															var good = _v12;
															return good;
														} else {
															var bad5 = _v12;
															var committed5 = bad5.a;
															var x5 = bad5.b;
															if (committed5) {
																return bad5;
															} else {
																var _v13 = attempt6(s);
																if (_v13.$ === 'Good') {
																	var good = _v13;
																	return good;
																} else {
																	var bad6 = _v13;
																	var committed6 = bad6.a;
																	var x6 = bad6.b;
																	return committed6 ? bad6 : A2(
																		$stil4m$elm_syntax$ParserFast$Bad,
																		false,
																		A3(
																			$stil4m$elm_syntax$ParserFast$ExpectingOneOf,
																			x0,
																			x1,
																			_List_fromArray(
																				[x2, x3, x4, x5, x6])));
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
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValueMap = function (charToRes) {
	return A7(
		$stil4m$elm_syntax$ParserFast$oneOf7,
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'\'',
			charToRes(
				_Utils_chr('\''))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'\"',
			charToRes(
				_Utils_chr('\"'))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'n',
			charToRes(
				_Utils_chr('\n'))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			't',
			charToRes(
				_Utils_chr('\t'))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'r',
			charToRes(
				_Utils_chr('\u000D'))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'\\',
			charToRes(
				_Utils_chr('\\'))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'u{',
			A2(
				$stil4m$elm_syntax$ParserFast$followedBySymbol,
				'}',
				A3(
					$stil4m$elm_syntax$ParserFast$ifFollowedByWhileMapWithoutLinebreak,
					function (hex) {
						return charToRes(
							$elm$core$Char$fromCode(
								$stil4m$elm_syntax$Elm$Parser$Tokens$hexStringToInt(hex)));
					},
					$elm$core$Char$isHexDigit,
					$elm$core$Char$isHexDigit))));
};
var $stil4m$elm_syntax$ParserFast$oneOf2MapWithStartRowColumnAndEndRowColumn = F4(
	function (firstToChoice, _v0, secondToChoice, _v1) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v2 = attemptFirst(s);
				if (_v2.$ === 'Good') {
					var first = _v2.a;
					var s1 = _v2.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A5(firstToChoice, s.row, s.col, first, s1.row, s1.col),
						s1);
				} else {
					var firstCommitted = _v2.a;
					var firstX = _v2.b;
					if (firstCommitted) {
						return A2($stil4m$elm_syntax$ParserFast$Bad, firstCommitted, firstX);
					} else {
						var _v3 = attemptSecond(s);
						if (_v3.$ === 'Good') {
							var second = _v3.a;
							var s1 = _v3.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A5(secondToChoice, s.row, s.col, second, s1.row, s1.col),
								s1);
						} else {
							var secondCommitted = _v3.a;
							var secondX = _v3.b;
							return secondCommitted ? A2($stil4m$elm_syntax$ParserFast$Bad, secondCommitted, secondX) : A2(
								$stil4m$elm_syntax$ParserFast$Bad,
								false,
								A3($stil4m$elm_syntax$ParserFast$ExpectingOneOf, firstX, secondX, _List_Nil));
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteralMapWithRange = function (rangeAndCharToRes) {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'\'',
		A2(
			$stil4m$elm_syntax$ParserFast$followedBySymbol,
			'\'',
			A4(
				$stil4m$elm_syntax$ParserFast$oneOf2MapWithStartRowColumnAndEndRowColumn,
				F5(
					function (startRow, startColumn, _char, endRow, endColumn) {
						return A2(
							rangeAndCharToRes,
							{
								end: {column: endColumn + 1, row: endRow},
								start: {column: startColumn - 1, row: startRow}
							},
							_char);
					}),
				A2(
					$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
					'\\',
					$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValueMap($elm$core$Basics$identity)),
				F5(
					function (startRow, startColumn, _char, endRow, endColumn) {
						return A2(
							rangeAndCharToRes,
							{
								end: {column: endColumn + 1, row: endRow},
								start: {column: startColumn - 1, row: startRow}
							},
							_char);
					}),
				$stil4m$elm_syntax$ParserFast$anyChar)));
};
var $stil4m$elm_syntax$Elm$Parser$Expression$charLiteralExpression = $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteralMapWithRange(
	F2(
		function (range, _char) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Expression$CharLiteral(_char))
			};
		}));
var $stil4m$elm_syntax$Elm$Parser$Expression$errUnknownInfixOperator = $elm$core$Result$Err('unknown infix operator');
var $stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateMapWithRangeWithoutLinebreak = F4(
	function (toResult, firstIsOkay, afterFirstIsOkay, resultIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var firstOffset = A3($stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak, firstIsOkay, s0.offset, s0.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate, s0.row, s0.col));
				} else {
					var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, afterFirstIsOkay, firstOffset, s0.row, s0.col + 1, s0.src, s0.indent);
					var name = A3($elm$core$String$slice, s0.offset, s1.offset, s0.src);
					return resultIsOkay(name) ? A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A2(
							toResult,
							{
								end: {column: s1.col, row: s1.row},
								start: {column: s0.col, row: s0.row}
							},
							name),
						s1) : A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingStringSatisfyingPredicate, s0.row, s0.col + 1));
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$isNotReserved = function (name) {
	switch (name) {
		case 'module':
			return false;
		case 'exposing':
			return false;
		case 'import':
			return false;
		case 'as':
			return false;
		case 'if':
			return false;
		case 'then':
			return false;
		case 'else':
			return false;
		case 'let':
			return false;
		case 'in':
			return false;
		case 'case':
			return false;
		case 'of':
			return false;
		case 'port':
			return false;
		case 'type':
			return false;
		case 'where':
			return false;
		default:
			return true;
	}
};
var $stil4m$elm_syntax$Char$Extra$charCodeIsDigit = function (code) {
	return (code <= 57) && (48 <= code);
};
var $stil4m$elm_syntax$Char$Extra$charCodeIsLower = function (code) {
	return (97 <= code) && (code <= 122);
};
var $stil4m$elm_syntax$Char$Extra$charCodeIsUpper = function (code) {
	return (code <= 90) && (65 <= code);
};
var $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast = function (c) {
	var code = $elm$core$Char$toCode(c);
	return $stil4m$elm_syntax$Char$Extra$charCodeIsLower(code) || ($stil4m$elm_syntax$Char$Extra$charCodeIsUpper(code) || ($stil4m$elm_syntax$Char$Extra$charCodeIsDigit(code) || ((code === 95) || (((code !== 32) && (code !== 10)) && ((code < 256) ? (((48 <= code) && (code <= 57)) || (((65 <= code) && (code <= 90)) || (((97 <= code) && (code <= 122)) || ((code === 170) || (((178 <= code) && (code <= 179)) || ((code === 181) || (((185 <= code) && (code <= 186)) || (((188 <= code) && (code <= 190)) || (((192 <= code) && (code <= 214)) || (((216 <= code) && (code <= 246)) || ((248 <= code) && (code <= 255)))))))))))) : ((code < 43700) ? ((code < 4347) ? ((code < 2868) ? ((code < 2364) ? ((code < 1648) ? ((code < 930) ? (((256 <= code) && (code <= 705)) || (((710 <= code) && (code <= 721)) || (((736 <= code) && (code <= 740)) || (((880 <= code) && (code <= 884)) || (((886 <= code) && (code <= 887)) || (((890 <= code) && (code <= 893)) || ((code === 895) || ((code === 902) || (((904 <= code) && (code <= 906)) || ((code === 908) || (((910 <= code) && (code <= 929)) || ((!A2($elm$core$Basics$modBy, 2, code)) && ((748 <= code) && (code <= 750)))))))))))))) : (((931 <= code) && (code <= 1013)) || (((1015 <= code) && (code <= 1153)) || (((1162 <= code) && (code <= 1327)) || (((1329 <= code) && (code <= 1366)) || ((code === 1369) || (((1376 <= code) && (code <= 1416)) || (((1488 <= code) && (code <= 1514)) || (((1519 <= code) && (code <= 1522)) || (((1568 <= code) && (code <= 1610)) || (((1632 <= code) && (code <= 1641)) || ((1646 <= code) && (code <= 1647))))))))))))) : ((code < 2041) ? (((1649 <= code) && (code <= 1747)) || ((code === 1749) || (((1765 <= code) && (code <= 1766)) || (((1774 <= code) && (code <= 1788)) || ((code === 1791) || ((code === 1808) || (((1810 <= code) && (code <= 1839)) || (((1869 <= code) && (code <= 1957)) || ((code === 1969) || (((1984 <= code) && (code <= 2026)) || ((2036 <= code) && (code <= 2037)))))))))))) : ((code === 2042) || (((2048 <= code) && (code <= 2069)) || ((code === 2074) || ((code === 2084) || ((code === 2088) || (((2112 <= code) && (code <= 2136)) || (((2144 <= code) && (code <= 2154)) || (((2160 <= code) && (code <= 2183)) || (((2185 <= code) && (code <= 2190)) || (((2208 <= code) && (code <= 2249)) || ((2308 <= code) && (code <= 2361)))))))))))))) : ((code < 2609) ? ((code < 2492) ? ((code === 2365) || ((code === 2384) || (((2392 <= code) && (code <= 2401)) || (((2406 <= code) && (code <= 2415)) || (((2417 <= code) && (code <= 2432)) || (((2437 <= code) && (code <= 2444)) || (((2447 <= code) && (code <= 2448)) || (((2451 <= code) && (code <= 2472)) || (((2474 <= code) && (code <= 2480)) || ((code === 2482) || ((2486 <= code) && (code <= 2489)))))))))))) : ((code === 2493) || ((code === 2510) || (((2524 <= code) && (code <= 2525)) || (((2527 <= code) && (code <= 2529)) || (((2534 <= code) && (code <= 2545)) || (((2548 <= code) && (code <= 2553)) || ((code === 2556) || (((2565 <= code) && (code <= 2570)) || (((2575 <= code) && (code <= 2576)) || (((2579 <= code) && (code <= 2600)) || ((2602 <= code) && (code <= 2608))))))))))))) : ((code < 2737) ? (((2610 <= code) && (code <= 2611)) || (((2613 <= code) && (code <= 2614)) || (((2616 <= code) && (code <= 2617)) || (((2649 <= code) && (code <= 2652)) || ((code === 2654) || (((2662 <= code) && (code <= 2671)) || (((2674 <= code) && (code <= 2676)) || (((2693 <= code) && (code <= 2701)) || (((2703 <= code) && (code <= 2705)) || (((2707 <= code) && (code <= 2728)) || ((2730 <= code) && (code <= 2736)))))))))))) : (((2738 <= code) && (code <= 2739)) || (((2741 <= code) && (code <= 2745)) || ((code === 2749) || ((code === 2768) || (((2784 <= code) && (code <= 2785)) || (((2790 <= code) && (code <= 2799)) || ((code === 2809) || (((2821 <= code) && (code <= 2828)) || (((2831 <= code) && (code <= 2832)) || (((2835 <= code) && (code <= 2856)) || (((2858 <= code) && (code <= 2864)) || ((2866 <= code) && (code <= 2867)))))))))))))))) : ((code < 3411) ? ((code < 3132) ? ((code < 2971) ? (((2869 <= code) && (code <= 2873)) || ((code === 2877) || (((2908 <= code) && (code <= 2909)) || (((2911 <= code) && (code <= 2913)) || (((2918 <= code) && (code <= 2927)) || (((2929 <= code) && (code <= 2935)) || ((code === 2947) || (((2949 <= code) && (code <= 2954)) || (((2958 <= code) && (code <= 2960)) || (((2962 <= code) && (code <= 2965)) || ((2969 <= code) && (code <= 2970)))))))))))) : ((code === 2972) || (((2974 <= code) && (code <= 2975)) || (((2979 <= code) && (code <= 2980)) || (((2984 <= code) && (code <= 2986)) || (((2990 <= code) && (code <= 3001)) || ((code === 3024) || (((3046 <= code) && (code <= 3058)) || (((3077 <= code) && (code <= 3084)) || (((3086 <= code) && (code <= 3088)) || (((3090 <= code) && (code <= 3112)) || ((3114 <= code) && (code <= 3129))))))))))))) : ((code < 3252) ? ((code === 3133) || (((3160 <= code) && (code <= 3162)) || ((code === 3165) || (((3168 <= code) && (code <= 3169)) || (((3174 <= code) && (code <= 3183)) || (((3192 <= code) && (code <= 3198)) || ((code === 3200) || (((3205 <= code) && (code <= 3212)) || (((3214 <= code) && (code <= 3216)) || (((3218 <= code) && (code <= 3240)) || ((3242 <= code) && (code <= 3251)))))))))))) : (((3253 <= code) && (code <= 3257)) || ((code === 3261) || (((3293 <= code) && (code <= 3294)) || (((3296 <= code) && (code <= 3297)) || (((3302 <= code) && (code <= 3311)) || (((3313 <= code) && (code <= 3314)) || (((3332 <= code) && (code <= 3340)) || (((3342 <= code) && (code <= 3344)) || (((3346 <= code) && (code <= 3386)) || ((code === 3389) || (code === 3406))))))))))))) : ((code < 3775) ? ((code < 3633) ? (((3412 <= code) && (code <= 3414)) || (((3416 <= code) && (code <= 3425)) || (((3430 <= code) && (code <= 3448)) || (((3450 <= code) && (code <= 3455)) || (((3461 <= code) && (code <= 3478)) || (((3482 <= code) && (code <= 3505)) || (((3507 <= code) && (code <= 3515)) || ((code === 3517) || (((3520 <= code) && (code <= 3526)) || (((3558 <= code) && (code <= 3567)) || ((3585 <= code) && (code <= 3632)))))))))))) : (((3634 <= code) && (code <= 3635)) || (((3648 <= code) && (code <= 3654)) || (((3664 <= code) && (code <= 3673)) || (((3713 <= code) && (code <= 3714)) || ((code === 3716) || (((3718 <= code) && (code <= 3722)) || (((3724 <= code) && (code <= 3747)) || ((code === 3749) || (((3751 <= code) && (code <= 3760)) || (((3762 <= code) && (code <= 3763)) || (code === 3773)))))))))))) : ((code < 4175) ? (((3776 <= code) && (code <= 3780)) || ((code === 3782) || (((3792 <= code) && (code <= 3801)) || (((3804 <= code) && (code <= 3807)) || ((code === 3840) || (((3872 <= code) && (code <= 3891)) || (((3904 <= code) && (code <= 3911)) || (((3913 <= code) && (code <= 3948)) || (((3976 <= code) && (code <= 3980)) || (((4096 <= code) && (code <= 4138)) || ((4159 <= code) && (code <= 4169)))))))))))) : (((4176 <= code) && (code <= 4181)) || (((4186 <= code) && (code <= 4189)) || ((code === 4193) || (((4197 <= code) && (code <= 4198)) || (((4206 <= code) && (code <= 4208)) || (((4213 <= code) && (code <= 4225)) || ((code === 4238) || (((4240 <= code) && (code <= 4249)) || (((4256 <= code) && (code <= 4293)) || ((code === 4295) || ((code === 4301) || ((4304 <= code) && (code <= 4346))))))))))))))))) : ((code < 8454) ? ((code < 6527) ? ((code < 5760) ? ((code < 4801) ? (((4348 <= code) && (code <= 4680)) || (((4682 <= code) && (code <= 4685)) || (((4688 <= code) && (code <= 4694)) || ((code === 4696) || (((4698 <= code) && (code <= 4701)) || (((4704 <= code) && (code <= 4744)) || (((4746 <= code) && (code <= 4749)) || (((4752 <= code) && (code <= 4784)) || (((4786 <= code) && (code <= 4789)) || (((4792 <= code) && (code <= 4798)) || (code === 4800))))))))))) : (((4802 <= code) && (code <= 4805)) || (((4808 <= code) && (code <= 4822)) || (((4824 <= code) && (code <= 4880)) || (((4882 <= code) && (code <= 4885)) || (((4888 <= code) && (code <= 4954)) || (((4969 <= code) && (code <= 4988)) || (((4992 <= code) && (code <= 5007)) || (((5024 <= code) && (code <= 5109)) || (((5112 <= code) && (code <= 5117)) || (((5121 <= code) && (code <= 5740)) || ((5743 <= code) && (code <= 5759))))))))))))) : ((code < 6111) ? (((5761 <= code) && (code <= 5786)) || (((5792 <= code) && (code <= 5866)) || (((5870 <= code) && (code <= 5880)) || (((5888 <= code) && (code <= 5905)) || (((5919 <= code) && (code <= 5937)) || (((5952 <= code) && (code <= 5969)) || (((5984 <= code) && (code <= 5996)) || (((5998 <= code) && (code <= 6000)) || (((6016 <= code) && (code <= 6067)) || ((code === 6103) || (code === 6108))))))))))) : (((6112 <= code) && (code <= 6121)) || (((6128 <= code) && (code <= 6137)) || (((6160 <= code) && (code <= 6169)) || (((6176 <= code) && (code <= 6264)) || (((6272 <= code) && (code <= 6276)) || (((6279 <= code) && (code <= 6312)) || ((code === 6314) || (((6320 <= code) && (code <= 6389)) || (((6400 <= code) && (code <= 6430)) || (((6470 <= code) && (code <= 6509)) || ((6512 <= code) && (code <= 6516)))))))))))))) : ((code < 7417) ? ((code < 7042) ? (((6528 <= code) && (code <= 6571)) || (((6576 <= code) && (code <= 6601)) || (((6608 <= code) && (code <= 6618)) || (((6656 <= code) && (code <= 6678)) || (((6688 <= code) && (code <= 6740)) || (((6784 <= code) && (code <= 6793)) || (((6800 <= code) && (code <= 6809)) || ((code === 6823) || (((6917 <= code) && (code <= 6963)) || (((6981 <= code) && (code <= 6988)) || ((6992 <= code) && (code <= 7001)))))))))))) : (((7043 <= code) && (code <= 7072)) || (((7086 <= code) && (code <= 7141)) || (((7168 <= code) && (code <= 7203)) || (((7232 <= code) && (code <= 7241)) || (((7245 <= code) && (code <= 7293)) || (((7296 <= code) && (code <= 7304)) || (((7312 <= code) && (code <= 7354)) || (((7357 <= code) && (code <= 7359)) || (((7401 <= code) && (code <= 7404)) || (((7406 <= code) && (code <= 7411)) || ((7413 <= code) && (code <= 7414))))))))))))) : ((code < 8129) ? ((code === 7418) || (((7424 <= code) && (code <= 7615)) || (((7680 <= code) && (code <= 7957)) || (((7960 <= code) && (code <= 7965)) || (((7968 <= code) && (code <= 8005)) || (((8008 <= code) && (code <= 8013)) || (((8016 <= code) && (code <= 8023)) || (((8032 <= code) && (code <= 8061)) || (((8064 <= code) && (code <= 8116)) || (((8118 <= code) && (code <= 8124)) || ((code === 8126) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && ((8025 <= code) && (code <= 8031)))))))))))))) : (((8130 <= code) && (code <= 8132)) || (((8134 <= code) && (code <= 8140)) || (((8144 <= code) && (code <= 8147)) || (((8150 <= code) && (code <= 8155)) || (((8160 <= code) && (code <= 8172)) || (((8178 <= code) && (code <= 8180)) || (((8182 <= code) && (code <= 8188)) || (((8304 <= code) && (code <= 8305)) || (((8308 <= code) && (code <= 8313)) || (((8319 <= code) && (code <= 8329)) || (((8336 <= code) && (code <= 8348)) || (code === 8450))))))))))))))) : ((code < 12783) ? ((code < 11647) ? ((code < 9449) ? ((code === 8455) || (((8458 <= code) && (code <= 8467)) || ((code === 8469) || (((8473 <= code) && (code <= 8477)) || (((8490 <= code) && (code <= 8493)) || (((8495 <= code) && (code <= 8505)) || (((8508 <= code) && (code <= 8511)) || (((8517 <= code) && (code <= 8521)) || ((code === 8526) || (((8528 <= code) && (code <= 8585)) || (((9312 <= code) && (code <= 9371)) || ((!A2($elm$core$Basics$modBy, 2, code)) && ((8484 <= code) && (code <= 8488)))))))))))))) : (((9450 <= code) && (code <= 9471)) || (((10102 <= code) && (code <= 10131)) || (((11264 <= code) && (code <= 11492)) || (((11499 <= code) && (code <= 11502)) || (((11506 <= code) && (code <= 11507)) || ((code === 11517) || (((11520 <= code) && (code <= 11557)) || ((code === 11559) || ((code === 11565) || (((11568 <= code) && (code <= 11623)) || (code === 11631)))))))))))) : ((code < 12320) ? (((11648 <= code) && (code <= 11670)) || (((11680 <= code) && (code <= 11686)) || (((11688 <= code) && (code <= 11694)) || (((11696 <= code) && (code <= 11702)) || (((11704 <= code) && (code <= 11710)) || (((11712 <= code) && (code <= 11718)) || (((11720 <= code) && (code <= 11726)) || (((11728 <= code) && (code <= 11734)) || (((11736 <= code) && (code <= 11742)) || ((code === 11823) || ((12293 <= code) && (code <= 12295)))))))))))) : (((12321 <= code) && (code <= 12329)) || (((12337 <= code) && (code <= 12341)) || (((12344 <= code) && (code <= 12348)) || (((12353 <= code) && (code <= 12438)) || (((12445 <= code) && (code <= 12447)) || (((12449 <= code) && (code <= 12538)) || (((12540 <= code) && (code <= 12543)) || (((12549 <= code) && (code <= 12591)) || (((12593 <= code) && (code <= 12686)) || (((12690 <= code) && (code <= 12693)) || ((12704 <= code) && (code <= 12735)))))))))))))) : ((code < 43019) ? ((code < 42559) ? (((12784 <= code) && (code <= 12799)) || (((12832 <= code) && (code <= 12841)) || (((12872 <= code) && (code <= 12879)) || (((12881 <= code) && (code <= 12895)) || (((12928 <= code) && (code <= 12937)) || (((12977 <= code) && (code <= 12991)) || (((13312 <= code) && (code <= 19903)) || (((19968 <= code) && (code <= 42124)) || (((42192 <= code) && (code <= 42237)) || (((42240 <= code) && (code <= 42508)) || ((42512 <= code) && (code <= 42539)))))))))))) : (((42560 <= code) && (code <= 42606)) || (((42623 <= code) && (code <= 42653)) || (((42656 <= code) && (code <= 42735)) || (((42775 <= code) && (code <= 42783)) || (((42786 <= code) && (code <= 42888)) || (((42891 <= code) && (code <= 42954)) || (((42960 <= code) && (code <= 42961)) || (((42966 <= code) && (code <= 42969)) || (((42994 <= code) && (code <= 43009)) || (((43011 <= code) && (code <= 43013)) || (((43015 <= code) && (code <= 43018)) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && ((42963 <= code) && (code <= 42965))))))))))))))) : ((code < 43395) ? (((43020 <= code) && (code <= 43042)) || (((43056 <= code) && (code <= 43061)) || (((43072 <= code) && (code <= 43123)) || (((43138 <= code) && (code <= 43187)) || (((43216 <= code) && (code <= 43225)) || (((43250 <= code) && (code <= 43255)) || ((code === 43259) || (((43261 <= code) && (code <= 43262)) || (((43264 <= code) && (code <= 43301)) || (((43312 <= code) && (code <= 43334)) || ((43360 <= code) && (code <= 43388)))))))))))) : (((43396 <= code) && (code <= 43442)) || (((43471 <= code) && (code <= 43481)) || (((43488 <= code) && (code <= 43492)) || (((43494 <= code) && (code <= 43518)) || (((43520 <= code) && (code <= 43560)) || (((43584 <= code) && (code <= 43586)) || (((43588 <= code) && (code <= 43595)) || (((43600 <= code) && (code <= 43609)) || (((43616 <= code) && (code <= 43638)) || ((code === 43642) || (((43646 <= code) && (code <= 43695)) || (code === 43697))))))))))))))))) : ((code < 71351) ? ((code < 67671) ? ((code < 65548) ? ((code < 64286) ? ((code < 43867) ? (((43701 <= code) && (code <= 43702)) || (((43705 <= code) && (code <= 43709)) || (((43739 <= code) && (code <= 43741)) || (((43744 <= code) && (code <= 43754)) || (((43762 <= code) && (code <= 43764)) || (((43777 <= code) && (code <= 43782)) || (((43785 <= code) && (code <= 43790)) || (((43793 <= code) && (code <= 43798)) || (((43808 <= code) && (code <= 43814)) || (((43816 <= code) && (code <= 43822)) || (((43824 <= code) && (code <= 43866)) || ((!A2($elm$core$Basics$modBy, 2, code)) && ((43712 <= code) && (code <= 43714)))))))))))))) : (((43868 <= code) && (code <= 43881)) || (((43888 <= code) && (code <= 44002)) || (((44016 <= code) && (code <= 44025)) || (((44032 <= code) && (code <= 55203)) || (((55216 <= code) && (code <= 55238)) || (((55243 <= code) && (code <= 55291)) || (((63744 <= code) && (code <= 64109)) || (((64112 <= code) && (code <= 64217)) || (((64256 <= code) && (code <= 64262)) || (((64275 <= code) && (code <= 64279)) || (code === 64285)))))))))))) : ((code < 65135) ? (((64287 <= code) && (code <= 64296)) || (((64298 <= code) && (code <= 64310)) || (((64312 <= code) && (code <= 64316)) || ((code === 64318) || (((64320 <= code) && (code <= 64321)) || (((64323 <= code) && (code <= 64324)) || (((64326 <= code) && (code <= 64433)) || (((64467 <= code) && (code <= 64829)) || (((64848 <= code) && (code <= 64911)) || (((64914 <= code) && (code <= 64967)) || ((65008 <= code) && (code <= 65019)))))))))))) : (((65136 <= code) && (code <= 65140)) || (((65142 <= code) && (code <= 65276)) || (((65296 <= code) && (code <= 65305)) || (((65313 <= code) && (code <= 65338)) || (((65345 <= code) && (code <= 65370)) || (((65382 <= code) && (code <= 65470)) || (((65474 <= code) && (code <= 65479)) || (((65482 <= code) && (code <= 65487)) || (((65490 <= code) && (code <= 65495)) || (((65498 <= code) && (code <= 65500)) || ((65536 <= code) && (code <= 65547)))))))))))))) : ((code < 66775) ? ((code < 66272) ? (((65549 <= code) && (code <= 65574)) || (((65576 <= code) && (code <= 65594)) || (((65596 <= code) && (code <= 65597)) || (((65599 <= code) && (code <= 65613)) || (((65616 <= code) && (code <= 65629)) || (((65664 <= code) && (code <= 65786)) || (((65799 <= code) && (code <= 65843)) || (((65856 <= code) && (code <= 65912)) || (((65930 <= code) && (code <= 65931)) || (((66176 <= code) && (code <= 66204)) || ((66208 <= code) && (code <= 66256)))))))))))) : (((66273 <= code) && (code <= 66299)) || (((66304 <= code) && (code <= 66339)) || (((66349 <= code) && (code <= 66378)) || (((66384 <= code) && (code <= 66421)) || (((66432 <= code) && (code <= 66461)) || (((66464 <= code) && (code <= 66499)) || (((66504 <= code) && (code <= 66511)) || (((66513 <= code) && (code <= 66517)) || (((66560 <= code) && (code <= 66717)) || (((66720 <= code) && (code <= 66729)) || ((66736 <= code) && (code <= 66771))))))))))))) : ((code < 67071) ? (((66776 <= code) && (code <= 66811)) || (((66816 <= code) && (code <= 66855)) || (((66864 <= code) && (code <= 66915)) || (((66928 <= code) && (code <= 66938)) || (((66940 <= code) && (code <= 66954)) || (((66956 <= code) && (code <= 66962)) || (((66964 <= code) && (code <= 66965)) || (((66967 <= code) && (code <= 66977)) || (((66979 <= code) && (code <= 66993)) || (((66995 <= code) && (code <= 67001)) || ((67003 <= code) && (code <= 67004)))))))))))) : (((67072 <= code) && (code <= 67382)) || (((67392 <= code) && (code <= 67413)) || (((67424 <= code) && (code <= 67431)) || (((67456 <= code) && (code <= 67461)) || (((67463 <= code) && (code <= 67504)) || (((67506 <= code) && (code <= 67514)) || (((67584 <= code) && (code <= 67589)) || ((code === 67592) || (((67594 <= code) && (code <= 67637)) || (((67639 <= code) && (code <= 67640)) || ((code === 67644) || ((67647 <= code) && (code <= 67669)))))))))))))))) : ((code < 69871) ? ((code < 68471) ? ((code < 68116) ? (((67672 <= code) && (code <= 67702)) || (((67705 <= code) && (code <= 67742)) || (((67751 <= code) && (code <= 67759)) || (((67808 <= code) && (code <= 67826)) || (((67828 <= code) && (code <= 67829)) || (((67835 <= code) && (code <= 67867)) || (((67872 <= code) && (code <= 67897)) || (((67968 <= code) && (code <= 68023)) || (((68028 <= code) && (code <= 68047)) || (((68050 <= code) && (code <= 68096)) || ((68112 <= code) && (code <= 68115)))))))))))) : (((68117 <= code) && (code <= 68119)) || (((68121 <= code) && (code <= 68149)) || (((68160 <= code) && (code <= 68168)) || (((68192 <= code) && (code <= 68222)) || (((68224 <= code) && (code <= 68255)) || (((68288 <= code) && (code <= 68295)) || (((68297 <= code) && (code <= 68324)) || (((68331 <= code) && (code <= 68335)) || (((68352 <= code) && (code <= 68405)) || (((68416 <= code) && (code <= 68437)) || ((68440 <= code) && (code <= 68466))))))))))))) : ((code < 69423) ? (((68472 <= code) && (code <= 68497)) || (((68521 <= code) && (code <= 68527)) || (((68608 <= code) && (code <= 68680)) || (((68736 <= code) && (code <= 68786)) || (((68800 <= code) && (code <= 68850)) || (((68858 <= code) && (code <= 68899)) || (((68912 <= code) && (code <= 68921)) || (((69216 <= code) && (code <= 69246)) || (((69248 <= code) && (code <= 69289)) || (((69296 <= code) && (code <= 69297)) || ((69376 <= code) && (code <= 69415)))))))))))) : (((69424 <= code) && (code <= 69445)) || (((69457 <= code) && (code <= 69460)) || (((69488 <= code) && (code <= 69505)) || (((69552 <= code) && (code <= 69579)) || (((69600 <= code) && (code <= 69622)) || (((69635 <= code) && (code <= 69687)) || (((69714 <= code) && (code <= 69743)) || (((69745 <= code) && (code <= 69746)) || ((code === 69749) || (((69763 <= code) && (code <= 69807)) || ((69840 <= code) && (code <= 69864)))))))))))))) : ((code < 70404) ? ((code < 70112) ? (((69872 <= code) && (code <= 69881)) || (((69891 <= code) && (code <= 69926)) || (((69942 <= code) && (code <= 69951)) || ((code === 69956) || ((code === 69959) || (((69968 <= code) && (code <= 70002)) || ((code === 70006) || (((70019 <= code) && (code <= 70066)) || (((70081 <= code) && (code <= 70084)) || (((70096 <= code) && (code <= 70106)) || (code === 70108))))))))))) : (((70113 <= code) && (code <= 70132)) || (((70144 <= code) && (code <= 70161)) || (((70163 <= code) && (code <= 70187)) || (((70207 <= code) && (code <= 70208)) || (((70272 <= code) && (code <= 70278)) || ((code === 70280) || (((70282 <= code) && (code <= 70285)) || (((70287 <= code) && (code <= 70301)) || (((70303 <= code) && (code <= 70312)) || (((70320 <= code) && (code <= 70366)) || ((70384 <= code) && (code <= 70393))))))))))))) : ((code < 70735) ? (((70405 <= code) && (code <= 70412)) || (((70415 <= code) && (code <= 70416)) || (((70419 <= code) && (code <= 70440)) || (((70442 <= code) && (code <= 70448)) || (((70450 <= code) && (code <= 70451)) || (((70453 <= code) && (code <= 70457)) || ((code === 70461) || ((code === 70480) || (((70493 <= code) && (code <= 70497)) || (((70656 <= code) && (code <= 70708)) || ((70727 <= code) && (code <= 70730)))))))))))) : (((70736 <= code) && (code <= 70745)) || (((70751 <= code) && (code <= 70753)) || (((70784 <= code) && (code <= 70831)) || (((70852 <= code) && (code <= 70853)) || ((code === 70855) || (((70864 <= code) && (code <= 70873)) || (((71040 <= code) && (code <= 71086)) || (((71128 <= code) && (code <= 71131)) || (((71168 <= code) && (code <= 71215)) || ((code === 71236) || (((71248 <= code) && (code <= 71257)) || ((71296 <= code) && (code <= 71338))))))))))))))))) : ((code < 119893) ? ((code < 73727) ? ((code < 72703) ? ((code < 71959) ? ((code === 71352) || (((71360 <= code) && (code <= 71369)) || (((71424 <= code) && (code <= 71450)) || (((71472 <= code) && (code <= 71483)) || (((71488 <= code) && (code <= 71494)) || (((71680 <= code) && (code <= 71723)) || (((71840 <= code) && (code <= 71922)) || (((71935 <= code) && (code <= 71942)) || ((code === 71945) || (((71948 <= code) && (code <= 71955)) || ((71957 <= code) && (code <= 71958)))))))))))) : (((71960 <= code) && (code <= 71983)) || (((72016 <= code) && (code <= 72025)) || (((72096 <= code) && (code <= 72103)) || (((72106 <= code) && (code <= 72144)) || ((code === 72192) || (((72203 <= code) && (code <= 72242)) || ((code === 72250) || ((code === 72272) || (((72284 <= code) && (code <= 72329)) || ((code === 72349) || (((72368 <= code) && (code <= 72440)) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && (((71999 <= code) && (code <= 72001)) || ((72161 <= code) && (code <= 72163)))))))))))))))) : ((code < 73062) ? (((72704 <= code) && (code <= 72712)) || (((72714 <= code) && (code <= 72750)) || ((code === 72768) || (((72784 <= code) && (code <= 72812)) || (((72818 <= code) && (code <= 72847)) || (((72960 <= code) && (code <= 72966)) || (((72968 <= code) && (code <= 72969)) || (((72971 <= code) && (code <= 73008)) || ((code === 73030) || (((73040 <= code) && (code <= 73049)) || ((73056 <= code) && (code <= 73061)))))))))))) : (((73063 <= code) && (code <= 73064)) || (((73066 <= code) && (code <= 73097)) || ((code === 73112) || (((73120 <= code) && (code <= 73129)) || (((73440 <= code) && (code <= 73458)) || ((code === 73474) || (((73476 <= code) && (code <= 73488)) || (((73490 <= code) && (code <= 73523)) || (((73552 <= code) && (code <= 73561)) || ((code === 73648) || ((73664 <= code) && (code <= 73684)))))))))))))) : ((code < 94098) ? ((code < 92863) ? (((73728 <= code) && (code <= 74649)) || (((74752 <= code) && (code <= 74862)) || (((74880 <= code) && (code <= 75075)) || (((77712 <= code) && (code <= 77808)) || (((77824 <= code) && (code <= 78895)) || (((78913 <= code) && (code <= 78918)) || (((82944 <= code) && (code <= 83526)) || (((92160 <= code) && (code <= 92728)) || (((92736 <= code) && (code <= 92766)) || (((92768 <= code) && (code <= 92777)) || ((92784 <= code) && (code <= 92862)))))))))))) : (((92864 <= code) && (code <= 92873)) || (((92880 <= code) && (code <= 92909)) || (((92928 <= code) && (code <= 92975)) || (((92992 <= code) && (code <= 92995)) || (((93008 <= code) && (code <= 93017)) || (((93019 <= code) && (code <= 93025)) || (((93027 <= code) && (code <= 93047)) || (((93053 <= code) && (code <= 93071)) || (((93760 <= code) && (code <= 93846)) || (((93952 <= code) && (code <= 94026)) || (code === 94032)))))))))))) : ((code < 110927) ? (((94099 <= code) && (code <= 94111)) || (((94176 <= code) && (code <= 94177)) || ((code === 94179) || (((94208 <= code) && (code <= 100343)) || (((100352 <= code) && (code <= 101589)) || (((101632 <= code) && (code <= 101640)) || (((110576 <= code) && (code <= 110579)) || (((110581 <= code) && (code <= 110587)) || (((110589 <= code) && (code <= 110590)) || (((110592 <= code) && (code <= 110882)) || (code === 110898))))))))))) : (((110928 <= code) && (code <= 110930)) || ((code === 110933) || (((110948 <= code) && (code <= 110951)) || (((110960 <= code) && (code <= 111355)) || (((113664 <= code) && (code <= 113770)) || (((113776 <= code) && (code <= 113788)) || (((113792 <= code) && (code <= 113800)) || (((113808 <= code) && (code <= 113817)) || (((119488 <= code) && (code <= 119507)) || (((119520 <= code) && (code <= 119539)) || (((119648 <= code) && (code <= 119672)) || ((119808 <= code) && (code <= 119892)))))))))))))))) : ((code < 124911) ? ((code < 120597) ? ((code < 120085) ? (((119894 <= code) && (code <= 119964)) || (((119966 <= code) && (code <= 119967)) || ((code === 119970) || (((119973 <= code) && (code <= 119974)) || (((119977 <= code) && (code <= 119980)) || (((119982 <= code) && (code <= 119993)) || ((code === 119995) || (((119997 <= code) && (code <= 120003)) || (((120005 <= code) && (code <= 120069)) || (((120071 <= code) && (code <= 120074)) || ((120077 <= code) && (code <= 120084)))))))))))) : (((120086 <= code) && (code <= 120092)) || (((120094 <= code) && (code <= 120121)) || (((120123 <= code) && (code <= 120126)) || (((120128 <= code) && (code <= 120132)) || ((code === 120134) || (((120138 <= code) && (code <= 120144)) || (((120146 <= code) && (code <= 120485)) || (((120488 <= code) && (code <= 120512)) || (((120514 <= code) && (code <= 120538)) || (((120540 <= code) && (code <= 120570)) || ((120572 <= code) && (code <= 120596))))))))))))) : ((code < 123135) ? (((120598 <= code) && (code <= 120628)) || (((120630 <= code) && (code <= 120654)) || (((120656 <= code) && (code <= 120686)) || (((120688 <= code) && (code <= 120712)) || (((120714 <= code) && (code <= 120744)) || (((120746 <= code) && (code <= 120770)) || (((120772 <= code) && (code <= 120779)) || (((120782 <= code) && (code <= 120831)) || (((122624 <= code) && (code <= 122654)) || (((122661 <= code) && (code <= 122666)) || ((122928 <= code) && (code <= 122989)))))))))))) : (((123136 <= code) && (code <= 123180)) || (((123191 <= code) && (code <= 123197)) || (((123200 <= code) && (code <= 123209)) || ((code === 123214) || (((123536 <= code) && (code <= 123565)) || (((123584 <= code) && (code <= 123627)) || (((123632 <= code) && (code <= 123641)) || (((124112 <= code) && (code <= 124139)) || (((124144 <= code) && (code <= 124153)) || (((124896 <= code) && (code <= 124902)) || (((124904 <= code) && (code <= 124907)) || ((124909 <= code) && (code <= 124910))))))))))))))) : ((code < 126560) ? ((code < 126463) ? (((124912 <= code) && (code <= 124926)) || (((124928 <= code) && (code <= 125124)) || (((125127 <= code) && (code <= 125135)) || (((125184 <= code) && (code <= 125251)) || ((code === 125259) || (((125264 <= code) && (code <= 125273)) || (((126065 <= code) && (code <= 126123)) || (((126125 <= code) && (code <= 126127)) || (((126129 <= code) && (code <= 126132)) || (((126209 <= code) && (code <= 126253)) || ((126255 <= code) && (code <= 126269)))))))))))) : (((126464 <= code) && (code <= 126467)) || (((126469 <= code) && (code <= 126495)) || (((126497 <= code) && (code <= 126498)) || ((code === 126500) || ((code === 126503) || (((126505 <= code) && (code <= 126514)) || (((126516 <= code) && (code <= 126519)) || ((code === 126530) || (((126541 <= code) && (code <= 126543)) || (((126545 <= code) && (code <= 126546)) || ((code === 126548) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && (((126521 <= code) && (code <= 126523)) || (((126535 <= code) && (code <= 126539)) || ((126551 <= code) && (code <= 126559))))))))))))))))) : ((code < 126634) ? (((126561 <= code) && (code <= 126562)) || ((code === 126564) || (((126567 <= code) && (code <= 126570)) || (((126572 <= code) && (code <= 126578)) || (((126580 <= code) && (code <= 126583)) || (((126585 <= code) && (code <= 126588)) || ((code === 126590) || (((126592 <= code) && (code <= 126601)) || (((126603 <= code) && (code <= 126619)) || (((126625 <= code) && (code <= 126627)) || ((126629 <= code) && (code <= 126633)))))))))))) : (((126635 <= code) && (code <= 126651)) || (((127232 <= code) && (code <= 127244)) || (((130032 <= code) && (code <= 130041)) || (((131072 <= code) && (code <= 173791)) || (((173824 <= code) && (code <= 177977)) || (((177984 <= code) && (code <= 178205)) || (((178208 <= code) && (code <= 183969)) || (((183984 <= code) && (code <= 191456)) || (((191472 <= code) && (code <= 192093)) || (((194560 <= code) && (code <= 195101)) || (((196608 <= code) && (code <= 201546)) || ((201552 <= code) && (code <= 205743))))))))))))))))))))))));
};
var $elm$core$String$toLower = _String_toLower;
var $stil4m$elm_syntax$Char$Extra$unicodeIsLowerFast = function (c) {
	var code = $elm$core$Char$toCode(c);
	var cString = $elm$core$String$fromChar(c);
	return $stil4m$elm_syntax$Char$Extra$charCodeIsLower(code) || ((_Utils_eq(
		$elm$core$String$toLower(cString),
		cString + '') && (!_Utils_eq(
		$elm$core$String$toUpper(cString),
		cString + ''))) ? ((code <= 836) || (((838 <= code) && (code <= 8559)) || (((8576 <= code) && (code <= 9423)) || ((9450 <= code) && (code <= 983040))))) : ((code < 43001) ? ((code < 8457) ? ((code < 590) ? (((311 <= code) && (code <= 312)) || (((396 <= code) && (code <= 397)) || (((409 <= code) && (code <= 411)) || (((426 <= code) && (code <= 427)) || (((441 <= code) && (code <= 442)) || (((445 <= code) && (code <= 447)) || ((code === 545) || ((563 <= code) && (code <= 569))))))))) : (((591 <= code) && (code <= 659)) || (((661 <= code) && (code <= 687)) || (((1019 <= code) && (code <= 1020)) || (((1376 <= code) && (code <= 1416)) || (((7424 <= code) && (code <= 7467)) || (((7531 <= code) && (code <= 7543)) || (((7545 <= code) && (code <= 7578)) || (((7829 <= code) && (code <= 7837)) || (code === 7839)))))))))) : ((code < 11376) ? ((code === 8458) || (((8462 <= code) && (code <= 8463)) || ((code === 8467) || ((code === 8495) || ((code === 8500) || ((code === 8505) || (((8508 <= code) && (code <= 8509)) || ((8518 <= code) && (code <= 8521))))))))) : ((code === 11377) || (((11379 <= code) && (code <= 11380)) || (((11382 <= code) && (code <= 11387)) || (((11491 <= code) && (code <= 11492)) || (((42799 <= code) && (code <= 42801)) || (((42865 <= code) && (code <= 42872)) || ((code === 42894) || (((42899 <= code) && (code <= 42901)) || ((code === 42927) || ((A2($elm$core$Basics$modBy, 2, code) === 1) && ((42963 <= code) && (code <= 42965)))))))))))))) : ((code < 120353) ? ((code < 119994) ? ((code === 43002) || (((43824 <= code) && (code <= 43866)) || (((43872 <= code) && (code <= 43880)) || (((119834 <= code) && (code <= 119859)) || (((119886 <= code) && (code <= 119892)) || (((119894 <= code) && (code <= 119911)) || (((119938 <= code) && (code <= 119963)) || ((119990 <= code) && (code <= 119993))))))))) : ((code === 119995) || (((119997 <= code) && (code <= 120003)) || (((120005 <= code) && (code <= 120015)) || (((120042 <= code) && (code <= 120067)) || (((120094 <= code) && (code <= 120119)) || (((120146 <= code) && (code <= 120171)) || (((120198 <= code) && (code <= 120223)) || (((120250 <= code) && (code <= 120275)) || ((120302 <= code) && (code <= 120327))))))))))) : ((code < 120655) ? (((120354 <= code) && (code <= 120379)) || (((120406 <= code) && (code <= 120431)) || (((120458 <= code) && (code <= 120485)) || (((120514 <= code) && (code <= 120538)) || (((120540 <= code) && (code <= 120545)) || (((120572 <= code) && (code <= 120596)) || (((120598 <= code) && (code <= 120603)) || ((120630 <= code) && (code <= 120654))))))))) : (((120656 <= code) && (code <= 120661)) || (((120688 <= code) && (code <= 120712)) || (((120714 <= code) && (code <= 120719)) || (((120746 <= code) && (code <= 120770)) || (((120772 <= code) && (code <= 120777)) || ((code === 120779) || (((122624 <= code) && (code <= 122633)) || (((122635 <= code) && (code <= 122654)) || ((122661 <= code) && (code <= 122666))))))))))))));
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode = A4($stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateMapWithRangeWithoutLinebreak, $stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Char$Extra$unicodeIsLowerFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast, $stil4m$elm_syntax$Elm$Parser$Tokens$isNotReserved);
var $elm$core$Basics$ge = _Utils_ge;
var $stil4m$elm_syntax$Elm$Syntax$Expression$GLSLExpression = function (a) {
	return {$: 'GLSLExpression', a: a};
};
var $stil4m$elm_syntax$ParserFast$loopUntilHelp = F7(
	function (committedSoFar, endParser, element, soFar, reduce, foldedToRes, s0) {
		loopUntilHelp:
		while (true) {
			var parseEnd = endParser.a;
			var parseElement = element.a;
			var _v0 = parseEnd(s0);
			if (_v0.$ === 'Good') {
				var s1 = _v0.b;
				return A2(
					$stil4m$elm_syntax$ParserFast$Good,
					foldedToRes(soFar),
					s1);
			} else {
				var endCommitted = _v0.a;
				var endX = _v0.b;
				if (endCommitted) {
					return A2($stil4m$elm_syntax$ParserFast$Bad, true, endX);
				} else {
					var _v1 = parseElement(s0);
					if (_v1.$ === 'Good') {
						var elementResult = _v1.a;
						var s1 = _v1.b;
						var $temp$committedSoFar = true,
							$temp$endParser = endParser,
							$temp$element = element,
							$temp$soFar = A2(reduce, elementResult, soFar),
							$temp$reduce = reduce,
							$temp$foldedToRes = foldedToRes,
							$temp$s0 = s1;
						committedSoFar = $temp$committedSoFar;
						endParser = $temp$endParser;
						element = $temp$element;
						soFar = $temp$soFar;
						reduce = $temp$reduce;
						foldedToRes = $temp$foldedToRes;
						s0 = $temp$s0;
						continue loopUntilHelp;
					} else {
						var elementCommitted = _v1.a;
						var x = _v1.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, committedSoFar || elementCommitted, x);
					}
				}
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$loopUntil = F5(
	function (endParser, element, initialFolded, reduce, foldedToRes) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				return A7($stil4m$elm_syntax$ParserFast$loopUntilHelp, false, endParser, element, initialFolded, reduce, foldedToRes, s);
			});
	});
var $stil4m$elm_syntax$ParserFast$mapWithRange = F2(
	function (combineStartAndResult, _v0) {
		var parse = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var a = _v1.a;
					var s1 = _v1.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A2(
							combineStartAndResult,
							{
								end: {column: s1.col, row: s1.row},
								start: {column: s0.col, row: s0.row}
							},
							a),
						s1);
				} else {
					var committed = _v1.a;
					var x = _v1.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$glslExpressionAfterOpeningSquareBracket = A2(
	$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
	'glsl|',
	A2(
		$stil4m$elm_syntax$ParserFast$mapWithRange,
		F2(
			function (range, s) {
				return {
					comments: $stil4m$elm_syntax$Rope$empty,
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						{
							end: {column: range.end.column + 2, row: range.end.row},
							start: {column: range.start.column - 6, row: range.start.row}
						},
						$stil4m$elm_syntax$Elm$Syntax$Expression$GLSLExpression(s))
				};
			}),
		A5(
			$stil4m$elm_syntax$ParserFast$loopUntil,
			A2($stil4m$elm_syntax$ParserFast$symbol, '|]', _Utils_Tuple0),
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				A2($stil4m$elm_syntax$ParserFast$symbol, '|', '|'),
				$stil4m$elm_syntax$ParserFast$while(
					function (c) {
						return !_Utils_eq(
							c,
							_Utils_chr('|'));
					})),
			'',
			F2(
				function (extension, soFar) {
					return soFar + (extension + '');
				}),
			$elm$core$Basics$identity)));
var $stil4m$elm_syntax$ParserFast$ExpectingKeyword = F3(
	function (a, b, c) {
		return {$: 'ExpectingKeyword', a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$Char$Extra$isLatinAlphaNumOrUnderscoreFast = function (c) {
	var code = $elm$core$Char$toCode(c);
	return $stil4m$elm_syntax$Char$Extra$charCodeIsLower(code) || ($stil4m$elm_syntax$Char$Extra$charCodeIsUpper(code) || ($stil4m$elm_syntax$Char$Extra$charCodeIsDigit(code) || (code === 95)));
};
var $stil4m$elm_syntax$ParserFast$isSubCharAlphaNumOrUnderscore = F2(
	function (offset, string) {
		return A2(
			$elm$core$String$any,
			$stil4m$elm_syntax$Char$Extra$isLatinAlphaNumOrUnderscoreFast,
			A3($elm$core$String$slice, offset, offset + 1, string));
	});
var $stil4m$elm_syntax$ParserFast$keyword = F2(
	function (kwd, res) {
		var kwdLength = $elm$core$String$length(kwd);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + kwdLength;
				return (_Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					kwd + '') && (!A2($stil4m$elm_syntax$ParserFast$isSubCharAlphaNumOrUnderscore, newOffset, s.src))) ? A2(
					$stil4m$elm_syntax$ParserFast$Good,
					res,
					{col: s.col + kwdLength, indent: s.indent, offset: newOffset, row: s.row, src: s.src}) : A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A3($stil4m$elm_syntax$ParserFast$ExpectingKeyword, s.row, s.col, kwd));
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$inToken = A2($stil4m$elm_syntax$ParserFast$keyword, 'in', _Utils_Tuple0);
var $stil4m$elm_syntax$ParserFast$keywordFollowedBy = F2(
	function (kwd, _v0) {
		var parseNext = _v0.a;
		var kwdLength = $elm$core$String$length(kwd);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + kwdLength;
				return (_Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					kwd + '') && (!A2($stil4m$elm_syntax$ParserFast$isSubCharAlphaNumOrUnderscore, newOffset, s.src))) ? $stil4m$elm_syntax$ParserFast$pStepCommit(
					parseNext(
						{col: s.col + kwdLength, indent: s.indent, offset: newOffset, row: s.row, src: s.src})) : A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A3($stil4m$elm_syntax$ParserFast$ExpectingKeyword, s.row, s.col, kwd));
			});
	});
var $stil4m$elm_syntax$ParserFast$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var committed = _v2.a;
					var x = _v2.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v2.a;
					var s1 = _v2.b;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var x = _v3.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v3.a;
						var s2 = _v3.b;
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$columnIndentAndThen = function (callback) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s) {
			var _v0 = A2(callback, s.col, s.indent);
			var parse = _v0.a;
			return parse(s);
		});
};
var $stil4m$elm_syntax$ParserFast$ExpectingCustom = F3(
	function (a, b, c) {
		return {$: 'ExpectingCustom', a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$ParserFast$problem = function (msg) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s) {
			return A2(
				$stil4m$elm_syntax$ParserFast$Bad,
				false,
				A3($stil4m$elm_syntax$ParserFast$ExpectingCustom, s.row, s.col, msg));
		});
};
var $stil4m$elm_syntax$Elm$Parser$Layout$problemTopIndentation = $stil4m$elm_syntax$ParserFast$problem('must be on top indentation');
var $stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy = function (nextParser) {
	return $stil4m$elm_syntax$ParserFast$columnIndentAndThen(
		F2(
			function (column, indent) {
				return (!(column - indent)) ? nextParser : $stil4m$elm_syntax$Elm$Parser$Layout$problemTopIndentation;
			}));
};
var $stil4m$elm_syntax$ParserFast$skipWhileWhitespaceHelp = F5(
	function (offset, row, col, src, indent) {
		skipWhileWhitespaceHelp:
		while (true) {
			var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
			switch (_v0) {
				case ' ':
					var $temp$offset = offset + 1,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileWhitespaceHelp;
				case '\n':
					var $temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileWhitespaceHelp;
				case '\u000D':
					var $temp$offset = offset + 1,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					continue skipWhileWhitespaceHelp;
				default:
					return {col: col, indent: indent, offset: offset, row: row, src: src};
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$followedBySkipWhileWhitespace = function (_v0) {
	var parseBefore = _v0.a;
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var _v1 = parseBefore(s0);
			if (_v1.$ === 'Good') {
				var res = _v1.a;
				var s1 = _v1.b;
				var s2 = A5($stil4m$elm_syntax$ParserFast$skipWhileWhitespaceHelp, s1.offset, s1.row, s1.col, s1.src, s1.indent);
				return A2($stil4m$elm_syntax$ParserFast$Good, res, s2);
			} else {
				var bad = _v1;
				return bad;
			}
		});
};
var $stil4m$elm_syntax$ParserFast$map2OrSucceed = F4(
	function (func, _v0, _v1, fallback) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var c1 = _v2.a;
					var x = _v2.b;
					return c1 ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2($stil4m$elm_syntax$ParserFast$Good, fallback, s0);
				} else {
					var a = _v2.a;
					var s1 = _v2.b;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var x = _v3.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v3.a;
						var s2 = _v3.b;
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$offsetSourceAndThen = function (callback) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s) {
			var _v0 = A2(callback, s.offset, s.src);
			var parse = _v0.a;
			return parse(s);
		});
};
var $stil4m$elm_syntax$Elm$Parser$Comments$problemUnexpectedDocumentation = $stil4m$elm_syntax$ParserFast$problem('unexpected documentation comment');
var $stil4m$elm_syntax$Elm$Parser$Comments$multilineComment = $stil4m$elm_syntax$ParserFast$offsetSourceAndThen(
	F2(
		function (offset, source) {
			var _v0 = A3($elm$core$String$slice, offset + 2, offset + 3, source);
			if (_v0 === '|') {
				return $stil4m$elm_syntax$Elm$Parser$Comments$problemUnexpectedDocumentation;
			} else {
				return $stil4m$elm_syntax$Elm$Parser$Comments$multiLineCommentNoCheck;
			}
		}));
var $stil4m$elm_syntax$Rope$Leaf = F2(
	function (a, b) {
		return {$: 'Leaf', a: a, b: b};
	});
var $stil4m$elm_syntax$Rope$one = function (onlyElement) {
	return A2($stil4m$elm_syntax$Rope$Leaf, onlyElement, _Utils_Tuple0);
};
var $stil4m$elm_syntax$ParserFast$loopWhileSucceedsHelp = F5(
	function (element, soFar, reduce, foldedToRes, s0) {
		loopWhileSucceedsHelp:
		while (true) {
			var parseElement = element.a;
			var _v0 = parseElement(s0);
			if (_v0.$ === 'Good') {
				var elementResult = _v0.a;
				var s1 = _v0.b;
				var $temp$element = element,
					$temp$soFar = A2(reduce, elementResult, soFar),
					$temp$reduce = reduce,
					$temp$foldedToRes = foldedToRes,
					$temp$s0 = s1;
				element = $temp$element;
				soFar = $temp$soFar;
				reduce = $temp$reduce;
				foldedToRes = $temp$foldedToRes;
				s0 = $temp$s0;
				continue loopWhileSucceedsHelp;
			} else {
				var elementCommitted = _v0.a;
				var x = _v0.b;
				return elementCommitted ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2(
					$stil4m$elm_syntax$ParserFast$Good,
					foldedToRes(soFar),
					s0);
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$loopWhileSucceeds = F4(
	function (element, initialFolded, reduce, foldedToRes) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				return A5($stil4m$elm_syntax$ParserFast$loopWhileSucceedsHelp, element, initialFolded, reduce, foldedToRes, s);
			});
	});
var $stil4m$elm_syntax$Rope$prependToFilled = F2(
	function (rightLikelyFilled, left) {
		if (left.$ === 'Nothing') {
			return $elm$core$Maybe$Just(rightLikelyFilled);
		} else {
			var leftLikelyFilled = left.a;
			return $elm$core$Maybe$Just(
				A2($stil4m$elm_syntax$Rope$Branch2, leftLikelyFilled, rightLikelyFilled));
		}
	});
var $stil4m$elm_syntax$ParserFast$whileMapWithRange = F2(
	function (isGood, rangeAndConsumedStringToRes) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileHelp, isGood, s0.offset, s0.row, s0.col, s0.src, s0.indent);
				return A2(
					$stil4m$elm_syntax$ParserFast$Good,
					A2(
						rangeAndConsumedStringToRes,
						{
							end: {column: s1.col, row: s1.row},
							start: {column: s0.col, row: s0.row}
						},
						A3($elm$core$String$slice, s0.offset, s1.offset, s0.src)),
					s1);
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Comments$singleLineComment = A2(
	$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
	'--',
	A2(
		$stil4m$elm_syntax$ParserFast$whileMapWithRange,
		function (c) {
			return (!_Utils_eq(
				c,
				_Utils_chr('\u000D'))) && ((!_Utils_eq(
				c,
				_Utils_chr('\n'))) && (!$stil4m$elm_syntax$Char$Extra$isUtf16Surrogate(c)));
		},
		F2(
			function (range, content) {
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					{
						end: {column: range.end.column, row: range.start.row},
						start: {column: range.start.column - 2, row: range.start.row}
					},
					'--' + content);
			})));
var $stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmptyLoop = A4(
	$stil4m$elm_syntax$ParserFast$loopWhileSucceeds,
	$stil4m$elm_syntax$ParserFast$followedBySkipWhileWhitespace(
		A2($stil4m$elm_syntax$ParserFast$oneOf2, $stil4m$elm_syntax$Elm$Parser$Comments$singleLineComment, $stil4m$elm_syntax$Elm$Parser$Comments$multilineComment)),
	$stil4m$elm_syntax$Rope$empty,
	F2(
		function (right, soFar) {
			return A2(
				$stil4m$elm_syntax$Rope$prependToFilled,
				$stil4m$elm_syntax$Rope$one(right),
				soFar);
		}),
	$elm$core$Basics$identity);
var $stil4m$elm_syntax$Elm$Parser$Layout$fromMultilineCommentNodeOrEmptyOnProblem = A4(
	$stil4m$elm_syntax$ParserFast$map2OrSucceed,
	F2(
		function (comment, commentsAfter) {
			return A2(
				$stil4m$elm_syntax$Rope$filledPrependTo,
				commentsAfter,
				$stil4m$elm_syntax$Rope$one(comment));
		}),
	$stil4m$elm_syntax$ParserFast$followedBySkipWhileWhitespace($stil4m$elm_syntax$Elm$Parser$Comments$multilineComment),
	$stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmptyLoop,
	$stil4m$elm_syntax$Rope$empty);
var $stil4m$elm_syntax$Elm$Parser$Layout$fromSingleLineCommentNode = A3(
	$stil4m$elm_syntax$ParserFast$map2,
	F2(
		function (content, commentsAfter) {
			return A2(
				$stil4m$elm_syntax$Rope$filledPrependTo,
				commentsAfter,
				$stil4m$elm_syntax$Rope$one(content));
		}),
	$stil4m$elm_syntax$ParserFast$followedBySkipWhileWhitespace($stil4m$elm_syntax$Elm$Parser$Comments$singleLineComment),
	$stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmptyLoop);
var $stil4m$elm_syntax$ParserFast$offsetSourceAndThenOrSucceed = F2(
	function (callback, fallback) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v0 = A2(callback, s.offset, s.src);
				if (_v0.$ === 'Nothing') {
					return A2($stil4m$elm_syntax$ParserFast$Good, fallback, s);
				} else {
					var parse = _v0.a.a;
					return parse(s);
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$skipWhileWhitespaceFollowedBy = function (_v0) {
	var parseNext = _v0.a;
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var s1 = A5($stil4m$elm_syntax$ParserFast$skipWhileWhitespaceHelp, s0.offset, s0.row, s0.col, s0.src, s0.indent);
			return $stil4m$elm_syntax$ParserFast$pStepCommit(
				parseNext(s1));
		});
};
var $stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmpty = $stil4m$elm_syntax$ParserFast$skipWhileWhitespaceFollowedBy(
	A2(
		$stil4m$elm_syntax$ParserFast$offsetSourceAndThenOrSucceed,
		F2(
			function (offset, source) {
				var _v0 = A3($elm$core$String$slice, offset, offset + 2, source);
				switch (_v0) {
					case '--':
						return $elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Parser$Layout$fromSingleLineCommentNode);
					case '{-':
						return $elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Parser$Layout$fromMultilineCommentNodeOrEmptyOnProblem);
					default:
						return $elm$core$Maybe$Nothing;
				}
			}),
		$stil4m$elm_syntax$Rope$empty));
var $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout = $stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmpty;
var $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedBy = function (nextParser) {
	return A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (commentsBefore, after) {
				return {comments: commentsBefore, syntax: after};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(nextParser));
};
var $stil4m$elm_syntax$ParserFast$lazy = function (thunk) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s) {
			var _v0 = thunk(_Utils_Tuple0);
			var parse = _v0.a;
			return parse(s);
		});
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Literal = function (a) {
	return {$: 'Literal', a: a};
};
var $stil4m$elm_syntax$ParserFast$whileWithoutLinebreak = function (isGood) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, isGood, s0.offset, s0.row, s0.col, s0.src, s0.indent);
			return A2(
				$stil4m$elm_syntax$ParserFast$Good,
				A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
				s1);
		});
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$singleQuotedStringLiteralAfterDoubleQuote = A5(
	$stil4m$elm_syntax$ParserFast$loopUntil,
	A2($stil4m$elm_syntax$ParserFast$symbol, '\"', _Utils_Tuple0),
	A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'\\',
			$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValueMap($elm$core$String$fromChar)),
		$stil4m$elm_syntax$ParserFast$whileWithoutLinebreak(
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('\"'))) && ((!_Utils_eq(
					c,
					_Utils_chr('\\'))) && (!$stil4m$elm_syntax$Char$Extra$isUtf16Surrogate(c)));
			})),
	'',
	F2(
		function (extension, soFar) {
			return soFar + (extension + '');
		}),
	$elm$core$Basics$identity);
var $stil4m$elm_syntax$Elm$Parser$Tokens$tripleQuotedStringLiteralOfterTripleDoubleQuote = A5(
	$stil4m$elm_syntax$ParserFast$loopUntil,
	A2($stil4m$elm_syntax$ParserFast$symbol, '\"\"\"', _Utils_Tuple0),
	A3(
		$stil4m$elm_syntax$ParserFast$oneOf3,
		A2($stil4m$elm_syntax$ParserFast$symbol, '\"', '\"'),
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'\\',
			$stil4m$elm_syntax$Elm$Parser$Tokens$escapedCharValueMap($elm$core$String$fromChar)),
		$stil4m$elm_syntax$ParserFast$while(
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('\"'))) && ((!_Utils_eq(
					c,
					_Utils_chr('\\'))) && (!$stil4m$elm_syntax$Char$Extra$isUtf16Surrogate(c)));
			})),
	'',
	F2(
		function (extension, soFar) {
			return soFar + (extension + '');
		}),
	$elm$core$Basics$identity);
var $stil4m$elm_syntax$Elm$Parser$Tokens$singleOrTripleQuotedStringLiteralMapWithRange = function (rangeAndStringToRes) {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'\"',
		A4(
			$stil4m$elm_syntax$ParserFast$oneOf2MapWithStartRowColumnAndEndRowColumn,
			F5(
				function (startRow, startColumn, string, endRow, endColumn) {
					return A2(
						rangeAndStringToRes,
						{
							end: {column: endColumn, row: endRow},
							start: {column: startColumn - 1, row: startRow}
						},
						string);
				}),
			A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '\"\"', $stil4m$elm_syntax$Elm$Parser$Tokens$tripleQuotedStringLiteralOfterTripleDoubleQuote),
			F5(
				function (startRow, startColumn, string, endRow, endColumn) {
					return A2(
						rangeAndStringToRes,
						{
							end: {column: endColumn, row: endRow},
							start: {column: startColumn - 1, row: startRow}
						},
						string);
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$singleQuotedStringLiteralAfterDoubleQuote));
};
var $stil4m$elm_syntax$Elm$Parser$Expression$literalExpression = $stil4m$elm_syntax$Elm$Parser$Tokens$singleOrTripleQuotedStringLiteralMapWithRange(
	F2(
		function (range, string) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Expression$Literal(string))
			};
		}));
var $stil4m$elm_syntax$ParserFast$loopWhileSucceedsOntoResultFromParser = F4(
	function (element, _v0, reduce, foldedToRes) {
		var parseInitialFolded = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parseInitialFolded(s0);
				if (_v1.$ === 'Good') {
					var initialFolded = _v1.a;
					var s1 = _v1.b;
					return A5($stil4m$elm_syntax$ParserFast$loopWhileSucceedsHelp, element, initialFolded, reduce, foldedToRes, s1);
				} else {
					var committed = _v1.a;
					var x = _v1.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				}
			});
	});
var $stil4m$elm_syntax$Rope$prependTo = F2(
	function (right, left) {
		if (left.$ === 'Nothing') {
			return right;
		} else {
			var leftLikelyFilled = left.a;
			if (right.$ === 'Nothing') {
				return left;
			} else {
				var rightLikelyFilled = right.a;
				return $elm$core$Maybe$Just(
					A2($stil4m$elm_syntax$Rope$Branch2, leftLikelyFilled, rightLikelyFilled));
			}
		}
	});
var $stil4m$elm_syntax$ParserWithComments$many = function (p) {
	return A4(
		$stil4m$elm_syntax$ParserFast$loopWhileSucceeds,
		p,
		_Utils_Tuple2($stil4m$elm_syntax$Rope$empty, _List_Nil),
		F2(
			function (pResult, _v0) {
				var commentsSoFar = _v0.a;
				var itemsSoFar = _v0.b;
				return _Utils_Tuple2(
					A2($stil4m$elm_syntax$Rope$prependTo, pResult.comments, commentsSoFar),
					A2($elm$core$List$cons, pResult.syntax, itemsSoFar));
			}),
		function (_v1) {
			var commentsSoFar = _v1.a;
			var itemsSoFar = _v1.b;
			return {
				comments: commentsSoFar,
				syntax: $elm$core$List$reverse(itemsSoFar)
			};
		});
};
var $stil4m$elm_syntax$ParserWithComments$manyWithoutReverse = function (p) {
	return A4(
		$stil4m$elm_syntax$ParserFast$loopWhileSucceeds,
		p,
		_Utils_Tuple2($stil4m$elm_syntax$Rope$empty, _List_Nil),
		F2(
			function (pResult, _v0) {
				var commentsSoFar = _v0.a;
				var itemsSoFar = _v0.b;
				return _Utils_Tuple2(
					A2($stil4m$elm_syntax$Rope$prependTo, pResult.comments, commentsSoFar),
					A2($elm$core$List$cons, pResult.syntax, itemsSoFar));
			}),
		function (_v1) {
			var commentsSoFar = _v1.a;
			var itemsSoFar = _v1.b;
			return {comments: commentsSoFar, syntax: itemsSoFar};
		});
};
var $stil4m$elm_syntax$ParserFast$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var a = _v1.a;
					var s1 = _v1.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						func(a),
						s1);
				} else {
					var committed = _v1.a;
					var x = _v1.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v3 = parseA(s0);
				if (_v3.$ === 'Bad') {
					var committed = _v3.a;
					var x = _v3.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v3.a;
					var s1 = _v3.b;
					var _v4 = parseB(s1);
					if (_v4.$ === 'Bad') {
						var x = _v4.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v4.a;
						var s2 = _v4.b;
						var _v5 = parseC(s2);
						if (_v5.$ === 'Bad') {
							var x = _v5.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v5.a;
							var s3 = _v5.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A3(func, a, b, c),
								s3);
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map3WithStartLocation = F4(
	function (func, _v0, _v1, _v2) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v3 = parseA(s0);
				if (_v3.$ === 'Bad') {
					var committed = _v3.a;
					var x = _v3.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v3.a;
					var s1 = _v3.b;
					var _v4 = parseB(s1);
					if (_v4.$ === 'Bad') {
						var x = _v4.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v4.a;
						var s2 = _v4.b;
						var _v5 = parseC(s2);
						if (_v5.$ === 'Bad') {
							var x = _v5.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v5.a;
							var s3 = _v5.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A4(
									func,
									{column: s0.col, row: s0.row},
									a,
									b,
									c),
								s3);
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map4 = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v4 = parseA(s0);
				if (_v4.$ === 'Bad') {
					var committed = _v4.a;
					var x = _v4.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v4.a;
					var s1 = _v4.b;
					var _v5 = parseB(s1);
					if (_v5.$ === 'Bad') {
						var x = _v5.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v5.a;
						var s2 = _v5.b;
						var _v6 = parseC(s2);
						if (_v6.$ === 'Bad') {
							var x = _v6.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v6.a;
							var s3 = _v6.b;
							var _v7 = parseD(s3);
							if (_v7.$ === 'Bad') {
								var x = _v7.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v7.a;
								var s4 = _v7.b;
								return A2(
									$stil4m$elm_syntax$ParserFast$Good,
									A4(func, a, b, c, d),
									s4);
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map4OrSucceed = F6(
	function (func, _v0, _v1, _v2, _v3, fallback) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v4 = parseA(s0);
				if (_v4.$ === 'Bad') {
					var c1 = _v4.a;
					var x = _v4.b;
					return c1 ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2($stil4m$elm_syntax$ParserFast$Good, fallback, s0);
				} else {
					var a = _v4.a;
					var s1 = _v4.b;
					var _v5 = parseB(s1);
					if (_v5.$ === 'Bad') {
						var x = _v5.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v5.a;
						var s2 = _v5.b;
						var _v6 = parseC(s2);
						if (_v6.$ === 'Bad') {
							var x = _v6.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v6.a;
							var s3 = _v6.b;
							var _v7 = parseD(s3);
							if (_v7.$ === 'Bad') {
								var x = _v7.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v7.a;
								var s4 = _v7.b;
								return A2(
									$stil4m$elm_syntax$ParserFast$Good,
									A4(func, a, b, c, d),
									s4);
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map4WithRange = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v4 = parseA(s0);
				if (_v4.$ === 'Bad') {
					var committed = _v4.a;
					var x = _v4.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v4.a;
					var s1 = _v4.b;
					var _v5 = parseB(s1);
					if (_v5.$ === 'Bad') {
						var x = _v5.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v5.a;
						var s2 = _v5.b;
						var _v6 = parseC(s2);
						if (_v6.$ === 'Bad') {
							var x = _v6.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v6.a;
							var s3 = _v6.b;
							var _v7 = parseD(s3);
							if (_v7.$ === 'Bad') {
								var x = _v7.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v7.a;
								var s4 = _v7.b;
								return A2(
									$stil4m$elm_syntax$ParserFast$Good,
									A5(
										func,
										{
											end: {column: s4.col, row: s4.row},
											start: {column: s0.col, row: s0.row}
										},
										a,
										b,
										c,
										d),
									s4);
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map5 = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v5 = parseA(s0);
				if (_v5.$ === 'Bad') {
					var committed = _v5.a;
					var x = _v5.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v5.a;
					var s1 = _v5.b;
					var _v6 = parseB(s1);
					if (_v6.$ === 'Bad') {
						var x = _v6.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v6.a;
						var s2 = _v6.b;
						var _v7 = parseC(s2);
						if (_v7.$ === 'Bad') {
							var x = _v7.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v7.a;
							var s3 = _v7.b;
							var _v8 = parseD(s3);
							if (_v8.$ === 'Bad') {
								var x = _v8.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v8.a;
								var s4 = _v8.b;
								var _v9 = parseE(s4);
								if (_v9.$ === 'Bad') {
									var x = _v9.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v9.a;
									var s5 = _v9.b;
									return A2(
										$stil4m$elm_syntax$ParserFast$Good,
										A5(func, a, b, c, d, e),
										s5);
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map5WithRange = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v5 = parseA(s0);
				if (_v5.$ === 'Bad') {
					var committed = _v5.a;
					var x = _v5.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v5.a;
					var s1 = _v5.b;
					var _v6 = parseB(s1);
					if (_v6.$ === 'Bad') {
						var x = _v6.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v6.a;
						var s2 = _v6.b;
						var _v7 = parseC(s2);
						if (_v7.$ === 'Bad') {
							var x = _v7.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v7.a;
							var s3 = _v7.b;
							var _v8 = parseD(s3);
							if (_v8.$ === 'Bad') {
								var x = _v8.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v8.a;
								var s4 = _v8.b;
								var _v9 = parseE(s4);
								if (_v9.$ === 'Bad') {
									var x = _v9.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v9.a;
									var s5 = _v9.b;
									return A2(
										$stil4m$elm_syntax$ParserFast$Good,
										A6(
											func,
											{
												end: {column: s5.col, row: s5.row},
												start: {column: s0.col, row: s0.row}
											},
											a,
											b,
											c,
											d,
											e),
										s5);
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map5WithStartLocation = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v5 = parseA(s0);
				if (_v5.$ === 'Bad') {
					var committed = _v5.a;
					var x = _v5.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v5.a;
					var s1 = _v5.b;
					var _v6 = parseB(s1);
					if (_v6.$ === 'Bad') {
						var x = _v6.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v6.a;
						var s2 = _v6.b;
						var _v7 = parseC(s2);
						if (_v7.$ === 'Bad') {
							var x = _v7.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v7.a;
							var s3 = _v7.b;
							var _v8 = parseD(s3);
							if (_v8.$ === 'Bad') {
								var x = _v8.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v8.a;
								var s4 = _v8.b;
								var _v9 = parseE(s4);
								if (_v9.$ === 'Bad') {
									var x = _v9.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v9.a;
									var s5 = _v9.b;
									return A2(
										$stil4m$elm_syntax$ParserFast$Good,
										A6(
											func,
											{column: s0.col, row: s0.row},
											a,
											b,
											c,
											d,
											e),
										s5);
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map6WithStartLocation = F7(
	function (func, _v0, _v1, _v2, _v3, _v4, _v5) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		var parseF = _v5.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v6 = parseA(s0);
				if (_v6.$ === 'Bad') {
					var committed = _v6.a;
					var x = _v6.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v6.a;
					var s1 = _v6.b;
					var _v7 = parseB(s1);
					if (_v7.$ === 'Bad') {
						var x = _v7.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v7.a;
						var s2 = _v7.b;
						var _v8 = parseC(s2);
						if (_v8.$ === 'Bad') {
							var x = _v8.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v8.a;
							var s3 = _v8.b;
							var _v9 = parseD(s3);
							if (_v9.$ === 'Bad') {
								var x = _v9.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v9.a;
								var s4 = _v9.b;
								var _v10 = parseE(s4);
								if (_v10.$ === 'Bad') {
									var x = _v10.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v10.a;
									var s5 = _v10.b;
									var _v11 = parseF(s5);
									if (_v11.$ === 'Bad') {
										var x = _v11.b;
										return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
									} else {
										var f = _v11.a;
										var s6 = _v11.b;
										return A2(
											$stil4m$elm_syntax$ParserFast$Good,
											A7(
												func,
												{column: s0.col, row: s0.row},
												a,
												b,
												c,
												d,
												e,
												f),
											s6);
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map8WithStartLocation = F9(
	function (func, _v0, _v1, _v2, _v3, _v4, _v5, _v6, _v7) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		var parseF = _v5.a;
		var parseG = _v6.a;
		var parseH = _v7.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v8 = parseA(s0);
				if (_v8.$ === 'Bad') {
					var committed = _v8.a;
					var x = _v8.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v8.a;
					var s1 = _v8.b;
					var _v9 = parseB(s1);
					if (_v9.$ === 'Bad') {
						var x = _v9.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v9.a;
						var s2 = _v9.b;
						var _v10 = parseC(s2);
						if (_v10.$ === 'Bad') {
							var x = _v10.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v10.a;
							var s3 = _v10.b;
							var _v11 = parseD(s3);
							if (_v11.$ === 'Bad') {
								var x = _v11.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v11.a;
								var s4 = _v11.b;
								var _v12 = parseE(s4);
								if (_v12.$ === 'Bad') {
									var x = _v12.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v12.a;
									var s5 = _v12.b;
									var _v13 = parseF(s5);
									if (_v13.$ === 'Bad') {
										var x = _v13.b;
										return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
									} else {
										var f = _v13.a;
										var s6 = _v13.b;
										var _v14 = parseG(s6);
										if (_v14.$ === 'Bad') {
											var x = _v14.b;
											return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
										} else {
											var g = _v14.a;
											var s7 = _v14.b;
											var _v15 = parseH(s7);
											if (_v15.$ === 'Bad') {
												var x = _v15.b;
												return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
											} else {
												var h = _v15.a;
												var s8 = _v15.b;
												return A2(
													$stil4m$elm_syntax$ParserFast$Good,
													A9(
														func,
														{column: s0.col, row: s0.row},
														a,
														b,
														c,
														d,
														e,
														f,
														g,
														h),
													s8);
											}
										}
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$validateEndColumnIndentation = F3(
	function (isOkay, problemOnIsNotOkay, _v0) {
		var parse = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var good = _v1;
					var s1 = good.b;
					return A2(isOkay, s1.col, s1.indent) ? good : A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						true,
						A3($stil4m$elm_syntax$ParserFast$ExpectingCustom, s1.row, s1.col, problemOnIsNotOkay));
				} else {
					var bad = _v1;
					return bad;
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Layout$endsPositivelyIndented = function (parser) {
	return A3(
		$stil4m$elm_syntax$ParserFast$validateEndColumnIndentation,
		F2(
			function (column, indent) {
				return _Utils_cmp(column, indent) > 0;
			}),
		'must be positively indented',
		parser);
};
var $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout = $stil4m$elm_syntax$Elm$Parser$Layout$endsPositivelyIndented($stil4m$elm_syntax$Elm$Parser$Layout$whitespaceAndCommentsOrEmpty);
var $stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides = function (x) {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (before, v, after) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						after,
						A2($stil4m$elm_syntax$Rope$prependTo, v.comments, before)),
					syntax: v.syntax
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		x,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout);
};
var $stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccess = A4(
	$stil4m$elm_syntax$ParserFast$loopWhileSucceeds,
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '.', $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode),
	_List_Nil,
	$elm$core$List$cons,
	$elm$core$List$reverse);
var $stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccessMap = function (fieldsToRes) {
	return A4(
		$stil4m$elm_syntax$ParserFast$loopWhileSucceeds,
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '.', $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode),
		_List_Nil,
		$elm$core$List$cons,
		function (reversed) {
			return fieldsToRes(
				$elm$core$List$reverse(reversed));
		});
};
var $stil4m$elm_syntax$Elm$Parser$Expression$negationWhitespaceProblem = $stil4m$elm_syntax$ParserFast$problem('if a negation sign is not preceded by whitespace, it\'s considered subtraction');
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$equal = A2($stil4m$elm_syntax$ParserFast$symbol, '=', _Utils_Tuple0);
var $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithAs = function (a) {
	return {$: 'PatternComposedWithAs', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithCons = function (a) {
	return {$: 'PatternComposedWithCons', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithNothing = function (a) {
	return {$: 'PatternComposedWithNothing', a: a};
};
var $stil4m$elm_syntax$ParserFast$symbolWithRange = F2(
	function (str, startAndEndLocationToRes) {
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + strLength;
				if (_Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					str + '')) {
					var newCol = s.col + strLength;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						startAndEndLocationToRes(
							{
								end: {column: newCol, row: s.row},
								start: {column: s.col, row: s.row}
							}),
						{col: newCol, indent: s.indent, offset: newOffset, row: s.row, src: s.src});
				} else {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s.row, s.col, str));
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$allPattern = A2(
	$stil4m$elm_syntax$ParserFast$symbolWithRange,
	'_',
	function (range) {
		return {
			comments: $stil4m$elm_syntax$Rope$empty,
			syntax: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern)
		};
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$charPattern = $stil4m$elm_syntax$Elm$Parser$Tokens$characterLiteralMapWithRange(
	F2(
		function (range, _char) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$CharPattern(_char))
			};
		}));
var $stil4m$elm_syntax$ParserFast$ExpectingNumber = F2(
	function (a, b) {
		return {$: 'ExpectingNumber', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$Decimal = {$: 'Decimal'};
var $stil4m$elm_syntax$ParserFast$Hexadecimal = {$: 'Hexadecimal'};
var $stil4m$elm_syntax$ParserFast$convert0OrMore0To9s = F3(
	function (soFar, offset, src) {
		convert0OrMore0To9s:
		while (true) {
			var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
			switch (_v0) {
				case '0':
					var $temp$soFar = soFar * 10,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '1':
					var $temp$soFar = (soFar * 10) + 1,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '2':
					var $temp$soFar = (soFar * 10) + 2,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '3':
					var $temp$soFar = (soFar * 10) + 3,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '4':
					var $temp$soFar = (soFar * 10) + 4,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '5':
					var $temp$soFar = (soFar * 10) + 5,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '6':
					var $temp$soFar = (soFar * 10) + 6,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '7':
					var $temp$soFar = (soFar * 10) + 7,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '8':
					var $temp$soFar = (soFar * 10) + 8,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				case '9':
					var $temp$soFar = (soFar * 10) + 9,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMore0To9s;
				default:
					return {_int: soFar, offset: offset};
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal = F3(
	function (soFar, offset, src) {
		convert0OrMoreHexadecimal:
		while (true) {
			var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
			switch (_v0) {
				case '0':
					var $temp$soFar = soFar * 16,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '1':
					var $temp$soFar = (soFar * 16) + 1,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '2':
					var $temp$soFar = (soFar * 16) + 2,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '3':
					var $temp$soFar = (soFar * 16) + 3,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '4':
					var $temp$soFar = (soFar * 16) + 4,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '5':
					var $temp$soFar = (soFar * 16) + 5,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '6':
					var $temp$soFar = (soFar * 16) + 6,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '7':
					var $temp$soFar = (soFar * 16) + 7,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '8':
					var $temp$soFar = (soFar * 16) + 8,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case '9':
					var $temp$soFar = (soFar * 16) + 9,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'a':
					var $temp$soFar = (soFar * 16) + 10,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'A':
					var $temp$soFar = (soFar * 16) + 10,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'b':
					var $temp$soFar = (soFar * 16) + 11,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'B':
					var $temp$soFar = (soFar * 16) + 11,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'c':
					var $temp$soFar = (soFar * 16) + 12,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'C':
					var $temp$soFar = (soFar * 16) + 12,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'd':
					var $temp$soFar = (soFar * 16) + 13,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'D':
					var $temp$soFar = (soFar * 16) + 13,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'e':
					var $temp$soFar = (soFar * 16) + 14,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'E':
					var $temp$soFar = (soFar * 16) + 14,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'f':
					var $temp$soFar = (soFar * 16) + 15,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				case 'F':
					var $temp$soFar = (soFar * 16) + 15,
						$temp$offset = offset + 1,
						$temp$src = src;
					soFar = $temp$soFar;
					offset = $temp$offset;
					src = $temp$src;
					continue convert0OrMoreHexadecimal;
				default:
					return {_int: soFar, offset: offset};
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$convert1OrMoreHexadecimal = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '0':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 0, offset + 1, src);
			case '1':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 1, offset + 1, src);
			case '2':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 2, offset + 1, src);
			case '3':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 3, offset + 1, src);
			case '4':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 4, offset + 1, src);
			case '5':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 5, offset + 1, src);
			case '6':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 6, offset + 1, src);
			case '7':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 7, offset + 1, src);
			case '8':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 8, offset + 1, src);
			case '9':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 9, offset + 1, src);
			case 'a':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 10, offset + 1, src);
			case 'A':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 10, offset + 1, src);
			case 'b':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 11, offset + 1, src);
			case 'B':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 11, offset + 1, src);
			case 'c':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 12, offset + 1, src);
			case 'C':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 12, offset + 1, src);
			case 'd':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 13, offset + 1, src);
			case 'D':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 13, offset + 1, src);
			case 'e':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 14, offset + 1, src);
			case 'E':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 14, offset + 1, src);
			case 'f':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 15, offset + 1, src);
			case 'F':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMoreHexadecimal, 15, offset + 1, src);
			default:
				return {_int: 0, offset: -1};
		}
	});
var $stil4m$elm_syntax$ParserFast$errorAsBaseOffsetAndInt = {
	base: $stil4m$elm_syntax$ParserFast$Decimal,
	offsetAndInt: {_int: 0, offset: -1}
};
var $stil4m$elm_syntax$ParserFast$convertIntegerDecimalOrHexadecimal = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '0':
				var _v1 = A3($elm$core$String$slice, offset + 1, offset + 2, src);
				if (_v1 === 'x') {
					var hex = A2($stil4m$elm_syntax$ParserFast$convert1OrMoreHexadecimal, offset + 2, src);
					return {
						base: $stil4m$elm_syntax$ParserFast$Hexadecimal,
						offsetAndInt: {_int: hex._int, offset: hex.offset}
					};
				} else {
					return {
						base: $stil4m$elm_syntax$ParserFast$Decimal,
						offsetAndInt: {_int: 0, offset: offset + 1}
					};
				}
			case '1':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 1, offset + 1, src)
				};
			case '2':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 2, offset + 1, src)
				};
			case '3':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 3, offset + 1, src)
				};
			case '4':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 4, offset + 1, src)
				};
			case '5':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 5, offset + 1, src)
				};
			case '6':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 6, offset + 1, src)
				};
			case '7':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 7, offset + 1, src)
				};
			case '8':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 8, offset + 1, src)
				};
			case '9':
				return {
					base: $stil4m$elm_syntax$ParserFast$Decimal,
					offsetAndInt: A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 9, offset + 1, src)
				};
			default:
				return $stil4m$elm_syntax$ParserFast$errorAsBaseOffsetAndInt;
		}
	});
var $stil4m$elm_syntax$ParserFast$integerDecimalOrHexadecimalMapWithRange = F2(
	function (rangeAndIntDecimalToRes, rangeAndIntHexadecimalToRes) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var s1 = A2($stil4m$elm_syntax$ParserFast$convertIntegerDecimalOrHexadecimal, s0.offset, s0.src);
				if (_Utils_eq(s1.offsetAndInt.offset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingNumber, s0.row, s0.col));
				} else {
					var newColumn = s0.col + (s1.offsetAndInt.offset - s0.offset);
					var range = {
						end: {column: newColumn, row: s0.row},
						start: {column: s0.col, row: s0.row}
					};
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						function () {
							var _v0 = s1.base;
							if (_v0.$ === 'Decimal') {
								return A2(rangeAndIntDecimalToRes, range, s1.offsetAndInt._int);
							} else {
								return A2(rangeAndIntHexadecimalToRes, range, s1.offsetAndInt._int);
							}
						}(),
						{col: newColumn, indent: s0.indent, offset: s1.offsetAndInt.offset, row: s0.row, src: s0.src});
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$numberPart = A2(
	$stil4m$elm_syntax$ParserFast$integerDecimalOrHexadecimalMapWithRange,
	F2(
		function (range, n) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$IntPattern(n))
			};
		}),
	F2(
		function (range, n) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$HexPattern(n))
			};
		}));
var $stil4m$elm_syntax$ParserFast$oneOf2OrSucceed = F3(
	function (_v0, _v1, thirdRes) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v2 = attemptFirst(s);
				if (_v2.$ === 'Good') {
					var firstGood = _v2;
					return firstGood;
				} else {
					var firstBad = _v2;
					var firstCommitted = firstBad.a;
					if (firstCommitted) {
						return firstBad;
					} else {
						var _v3 = attemptSecond(s);
						if (_v3.$ === 'Good') {
							var secondGood = _v3;
							return secondGood;
						} else {
							var secondBad = _v3;
							var secondCommitted = secondBad.a;
							return secondCommitted ? secondBad : A2($stil4m$elm_syntax$ParserFast$Good, thirdRes, s);
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$oneOf9 = F9(
	function (_v0, _v1, _v2, _v3, _v4, _v5, _v6, _v7, _v8) {
		var attempt0 = _v0.a;
		var attempt1 = _v1.a;
		var attempt2 = _v2.a;
		var attempt3 = _v3.a;
		var attempt4 = _v4.a;
		var attempt5 = _v5.a;
		var attempt6 = _v6.a;
		var attempt7 = _v7.a;
		var attempt8 = _v8.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v9 = attempt0(s);
				if (_v9.$ === 'Good') {
					var good = _v9;
					return good;
				} else {
					var bad0 = _v9;
					var committed0 = bad0.a;
					var x0 = bad0.b;
					if (committed0) {
						return bad0;
					} else {
						var _v10 = attempt1(s);
						if (_v10.$ === 'Good') {
							var good = _v10;
							return good;
						} else {
							var bad1 = _v10;
							var committed1 = bad1.a;
							var x1 = bad1.b;
							if (committed1) {
								return bad1;
							} else {
								var _v11 = attempt2(s);
								if (_v11.$ === 'Good') {
									var good = _v11;
									return good;
								} else {
									var bad2 = _v11;
									var committed2 = bad2.a;
									var x2 = bad2.b;
									if (committed2) {
										return bad2;
									} else {
										var _v12 = attempt3(s);
										if (_v12.$ === 'Good') {
											var good = _v12;
											return good;
										} else {
											var bad3 = _v12;
											var committed3 = bad3.a;
											var x3 = bad3.b;
											if (committed3) {
												return bad3;
											} else {
												var _v13 = attempt4(s);
												if (_v13.$ === 'Good') {
													var good = _v13;
													return good;
												} else {
													var bad4 = _v13;
													var committed4 = bad4.a;
													var x4 = bad4.b;
													if (committed4) {
														return bad4;
													} else {
														var _v14 = attempt5(s);
														if (_v14.$ === 'Good') {
															var good = _v14;
															return good;
														} else {
															var bad5 = _v14;
															var committed5 = bad5.a;
															var x5 = bad5.b;
															if (committed5) {
																return bad5;
															} else {
																var _v15 = attempt6(s);
																if (_v15.$ === 'Good') {
																	var good = _v15;
																	return good;
																} else {
																	var bad6 = _v15;
																	var committed6 = bad6.a;
																	var x6 = bad6.b;
																	if (committed6) {
																		return bad6;
																	} else {
																		var _v16 = attempt7(s);
																		if (_v16.$ === 'Good') {
																			var good = _v16;
																			return good;
																		} else {
																			var bad7 = _v16;
																			var committed7 = bad7.a;
																			var x7 = bad7.b;
																			if (committed7) {
																				return bad7;
																			} else {
																				var _v17 = attempt8(s);
																				if (_v17.$ === 'Good') {
																					var good = _v17;
																					return good;
																				} else {
																					var bad8 = _v17;
																					var committed8 = bad8.a;
																					var x8 = bad8.b;
																					return committed8 ? bad8 : A2(
																						$stil4m$elm_syntax$ParserFast$Bad,
																						false,
																						A3(
																							$stil4m$elm_syntax$ParserFast$ExpectingOneOf,
																							x0,
																							x1,
																							_List_fromArray(
																								[x2, x3, x4, x5, x6, x7, x8])));
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
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Patterns$patternListEmpty = $stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern(_List_Nil);
var $stil4m$elm_syntax$Elm$Parser$Layout$problemPositivelyIndented = $stil4m$elm_syntax$ParserFast$problem('must be positively indented');
var $stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy = function (nextParser) {
	return $stil4m$elm_syntax$ParserFast$columnIndentAndThen(
		F2(
			function (column, indent) {
				return (_Utils_cmp(column, indent) > 0) ? nextParser : $stil4m$elm_syntax$Elm$Parser$Layout$problemPositivelyIndented;
			}));
};
var $stil4m$elm_syntax$ParserFast$ifFollowedByWhileWithoutLinebreak = F2(
	function (firstIsOkay, afterFirstIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var firstOffset = A3($stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak, firstIsOkay, s.offset, s.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate, s.row, s.col));
				} else {
					var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, afterFirstIsOkay, firstOffset, s.row, s.col + 1, s.src, s.indent);
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A3($elm$core$String$slice, s.offset, s1.offset, s.src),
						s1);
				}
			});
	});
var $stil4m$elm_syntax$Char$Extra$unicodeIsUpperFast = function (c) {
	var code = $elm$core$Char$toCode(c);
	return $stil4m$elm_syntax$Char$Extra$charCodeIsUpper(code) || function () {
		var cString = $elm$core$String$fromChar(c);
		return (_Utils_eq(
			$elm$core$String$toUpper(cString),
			cString + '') && (!_Utils_eq(
			$elm$core$String$toLower(cString),
			cString + ''))) ? ((code <= 8543) || (((8560 <= code) && (code <= 9397)) || ((9424 <= code) && (code <= 983040)))) : ((code < 120015) ? ((code < 8509) ? (((978 <= code) && (code <= 980)) || ((code === 8450) || ((code === 8455) || (((8459 <= code) && (code <= 8461)) || (((8464 <= code) && (code <= 8466)) || ((code === 8469) || (((8473 <= code) && (code <= 8477)) || ((code === 8484) || ((code === 8488) || (((8490 <= code) && (code <= 8493)) || ((8496 <= code) && (code <= 8499)))))))))))) : (((8510 <= code) && (code <= 8511)) || ((code === 8517) || (((119808 <= code) && (code <= 119833)) || (((119860 <= code) && (code <= 119885)) || (((119912 <= code) && (code <= 119937)) || ((code === 119964) || (((119966 <= code) && (code <= 119967)) || ((code === 119970) || (((119973 <= code) && (code <= 119974)) || (((119977 <= code) && (code <= 119980)) || ((119982 <= code) && (code <= 119989))))))))))))) : ((code < 120223) ? (((120016 <= code) && (code <= 120041)) || (((120068 <= code) && (code <= 120069)) || (((120071 <= code) && (code <= 120074)) || (((120077 <= code) && (code <= 120084)) || (((120086 <= code) && (code <= 120092)) || (((120120 <= code) && (code <= 120121)) || (((120123 <= code) && (code <= 120126)) || (((120128 <= code) && (code <= 120132)) || ((code === 120134) || (((120138 <= code) && (code <= 120144)) || ((120172 <= code) && (code <= 120197)))))))))))) : (((120224 <= code) && (code <= 120249)) || (((120276 <= code) && (code <= 120301)) || (((120328 <= code) && (code <= 120353)) || (((120380 <= code) && (code <= 120405)) || (((120432 <= code) && (code <= 120457)) || (((120488 <= code) && (code <= 120512)) || (((120546 <= code) && (code <= 120570)) || (((120604 <= code) && (code <= 120628)) || (((120662 <= code) && (code <= 120686)) || (((120720 <= code) && (code <= 120744)) || (code === 120778)))))))))))));
	}();
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$typeName = A2($stil4m$elm_syntax$ParserFast$ifFollowedByWhileWithoutLinebreak, $stil4m$elm_syntax$Char$Extra$unicodeIsUpperFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast);
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeDotTypeNamesTuple() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map2OrSucceed,
		F2(
			function (startName, afterStartName) {
				if (afterStartName.$ === 'Nothing') {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(_List_Nil, startName));
				} else {
					var _v1 = afterStartName.a;
					var qualificationAfter = _v1.a;
					var unqualified = _v1.b;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							A2($elm$core$List$cons, startName, qualificationAfter),
							unqualified));
				}
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '.', $stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v2) {
				return $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeDotTypeNamesTuple();
			}),
		$elm$core$Maybe$Nothing);
}
try {
	var $stil4m$elm_syntax$Elm$Parser$Patterns$maybeDotTypeNamesTuple = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeDotTypeNamesTuple();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeDotTypeNamesTuple = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$maybeDotTypeNamesTuple;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.Patterns` are causing infinite recursion:\n\n  ┌─────┐\n  │    maybeDotTypeNamesTuple\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedNameRefNode = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, firstName, after) {
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				range,
				function () {
					if (after.$ === 'Nothing') {
						return {moduleName: _List_Nil, name: firstName};
					} else {
						var _v1 = after.a;
						var qualificationAfter = _v1.a;
						var unqualified = _v1.b;
						return {
							moduleName: A2($elm$core$List$cons, firstName, qualificationAfter),
							name: unqualified
						};
					}
				}());
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
	$stil4m$elm_syntax$Elm$Parser$Patterns$maybeDotTypeNamesTuple);
var $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternWithoutConsumeArgs = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, firstName, after) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
						function () {
							if (after.$ === 'Nothing') {
								return {moduleName: _List_Nil, name: firstName};
							} else {
								var _v1 = after.a;
								var qualificationAfter = _v1.a;
								var unqualified = _v1.b;
								return {
									moduleName: A2($elm$core$List$cons, firstName, qualificationAfter),
									name: unqualified
								};
							}
						}(),
						_List_Nil))
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
	$stil4m$elm_syntax$Elm$Parser$Patterns$maybeDotTypeNamesTuple);
var $stil4m$elm_syntax$Elm$Parser$Patterns$recordPattern = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, commentsBeforeElements, elements) {
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, elements.comments, commentsBeforeElements),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$RecordPattern(elements.syntax))
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '{', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		A2(
			$stil4m$elm_syntax$ParserFast$followedBySymbol,
			'}',
			A4(
				$stil4m$elm_syntax$ParserFast$map3,
				F3(
					function (head, commentsAfterHead, tail) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, tail.comments, commentsAfterHead),
							syntax: A2($elm$core$List$cons, head, tail.syntax)
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$ParserWithComments$many(
					A2(
						$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
						',',
						A4(
							$stil4m$elm_syntax$ParserFast$map3,
							F3(
								function (beforeName, name, afterName) {
									return {
										comments: A2($stil4m$elm_syntax$Rope$prependTo, afterName, beforeName),
										syntax: name
									};
								}),
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
							$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))))),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'}',
			{comments: $stil4m$elm_syntax$Rope$empty, syntax: _List_Nil})));
var $stil4m$elm_syntax$Elm$Parser$Patterns$stringPattern = $stil4m$elm_syntax$Elm$Parser$Tokens$singleOrTripleQuotedStringLiteralMapWithRange(
	F2(
		function (range, string) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern(string))
			};
		}));
var $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange = function (rangeAndNameToResult) {
	return A4($stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateMapWithRangeWithoutLinebreak, rangeAndNameToResult, $stil4m$elm_syntax$Char$Extra$unicodeIsLowerFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast, $stil4m$elm_syntax$Elm$Parser$Tokens$isNotReserved);
};
var $stil4m$elm_syntax$Elm$Parser$Patterns$varPattern = $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange(
	F2(
		function (range, _var) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(_var))
			};
		}));
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePatternTryToCompose() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (x, commentsAfterLeft, maybeComposedWithResult) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						maybeComposedWithResult.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterLeft, x.comments)),
					syntax: function () {
						var _v7 = maybeComposedWithResult.syntax;
						switch (_v7.$) {
							case 'PatternComposedWithNothing':
								return x.syntax;
							case 'PatternComposedWithAs':
								var anotherName = _v7.a;
								return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Pattern$AsPattern, x.syntax, anotherName);
							default:
								var y = _v7.a;
								return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Pattern$UnConsPattern, x.syntax, y);
						}
					}()
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern(),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeComposedWith());
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern() {
	return A9(
		$stil4m$elm_syntax$ParserFast$oneOf9,
		$stil4m$elm_syntax$Elm$Parser$Patterns$varPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternWithConsumeArgs(),
		$stil4m$elm_syntax$Elm$Parser$Patterns$allPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern(),
		$stil4m$elm_syntax$Elm$Parser$Patterns$recordPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$stringPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern(),
		$stil4m$elm_syntax$Elm$Parser$Patterns$numberPart,
		$stil4m$elm_syntax$Elm$Parser$Patterns$charPattern);
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternWithConsumeArgs() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (_v4, afterStartName, argsReverse) {
				var nameRange = _v4.a;
				var name = _v4.b;
				var range = function () {
					var _v5 = argsReverse.syntax;
					if (!_v5.b) {
						return nameRange;
					} else {
						var _v6 = _v5.a;
						var lastArgRange = _v6.a;
						return {end: lastArgRange.end, start: nameRange.start};
					}
				}();
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, argsReverse.comments, afterStartName),
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						A2(
							$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
							name,
							$elm$core$List$reverse(argsReverse.syntax)))
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedNameRefNode,
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
			$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy(
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (arg, commentsAfterArg) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterArg, arg.comments),
								syntax: arg.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$patternNotDirectlyComposing(),
					$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout))));
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$patternNotDirectlyComposing() {
	return A9(
		$stil4m$elm_syntax$ParserFast$oneOf9,
		$stil4m$elm_syntax$Elm$Parser$Patterns$varPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternWithoutConsumeArgs,
		$stil4m$elm_syntax$Elm$Parser$Patterns$allPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern(),
		$stil4m$elm_syntax$Elm$Parser$Patterns$recordPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$stringPattern,
		$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern(),
		$stil4m$elm_syntax$Elm$Parser$Patterns$numberPart,
		$stil4m$elm_syntax$Elm$Parser$Patterns$charPattern);
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern() {
	return A3(
		$stil4m$elm_syntax$ParserFast$map2WithRange,
		F3(
			function (range, commentsBeforeElements, maybeElements) {
				if (maybeElements.$ === 'Nothing') {
					return {
						comments: commentsBeforeElements,
						syntax: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, $stil4m$elm_syntax$Elm$Parser$Patterns$patternListEmpty)
					};
				} else {
					var elements = maybeElements.a;
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, elements.comments, commentsBeforeElements),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							range,
							$stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern(elements.syntax))
					};
				}
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '[', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		A2(
			$stil4m$elm_syntax$ParserFast$oneOf2,
			A2($stil4m$elm_syntax$ParserFast$symbol, ']', $elm$core$Maybe$Nothing),
			A2(
				$stil4m$elm_syntax$ParserFast$followedBySymbol,
				']',
				A4(
					$stil4m$elm_syntax$ParserFast$map3,
					F3(
						function (head, commentsAfterHead, tail) {
							return $elm$core$Maybe$Just(
								{
									comments: A2(
										$stil4m$elm_syntax$Rope$prependTo,
										commentsAfterHead,
										A2($stil4m$elm_syntax$Rope$prependTo, tail.comments, head.comments)),
									syntax: A2($elm$core$List$cons, head.syntax, tail.syntax)
								});
						}),
					$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern(),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$ParserWithComments$many(
						A2(
							$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
							',',
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
								$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern())))))));
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeComposedWith() {
	return A3(
		$stil4m$elm_syntax$ParserFast$oneOf2OrSucceed,
		A2(
			$stil4m$elm_syntax$ParserFast$keywordFollowedBy,
			'as',
			A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsAfterAs, name) {
						return {
							comments: commentsAfterAs,
							syntax: $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithAs(name)
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode)),
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'::',
			A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsAfterCons, patternResult) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterCons, patternResult.comments),
							syntax: $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithCons(patternResult.syntax)
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern())),
		{
			comments: $stil4m$elm_syntax$Rope$empty,
			syntax: $stil4m$elm_syntax$Elm$Parser$Patterns$PatternComposedWithNothing(_Utils_Tuple0)
		});
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'(',
		A3(
			$stil4m$elm_syntax$ParserFast$map2WithRange,
			F3(
				function (range, commentsBeforeHead, contentResult) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, contentResult.comments, commentsBeforeHead),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: range.end,
								start: {column: range.start.column - 1, row: range.start.row}
							},
							contentResult.syntax)
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				A2(
					$stil4m$elm_syntax$ParserFast$symbol,
					')',
					{comments: $stil4m$elm_syntax$Rope$empty, syntax: $stil4m$elm_syntax$Elm$Syntax$Pattern$UnitPattern}),
				A4(
					$stil4m$elm_syntax$ParserFast$map3,
					F3(
						function (headResult, commentsAfterHead, tailResult) {
							return {
								comments: A2(
									$stil4m$elm_syntax$Rope$prependTo,
									tailResult.comments,
									A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterHead, headResult.comments)),
								syntax: function () {
									var _v1 = tailResult.syntax;
									if (_v1.$ === 'Nothing') {
										return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern(headResult.syntax);
									} else {
										var secondAndMaybeThirdPart = _v1.a;
										var _v2 = secondAndMaybeThirdPart.maybeThirdPart;
										if (_v2.$ === 'Nothing') {
											return $stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
												_List_fromArray(
													[headResult.syntax, secondAndMaybeThirdPart.secondPart]));
										} else {
											var thirdPart = _v2.a;
											return $stil4m$elm_syntax$Elm$Syntax$Pattern$TuplePattern(
												_List_fromArray(
													[headResult.syntax, secondAndMaybeThirdPart.secondPart, thirdPart]));
										}
									}
								}()
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern(),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					A2(
						$stil4m$elm_syntax$ParserFast$oneOf2,
						A2(
							$stil4m$elm_syntax$ParserFast$symbol,
							')',
							{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}),
						A2(
							$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
							',',
							A5(
								$stil4m$elm_syntax$ParserFast$map4,
								F4(
									function (commentsBefore, secondPart, commentsAfter, maybeThirdPart) {
										return {
											comments: A2(
												$stil4m$elm_syntax$Rope$prependTo,
												maybeThirdPart.comments,
												A2(
													$stil4m$elm_syntax$Rope$prependTo,
													commentsAfter,
													A2($stil4m$elm_syntax$Rope$prependTo, secondPart.comments, commentsBefore))),
											syntax: $elm$core$Maybe$Just(
												{maybeThirdPart: maybeThirdPart.syntax, secondPart: secondPart.syntax})
										};
									}),
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
								$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern(),
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
								A2(
									$stil4m$elm_syntax$ParserFast$oneOf2,
									A2(
										$stil4m$elm_syntax$ParserFast$symbol,
										')',
										{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}),
									A2(
										$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
										',',
										A2(
											$stil4m$elm_syntax$ParserFast$followedBySymbol,
											')',
											A4(
												$stil4m$elm_syntax$ParserFast$map3,
												F3(
													function (commentsBefore, thirdPart, commentsAfter) {
														return {
															comments: A2(
																$stil4m$elm_syntax$Rope$prependTo,
																commentsAfter,
																A2($stil4m$elm_syntax$Rope$prependTo, thirdPart.comments, commentsBefore)),
															syntax: $elm$core$Maybe$Just(thirdPart.syntax)
														};
													}),
												$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
												$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern(),
												$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)))))))))));
}
function $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern() {
	return $stil4m$elm_syntax$ParserFast$lazy(
		function (_v0) {
			return $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePatternTryToCompose();
		});
}
try {
	var $stil4m$elm_syntax$Elm$Parser$Patterns$composablePatternTryToCompose = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePatternTryToCompose();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePatternTryToCompose = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$composablePatternTryToCompose;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$composablePattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$composablePattern = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$composablePattern;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternWithConsumeArgs = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternWithConsumeArgs();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$qualifiedPatternWithConsumeArgs = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$qualifiedPatternWithConsumeArgs;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$patternNotDirectlyComposing();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$patternNotDirectlyComposing = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$listPattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$listPattern = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$listPattern;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$maybeComposedWith = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeComposedWith();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$maybeComposedWith = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$maybeComposedWith;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$parensPattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$parensPattern = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$parensPattern;
	};
	var $stil4m$elm_syntax$Elm$Parser$Patterns$pattern = $stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern();
	$stil4m$elm_syntax$Elm$Parser$Patterns$cyclic$pattern = function () {
		return $stil4m$elm_syntax$Elm$Parser$Patterns$pattern;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.Patterns` are causing infinite recursion:\n\n  ┌─────┐\n  │    composablePatternTryToCompose\n  │     ↓\n  │    composablePattern\n  │     ↓\n  │    qualifiedPatternWithConsumeArgs\n  │     ↓\n  │    patternNotDirectlyComposing\n  │     ↓\n  │    listPattern\n  │     ↓\n  │    maybeComposedWith\n  │     ↓\n  │    parensPattern\n  │     ↓\n  │    pattern\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$ParserWithComments$until = F2(
	function (end, element) {
		return A5(
			$stil4m$elm_syntax$ParserFast$loopUntil,
			end,
			element,
			_Utils_Tuple2($stil4m$elm_syntax$Rope$empty, _List_Nil),
			F2(
				function (pResult, _v0) {
					var commentsSoFar = _v0.a;
					var itemsSoFar = _v0.b;
					return _Utils_Tuple2(
						A2($stil4m$elm_syntax$Rope$prependTo, pResult.comments, commentsSoFar),
						A2($elm$core$List$cons, pResult.syntax, itemsSoFar));
				}),
			function (_v1) {
				var commentsSoFar = _v1.a;
				var itemsSoFar = _v1.b;
				return {
					comments: commentsSoFar,
					syntax: $elm$core$List$reverse(itemsSoFar)
				};
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$parameterPatternsEqual = A2(
	$stil4m$elm_syntax$ParserWithComments$until,
	$stil4m$elm_syntax$Elm$Parser$Tokens$equal,
	A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (patternResult, commentsAfterPattern) {
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterPattern, patternResult.comments),
					syntax: patternResult.syntax
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout));
var $stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedPlusFollowedBy = F2(
	function (extraIndent, nextParser) {
		return $stil4m$elm_syntax$ParserFast$columnIndentAndThen(
			F2(
				function (column, indent) {
					return (_Utils_cmp(column, indent + extraIndent) > 0) ? nextParser : $stil4m$elm_syntax$Elm$Parser$Layout$problemPositivelyIndented;
				}));
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$problemCannotMixNonAssociativeInfixOperators = $stil4m$elm_syntax$ParserFast$problem('cannot mix non-associative infix operators without parenthesis');
var $stil4m$elm_syntax$Elm$Parser$Expression$rangeMoveStartLeftByOneColumn = function (range) {
	return {
		end: range.end,
		start: {column: range.start.column - 1, row: range.start.row}
	};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccessFunction = function (a) {
	return {$: 'RecordAccessFunction', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expression$recordAccessFunctionExpression = A2(
	$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
	'.',
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange(
		F2(
			function (range, field) {
				return {
					comments: $stil4m$elm_syntax$Rope$empty,
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						$stil4m$elm_syntax$Elm$Parser$Expression$rangeMoveStartLeftByOneColumn(range),
						$stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccessFunction('.' + field))
				};
			})));
var $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable = function (a) {
	return {$: 'Floatable', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Hex = function (a) {
	return {$: 'Hex', a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Integer = function (a) {
	return {$: 'Integer', a: a};
};
var $stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9 = F2(
	function (offset, src) {
		skip0OrMoreDigits0To9:
		while (true) {
			var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
			switch (_v0) {
				case '0':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '1':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '2':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '3':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '4':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '5':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '6':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '7':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '8':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				case '9':
					var $temp$offset = offset + 1,
						$temp$src = src;
					offset = $temp$offset;
					src = $temp$src;
					continue skip0OrMoreDigits0To9;
				default:
					return offset;
			}
		}
	});
var $stil4m$elm_syntax$ParserFast$skip1OrMoreDigits0To9 = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '0':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '1':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '2':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '3':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '4':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '5':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '6':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '7':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '8':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			case '9':
				return A2($stil4m$elm_syntax$ParserFast$skip0OrMoreDigits0To9, offset + 1, src);
			default:
				return -1;
		}
	});
var $stil4m$elm_syntax$ParserFast$skipAfterFloatExponentMark = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '+':
				return A2($stil4m$elm_syntax$ParserFast$skip1OrMoreDigits0To9, offset + 1, src);
			case '-':
				return A2($stil4m$elm_syntax$ParserFast$skip1OrMoreDigits0To9, offset + 1, src);
			default:
				return A2($stil4m$elm_syntax$ParserFast$skip1OrMoreDigits0To9, offset, src);
		}
	});
var $stil4m$elm_syntax$ParserFast$skipFloatAfterIntegerDecimal = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '.':
				var offsetAfterDigits = A2($stil4m$elm_syntax$ParserFast$skip1OrMoreDigits0To9, offset + 1, src);
				if (_Utils_eq(offsetAfterDigits, -1)) {
					return -1;
				} else {
					var _v1 = A3($elm$core$String$slice, offsetAfterDigits, offsetAfterDigits + 1, src);
					switch (_v1) {
						case 'e':
							return A2($stil4m$elm_syntax$ParserFast$skipAfterFloatExponentMark, offsetAfterDigits + 1, src);
						case 'E':
							return A2($stil4m$elm_syntax$ParserFast$skipAfterFloatExponentMark, offsetAfterDigits + 1, src);
						default:
							return offsetAfterDigits;
					}
				}
			case 'e':
				return A2($stil4m$elm_syntax$ParserFast$skipAfterFloatExponentMark, offset + 1, src);
			case 'E':
				return A2($stil4m$elm_syntax$ParserFast$skipAfterFloatExponentMark, offset + 1, src);
			default:
				return -1;
		}
	});
var $elm$core$String$toFloat = _String_toFloat;
var $stil4m$elm_syntax$ParserFast$floatOrIntegerDecimalOrHexadecimalMapWithRange = F3(
	function (rangeAndFloatToRes, rangeAndIntDecimalToRes, rangeAndIntHexadecimalToRes) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var s1 = A2($stil4m$elm_syntax$ParserFast$convertIntegerDecimalOrHexadecimal, s0.offset, s0.src);
				if (_Utils_eq(s1.offsetAndInt.offset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingNumber, s0.row, s0.col));
				} else {
					var offsetAfterFloat = A2($stil4m$elm_syntax$ParserFast$skipFloatAfterIntegerDecimal, s1.offsetAndInt.offset, s0.src);
					if (_Utils_eq(offsetAfterFloat, -1)) {
						var newColumn = s0.col + (s1.offsetAndInt.offset - s0.offset);
						var range = {
							end: {column: newColumn, row: s0.row},
							start: {column: s0.col, row: s0.row}
						};
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							function () {
								var _v0 = s1.base;
								if (_v0.$ === 'Decimal') {
									return A2(rangeAndIntDecimalToRes, range, s1.offsetAndInt._int);
								} else {
									return A2(rangeAndIntHexadecimalToRes, range, s1.offsetAndInt._int);
								}
							}(),
							{col: newColumn, indent: s0.indent, offset: s1.offsetAndInt.offset, row: s0.row, src: s0.src});
					} else {
						var _v1 = $elm$core$String$toFloat(
							A3($elm$core$String$slice, s0.offset, offsetAfterFloat, s0.src));
						if (_v1.$ === 'Just') {
							var _float = _v1.a;
							var newColumn = s0.col + (offsetAfterFloat - s0.offset);
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A2(
									rangeAndFloatToRes,
									{
										end: {column: newColumn, row: s0.row},
										start: {column: s0.col, row: s0.row}
									},
									_float),
								{col: newColumn, indent: s0.indent, offset: offsetAfterFloat, row: s0.row, src: s0.src});
						} else {
							return A2(
								$stil4m$elm_syntax$ParserFast$Bad,
								false,
								A2($stil4m$elm_syntax$ParserFast$ExpectingNumber, s0.row, s0.col));
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$numberExpression = A3(
	$stil4m$elm_syntax$ParserFast$floatOrIntegerDecimalOrHexadecimalMapWithRange,
	F2(
		function (range, n) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Expression$Floatable(n))
			};
		}),
	F2(
		function (range, n) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Expression$Integer(n))
			};
		}),
	F2(
		function (range, n) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Expression$Hex(n))
			};
		}));
var $stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue = F2(
	function (a, b) {
		return {$: 'FunctionOrValue', a: a, b: b};
	});
var $stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateWithoutLinebreak = F3(
	function (firstIsOkay, afterFirstIsOkay, resultIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var firstOffset = A3($stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak, firstIsOkay, s.offset, s.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate, s.row, s.col));
				} else {
					var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, afterFirstIsOkay, firstOffset, s.row, s.col + 1, s.src, s.indent);
					var name = A3($elm$core$String$slice, s.offset, s1.offset, s.src);
					return resultIsOkay(name) ? A2($stil4m$elm_syntax$ParserFast$Good, name, s1) : A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingStringSatisfyingPredicate, s.row, s.col + 1));
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$functionName = A3($stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateWithoutLinebreak, $stil4m$elm_syntax$Char$Extra$unicodeIsLowerFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast, $stil4m$elm_syntax$Elm$Parser$Tokens$isNotReserved);
var $stil4m$elm_syntax$ParserFast$oneOf2Map = F4(
	function (firstToChoice, _v0, secondToChoice, _v1) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v2 = attemptFirst(s);
				if (_v2.$ === 'Good') {
					var first = _v2.a;
					var s1 = _v2.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						firstToChoice(first),
						s1);
				} else {
					var firstCommitted = _v2.a;
					var firstX = _v2.b;
					if (firstCommitted) {
						return A2($stil4m$elm_syntax$ParserFast$Bad, firstCommitted, firstX);
					} else {
						var _v3 = attemptSecond(s);
						if (_v3.$ === 'Good') {
							var second = _v3.a;
							var s1 = _v3.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								secondToChoice(second),
								s1);
						} else {
							var secondCommitted = _v3.a;
							var secondX = _v3.b;
							return secondCommitted ? A2($stil4m$elm_syntax$ParserFast$Bad, secondCommitted, secondX) : A2(
								$stil4m$elm_syntax$ParserFast$Bad,
								false,
								A3($stil4m$elm_syntax$ParserFast$ExpectingOneOf, firstX, secondX, _List_Nil));
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$orSucceed = F2(
	function (_v0, secondRes) {
		var attemptFirst = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v1 = attemptFirst(s);
				if (_v1.$ === 'Good') {
					var firstGood = _v1;
					return firstGood;
				} else {
					var firstBad = _v1;
					var firstCommitted = firstBad.a;
					return firstCommitted ? firstBad : A2($stil4m$elm_syntax$ParserFast$Good, secondRes, s);
				}
			});
	});
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$maybeDotReferenceExpressionTuple() {
	return A2(
		$stil4m$elm_syntax$ParserFast$orSucceed,
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'.',
			A4(
				$stil4m$elm_syntax$ParserFast$oneOf2Map,
				$elm$core$Maybe$Just,
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (firstName, after) {
							if (after.$ === 'Nothing') {
								return _Utils_Tuple3(_List_Nil, firstName, _List_Nil);
							} else {
								var _v1 = after.a;
								var qualificationAfter = _v1.a;
								var unqualified = _v1.b;
								var recordAccess = _v1.c;
								return _Utils_Tuple3(
									A2($elm$core$List$cons, firstName, qualificationAfter),
									unqualified,
									recordAccess);
							}
						}),
					$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
					$stil4m$elm_syntax$ParserFast$lazy(
						function (_v2) {
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$maybeDotReferenceExpressionTuple();
						})),
				$elm$core$Basics$identity,
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (name, recordAccesses) {
							return $elm$core$Maybe$Just(
								_Utils_Tuple3(_List_Nil, name, recordAccesses));
						}),
					$stil4m$elm_syntax$Elm$Parser$Tokens$functionName,
					$stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccess))),
		$elm$core$Maybe$Nothing);
}
try {
	var $stil4m$elm_syntax$Elm$Parser$Expression$maybeDotReferenceExpressionTuple = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$maybeDotReferenceExpressionTuple();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$maybeDotReferenceExpressionTuple = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$maybeDotReferenceExpressionTuple;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.Expression` are causing infinite recursion:\n\n  ┌─────┐\n  │    maybeDotReferenceExpressionTuple\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$Elm$Parser$Expression$qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, firstName, after) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: function () {
					if (after.$ === 'Nothing') {
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							range,
							A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, firstName));
					} else {
						var _v1 = after.a;
						var qualificationAfter = _v1.a;
						var unqualified = _v1.b;
						var recordAccesses = _v1.c;
						if (!recordAccesses.b) {
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								range,
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue,
									A2($elm$core$List$cons, firstName, qualificationAfter),
									unqualified));
						} else {
							var _v3 = recordAccesses.a;
							var firstRecordAccessRange = _v3.a;
							var referenceNode = A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{
									end: {column: firstRecordAccessRange.start.column - 1, row: firstRecordAccessRange.start.row},
									start: range.start
								},
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue,
									A2($elm$core$List$cons, firstName, qualificationAfter),
									unqualified));
							return A3(
								$elm$core$List$foldl,
								F2(
									function (fieldNode, leftNode) {
										var fieldRange = fieldNode.a;
										var leftRange = leftNode.a;
										return A2(
											$stil4m$elm_syntax$Elm$Syntax$Node$Node,
											{end: fieldRange.end, start: leftRange.start},
											A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, leftNode, fieldNode));
									}),
								referenceNode,
								recordAccesses);
						}
					}
				}()
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
	$stil4m$elm_syntax$Elm$Parser$Expression$maybeDotReferenceExpressionTuple);
var $stil4m$elm_syntax$Elm$Parser$Expression$unqualifiedFunctionReferenceExpressionFollowedByRecordAccess = A3(
	$stil4m$elm_syntax$ParserFast$map2,
	F2(
		function (leftestResult, recordAccesses) {
			if (!recordAccesses.b) {
				return leftestResult;
			} else {
				return {
					comments: leftestResult.comments,
					syntax: A3(
						$elm$core$List$foldl,
						F2(
							function (fieldNode, leftNode) {
								var fieldRange = fieldNode.a;
								var leftRange = leftNode.a;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									{end: fieldRange.end, start: leftRange.start},
									A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, leftNode, fieldNode));
							}),
						leftestResult.syntax,
						recordAccesses)
				};
			}
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange(
		F2(
			function (range, unqualified) {
				return {
					comments: $stil4m$elm_syntax$Rope$empty,
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, unqualified))
				};
			})),
	$stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccess);
var $stil4m$elm_syntax$Elm$Parser$Expression$referenceOrNumberExpression = A3($stil4m$elm_syntax$ParserFast$oneOf3, $stil4m$elm_syntax$Elm$Parser$Expression$qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess, $stil4m$elm_syntax$Elm$Parser$Expression$unqualifiedFunctionReferenceExpressionFollowedByRecordAccess, $stil4m$elm_syntax$Elm$Parser$Expression$numberExpression);
var $stil4m$elm_syntax$ParserFast$symbolBacktrackableFollowedBy = F2(
	function (str, _v0) {
		var parseNext = _v0.a;
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + strLength;
				return _Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					str + '') ? parseNext(
					{col: s.col + strLength, indent: s.indent, offset: newOffset, row: s.row, src: s.src}) : A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s.row, s.col, str));
			});
	});
var $stil4m$elm_syntax$ParserFast$symbolWithEndLocation = F2(
	function (str, endLocationToRes) {
		var strLength = $elm$core$String$length(str);
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var newOffset = s.offset + strLength;
				if (_Utils_eq(
					A3($elm$core$String$slice, s.offset, newOffset, s.src),
					str + '')) {
					var newCol = s.col + strLength;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						endLocationToRes(
							{column: newCol, row: s.row}),
						{col: newCol, indent: s.indent, offset: newOffset, row: s.row, src: s.src});
				} else {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A3($stil4m$elm_syntax$ParserFast$ExpectingSymbol, s.row, s.col, str));
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$temporaryErrPrecedenceTooHigh = $elm$core$Result$Err('infix operator precedence too high');
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$FieldsAfterName = function (a) {
	return {$: 'FieldsAfterName', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$RecordExtensionExpressionAfterName = function (a) {
	return {$: 'RecordExtensionExpressionAfterName', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange(
	F2(
		function (range, _var) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(_var))
			};
		}));
var $stil4m$elm_syntax$ParserFast$map3WithRange = F4(
	function (func, _v0, _v1, _v2) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v3 = parseA(s0);
				if (_v3.$ === 'Bad') {
					var committed = _v3.a;
					var x = _v3.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v3.a;
					var s1 = _v3.b;
					var _v4 = parseB(s1);
					if (_v4.$ === 'Bad') {
						var x = _v4.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v4.a;
						var s2 = _v4.b;
						var _v5 = parseC(s2);
						if (_v5.$ === 'Bad') {
							var x = _v5.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v5.a;
							var s3 = _v5.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A4(
									func,
									{
										end: {column: s3.col, row: s3.row},
										start: {column: s0.col, row: s0.row}
									},
									a,
									b,
									c),
								s3);
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$map6WithRange = F7(
	function (func, _v0, _v1, _v2, _v3, _v4, _v5) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		var parseF = _v5.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v6 = parseA(s0);
				if (_v6.$ === 'Bad') {
					var committed = _v6.a;
					var x = _v6.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v6.a;
					var s1 = _v6.b;
					var _v7 = parseB(s1);
					if (_v7.$ === 'Bad') {
						var x = _v7.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v7.a;
						var s2 = _v7.b;
						var _v8 = parseC(s2);
						if (_v8.$ === 'Bad') {
							var x = _v8.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v8.a;
							var s3 = _v8.b;
							var _v9 = parseD(s3);
							if (_v9.$ === 'Bad') {
								var x = _v9.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v9.a;
								var s4 = _v9.b;
								var _v10 = parseE(s4);
								if (_v10.$ === 'Bad') {
									var x = _v10.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v10.a;
									var s5 = _v10.b;
									var _v11 = parseF(s5);
									if (_v11.$ === 'Bad') {
										var x = _v11.b;
										return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
									} else {
										var f = _v11.a;
										var s6 = _v11.b;
										return A2(
											$stil4m$elm_syntax$ParserFast$Good,
											A7(
												func,
												{
													end: {column: s6.col, row: s6.row},
													start: {column: s0.col, row: s0.row}
												},
												a,
												b,
												c,
												d,
												e,
												f),
											s6);
									}
								}
							}
						}
					}
				}
			});
	});
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$maybeDotTypeNamesTuple() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map2OrSucceed,
		F2(
			function (firstName, afterFirstName) {
				if (afterFirstName.$ === 'Nothing') {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(_List_Nil, firstName));
				} else {
					var _v1 = afterFirstName.a;
					var qualificationAfter = _v1.a;
					var unqualified = _v1.b;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							A2($elm$core$List$cons, firstName, qualificationAfter),
							unqualified));
				}
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '.', $stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v2) {
				return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$maybeDotTypeNamesTuple();
			}),
		$elm$core$Maybe$Nothing);
}
try {
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$maybeDotTypeNamesTuple = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$maybeDotTypeNamesTuple();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$maybeDotTypeNamesTuple = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$maybeDotTypeNamesTuple;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.TypeAnnotation` are causing infinite recursion:\n\n  ┌─────┐\n  │    maybeDotTypeNamesTuple\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$ParserFast$oneOf4 = F4(
	function (_v0, _v1, _v2, _v3) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		var attemptThird = _v2.a;
		var attemptFourth = _v3.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v4 = attemptFirst(s);
				if (_v4.$ === 'Good') {
					var firstGood = _v4;
					return firstGood;
				} else {
					var firstBad = _v4;
					var firstCommitted = firstBad.a;
					var firstX = firstBad.b;
					if (firstCommitted) {
						return firstBad;
					} else {
						var _v5 = attemptSecond(s);
						if (_v5.$ === 'Good') {
							var secondGood = _v5;
							return secondGood;
						} else {
							var secondBad = _v5;
							var secondCommitted = secondBad.a;
							var secondX = secondBad.b;
							if (secondCommitted) {
								return secondBad;
							} else {
								var _v6 = attemptThird(s);
								if (_v6.$ === 'Good') {
									var thirdGood = _v6;
									return thirdGood;
								} else {
									var thirdBad = _v6;
									var thirdCommitted = thirdBad.a;
									var thirdX = thirdBad.b;
									if (thirdCommitted) {
										return thirdBad;
									} else {
										var _v7 = attemptFourth(s);
										if (_v7.$ === 'Good') {
											var fourthGood = _v7;
											return fourthGood;
										} else {
											var fourthBad = _v7;
											var fourthCommitted = fourthBad.a;
											var fourthX = fourthBad.b;
											return fourthCommitted ? fourthBad : A2(
												$stil4m$elm_syntax$ParserFast$Bad,
												false,
												A3(
													$stil4m$elm_syntax$ParserFast$ExpectingOneOf,
													firstX,
													secondX,
													_List_fromArray(
														[thirdX, fourthX])));
										}
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationRecordEmpty = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(_List_Nil);
var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotationWithoutArguments = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, startName, afterStartName) {
			var name = function () {
				if (afterStartName.$ === 'Nothing') {
					return _Utils_Tuple2(_List_Nil, startName);
				} else {
					var _v1 = afterStartName.a;
					var qualificationAfterStartName = _v1.a;
					var unqualified = _v1.b;
					return _Utils_Tuple2(
						A2($elm$core$List$cons, startName, qualificationAfterStartName),
						unqualified);
				}
			}();
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, name),
						_List_Nil))
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$maybeDotTypeNamesTuple);
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnIncludingTypedWithArguments() {
	return A4(
		$stil4m$elm_syntax$ParserFast$oneOf4,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation(),
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typedTypeAnnotationWithArgumentsOptimisticLayout(),
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation());
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typedTypeAnnotationWithArgumentsOptimisticLayout() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (nameNode, commentsAfterName, argsReverse) {
				var nameRange = nameNode.a;
				var range = function () {
					var _v8 = argsReverse.syntax;
					if (!_v8.b) {
						return nameRange;
					} else {
						var _v9 = _v8.a;
						var lastArgRange = _v9.a;
						return {end: lastArgRange.end, start: nameRange.start};
					}
				}();
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, argsReverse.comments, commentsAfterName),
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						A2(
							$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
							nameNode,
							$elm$core$List$reverse(argsReverse.syntax)))
				};
			}),
		A3(
			$stil4m$elm_syntax$ParserFast$map2WithRange,
			F3(
				function (range, startName, afterStartName) {
					var name = function () {
						if (afterStartName.$ === 'Nothing') {
							return _Utils_Tuple2(_List_Nil, startName);
						} else {
							var _v11 = afterStartName.a;
							var qualificationAfterStartName = _v11.a;
							var unqualified = _v11.b;
							return _Utils_Tuple2(
								A2($elm$core$List$cons, startName, qualificationAfterStartName),
								unqualified);
						}
					}();
					return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, name);
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$maybeDotTypeNamesTuple),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
			$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy(
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (typeAnnotationResult, commentsAfter) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, typeAnnotationResult.comments),
								syntax: typeAnnotationResult.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnExcludingTypedWithArguments(),
					$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout))));
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnExcludingTypedWithArguments() {
	return A4(
		$stil4m$elm_syntax$ParserFast$oneOf4,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation(),
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotationWithoutArguments,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$genericTypeAnnotation,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation());
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'(',
		A2(
			$stil4m$elm_syntax$ParserFast$oneOf2,
			A2(
				$stil4m$elm_syntax$ParserFast$symbolWithEndLocation,
				')',
				function (end) {
					return {
						comments: $stil4m$elm_syntax$Rope$empty,
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: end,
								start: {column: end.column - 2, row: end.row}
							},
							$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit)
					};
				}),
			A5(
				$stil4m$elm_syntax$ParserFast$map4WithRange,
				F5(
					function (rangeAfterOpeningParens, commentsBeforeFirstPart, firstPart, commentsAfterFirstPart, lastToSecondPart) {
						return {
							comments: A2(
								$stil4m$elm_syntax$Rope$prependTo,
								lastToSecondPart.comments,
								A2(
									$stil4m$elm_syntax$Rope$prependTo,
									commentsAfterFirstPart,
									A2($stil4m$elm_syntax$Rope$prependTo, firstPart.comments, commentsBeforeFirstPart))),
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{
									end: rangeAfterOpeningParens.end,
									start: {column: rangeAfterOpeningParens.start.column - 1, row: rangeAfterOpeningParens.start.row}
								},
								function () {
									var _v5 = lastToSecondPart.syntax;
									if (_v5.$ === 'Nothing') {
										var _v6 = firstPart.syntax;
										var firstPartType = _v6.b;
										return firstPartType;
									} else {
										var firstAndMaybeThirdPart = _v5.a;
										var _v7 = firstAndMaybeThirdPart.maybeThirdPart;
										if (_v7.$ === 'Nothing') {
											return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
												_List_fromArray(
													[firstPart.syntax, firstAndMaybeThirdPart.secondPart]));
										} else {
											var thirdPart = _v7.a;
											return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
												_List_fromArray(
													[firstPart.syntax, firstAndMaybeThirdPart.secondPart, thirdPart]));
										}
									}
								}())
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				A2(
					$stil4m$elm_syntax$ParserFast$oneOf2,
					A2(
						$stil4m$elm_syntax$ParserFast$symbol,
						')',
						{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}),
					A2(
						$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
						',',
						A5(
							$stil4m$elm_syntax$ParserFast$map4,
							F4(
								function (commentsBefore, secondPartResult, commentsAfter, maybeThirdPartResult) {
									return {
										comments: A2(
											$stil4m$elm_syntax$Rope$prependTo,
											commentsAfter,
											A2($stil4m$elm_syntax$Rope$prependTo, secondPartResult.comments, commentsBefore)),
										syntax: $elm$core$Maybe$Just(
											{maybeThirdPart: maybeThirdPartResult.syntax, secondPart: secondPartResult.syntax})
									};
								}),
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
							$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
							$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
							A2(
								$stil4m$elm_syntax$ParserFast$oneOf2,
								A2(
									$stil4m$elm_syntax$ParserFast$symbol,
									')',
									{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}),
								A2(
									$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
									',',
									A2(
										$stil4m$elm_syntax$ParserFast$followedBySymbol,
										')',
										A4(
											$stil4m$elm_syntax$ParserFast$map3,
											F3(
												function (commentsBefore, thirdPartResult, commentsAfter) {
													return {
														comments: A2(
															$stil4m$elm_syntax$Rope$prependTo,
															commentsAfter,
															A2($stil4m$elm_syntax$Rope$prependTo, thirdPartResult.comments, commentsBefore)),
														syntax: $elm$core$Maybe$Just(thirdPartResult.syntax)
													};
												}),
											$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
											$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
											$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))))))))));
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation() {
	return A3(
		$stil4m$elm_syntax$ParserFast$map2WithRange,
		F3(
			function (range, commentsBefore, afterCurly) {
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, afterCurly.comments, commentsBefore),
					syntax: function () {
						var _v3 = afterCurly.syntax;
						if (_v3.$ === 'Nothing') {
							return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationRecordEmpty);
						} else {
							var afterCurlyResult = _v3.a;
							return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, afterCurlyResult);
						}
					}()
				};
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '{', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		A2(
			$stil4m$elm_syntax$ParserFast$oneOf2,
			A2(
				$stil4m$elm_syntax$ParserFast$followedBySymbol,
				'}',
				A4(
					$stil4m$elm_syntax$ParserFast$map3,
					F3(
						function (firstNameNode, commentsAfterFirstName, afterFirstName) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, afterFirstName.comments, commentsAfterFirstName),
								syntax: $elm$core$Maybe$Just(
									function () {
										var _v4 = afterFirstName.syntax;
										if (_v4.$ === 'RecordExtensionExpressionAfterName') {
											var fields = _v4.a;
											return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, firstNameNode, fields);
										} else {
											var fieldsAfterName = _v4.a;
											return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(
												A2(
													$elm$core$List$cons,
													A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $elm$core$Tuple$pair, firstNameNode, fieldsAfterName.firstFieldValue),
													fieldsAfterName.tailFields));
										}
									}())
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					A2(
						$stil4m$elm_syntax$ParserFast$oneOf2,
						A2(
							$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
							'|',
							A4(
								$stil4m$elm_syntax$ParserFast$map3WithRange,
								F4(
									function (range, commentsBefore, head, tail) {
										return {
											comments: A2(
												$stil4m$elm_syntax$Rope$prependTo,
												tail.comments,
												A2($stil4m$elm_syntax$Rope$prependTo, head.comments, commentsBefore)),
											syntax: $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$RecordExtensionExpressionAfterName(
												A2(
													$stil4m$elm_syntax$Elm$Syntax$Node$Node,
													range,
													A2($elm$core$List$cons, head.syntax, tail.syntax)))
										};
									}),
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
								$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition(),
								$stil4m$elm_syntax$ParserWithComments$many(
									A2(
										$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
										',',
										A3(
											$stil4m$elm_syntax$ParserFast$map2,
											F2(
												function (commentsBefore, field) {
													return {
														comments: A2($stil4m$elm_syntax$Rope$prependTo, field.comments, commentsBefore),
														syntax: field.syntax
													};
												}),
											$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
											$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition()))))),
						A2(
							$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
							':',
							A5(
								$stil4m$elm_syntax$ParserFast$map4,
								F4(
									function (commentsBeforeFirstFieldValue, firstFieldValue, commentsAfterFirstFieldValue, tailFields) {
										return {
											comments: A2(
												$stil4m$elm_syntax$Rope$prependTo,
												tailFields.comments,
												A2(
													$stil4m$elm_syntax$Rope$prependTo,
													commentsAfterFirstFieldValue,
													A2($stil4m$elm_syntax$Rope$prependTo, firstFieldValue.comments, commentsBeforeFirstFieldValue))),
											syntax: $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$FieldsAfterName(
												{firstFieldValue: firstFieldValue.syntax, tailFields: tailFields.syntax})
										};
									}),
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
								$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
								A2(
									$stil4m$elm_syntax$ParserFast$orSucceed,
									A2(
										$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
										',',
										$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation()),
									{comments: $stil4m$elm_syntax$Rope$empty, syntax: _List_Nil})))))),
			A2(
				$stil4m$elm_syntax$ParserFast$symbol,
				'}',
				{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing})));
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (commentsBefore, head, tail) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						tail.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, head.comments, commentsBefore)),
					syntax: A2($elm$core$List$cons, head.syntax, tail.syntax)
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition(),
		$stil4m$elm_syntax$ParserWithComments$many(
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				',',
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (commentsBefore, field) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, field.comments, commentsBefore),
								syntax: field.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition()))));
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition() {
	return A7(
		$stil4m$elm_syntax$ParserFast$map6WithRange,
		F7(
			function (range, commentsBeforeFunctionName, name, commentsAfterFunctionName, commentsAfterColon, value, commentsAfterValue) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterValue,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							value.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterColon,
								A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterFunctionName, commentsBeforeFunctionName)))),
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						_Utils_Tuple2(name, value.syntax))
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation(),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout);
}
function $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (inType, commentsAfterIn, maybeOut) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						maybeOut.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterIn, inType.comments)),
					syntax: function () {
						var _v0 = maybeOut.syntax;
						if (_v0.$ === 'Nothing') {
							return inType.syntax;
						} else {
							var out = _v0.a;
							return A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, inType.syntax, out);
						}
					}()
				};
			}),
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v1) {
				return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnIncludingTypedWithArguments();
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		A4(
			$stil4m$elm_syntax$ParserFast$map2OrSucceed,
			F2(
				function (commentsAfterArrow, typeAnnotationResult) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, typeAnnotationResult.comments, commentsAfterArrow),
						syntax: $elm$core$Maybe$Just(typeAnnotationResult.syntax)
					};
				}),
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				'->',
				A2($stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedPlusFollowedBy, 2, $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)),
			$stil4m$elm_syntax$ParserFast$lazy(
				function (_v2) {
					return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation();
				}),
			{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}));
}
try {
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFnIncludingTypedWithArguments = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnIncludingTypedWithArguments();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnIncludingTypedWithArguments = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFnIncludingTypedWithArguments;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotationWithArgumentsOptimisticLayout = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typedTypeAnnotationWithArgumentsOptimisticLayout();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typedTypeAnnotationWithArgumentsOptimisticLayout = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typedTypeAnnotationWithArgumentsOptimisticLayout;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFnExcludingTypedWithArguments = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnExcludingTypedWithArguments();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotationNoFnExcludingTypedWithArguments = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFnExcludingTypedWithArguments;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$parensTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$parensTypeAnnotation = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$parensTypeAnnotation;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordTypeAnnotation = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordTypeAnnotation;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldsTypeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldsTypeAnnotation = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldsTypeAnnotation;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldDefinition = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$recordFieldDefinition = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$recordFieldDefinition;
	};
	var $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation = $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation();
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$cyclic$typeAnnotation = function () {
		return $stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.TypeAnnotation` are causing infinite recursion:\n\n  ┌─────┐\n  │    typeAnnotationNoFnIncludingTypedWithArguments\n  │     ↓\n  │    typedTypeAnnotationWithArgumentsOptimisticLayout\n  │     ↓\n  │    typeAnnotationNoFnExcludingTypedWithArguments\n  │     ↓\n  │    parensTypeAnnotation\n  │     ↓\n  │    recordTypeAnnotation\n  │     ↓\n  │    recordFieldsTypeAnnotation\n  │     ↓\n  │    recordFieldDefinition\n  │     ↓\n  │    typeAnnotation\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$ParserFast$validate = F3(
	function (isOkay, problemOnNotOkay, _v0) {
		var parseA = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var committed = _v1.a;
					var x = _v1.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var good = _v1;
					var a = good.a;
					var s1 = good.b;
					return isOkay(a) ? good : A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						true,
						A3($stil4m$elm_syntax$ParserFast$ExpectingCustom, s1.row, s1.col, problemOnNotOkay));
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$whileWithoutLinebreakAnd2PartUtf16ToResultAndThen = F3(
	function (whileCharIsOkay, consumedStringToIntermediateOrErr, intermediateToFollowupParser) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var s1Offset = A3($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakAnd2PartUtf16Help, whileCharIsOkay, s0.offset, s0.src);
				var whileContent = A3($elm$core$String$slice, s0.offset, s1Offset, s0.src);
				var _v0 = consumedStringToIntermediateOrErr(whileContent);
				if (_v0.$ === 'Err') {
					var problemMessage = _v0.a;
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A3($stil4m$elm_syntax$ParserFast$ExpectingCustom, s0.row, s0.col, problemMessage));
				} else {
					var intermediate = _v0.a;
					var s1Column = s0.col + (s1Offset - s0.offset);
					var _v1 = intermediateToFollowupParser(intermediate);
					var parseFollowup = _v1.a;
					return $stil4m$elm_syntax$ParserFast$pStepCommit(
						parseFollowup(
							{col: s1Column, indent: s0.indent, offset: s1Offset, row: s0.row, src: s0.src}));
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$changeIndent = F2(
	function (newIndent, s) {
		return {col: s.col, indent: newIndent, offset: s.offset, row: s.row, src: s.src};
	});
var $stil4m$elm_syntax$ParserFast$withIndentSetToColumn = function (_v0) {
	var parse = _v0.a;
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var _v1 = parse(
				A2($stil4m$elm_syntax$ParserFast$changeIndent, s0.col, s0));
			if (_v1.$ === 'Good') {
				var a = _v1.a;
				var s1 = _v1.b;
				return A2(
					$stil4m$elm_syntax$ParserFast$Good,
					a,
					A2($stil4m$elm_syntax$ParserFast$changeIndent, s0.indent, s1));
			} else {
				var bad = _v1;
				return bad;
			}
		});
};
var $stil4m$elm_syntax$ParserFast$withIndentSetToColumnMinus = F2(
	function (columnToMoveIndentationBaseBackBy, _v0) {
		var parse = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parse(
					A2($stil4m$elm_syntax$ParserFast$changeIndent, s0.col - columnToMoveIndentationBaseBackBy, s0));
				if (_v1.$ === 'Good') {
					var a = _v1.a;
					var s1 = _v1.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						a,
						A2($stil4m$elm_syntax$ParserFast$changeIndent, s0.indent, s1));
				} else {
					var bad = _v1;
					return bad;
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$extendedSubExpressionOptimisticLayout = F2(
	function (toResult, afterCommitting) {
		return A4(
			$stil4m$elm_syntax$ParserFast$loopWhileSucceedsOntoResultFromParser,
			$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy(
				A2($stil4m$elm_syntax$Elm$Parser$Expression$infixOperatorAndThen, toResult, afterCommitting)),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpressionMaybeAppliedOptimisticLayout(),
			F2(
				function (extensionRightResult, leftNodeWithComments) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, extensionRightResult.comments, leftNodeWithComments.comments),
						syntax: A2($stil4m$elm_syntax$Elm$Parser$Expression$applyExtensionRight, extensionRightResult.syntax, leftNodeWithComments.syntax)
					};
				}),
			$elm$core$Basics$identity);
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$infixLeft = F2(
	function (leftPrecedence, symbol) {
		return {
			extensionRight: A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsBeforeFirst, first) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, first.comments, commentsBeforeFirst),
							syntax: $stil4m$elm_syntax$Elm$Parser$Expression$ExtendRightByOperation(
								{direction: $stil4m$elm_syntax$Elm$Syntax$Infix$Left, expression: first.syntax, symbol: symbol})
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				A2(
					$stil4m$elm_syntax$Elm$Parser$Expression$extendedSubExpressionOptimisticLayout,
					function (info) {
						return (_Utils_cmp(info.leftPrecedence, leftPrecedence) > 0) ? $elm$core$Result$Ok(info) : $stil4m$elm_syntax$Elm$Parser$Expression$temporaryErrPrecedenceTooHigh;
					},
					function ($) {
						return $.extensionRight;
					})),
			leftPrecedence: leftPrecedence,
			symbol: symbol
		};
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative = F2(
	function (leftPrecedence, symbol) {
		return {
			extensionRight: A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsBefore, right) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, right.comments, commentsBefore),
							syntax: $stil4m$elm_syntax$Elm$Parser$Expression$ExtendRightByOperation(
								{direction: $stil4m$elm_syntax$Elm$Syntax$Infix$Non, expression: right.syntax, symbol: symbol})
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				A2(
					$stil4m$elm_syntax$Elm$Parser$Expression$extendedSubExpressionOptimisticLayout,
					function (info) {
						return (_Utils_cmp(info.leftPrecedence, leftPrecedence) > -1) ? $elm$core$Result$Ok(info) : $stil4m$elm_syntax$Elm$Parser$Expression$temporaryErrPrecedenceTooHigh;
					},
					function (info) {
						return _Utils_eq(info.leftPrecedence, leftPrecedence) ? $stil4m$elm_syntax$Elm$Parser$Expression$problemCannotMixNonAssociativeInfixOperators : info.extensionRight;
					})),
			leftPrecedence: leftPrecedence,
			symbol: symbol
		};
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$infixOperatorAndThen = F2(
	function (toResult, afterCommitting) {
		return A3(
			$stil4m$elm_syntax$ParserFast$whileWithoutLinebreakAnd2PartUtf16ToResultAndThen,
			$stil4m$elm_syntax$Elm$Parser$Tokens$isOperatorSymbolChar,
			function (operator) {
				switch (operator) {
					case '|>':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApR());
					case '++':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5append());
					case '<|':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApL());
					case '>>':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeR());
					case '==':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Eq());
					case '*':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Mul());
					case '::':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Cons());
					case '+':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Add());
					case '-':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Sub());
					case '|.':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Ignore());
					case '&&':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence3And());
					case '|=':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Keep());
					case '<<':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeL());
					case '/=':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Neq());
					case '//':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Idiv());
					case '/':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Fdiv());
					case '</>':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Slash());
					case '||':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence2Or());
					case '<=':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Le());
					case '>=':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Ge());
					case '>':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Gt());
					case '<?>':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8QuestionMark());
					case '<':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Lt());
					case '^':
						return toResult(
							$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8Pow());
					default:
						return $stil4m$elm_syntax$Elm$Parser$Expression$errUnknownInfixOperator;
				}
			},
			afterCommitting);
	});
var $stil4m$elm_syntax$Elm$Parser$Expression$infixRight = F2(
	function (leftPrecedence, symbol) {
		return {
			extensionRight: A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsBeforeFirst, first) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, first.comments, commentsBeforeFirst),
							syntax: $stil4m$elm_syntax$Elm$Parser$Expression$ExtendRightByOperation(
								{direction: $stil4m$elm_syntax$Elm$Syntax$Infix$Right, expression: first.syntax, symbol: symbol})
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				A2(
					$stil4m$elm_syntax$Elm$Parser$Expression$extendedSubExpressionOptimisticLayout,
					function (info) {
						return (_Utils_cmp(info.leftPrecedence, leftPrecedence) > -1) ? $elm$core$Result$Ok(info) : $stil4m$elm_syntax$Elm$Parser$Expression$temporaryErrPrecedenceTooHigh;
					},
					function ($) {
						return $.extensionRight;
					})),
			leftPrecedence: leftPrecedence,
			symbol: symbol
		};
	});
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letOrUnqualifiedReferenceExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letExpression(),
		$stil4m$elm_syntax$Elm$Parser$Expression$unqualifiedFunctionReferenceExpressionFollowedByRecordAccess);
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$keywordFollowedBy,
		'let',
		A4(
			$stil4m$elm_syntax$ParserFast$map3WithStartLocation,
			F4(
				function (start, declarations, commentsAfterIn, expressionResult) {
					var _v38 = expressionResult.syntax;
					var expressionRange = _v38.a;
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							expressionResult.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterIn, declarations.comments)),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: expressionRange.end,
								start: {column: start.column - 3, row: start.row}
							},
							$stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression(
								{declarations: declarations.declarations, expression: expressionResult.syntax}))
					};
				}),
			A2(
				$stil4m$elm_syntax$ParserFast$withIndentSetToColumnMinus,
				3,
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (commentsAfterLet, declarations) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, declarations.comments, commentsAfterLet),
								declarations: declarations.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$ParserFast$withIndentSetToColumn(
						$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDeclarationsIn()))),
			A2($stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedPlusFollowedBy, 2, $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDeclarationsIn() {
	return $stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(
		A4(
			$stil4m$elm_syntax$ParserFast$map3,
			F3(
				function (headLetResult, commentsAfter, tailLetResult) {
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							tailLetResult.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, headLetResult.comments)),
						syntax: A2($elm$core$List$cons, headLetResult.syntax, tailLetResult.syntax)
					};
				}),
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letFunction(),
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDestructuringDeclaration()),
			$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
			A2(
				$stil4m$elm_syntax$ParserWithComments$until,
				$stil4m$elm_syntax$Elm$Parser$Tokens$inToken,
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$blockElement())));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$blockElement() {
	return $stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(
		A3(
			$stil4m$elm_syntax$ParserFast$map2,
			F2(
				function (letDeclarationResult, commentsAfter) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, letDeclarationResult.comments),
						syntax: letDeclarationResult.syntax
					};
				}),
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letFunction(),
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDestructuringDeclaration()),
			$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseOrUnqualifiedReferenceExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseExpression(),
		$stil4m$elm_syntax$Elm$Parser$Expression$unqualifiedFunctionReferenceExpressionFollowedByRecordAccess);
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$keywordFollowedBy,
		'case',
		A6(
			$stil4m$elm_syntax$ParserFast$map5WithStartLocation,
			F6(
				function (start, commentsAfterCase, casedExpressionResult, commentsBeforeOf, commentsAfterOf, casesResult) {
					var _v32 = casesResult.syntax;
					var firstCase = _v32.a;
					var lastToSecondCase = _v32.b;
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							casesResult.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterOf,
								A2(
									$stil4m$elm_syntax$Rope$prependTo,
									commentsBeforeOf,
									A2($stil4m$elm_syntax$Rope$prependTo, casedExpressionResult.comments, commentsAfterCase)))),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: function () {
									if (lastToSecondCase.b) {
										var _v34 = lastToSecondCase.a;
										var _v35 = _v34.b;
										var lastCaseExpressionRange = _v35.a;
										return lastCaseExpressionRange.end;
									} else {
										var _v36 = firstCase;
										var _v37 = _v36.b;
										var firstCaseExpressionRange = _v37.a;
										return firstCaseExpressionRange.end;
									}
								}(),
								start: {column: start.column - 4, row: start.row}
							},
							$stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(
								{
									cases: A2(
										$elm$core$List$cons,
										firstCase,
										$elm$core$List$reverse(lastToSecondCase)),
									expression: casedExpressionResult.syntax
								}))
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'of', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$ParserFast$withIndentSetToColumn(
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatements())));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatements() {
	return A6(
		$stil4m$elm_syntax$ParserFast$map5,
		F5(
			function (firstCasePatternResult, commentsAfterFirstCasePattern, commentsAfterFirstCaseArrowRight, firstCaseExpressionResult, lastToSecondCase) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						lastToSecondCase.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							firstCaseExpressionResult.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterFirstCaseArrowRight,
								A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterFirstCasePattern, firstCasePatternResult.comments)))),
					syntax: _Utils_Tuple2(
						_Utils_Tuple2(firstCasePatternResult.syntax, firstCaseExpressionResult.syntax),
						lastToSecondCase.syntax)
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$pattern,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '->', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
		$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatement()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatement() {
	return $stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(
		A5(
			$stil4m$elm_syntax$ParserFast$map4,
			F4(
				function (pattern, commentsBeforeArrowRight, commentsAfterArrowRight, expr) {
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							expr.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterArrowRight,
								A2($stil4m$elm_syntax$Rope$prependTo, commentsBeforeArrowRight, pattern.comments))),
						syntax: _Utils_Tuple2(pattern.syntax, expr.syntax)
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Patterns$pattern,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '->', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionIfNecessaryFollowedByRecordAccess() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'(',
		A3(
			$stil4m$elm_syntax$ParserFast$oneOf3,
			A2(
				$stil4m$elm_syntax$ParserFast$symbolWithEndLocation,
				')',
				function (end) {
					return {
						comments: $stil4m$elm_syntax$Rope$empty,
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: end,
								start: {column: end.column - 2, row: end.row}
							},
							$stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr)
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Expression$allowedPrefixOperatorFollowedByClosingParensOneOf,
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionInnerAfterOpeningParens()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionInnerAfterOpeningParens() {
	return A5(
		$stil4m$elm_syntax$ParserFast$map4WithRange,
		F5(
			function (rangeAfterOpeningParens, commentsBeforeFirstPart, firstPart, commentsAfterFirstPart, tailParts) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						tailParts.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsAfterFirstPart,
							A2($stil4m$elm_syntax$Rope$prependTo, firstPart.comments, commentsBeforeFirstPart))),
					syntax: function () {
						var _v27 = tailParts.syntax;
						if (_v27.$ === 'TupledParenthesizedFollowedByRecordAccesses') {
							var recordAccesses = _v27.a;
							if (!recordAccesses.b) {
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									{
										end: rangeAfterOpeningParens.end,
										start: {column: rangeAfterOpeningParens.start.column - 1, row: rangeAfterOpeningParens.start.row}
									},
									$stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(firstPart.syntax));
							} else {
								var _v29 = recordAccesses.a;
								var firstRecordAccessRange = _v29.a;
								var range = {
									end: {column: firstRecordAccessRange.start.column - 1, row: firstRecordAccessRange.start.row},
									start: {column: rangeAfterOpeningParens.start.column - 1, row: rangeAfterOpeningParens.start.row}
								};
								var parenthesizedNode = A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									range,
									$stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(firstPart.syntax));
								return A3(
									$elm$core$List$foldl,
									F2(
										function (fieldNode, leftNode) {
											var fieldRange = fieldNode.a;
											var leftRange = leftNode.a;
											return A2(
												$stil4m$elm_syntax$Elm$Syntax$Node$Node,
												{end: fieldRange.end, start: leftRange.start},
												A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, leftNode, fieldNode));
										}),
									parenthesizedNode,
									recordAccesses);
							}
						} else {
							var _v30 = _v27.a;
							var secondPart = _v30.a;
							var maybeThirdPart = _v30.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{
									end: rangeAfterOpeningParens.end,
									start: {column: rangeAfterOpeningParens.start.column - 1, row: rangeAfterOpeningParens.start.row}
								},
								function () {
									if (maybeThirdPart.$ === 'Nothing') {
										return $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
											_List_fromArray(
												[firstPart.syntax, secondPart]));
									} else {
										var thirdPart = maybeThirdPart.a;
										return $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
											_List_fromArray(
												[firstPart.syntax, secondPart, thirdPart]));
									}
								}());
						}
					}()
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A2(
			$stil4m$elm_syntax$ParserFast$oneOf2,
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				')',
				$stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccessMap(
					function (recordAccesses) {
						return {
							comments: $stil4m$elm_syntax$Rope$empty,
							syntax: $stil4m$elm_syntax$Elm$Parser$Expression$TupledParenthesizedFollowedByRecordAccesses(recordAccesses)
						};
					})),
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				',',
				A5(
					$stil4m$elm_syntax$ParserFast$map4,
					F4(
						function (commentsBefore, partResult, commentsAfter, maybeThirdPart) {
							return {
								comments: A2(
									$stil4m$elm_syntax$Rope$prependTo,
									maybeThirdPart.comments,
									A2(
										$stil4m$elm_syntax$Rope$prependTo,
										commentsAfter,
										A2($stil4m$elm_syntax$Rope$prependTo, partResult.comments, commentsBefore))),
								syntax: $stil4m$elm_syntax$Elm$Parser$Expression$TupledTwoOrThree(
									_Utils_Tuple2(partResult.syntax, maybeThirdPart.syntax))
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					A2(
						$stil4m$elm_syntax$ParserFast$oneOf2,
						A2(
							$stil4m$elm_syntax$ParserFast$symbol,
							')',
							{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}),
						A2(
							$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
							',',
							A2(
								$stil4m$elm_syntax$ParserFast$followedBySymbol,
								')',
								A4(
									$stil4m$elm_syntax$ParserFast$map3,
									F3(
										function (commentsBefore, partResult, commentsAfter) {
											return {
												comments: A2(
													$stil4m$elm_syntax$Rope$prependTo,
													commentsAfter,
													A2($stil4m$elm_syntax$Rope$prependTo, partResult.comments, commentsBefore)),
												syntax: $elm$core$Maybe$Just(partResult.syntax)
											};
										}),
									$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
									$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
									$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))))))));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordExpressionFollowedByRecordAccess() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'{',
		A3(
			$stil4m$elm_syntax$ParserFast$map2,
			F2(
				function (leftestResult, recordAccesses) {
					if (!recordAccesses.b) {
						return leftestResult;
					} else {
						return {
							comments: leftestResult.comments,
							syntax: A3(
								$elm$core$List$foldl,
								F2(
									function (fieldNode, leftNode) {
										var fieldRange = fieldNode.a;
										var leftRange = leftNode.a;
										return A2(
											$stil4m$elm_syntax$Elm$Syntax$Node$Node,
											{end: fieldRange.end, start: leftRange.start},
											A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, leftNode, fieldNode));
									}),
								leftestResult.syntax,
								recordAccesses)
						};
					}
				}),
			A3(
				$stil4m$elm_syntax$ParserFast$map2WithRange,
				F3(
					function (range, commentsBefore, afterCurly) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, afterCurly.comments, commentsBefore),
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								$stil4m$elm_syntax$Elm$Parser$Expression$rangeMoveStartLeftByOneColumn(range),
								afterCurly.syntax)
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordContentsCurlyEnd()),
			$stil4m$elm_syntax$Elm$Parser$Expression$multiRecordAccess));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordContentsCurlyEnd() {
	return A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		A6(
			$stil4m$elm_syntax$ParserFast$map5,
			F5(
				function (nameNode, commentsAfterFunctionName, afterNameBeforeFields, tailFields, commentsBeforeClosingCurly) {
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsBeforeClosingCurly,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								tailFields.comments,
								A2($stil4m$elm_syntax$Rope$prependTo, afterNameBeforeFields.comments, commentsAfterFunctionName))),
						syntax: function () {
							var _v25 = afterNameBeforeFields.syntax;
							if (_v25.$ === 'RecordUpdateFirstSetter') {
								var firstField = _v25.a;
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression,
									nameNode,
									A2($elm$core$List$cons, firstField, tailFields.syntax));
							} else {
								var firstFieldValue = _v25.a;
								return $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(
									A2(
										$elm$core$List$cons,
										A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $elm$core$Tuple$pair, nameNode, firstFieldValue),
										tailFields.syntax));
							}
						}()
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				A2(
					$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
					'|',
					A3(
						$stil4m$elm_syntax$ParserFast$map2,
						F2(
							function (commentsBefore, setterResult) {
								return {
									comments: A2($stil4m$elm_syntax$Rope$prependTo, setterResult.comments, commentsBefore),
									syntax: $stil4m$elm_syntax$Elm$Parser$Expression$RecordUpdateFirstSetter(setterResult.syntax)
								};
							}),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
						$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordSetterNodeWithLayout())),
				A2(
					$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
					'=',
					A4(
						$stil4m$elm_syntax$ParserFast$map3,
						F3(
							function (commentsBefore, expressionResult, commentsAfter) {
								return {
									comments: A2(
										$stil4m$elm_syntax$Rope$prependTo,
										commentsAfter,
										A2($stil4m$elm_syntax$Rope$prependTo, expressionResult.comments, commentsBefore)),
									syntax: $stil4m$elm_syntax$Elm$Parser$Expression$FieldsFirstValue(expressionResult.syntax)
								};
							}),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
						$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordFields(),
			A2($stil4m$elm_syntax$ParserFast$followedBySymbol, '}', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)),
		A2(
			$stil4m$elm_syntax$ParserFast$symbol,
			'}',
			{
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(_List_Nil)
			}));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordFields() {
	return $stil4m$elm_syntax$ParserWithComments$many(
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			',',
			A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (commentsBefore, setterResult) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, setterResult.comments, commentsBefore),
							syntax: setterResult.syntax
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordSetterNodeWithLayout())));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordSetterNodeWithLayout() {
	return A6(
		$stil4m$elm_syntax$ParserFast$map5WithRange,
		F6(
			function (range, name, commentsAfterFunctionName, commentsAfterEquals, expressionResult, commentsAfterExpression) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterExpression,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							expressionResult.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterEquals, commentsAfterFunctionName))),
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						_Utils_Tuple2(name, expressionResult.syntax))
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '=', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout);
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letFunction() {
	return A3(
		$stil4m$elm_syntax$ParserFast$validate,
		function (result) {
			var _v18 = result.syntax;
			var letDeclaration = _v18.b;
			if (letDeclaration.$ === 'LetDestructuring') {
				return true;
			} else {
				var letFunctionDeclaration = letDeclaration.a;
				var _v20 = letFunctionDeclaration.signature;
				if (_v20.$ === 'Nothing') {
					return true;
				} else {
					var _v21 = _v20.a;
					var signature = _v21.b;
					var _v22 = signature.name;
					var signatureName = _v22.b;
					var _v23 = letFunctionDeclaration.declaration;
					var implementation = _v23.b;
					var _v24 = implementation.name;
					var implementationName = _v24.b;
					return _Utils_eq(implementationName, signatureName + '');
				}
			}
		},
		'Expected to find the same name for declaration and signature',
		A7(
			$stil4m$elm_syntax$ParserFast$map6WithStartLocation,
			F7(
				function (startNameStart, startNameNode, commentsAfterStartName, maybeSignature, _arguments, commentsAfterEqual, expressionResult) {
					var allComments = A2(
						$stil4m$elm_syntax$Rope$prependTo,
						expressionResult.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsAfterEqual,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								_arguments.comments,
								function () {
									if (maybeSignature.$ === 'Nothing') {
										return commentsAfterStartName;
									} else {
										var signature = maybeSignature.a;
										return A2($stil4m$elm_syntax$Rope$prependTo, signature.comments, commentsAfterStartName);
									}
								}())));
					if (maybeSignature.$ === 'Nothing') {
						var _v14 = expressionResult.syntax;
						var expressionRange = _v14.a;
						return {
							comments: allComments,
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: expressionRange.end, start: startNameStart},
								$stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction(
									{
										declaration: A2(
											$stil4m$elm_syntax$Elm$Syntax$Node$Node,
											{end: expressionRange.end, start: startNameStart},
											{_arguments: _arguments.syntax, expression: expressionResult.syntax, name: startNameNode}),
										documentation: $elm$core$Maybe$Nothing,
										signature: $elm$core$Maybe$Nothing
									}))
						};
					} else {
						var signature = maybeSignature.a;
						var _v15 = signature.implementationName;
						var implementationNameRange = _v15.a;
						var _v16 = expressionResult.syntax;
						var expressionRange = _v16.a;
						return {
							comments: allComments,
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: expressionRange.end, start: startNameStart},
								$stil4m$elm_syntax$Elm$Syntax$Expression$LetFunction(
									{
										declaration: A2(
											$stil4m$elm_syntax$Elm$Syntax$Node$Node,
											{end: expressionRange.end, start: implementationNameRange.start},
											{_arguments: _arguments.syntax, expression: expressionResult.syntax, name: signature.implementationName}),
										documentation: $elm$core$Maybe$Nothing,
										signature: $elm$core$Maybe$Just(
											A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Signature$Signature, startNameNode, signature.typeAnnotation))
									}))
						};
					}
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A6(
				$stil4m$elm_syntax$ParserFast$map4OrSucceed,
				F4(
					function (commentsBeforeTypeAnnotation, typeAnnotationResult, implementationName, afterImplementationName) {
						return $elm$core$Maybe$Just(
							{
								comments: A2(
									$stil4m$elm_syntax$Rope$prependTo,
									afterImplementationName,
									A2(
										$stil4m$elm_syntax$Rope$prependTo,
										implementationName.comments,
										A2($stil4m$elm_syntax$Rope$prependTo, typeAnnotationResult.comments, commentsBeforeTypeAnnotation))),
								implementationName: implementationName.syntax,
								typeAnnotation: typeAnnotationResult.syntax
							});
					}),
				A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
				$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
				$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedBy($stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode),
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
				$elm$core$Maybe$Nothing),
			$stil4m$elm_syntax$Elm$Parser$Expression$parameterPatternsEqual,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDestructuringDeclaration() {
	return A5(
		$stil4m$elm_syntax$ParserFast$map4,
		F4(
			function (pattern, commentsAfterPattern, commentsAfterEquals, expressionResult) {
				var _v11 = pattern.syntax;
				var start = _v11.a.start;
				var _v12 = expressionResult.syntax;
				var end = _v12.a.end;
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						expressionResult.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsAfterEquals,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterPattern, pattern.comments))),
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						{end: end, start: start},
						A2($stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring, pattern.syntax, expressionResult.syntax))
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '=', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression());
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$lambdaExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'\\',
		A7(
			$stil4m$elm_syntax$ParserFast$map6WithStartLocation,
			F7(
				function (start, commentsAfterBackslash, firstArg, commentsAfterFirstArg, secondUpArgs, commentsAfterArrow, expressionResult) {
					var _v10 = expressionResult.syntax;
					var expressionRange = _v10.a;
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							expressionResult.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterArrow,
								A2(
									$stil4m$elm_syntax$Rope$prependTo,
									secondUpArgs.comments,
									A2(
										$stil4m$elm_syntax$Rope$prependTo,
										commentsAfterFirstArg,
										A2($stil4m$elm_syntax$Rope$prependTo, firstArg.comments, commentsAfterBackslash))))),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: expressionRange.end,
								start: {column: start.column - 1, row: start.row}
							},
							$stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression(
								{
									args: A2($elm$core$List$cons, firstArg.syntax, secondUpArgs.syntax),
									expression: expressionResult.syntax
								}))
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2(
				$stil4m$elm_syntax$ParserWithComments$until,
				A2($stil4m$elm_syntax$ParserFast$symbol, '->', _Utils_Tuple0),
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (patternResult, commentsAfter) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, patternResult.comments),
								syntax: patternResult.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing,
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifOrUnqualifiedReferenceExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifBlockExpression(),
		$stil4m$elm_syntax$Elm$Parser$Expression$unqualifiedFunctionReferenceExpressionFollowedByRecordAccess);
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifBlockExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$keywordFollowedBy,
		'if',
		A9(
			$stil4m$elm_syntax$ParserFast$map8WithStartLocation,
			F9(
				function (start, commentsAfterIf, condition, commentsBeforeThen, commentsAfterThen, ifTrue, commentsBeforeElse, commentsAfterElse, ifFalse) {
					var _v9 = ifFalse.syntax;
					var ifFalseRange = _v9.a;
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							ifFalse.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterElse,
								A2(
									$stil4m$elm_syntax$Rope$prependTo,
									commentsBeforeElse,
									A2(
										$stil4m$elm_syntax$Rope$prependTo,
										ifTrue.comments,
										A2(
											$stil4m$elm_syntax$Rope$prependTo,
											commentsAfterThen,
											A2(
												$stil4m$elm_syntax$Rope$prependTo,
												commentsBeforeThen,
												A2($stil4m$elm_syntax$Rope$prependTo, condition.comments, commentsAfterIf))))))),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: ifFalseRange.end,
								start: {column: start.column - 2, row: start.row}
							},
							A3($stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock, condition.syntax, ifTrue.syntax, ifFalse.syntax))
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'then', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'else', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$listOrGlslExpression() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'[',
		$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expressionAfterOpeningSquareBracket());
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expressionAfterOpeningSquareBracket() {
	return A2(
		$stil4m$elm_syntax$ParserFast$oneOf2,
		$stil4m$elm_syntax$Elm$Parser$Expression$glslExpressionAfterOpeningSquareBracket,
		A3(
			$stil4m$elm_syntax$ParserFast$map2WithRange,
			F3(
				function (range, commentsBefore, elements) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, elements.comments, commentsBefore),
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{
								end: range.end,
								start: {column: range.start.column - 1, row: range.start.row}
							},
							elements.syntax)
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			A2(
				$stil4m$elm_syntax$ParserFast$oneOf2,
				A2(
					$stil4m$elm_syntax$ParserFast$symbol,
					']',
					{
						comments: $stil4m$elm_syntax$Rope$empty,
						syntax: $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(_List_Nil)
					}),
				A2(
					$stil4m$elm_syntax$ParserFast$followedBySymbol,
					']',
					A4(
						$stil4m$elm_syntax$ParserFast$map3,
						F3(
							function (head, commentsAfterHead, tail) {
								return {
									comments: A2(
										$stil4m$elm_syntax$Rope$prependTo,
										tail.comments,
										A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterHead, head.comments)),
									syntax: $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(
										A2($elm$core$List$cons, head.syntax, tail.syntax))
								};
							}),
						$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression(),
						$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
						$stil4m$elm_syntax$ParserWithComments$many(
							A2(
								$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
								',',
								$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides(
									$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression()))))))));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression() {
	return A2(
		$stil4m$elm_syntax$Elm$Parser$Expression$extendedSubExpressionOptimisticLayout,
		$elm$core$Result$Ok,
		function ($) {
			return $.extensionRight;
		});
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeL() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 9, '<<');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8QuestionMark() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 8, '<?>');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Mul() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 7, '*');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Idiv() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 7, '//');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Fdiv() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 7, '/');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Sub() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 6, '-');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Ignore() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 6, '|.');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Add() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 6, '+');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Keep() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 5, '|=');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApR() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixLeft, 1, '|>');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Neq() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '/=');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Lt() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '<');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Le() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '<=');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Gt() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '>');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Ge() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '>=');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Eq() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixNonAssociative, 4, '==');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeR() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 9, '>>');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8Pow() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 8, '^');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Slash() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 7, '</>');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5append() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 5, '++');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Cons() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 5, '::');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence3And() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 3, '&&');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence2Or() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 2, '||');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApL() {
	return A2($stil4m$elm_syntax$Elm$Parser$Expression$infixRight, 1, '<|');
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus() {
	return A2(
		$stil4m$elm_syntax$ParserFast$map,
		function (subExpressionResult) {
			var _v6 = subExpressionResult.syntax;
			var subExpressionRange = _v6.a;
			return {
				comments: subExpressionResult.comments,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					{
						end: subExpressionRange.end,
						start: {column: subExpressionRange.start.column - 1, row: subExpressionRange.start.row}
					},
					$stil4m$elm_syntax$Elm$Syntax$Expression$Negation(subExpressionResult.syntax))
			};
		},
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v7) {
				return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression();
			}));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationOperation() {
	return A2(
		$stil4m$elm_syntax$ParserFast$symbolBacktrackableFollowedBy,
		'-',
		$stil4m$elm_syntax$ParserFast$offsetSourceAndThen(
			F2(
				function (offset, source) {
					var _v5 = A3($elm$core$String$slice, offset - 2, offset - 1, source);
					switch (_v5) {
						case ' ':
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
						case '(':
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
						case ')':
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
						case '}':
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
						case '':
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
						default:
							return $stil4m$elm_syntax$Elm$Parser$Expression$negationWhitespaceProblem;
					}
				})));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression() {
	return $stil4m$elm_syntax$ParserFast$offsetSourceAndThen(
		F2(
			function (offset, source) {
				var _v4 = A3($elm$core$String$slice, offset, offset + 1, source);
				switch (_v4) {
					case '\"':
						return $stil4m$elm_syntax$Elm$Parser$Expression$literalExpression;
					case '(':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionIfNecessaryFollowedByRecordAccess();
					case '[':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$listOrGlslExpression();
					case '{':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordExpressionFollowedByRecordAccess();
					case 'c':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseOrUnqualifiedReferenceExpression();
					case '\\':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$lambdaExpression();
					case 'l':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letOrUnqualifiedReferenceExpression();
					case 'i':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifOrUnqualifiedReferenceExpression();
					case '.':
						return $stil4m$elm_syntax$Elm$Parser$Expression$recordAccessFunctionExpression;
					case '-':
						return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationOperation();
					case '\'':
						return $stil4m$elm_syntax$Elm$Parser$Expression$charLiteralExpression;
					default:
						return $stil4m$elm_syntax$Elm$Parser$Expression$referenceOrNumberExpression;
				}
			}));
}
function $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpressionMaybeAppliedOptimisticLayout() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (leftExpressionResult, commentsBeforeExtension, maybeArgsReverse) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						maybeArgsReverse.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsBeforeExtension, leftExpressionResult.comments)),
					syntax: function () {
						var _v0 = maybeArgsReverse.syntax;
						if (!_v0.b) {
							return leftExpressionResult.syntax;
						} else {
							var argsReverse = _v0;
							var _v1 = argsReverse.a;
							var lastArgRange = _v1.a;
							var leftNode = leftExpressionResult.syntax;
							var leftRange = leftNode.a;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: lastArgRange.end, start: leftRange.start},
								$stil4m$elm_syntax$Elm$Syntax$Expression$Application(
									A2(
										$elm$core$List$cons,
										leftNode,
										$elm$core$List$reverse(argsReverse))));
						}
					}()
				};
			}),
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v2) {
				return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression();
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
			A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (arg, commentsAfter) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, arg.comments),
							syntax: arg.syntax
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy(
					$stil4m$elm_syntax$ParserFast$lazy(
						function (_v3) {
							return $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression();
						})),
				$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout)));
}
try {
	var $stil4m$elm_syntax$Elm$Parser$Expression$letOrUnqualifiedReferenceExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letOrUnqualifiedReferenceExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letOrUnqualifiedReferenceExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$letOrUnqualifiedReferenceExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$letExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$letExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$letDeclarationsIn = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDeclarationsIn();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDeclarationsIn = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$letDeclarationsIn;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$blockElement = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$blockElement();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$blockElement = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$blockElement;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$caseOrUnqualifiedReferenceExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseOrUnqualifiedReferenceExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseOrUnqualifiedReferenceExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$caseOrUnqualifiedReferenceExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$caseExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$caseExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$caseStatements = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatements();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatements = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$caseStatements;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$caseStatement = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatement();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$caseStatement = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$caseStatement;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$tupledExpressionIfNecessaryFollowedByRecordAccess = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionIfNecessaryFollowedByRecordAccess();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionIfNecessaryFollowedByRecordAccess = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$tupledExpressionIfNecessaryFollowedByRecordAccess;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$tupledExpressionInnerAfterOpeningParens = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionInnerAfterOpeningParens();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$tupledExpressionInnerAfterOpeningParens = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$tupledExpressionInnerAfterOpeningParens;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$recordExpressionFollowedByRecordAccess = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordExpressionFollowedByRecordAccess();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordExpressionFollowedByRecordAccess = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$recordExpressionFollowedByRecordAccess;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$recordContentsCurlyEnd = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordContentsCurlyEnd();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordContentsCurlyEnd = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$recordContentsCurlyEnd;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$recordFields = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordFields();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordFields = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$recordFields;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$recordSetterNodeWithLayout = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordSetterNodeWithLayout();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$recordSetterNodeWithLayout = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$recordSetterNodeWithLayout;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$letFunction = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letFunction();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letFunction = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$letFunction;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$letDestructuringDeclaration = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDestructuringDeclaration();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$letDestructuringDeclaration = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$letDestructuringDeclaration;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$lambdaExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$lambdaExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$lambdaExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$lambdaExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$ifOrUnqualifiedReferenceExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifOrUnqualifiedReferenceExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifOrUnqualifiedReferenceExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$ifOrUnqualifiedReferenceExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$ifBlockExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifBlockExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$ifBlockExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$ifBlockExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$listOrGlslExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$listOrGlslExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$listOrGlslExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$listOrGlslExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$expressionAfterOpeningSquareBracket = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expressionAfterOpeningSquareBracket();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expressionAfterOpeningSquareBracket = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$expressionAfterOpeningSquareBracket;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$expression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$expression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$expression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence9ComposeL = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeL();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeL = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence9ComposeL;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence8QuestionMark = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8QuestionMark();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8QuestionMark = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence8QuestionMark;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Mul = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Mul();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Mul = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Mul;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Idiv = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Idiv();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Idiv = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Idiv;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Fdiv = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Fdiv();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Fdiv = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Fdiv;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Sub = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Sub();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Sub = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Sub;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Ignore = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Ignore();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Ignore = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Ignore;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Add = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Add();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence6Add = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence6Add;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence5Keep = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Keep();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Keep = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence5Keep;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence1ApR = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApR();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApR = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence1ApR;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Neq = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Neq();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Neq = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Neq;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Lt = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Lt();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Lt = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Lt;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Le = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Le();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Le = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Le;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Gt = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Gt();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Gt = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Gt;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Ge = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Ge();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Ge = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Ge;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Eq = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Eq();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence4Eq = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence4Eq;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence9ComposeR = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeR();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence9ComposeR = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence9ComposeR;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence8Pow = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8Pow();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence8Pow = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence8Pow;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Slash = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Slash();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence7Slash = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence7Slash;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence5append = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5append();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5append = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence5append;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence5Cons = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Cons();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence5Cons = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence5Cons;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence3And = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence3And();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence3And = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence3And;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence2Or = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence2Or();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence2Or = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence2Or;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$precedence1ApL = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApL();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$precedence1ApL = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$precedence1ApL;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$negationAfterMinus = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationAfterMinus = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$negationAfterMinus;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$negationOperation = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationOperation();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$negationOperation = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$negationOperation;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$subExpression = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpression = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$subExpression;
	};
	var $stil4m$elm_syntax$Elm$Parser$Expression$subExpressionMaybeAppliedOptimisticLayout = $stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpressionMaybeAppliedOptimisticLayout();
	$stil4m$elm_syntax$Elm$Parser$Expression$cyclic$subExpressionMaybeAppliedOptimisticLayout = function () {
		return $stil4m$elm_syntax$Elm$Parser$Expression$subExpressionMaybeAppliedOptimisticLayout;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.Expression` are causing infinite recursion:\n\n  ┌─────┐\n  │    letOrUnqualifiedReferenceExpression\n  │     ↓\n  │    letExpression\n  │     ↓\n  │    letDeclarationsIn\n  │     ↓\n  │    blockElement\n  │     ↓\n  │    caseOrUnqualifiedReferenceExpression\n  │     ↓\n  │    caseExpression\n  │     ↓\n  │    caseStatements\n  │     ↓\n  │    caseStatement\n  │     ↓\n  │    tupledExpressionIfNecessaryFollowedByRecordAccess\n  │     ↓\n  │    tupledExpressionInnerAfterOpeningParens\n  │     ↓\n  │    recordExpressionFollowedByRecordAccess\n  │     ↓\n  │    recordContentsCurlyEnd\n  │     ↓\n  │    recordFields\n  │     ↓\n  │    recordSetterNodeWithLayout\n  │     ↓\n  │    letFunction\n  │     ↓\n  │    letDestructuringDeclaration\n  │     ↓\n  │    lambdaExpression\n  │     ↓\n  │    ifOrUnqualifiedReferenceExpression\n  │     ↓\n  │    ifBlockExpression\n  │     ↓\n  │    listOrGlslExpression\n  │     ↓\n  │    expressionAfterOpeningSquareBracket\n  │     ↓\n  │    expression\n  │     ↓\n  │    extendedSubExpressionOptimisticLayout\n  │     ↓\n  │    precedence9ComposeL\n  │     ↓\n  │    precedence8QuestionMark\n  │     ↓\n  │    precedence7Mul\n  │     ↓\n  │    precedence7Idiv\n  │     ↓\n  │    precedence7Fdiv\n  │     ↓\n  │    precedence6Sub\n  │     ↓\n  │    precedence6Ignore\n  │     ↓\n  │    precedence6Add\n  │     ↓\n  │    precedence5Keep\n  │     ↓\n  │    precedence1ApR\n  │     ↓\n  │    infixLeft\n  │     ↓\n  │    precedence4Neq\n  │     ↓\n  │    precedence4Lt\n  │     ↓\n  │    precedence4Le\n  │     ↓\n  │    precedence4Gt\n  │     ↓\n  │    precedence4Ge\n  │     ↓\n  │    precedence4Eq\n  │     ↓\n  │    infixNonAssociative\n  │     ↓\n  │    infixOperatorAndThen\n  │     ↓\n  │    precedence9ComposeR\n  │     ↓\n  │    precedence8Pow\n  │     ↓\n  │    precedence7Slash\n  │     ↓\n  │    precedence5append\n  │     ↓\n  │    precedence5Cons\n  │     ↓\n  │    precedence3And\n  │     ↓\n  │    precedence2Or\n  │     ↓\n  │    precedence1ApL\n  │     ↓\n  │    infixRight\n  │     ↓\n  │    negationAfterMinus\n  │     ↓\n  │    negationOperation\n  │     ↓\n  │    subExpression\n  │     ↓\n  │    subExpressionMaybeAppliedOptimisticLayout\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $stil4m$elm_syntax$ParserFast$map6 = F7(
	function (func, _v0, _v1, _v2, _v3, _v4, _v5) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		var parseF = _v5.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v6 = parseA(s0);
				if (_v6.$ === 'Bad') {
					var committed = _v6.a;
					var x = _v6.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v6.a;
					var s1 = _v6.b;
					var _v7 = parseB(s1);
					if (_v7.$ === 'Bad') {
						var x = _v7.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v7.a;
						var s2 = _v7.b;
						var _v8 = parseC(s2);
						if (_v8.$ === 'Bad') {
							var x = _v8.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v8.a;
							var s3 = _v8.b;
							var _v9 = parseD(s3);
							if (_v9.$ === 'Bad') {
								var x = _v9.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v9.a;
								var s4 = _v9.b;
								var _v10 = parseE(s4);
								if (_v10.$ === 'Bad') {
									var x = _v10.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v10.a;
									var s5 = _v10.b;
									var _v11 = parseF(s5);
									if (_v11.$ === 'Bad') {
										var x = _v11.b;
										return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
									} else {
										var f = _v11.a;
										var s6 = _v11.b;
										return A2(
											$stil4m$elm_syntax$ParserFast$Good,
											A6(func, a, b, c, d, e, f),
											s6);
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$parameterPatternsEqual = A2(
	$stil4m$elm_syntax$ParserWithComments$until,
	$stil4m$elm_syntax$Elm$Parser$Tokens$equal,
	A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (patternResult, commentsAfterPattern) {
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterPattern, patternResult.comments),
					syntax: patternResult.syntax
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Patterns$patternNotDirectlyComposing,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout));
var $stil4m$elm_syntax$Elm$Parser$Declarations$functionAfterDocumentation = A7(
	$stil4m$elm_syntax$ParserFast$map6,
	F6(
		function (startName, commentsAfterStartName, maybeSignature, _arguments, commentsAfterEqual, result) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					result.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterEqual,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							_arguments.comments,
							function () {
								if (maybeSignature.$ === 'Nothing') {
									return commentsAfterStartName;
								} else {
									var signature = maybeSignature.a;
									return A2($stil4m$elm_syntax$Rope$prependTo, signature.comments, commentsAfterStartName);
								}
							}()))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$FunctionDeclarationAfterDocumentation(
					{
						_arguments: _arguments.syntax,
						expression: result.syntax,
						signature: A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.syntax;
							},
							maybeSignature),
						startName: startName
					})
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	A6(
		$stil4m$elm_syntax$ParserFast$map4OrSucceed,
		F4(
			function (commentsBeforeTypeAnnotation, typeAnnotationResult, implementationName, afterImplementationName) {
				return $elm$core$Maybe$Just(
					{
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							afterImplementationName,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								implementationName.comments,
								A2($stil4m$elm_syntax$Rope$prependTo, typeAnnotationResult.comments, commentsBeforeTypeAnnotation))),
						syntax: {implementationName: implementationName.syntax, typeAnnotation: typeAnnotationResult.syntax}
					});
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
		$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedBy($stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode),
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$elm$core$Maybe$Nothing),
	$stil4m$elm_syntax$Elm$Parser$Declarations$parameterPatternsEqual,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Expression$expression);
var $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedByWithComments = function (nextParser) {
	return A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (commentsBefore, after) {
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, after.comments, commentsBefore),
					syntax: after.syntax
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(nextParser));
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$PortDeclarationAfterDocumentation = function (a) {
	return {$: 'PortDeclarationAfterDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$portDeclarationAfterDocumentation = A6(
	$stil4m$elm_syntax$ParserFast$map5,
	F5(
		function (commentsAfterPort, name, commentsAfterName, commentsAfterColon, typeAnnotationResult) {
			var nameRange = name.a;
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					commentsAfterColon,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						typeAnnotationResult.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterName, commentsAfterPort))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$PortDeclarationAfterDocumentation(
					{
						name: name,
						startLocation: {column: 1, row: nameRange.start.row},
						typeAnnotation: typeAnnotationResult.syntax
					})
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'port', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation);
var $stil4m$elm_syntax$Elm$Parser$Declarations$TypeDeclarationAfterDocumentation = function (a) {
	return {$: 'TypeDeclarationAfterDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$typeGenericListEquals = A2(
	$stil4m$elm_syntax$ParserWithComments$until,
	$stil4m$elm_syntax$Elm$Parser$Tokens$equal,
	A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (name, commentsAfterName) {
				return {comments: commentsAfterName, syntax: name};
			}),
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout));
var $stil4m$elm_syntax$ParserFast$ifFollowedByWhileMapWithRangeWithoutLinebreak = F3(
	function (rangeAndConsumedStringToRes, firstIsOkay, afterFirstIsOkay) {
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var firstOffset = A3($stil4m$elm_syntax$ParserFast$isSubCharWithoutLinebreak, firstIsOkay, s0.offset, s0.src);
				if (_Utils_eq(firstOffset, -1)) {
					return A2(
						$stil4m$elm_syntax$ParserFast$Bad,
						false,
						A2($stil4m$elm_syntax$ParserFast$ExpectingCharSatisfyingPredicate, s0.row, s0.col));
				} else {
					var s1 = A6($stil4m$elm_syntax$ParserFast$skipWhileWithoutLinebreakHelp, afterFirstIsOkay, firstOffset, s0.row, s0.col + 1, s0.src, s0.indent);
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						A2(
							rangeAndConsumedStringToRes,
							{
								end: {column: s1.col, row: s1.row},
								start: {column: s0.col, row: s0.row}
							},
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src)),
						s1);
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode = A3($stil4m$elm_syntax$ParserFast$ifFollowedByWhileMapWithRangeWithoutLinebreak, $stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Char$Extra$unicodeIsUpperFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast);
var $stil4m$elm_syntax$Elm$Parser$Declarations$valueConstructorOptimisticLayout = A4(
	$stil4m$elm_syntax$ParserFast$map3,
	F3(
		function (name, commentsAfterName, argumentsReverse) {
			var nameRange = name.a;
			var fullRange = function () {
				var _v0 = argumentsReverse.syntax;
				if (_v0.b) {
					var _v1 = _v0.a;
					var lastArgRange = _v1.a;
					return {end: lastArgRange.end, start: nameRange.start};
				} else {
					return nameRange;
				}
			}();
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, argumentsReverse.comments, commentsAfterName),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					fullRange,
					{
						_arguments: $elm$core$List$reverse(argumentsReverse.syntax),
						name: name
					})
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
	$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
		$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedFollowedBy(
			A3(
				$stil4m$elm_syntax$ParserFast$map2,
				F2(
					function (typeAnnotationResult, commentsAfter) {
						return {
							comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, typeAnnotationResult.comments),
							syntax: typeAnnotationResult.syntax
						};
					}),
				$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotationNoFnExcludingTypedWithArguments,
				$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout))));
var $stil4m$elm_syntax$Elm$Parser$Declarations$customTypeDefinitionAfterDocumentationAfterTypePrefix = A7(
	$stil4m$elm_syntax$ParserFast$map6,
	F6(
		function (name, commentsAfterName, parameters, commentsAfterEqual, headVariant, tailVariantsReverse) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					tailVariantsReverse.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						headVariant.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsAfterEqual,
							A2($stil4m$elm_syntax$Rope$prependTo, parameters.comments, commentsAfterName)))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$TypeDeclarationAfterDocumentation(
					{headVariant: headVariant.syntax, name: name, parameters: parameters.syntax, tailVariantsReverse: tailVariantsReverse.syntax})
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$typeGenericListEquals,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$valueConstructorOptimisticLayout,
	$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'|',
			A2(
				$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedPlusFollowedBy,
				1,
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (commentsBeforePipe, variantResult) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, variantResult.comments, commentsBeforePipe),
								syntax: variantResult.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$Elm$Parser$Declarations$valueConstructorOptimisticLayout)))));
var $stil4m$elm_syntax$Elm$Parser$Declarations$TypeAliasDeclarationAfterDocumentation = function (a) {
	return {$: 'TypeAliasDeclarationAfterDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$typeAliasDefinitionAfterDocumentationAfterTypePrefix = A7(
	$stil4m$elm_syntax$ParserFast$map6,
	F6(
		function (commentsAfterAlias, name, commentsAfterName, parameters, commentsAfterEquals, typeAnnotationResult) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					typeAnnotationResult.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterEquals,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							parameters.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterName, commentsAfterAlias)))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$TypeAliasDeclarationAfterDocumentation(
					{name: name, parameters: parameters.syntax, typeAnnotation: typeAnnotationResult.syntax})
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'alias', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$typeGenericListEquals,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation);
var $stil4m$elm_syntax$Elm$Parser$Declarations$typeOrTypeAliasDefinitionAfterDocumentation = A3(
	$stil4m$elm_syntax$ParserFast$map2,
	F2(
		function (commentsAfterType, declarationAfterDocumentation) {
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, declarationAfterDocumentation.comments, commentsAfterType),
				syntax: declarationAfterDocumentation.syntax
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'type', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2($stil4m$elm_syntax$ParserFast$oneOf2, $stil4m$elm_syntax$Elm$Parser$Declarations$typeAliasDefinitionAfterDocumentationAfterTypePrefix, $stil4m$elm_syntax$Elm$Parser$Declarations$customTypeDefinitionAfterDocumentationAfterTypePrefix));
var $stil4m$elm_syntax$Elm$Parser$Declarations$declarationWithDocumentation = A3(
	$stil4m$elm_syntax$ParserFast$validate,
	function (result) {
		var _v11 = result.syntax;
		var decl = _v11.b;
		if (decl.$ === 'FunctionDeclaration') {
			var letFunctionDeclaration = decl.a;
			var _v13 = letFunctionDeclaration.signature;
			if (_v13.$ === 'Nothing') {
				return true;
			} else {
				var _v14 = _v13.a;
				var signature = _v14.b;
				var _v15 = signature.name;
				var signatureName = _v15.b;
				var _v16 = letFunctionDeclaration.declaration;
				var implementation = _v16.b;
				var _v17 = implementation.name;
				var implementationName = _v17.b;
				return _Utils_eq(implementationName, signatureName + '');
			}
		} else {
			return true;
		}
	},
	'Expected to find the same name for declaration and signature',
	A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (documentation, afterDocumentation) {
				var start = $stil4m$elm_syntax$Elm$Syntax$Node$range(documentation).start;
				var _v0 = afterDocumentation.syntax;
				switch (_v0.$) {
					case 'FunctionDeclarationAfterDocumentation':
						var functionDeclarationAfterDocumentation = _v0.a;
						var _v1 = functionDeclarationAfterDocumentation.signature;
						if (_v1.$ === 'Just') {
							var signature = _v1.a;
							var _v2 = signature.implementationName;
							var implementationNameRange = _v2.a;
							var _v3 = functionDeclarationAfterDocumentation.expression;
							var expressionRange = _v3.a;
							return {
								comments: afterDocumentation.comments,
								syntax: A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									{end: expressionRange.end, start: start},
									$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
										{
											declaration: A2(
												$stil4m$elm_syntax$Elm$Syntax$Node$Node,
												{end: expressionRange.end, start: implementationNameRange.start},
												{_arguments: functionDeclarationAfterDocumentation._arguments, expression: functionDeclarationAfterDocumentation.expression, name: signature.implementationName}),
											documentation: $elm$core$Maybe$Just(documentation),
											signature: $elm$core$Maybe$Just(
												A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Signature$Signature, functionDeclarationAfterDocumentation.startName, signature.typeAnnotation))
										}))
							};
						} else {
							var _v4 = functionDeclarationAfterDocumentation.startName;
							var startNameRange = _v4.a;
							var _v5 = functionDeclarationAfterDocumentation.expression;
							var expressionRange = _v5.a;
							return {
								comments: afterDocumentation.comments,
								syntax: A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									{end: expressionRange.end, start: start},
									$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
										{
											declaration: A2(
												$stil4m$elm_syntax$Elm$Syntax$Node$Node,
												{end: expressionRange.end, start: startNameRange.start},
												{_arguments: functionDeclarationAfterDocumentation._arguments, expression: functionDeclarationAfterDocumentation.expression, name: functionDeclarationAfterDocumentation.startName}),
											documentation: $elm$core$Maybe$Just(documentation),
											signature: $elm$core$Maybe$Nothing
										}))
							};
						}
					case 'TypeDeclarationAfterDocumentation':
						var typeDeclarationAfterDocumentation = _v0.a;
						var end = function () {
							var _v6 = typeDeclarationAfterDocumentation.tailVariantsReverse;
							if (_v6.b) {
								var _v7 = _v6.a;
								var range = _v7.a;
								return range.end;
							} else {
								var _v8 = typeDeclarationAfterDocumentation.headVariant;
								var headVariantRange = _v8.a;
								return headVariantRange.end;
							}
						}();
						return {
							comments: afterDocumentation.comments,
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: end, start: start},
								$stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(
									{
										constructors: A2(
											$elm$core$List$cons,
											typeDeclarationAfterDocumentation.headVariant,
											$elm$core$List$reverse(typeDeclarationAfterDocumentation.tailVariantsReverse)),
										documentation: $elm$core$Maybe$Just(documentation),
										generics: typeDeclarationAfterDocumentation.parameters,
										name: typeDeclarationAfterDocumentation.name
									}))
						};
					case 'TypeAliasDeclarationAfterDocumentation':
						var typeAliasDeclarationAfterDocumentation = _v0.a;
						var _v9 = typeAliasDeclarationAfterDocumentation.typeAnnotation;
						var typeAnnotationRange = _v9.a;
						return {
							comments: afterDocumentation.comments,
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: typeAnnotationRange.end, start: start},
								$stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(
									{
										documentation: $elm$core$Maybe$Just(documentation),
										generics: typeAliasDeclarationAfterDocumentation.parameters,
										name: typeAliasDeclarationAfterDocumentation.name,
										typeAnnotation: typeAliasDeclarationAfterDocumentation.typeAnnotation
									}))
						};
					default:
						var portDeclarationAfterName = _v0.a;
						var _v10 = portDeclarationAfterName.typeAnnotation;
						var typeAnnotationRange = _v10.a;
						return {
							comments: A2(
								$stil4m$elm_syntax$Rope$filledPrependTo,
								afterDocumentation.comments,
								$stil4m$elm_syntax$Rope$one(documentation)),
							syntax: A2(
								$stil4m$elm_syntax$Elm$Syntax$Node$Node,
								{end: typeAnnotationRange.end, start: portDeclarationAfterName.startLocation},
								$stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration(
									{name: portDeclarationAfterName.name, typeAnnotation: portDeclarationAfterName.typeAnnotation}))
						};
				}
			}),
		$stil4m$elm_syntax$Elm$Parser$Comments$declarationDocumentation,
		$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedByWithComments(
			A3($stil4m$elm_syntax$ParserFast$oneOf3, $stil4m$elm_syntax$Elm$Parser$Declarations$functionAfterDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$typeOrTypeAliasDefinitionAfterDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$portDeclarationAfterDocumentation))));
var $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNotInfixNode = A4(
	$stil4m$elm_syntax$ParserFast$ifFollowedByWhileValidateMapWithRangeWithoutLinebreak,
	$stil4m$elm_syntax$Elm$Syntax$Node$Node,
	$stil4m$elm_syntax$Char$Extra$unicodeIsLowerFast,
	$stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast,
	function (name) {
		return (name !== 'infix') && $stil4m$elm_syntax$Elm$Parser$Tokens$isNotReserved(name);
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$functionDeclarationWithoutDocumentation = A3(
	$stil4m$elm_syntax$ParserFast$validate,
	function (result) {
		var _v5 = result.syntax;
		var decl = _v5.b;
		if (decl.$ === 'FunctionDeclaration') {
			var letFunctionDeclaration = decl.a;
			var _v7 = letFunctionDeclaration.signature;
			if (_v7.$ === 'Nothing') {
				return true;
			} else {
				var _v8 = _v7.a;
				var signature = _v8.b;
				var _v9 = signature.name;
				var signatureName = _v9.b;
				var _v10 = letFunctionDeclaration.declaration;
				var implementation = _v10.b;
				var _v11 = implementation.name;
				var implementationName = _v11.b;
				return _Utils_eq(implementationName, signatureName + '');
			}
		} else {
			return true;
		}
	},
	'Expected to find the same name for declaration and signature',
	A7(
		$stil4m$elm_syntax$ParserFast$map6WithStartLocation,
		F7(
			function (startNameStart, startNameNode, commentsAfterStartName, maybeSignature, _arguments, commentsAfterEqual, result) {
				var allComments = A2(
					$stil4m$elm_syntax$Rope$prependTo,
					result.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterEqual,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							_arguments.comments,
							function () {
								if (maybeSignature.$ === 'Nothing') {
									return commentsAfterStartName;
								} else {
									var signature = maybeSignature.a;
									return A2($stil4m$elm_syntax$Rope$prependTo, signature.comments, commentsAfterStartName);
								}
							}())));
				if (maybeSignature.$ === 'Nothing') {
					var _v1 = result.syntax;
					var expressionRange = _v1.a;
					return {
						comments: allComments,
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{end: expressionRange.end, start: startNameStart},
							$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
								{
									declaration: A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										{end: expressionRange.end, start: startNameStart},
										{_arguments: _arguments.syntax, expression: result.syntax, name: startNameNode}),
									documentation: $elm$core$Maybe$Nothing,
									signature: $elm$core$Maybe$Nothing
								}))
					};
				} else {
					var signature = maybeSignature.a;
					var _v2 = signature.implementationName;
					var implementationNameRange = _v2.a;
					var _v3 = result.syntax;
					var expressionRange = _v3.a;
					return {
						comments: allComments,
						syntax: A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{end: expressionRange.end, start: startNameStart},
							$stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
								{
									declaration: A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										{end: expressionRange.end, start: implementationNameRange.start},
										{_arguments: _arguments.syntax, expression: result.syntax, name: signature.implementationName}),
									documentation: $elm$core$Maybe$Nothing,
									signature: $elm$core$Maybe$Just(
										A3($stil4m$elm_syntax$Elm$Syntax$Node$combine, $stil4m$elm_syntax$Elm$Syntax$Signature$Signature, startNameNode, signature.typeAnnotation))
								}))
					};
				}
			}),
		$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNotInfixNode,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		A6(
			$stil4m$elm_syntax$ParserFast$map4OrSucceed,
			F4(
				function (commentsBeforeTypeAnnotation, typeAnnotationResult, implementationName, afterImplementationName) {
					return $elm$core$Maybe$Just(
						{
							comments: A2(
								$stil4m$elm_syntax$Rope$prependTo,
								afterImplementationName,
								A2(
									$stil4m$elm_syntax$Rope$prependTo,
									implementationName.comments,
									A2($stil4m$elm_syntax$Rope$prependTo, typeAnnotationResult.comments, commentsBeforeTypeAnnotation))),
							implementationName: implementationName.syntax,
							typeAnnotation: typeAnnotationResult.syntax
						});
				}),
			A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
			$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation,
			$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedBy($stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$elm$core$Maybe$Nothing),
		$stil4m$elm_syntax$Elm$Parser$Declarations$parameterPatternsEqual,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$Elm$Parser$Expression$expression));
var $stil4m$elm_syntax$Elm$Syntax$Declaration$InfixDeclaration = function (a) {
	return {$: 'InfixDeclaration', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$infixDirection = A3(
	$stil4m$elm_syntax$ParserFast$oneOf3,
	A2(
		$stil4m$elm_syntax$ParserFast$mapWithRange,
		$stil4m$elm_syntax$Elm$Syntax$Node$Node,
		A2($stil4m$elm_syntax$ParserFast$keyword, 'right', $stil4m$elm_syntax$Elm$Syntax$Infix$Right)),
	A2(
		$stil4m$elm_syntax$ParserFast$mapWithRange,
		$stil4m$elm_syntax$Elm$Syntax$Node$Node,
		A2($stil4m$elm_syntax$ParserFast$keyword, 'left', $stil4m$elm_syntax$Elm$Syntax$Infix$Left)),
	A2(
		$stil4m$elm_syntax$ParserFast$mapWithRange,
		$stil4m$elm_syntax$Elm$Syntax$Node$Node,
		A2($stil4m$elm_syntax$ParserFast$keyword, 'non', $stil4m$elm_syntax$Elm$Syntax$Infix$Non)));
var $stil4m$elm_syntax$ParserFast$errorAsOffsetAndInt = {_int: 0, offset: -1};
var $stil4m$elm_syntax$ParserFast$convertIntegerDecimal = F2(
	function (offset, src) {
		var _v0 = A3($elm$core$String$slice, offset, offset + 1, src);
		switch (_v0) {
			case '0':
				return {_int: 0, offset: offset + 1};
			case '1':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 1, offset + 1, src);
			case '2':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 2, offset + 1, src);
			case '3':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 3, offset + 1, src);
			case '4':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 4, offset + 1, src);
			case '5':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 5, offset + 1, src);
			case '6':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 6, offset + 1, src);
			case '7':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 7, offset + 1, src);
			case '8':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 8, offset + 1, src);
			case '9':
				return A3($stil4m$elm_syntax$ParserFast$convert0OrMore0To9s, 9, offset + 1, src);
			default:
				return $stil4m$elm_syntax$ParserFast$errorAsOffsetAndInt;
		}
	});
var $stil4m$elm_syntax$ParserFast$integerDecimalMapWithRange = function (rangeAndIntToRes) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s0) {
			var s1 = A2($stil4m$elm_syntax$ParserFast$convertIntegerDecimal, s0.offset, s0.src);
			if (_Utils_eq(s1.offset, -1)) {
				return A2(
					$stil4m$elm_syntax$ParserFast$Bad,
					false,
					A2($stil4m$elm_syntax$ParserFast$ExpectingNumber, s0.row, s0.col));
			} else {
				var newColumn = s0.col + (s1.offset - s0.offset);
				return A2(
					$stil4m$elm_syntax$ParserFast$Good,
					A2(
						rangeAndIntToRes,
						{
							end: {column: newColumn, row: s0.row},
							start: {column: s0.col, row: s0.row}
						},
						s1._int),
					{col: newColumn, indent: s0.indent, offset: s1.offset, row: s0.row, src: s0.src});
			}
		});
};
var $stil4m$elm_syntax$ParserFast$map9WithRange = function (func) {
	return function (_v0) {
		return function (_v1) {
			return function (_v2) {
				return function (_v3) {
					return function (_v4) {
						return function (_v5) {
							return function (_v6) {
								return function (_v7) {
									return function (_v8) {
										var parseA = _v0.a;
										var parseB = _v1.a;
										var parseC = _v2.a;
										var parseD = _v3.a;
										var parseE = _v4.a;
										var parseF = _v5.a;
										var parseG = _v6.a;
										var parseH = _v7.a;
										var parseI = _v8.a;
										return $stil4m$elm_syntax$ParserFast$Parser(
											function (s0) {
												var _v9 = parseA(s0);
												if (_v9.$ === 'Bad') {
													var committed = _v9.a;
													var x = _v9.b;
													return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
												} else {
													var a = _v9.a;
													var s1 = _v9.b;
													var _v10 = parseB(s1);
													if (_v10.$ === 'Bad') {
														var x = _v10.b;
														return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
													} else {
														var b = _v10.a;
														var s2 = _v10.b;
														var _v11 = parseC(s2);
														if (_v11.$ === 'Bad') {
															var x = _v11.b;
															return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
														} else {
															var c = _v11.a;
															var s3 = _v11.b;
															var _v12 = parseD(s3);
															if (_v12.$ === 'Bad') {
																var x = _v12.b;
																return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
															} else {
																var d = _v12.a;
																var s4 = _v12.b;
																var _v13 = parseE(s4);
																if (_v13.$ === 'Bad') {
																	var x = _v13.b;
																	return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
																} else {
																	var e = _v13.a;
																	var s5 = _v13.b;
																	var _v14 = parseF(s5);
																	if (_v14.$ === 'Bad') {
																		var x = _v14.b;
																		return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
																	} else {
																		var f = _v14.a;
																		var s6 = _v14.b;
																		var _v15 = parseG(s6);
																		if (_v15.$ === 'Bad') {
																			var x = _v15.b;
																			return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
																		} else {
																			var g = _v15.a;
																			var s7 = _v15.b;
																			var _v16 = parseH(s7);
																			if (_v16.$ === 'Bad') {
																				var x = _v16.b;
																				return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
																			} else {
																				var h = _v16.a;
																				var s8 = _v16.b;
																				var _v17 = parseI(s8);
																				if (_v17.$ === 'Bad') {
																					var x = _v17.b;
																					return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
																				} else {
																					var i = _v17.a;
																					var s9 = _v17.b;
																					return A2(
																						$stil4m$elm_syntax$ParserFast$Good,
																						func(
																							{
																								end: {column: s9.col, row: s9.row},
																								start: {column: s0.col, row: s0.row}
																							})(a)(b)(c)(d)(e)(f)(g)(h)(i),
																						s9);
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											});
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
var $stil4m$elm_syntax$Elm$Parser$Declarations$infixDeclaration = $stil4m$elm_syntax$ParserFast$map9WithRange(
	function (range) {
		return function (commentsAfterInfix) {
			return function (direction) {
				return function (commentsAfterDirection) {
					return function (precedence) {
						return function (commentsAfterPrecedence) {
							return function (operator) {
								return function (commentsAfterOperator) {
									return function (commentsAfterEqual) {
										return function (fn) {
											return {
												comments: A2(
													$stil4m$elm_syntax$Rope$prependTo,
													commentsAfterEqual,
													A2(
														$stil4m$elm_syntax$Rope$prependTo,
														commentsAfterOperator,
														A2(
															$stil4m$elm_syntax$Rope$prependTo,
															commentsAfterPrecedence,
															A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterDirection, commentsAfterInfix)))),
												syntax: A2(
													$stil4m$elm_syntax$Elm$Syntax$Node$Node,
													range,
													$stil4m$elm_syntax$Elm$Syntax$Declaration$InfixDeclaration(
														{direction: direction, _function: fn, operator: operator, precedence: precedence}))
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
	})(
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'infix', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))($stil4m$elm_syntax$Elm$Parser$Declarations$infixDirection)($stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)(
	$stil4m$elm_syntax$ParserFast$integerDecimalMapWithRange($stil4m$elm_syntax$Elm$Syntax$Node$Node))($stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)(
	A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'(',
		A4(
			$stil4m$elm_syntax$ParserFast$whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol,
			F2(
				function (operatorRange, operator) {
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						{
							end: {column: operatorRange.end.column + 1, row: operatorRange.end.row},
							start: {column: operatorRange.start.column - 1, row: operatorRange.start.row}
						},
						operator);
				}),
			$stil4m$elm_syntax$Elm$Parser$Tokens$isOperatorSymbolChar,
			$stil4m$elm_syntax$Elm$Parser$Tokens$isAllowedOperatorToken,
			')')))($stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)(
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '=', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout))($stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode);
var $stil4m$elm_syntax$ParserFast$oneOf5 = F5(
	function (_v0, _v1, _v2, _v3, _v4) {
		var attemptFirst = _v0.a;
		var attemptSecond = _v1.a;
		var attemptThird = _v2.a;
		var attemptFourth = _v3.a;
		var attemptFifth = _v4.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s) {
				var _v5 = attemptFirst(s);
				if (_v5.$ === 'Good') {
					var firstGood = _v5;
					return firstGood;
				} else {
					var firstBad = _v5;
					var firstCommitted = firstBad.a;
					var firstX = firstBad.b;
					if (firstCommitted) {
						return firstBad;
					} else {
						var _v6 = attemptSecond(s);
						if (_v6.$ === 'Good') {
							var secondGood = _v6;
							return secondGood;
						} else {
							var secondBad = _v6;
							var secondCommitted = secondBad.a;
							var secondX = secondBad.b;
							if (secondCommitted) {
								return secondBad;
							} else {
								var _v7 = attemptThird(s);
								if (_v7.$ === 'Good') {
									var thirdGood = _v7;
									return thirdGood;
								} else {
									var thirdBad = _v7;
									var thirdCommitted = thirdBad.a;
									var thirdX = thirdBad.b;
									if (thirdCommitted) {
										return thirdBad;
									} else {
										var _v8 = attemptFourth(s);
										if (_v8.$ === 'Good') {
											var fourthGood = _v8;
											return fourthGood;
										} else {
											var fourthBad = _v8;
											var fourthCommitted = fourthBad.a;
											var fourthX = fourthBad.b;
											if (fourthCommitted) {
												return fourthBad;
											} else {
												var _v9 = attemptFifth(s);
												if (_v9.$ === 'Good') {
													var fifthGood = _v9;
													return fifthGood;
												} else {
													var fifthBad = _v9;
													var fifthCommitted = fifthBad.a;
													var fifthX = fifthBad.b;
													return fifthCommitted ? fifthBad : A2(
														$stil4m$elm_syntax$ParserFast$Bad,
														false,
														A3(
															$stil4m$elm_syntax$ParserFast$ExpectingOneOf,
															firstX,
															secondX,
															_List_fromArray(
																[thirdX, fourthX, fifthX])));
												}
											}
										}
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$portDeclarationWithoutDocumentation = A6(
	$stil4m$elm_syntax$ParserFast$map5,
	F5(
		function (commentsAfterPort, name, commentsAfterName, commentsAfterColon, typeAnnotationResult) {
			var nameRange = name.a;
			var _v0 = typeAnnotationResult.syntax;
			var end = _v0.a.end;
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					typeAnnotationResult.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterColon,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterName, commentsAfterPort))),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					{
						end: end,
						start: {column: 1, row: nameRange.start.row}
					},
					$stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration(
						{name: name, typeAnnotation: typeAnnotationResult.syntax}))
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'port', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, ':', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation);
var $stil4m$elm_syntax$Elm$Parser$Declarations$TypeDeclarationWithoutDocumentation = function (a) {
	return {$: 'TypeDeclarationWithoutDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$customTypeDefinitionWithoutDocumentationAfterTypePrefix = A7(
	$stil4m$elm_syntax$ParserFast$map6,
	F6(
		function (name, commentsAfterName, parameters, commentsAfterEqual, headVariant, tailVariantsReverse) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					tailVariantsReverse.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						headVariant.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							commentsAfterEqual,
							A2($stil4m$elm_syntax$Rope$prependTo, parameters.comments, commentsAfterName)))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$TypeDeclarationWithoutDocumentation(
					{headVariant: headVariant.syntax, name: name, parameters: parameters.syntax, tailVariantsReverse: tailVariantsReverse.syntax})
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$typeGenericListEquals,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$valueConstructorOptimisticLayout,
	$stil4m$elm_syntax$ParserWithComments$manyWithoutReverse(
		A2(
			$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
			'|',
			A2(
				$stil4m$elm_syntax$Elm$Parser$Layout$positivelyIndentedPlusFollowedBy,
				1,
				A3(
					$stil4m$elm_syntax$ParserFast$map2,
					F2(
						function (commentsBeforePipe, variantResult) {
							return {
								comments: A2($stil4m$elm_syntax$Rope$prependTo, variantResult.comments, commentsBeforePipe),
								syntax: variantResult.syntax
							};
						}),
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
					$stil4m$elm_syntax$Elm$Parser$Declarations$valueConstructorOptimisticLayout)))));
var $stil4m$elm_syntax$ParserFast$map2WithStartLocation = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var committed = _v2.a;
					var x = _v2.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v2.a;
					var s1 = _v2.b;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var x = _v3.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v3.a;
						var s2 = _v3.b;
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							A3(
								func,
								{column: s0.col, row: s0.row},
								a,
								b),
							s2);
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Declarations$TypeAliasDeclarationWithoutDocumentation = function (a) {
	return {$: 'TypeAliasDeclarationWithoutDocumentation', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Declarations$typeAliasDefinitionWithoutDocumentationAfterTypePrefix = A7(
	$stil4m$elm_syntax$ParserFast$map6,
	F6(
		function (commentsAfterAlias, name, commentsAfterName, parameters, commentsAfterEqual, typeAnnotationResult) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					typeAnnotationResult.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterEqual,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							parameters.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterName, commentsAfterAlias)))),
				syntax: $stil4m$elm_syntax$Elm$Parser$Declarations$TypeAliasDeclarationWithoutDocumentation(
					{name: name, parameters: parameters.syntax, typeAnnotation: typeAnnotationResult.syntax})
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'alias', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Declarations$typeGenericListEquals,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$TypeAnnotation$typeAnnotation);
var $stil4m$elm_syntax$Elm$Parser$Declarations$typeOrTypeAliasDefinitionWithoutDocumentation = A3(
	$stil4m$elm_syntax$ParserFast$map2WithStartLocation,
	F3(
		function (start, commentsAfterType, afterStart) {
			var allComments = A2($stil4m$elm_syntax$Rope$prependTo, afterStart.comments, commentsAfterType);
			var _v0 = afterStart.syntax;
			if (_v0.$ === 'TypeDeclarationWithoutDocumentation') {
				var typeDeclarationAfterDocumentation = _v0.a;
				var end = function () {
					var _v1 = typeDeclarationAfterDocumentation.tailVariantsReverse;
					if (_v1.b) {
						var _v2 = _v1.a;
						var range = _v2.a;
						return range.end;
					} else {
						var _v3 = typeDeclarationAfterDocumentation.headVariant;
						var headVariantRange = _v3.a;
						return headVariantRange.end;
					}
				}();
				return {
					comments: allComments,
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						{end: end, start: start},
						$stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(
							{
								constructors: A2(
									$elm$core$List$cons,
									typeDeclarationAfterDocumentation.headVariant,
									$elm$core$List$reverse(typeDeclarationAfterDocumentation.tailVariantsReverse)),
								documentation: $elm$core$Maybe$Nothing,
								generics: typeDeclarationAfterDocumentation.parameters,
								name: typeDeclarationAfterDocumentation.name
							}))
				};
			} else {
				var typeAliasDeclarationAfterDocumentation = _v0.a;
				var _v4 = typeAliasDeclarationAfterDocumentation.typeAnnotation;
				var typeAnnotationRange = _v4.a;
				return {
					comments: allComments,
					syntax: A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						{end: typeAnnotationRange.end, start: start},
						$stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(
							{documentation: $elm$core$Maybe$Nothing, generics: typeAliasDeclarationAfterDocumentation.parameters, name: typeAliasDeclarationAfterDocumentation.name, typeAnnotation: typeAliasDeclarationAfterDocumentation.typeAnnotation}))
				};
			}
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'type', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2($stil4m$elm_syntax$ParserFast$oneOf2, $stil4m$elm_syntax$Elm$Parser$Declarations$typeAliasDefinitionWithoutDocumentationAfterTypePrefix, $stil4m$elm_syntax$Elm$Parser$Declarations$customTypeDefinitionWithoutDocumentationAfterTypePrefix));
var $stil4m$elm_syntax$Elm$Parser$Declarations$declaration = A5($stil4m$elm_syntax$ParserFast$oneOf5, $stil4m$elm_syntax$Elm$Parser$Declarations$functionDeclarationWithoutDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$declarationWithDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$typeOrTypeAliasDefinitionWithoutDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$portDeclarationWithoutDocumentation, $stil4m$elm_syntax$Elm$Parser$Declarations$infixDeclaration);
var $stil4m$elm_syntax$ParserFast$columnAndThen = function (callback) {
	return $stil4m$elm_syntax$ParserFast$Parser(
		function (s) {
			var _v0 = callback(s.col);
			var parse = _v0.a;
			return parse(s);
		});
};
var $stil4m$elm_syntax$Elm$Parser$Layout$problemModuleLevelIndentation = $stil4m$elm_syntax$ParserFast$problem('must be on module-level indentation');
var $stil4m$elm_syntax$Elm$Parser$Layout$moduleLevelIndentationFollowedBy = function (nextParser) {
	return $stil4m$elm_syntax$ParserFast$columnAndThen(
		function (column) {
			return (column === 1) ? nextParser : $stil4m$elm_syntax$Elm$Parser$Layout$problemModuleLevelIndentation;
		});
};
var $stil4m$elm_syntax$Elm$Parser$File$fileDeclarations = $stil4m$elm_syntax$ParserWithComments$many(
	$stil4m$elm_syntax$Elm$Parser$Layout$moduleLevelIndentationFollowedBy(
		A3(
			$stil4m$elm_syntax$ParserFast$map2,
			F2(
				function (declarationParsed, commentsAfter) {
					return {
						comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, declarationParsed.comments),
						syntax: declarationParsed.syntax
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Declarations$declaration,
			$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout)));
var $stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose = function (a) {
	return {$: 'FunctionExpose', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Expose$functionExpose = $stil4m$elm_syntax$Elm$Parser$Tokens$functionNameMapWithRange(
	F2(
		function (range, name) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose(name))
			};
		}));
var $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose = function (a) {
	return {$: 'InfixExpose', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Tokens$parensEnd = A2($stil4m$elm_syntax$ParserFast$symbol, ')', _Utils_Tuple0);
var $stil4m$elm_syntax$Elm$Parser$Expose$infixExpose = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, infixName, _v0) {
			return {
				comments: $stil4m$elm_syntax$Rope$empty,
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose(infixName))
			};
		}),
	A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'(',
		A2(
			$stil4m$elm_syntax$ParserFast$ifFollowedByWhileWithoutLinebreak,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr(')'))) && ((!_Utils_eq(
					c,
					_Utils_chr('\n'))) && (!_Utils_eq(
					c,
					_Utils_chr(' '))));
			},
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr(')'))) && ((!_Utils_eq(
					c,
					_Utils_chr('\n'))) && (!_Utils_eq(
					c,
					_Utils_chr(' '))));
			})),
	$stil4m$elm_syntax$Elm$Parser$Tokens$parensEnd);
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose = function (a) {
	return {$: 'TypeOrAliasExpose', a: a};
};
var $stil4m$elm_syntax$ParserFast$map2WithRangeOrSucceed = F4(
	function (func, _v0, _v1, fallback) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var c1 = _v2.a;
					var x = _v2.b;
					return c1 ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2($stil4m$elm_syntax$ParserFast$Good, fallback, s0);
				} else {
					var a = _v2.a;
					var s1 = _v2.b;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var x = _v3.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v3.a;
						var s2 = _v3.b;
						return A2(
							$stil4m$elm_syntax$ParserFast$Good,
							A3(
								func,
								{
									end: {column: s2.col, row: s2.row},
									start: {column: s0.col, row: s0.row}
								},
								a,
								b),
							s2);
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Expose$typeExpose = A4(
	$stil4m$elm_syntax$ParserFast$map3,
	F3(
		function (_v0, commentsBeforeMaybeOpen, maybeOpen) {
			var typeNameRange = _v0.a;
			var typeName = _v0.b;
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, maybeOpen.comments, commentsBeforeMaybeOpen),
				syntax: function () {
					var _v1 = maybeOpen.syntax;
					if (_v1.$ === 'Nothing') {
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							typeNameRange,
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(typeName));
					} else {
						var openRange = _v1.a;
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$Node$Node,
							{end: openRange.end, start: typeNameRange.start},
							$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
								{name: typeName, open: maybeOpen.syntax}));
					}
				}()
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode,
	$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
	A4(
		$stil4m$elm_syntax$ParserFast$map2WithRangeOrSucceed,
		F3(
			function (range, left, right) {
				return {
					comments: A2($stil4m$elm_syntax$Rope$prependTo, right, left),
					syntax: $elm$core$Maybe$Just(range)
				};
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '(', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		A2(
			$stil4m$elm_syntax$ParserFast$followedBySymbol,
			')',
			A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '..', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)),
		{comments: $stil4m$elm_syntax$Rope$empty, syntax: $elm$core$Maybe$Nothing}));
var $stil4m$elm_syntax$Elm$Parser$Expose$exposable = A3($stil4m$elm_syntax$ParserFast$oneOf3, $stil4m$elm_syntax$Elm$Parser$Expose$functionExpose, $stil4m$elm_syntax$Elm$Parser$Expose$typeExpose, $stil4m$elm_syntax$Elm$Parser$Expose$infixExpose);
var $stil4m$elm_syntax$Elm$Parser$Expose$exposingListInner = A2(
	$stil4m$elm_syntax$ParserFast$oneOf2,
	A4(
		$stil4m$elm_syntax$ParserFast$map3,
		F3(
			function (headElement, commentsAfterHeadElement, tailElements) {
				return {
					comments: A2(
						$stil4m$elm_syntax$Rope$prependTo,
						tailElements.comments,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterHeadElement, headElement.comments)),
					syntax: $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
						A2($elm$core$List$cons, headElement.syntax, tailElements.syntax))
				};
			}),
		$stil4m$elm_syntax$Elm$Parser$Expose$exposable,
		$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
		$stil4m$elm_syntax$ParserWithComments$many(
			A2(
				$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
				',',
				$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides($stil4m$elm_syntax$Elm$Parser$Expose$exposable)))),
	A2(
		$stil4m$elm_syntax$ParserFast$mapWithRange,
		F2(
			function (range, commentsAfterDotDot) {
				return {
					comments: commentsAfterDotDot,
					syntax: $stil4m$elm_syntax$Elm$Syntax$Exposing$All(range)
				};
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '..', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout)));
var $stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition = A4(
	$stil4m$elm_syntax$ParserFast$map3WithRange,
	F4(
		function (range, commentsAfterExposing, commentsBefore, exposingListInnerResult) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					exposingListInnerResult.comments,
					A2($stil4m$elm_syntax$Rope$prependTo, commentsBefore, commentsAfterExposing)),
				syntax: A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, exposingListInnerResult.syntax)
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, 'exposing', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '(', $stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout),
	A2($stil4m$elm_syntax$ParserFast$followedBySymbol, ')', $stil4m$elm_syntax$Elm$Parser$Expose$exposingListInner));
var $stil4m$elm_syntax$ParserFast$map3OrSucceed = F5(
	function (func, _v0, _v1, _v2, fallback) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v3 = parseA(s0);
				if (_v3.$ === 'Bad') {
					var c1 = _v3.a;
					var x = _v3.b;
					return c1 ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2($stil4m$elm_syntax$ParserFast$Good, fallback, s0);
				} else {
					var a = _v3.a;
					var s1 = _v3.b;
					var _v4 = parseB(s1);
					if (_v4.$ === 'Bad') {
						var x = _v4.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v4.a;
						var s2 = _v4.b;
						var _v5 = parseC(s2);
						if (_v5.$ === 'Bad') {
							var x = _v5.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v5.a;
							var s3 = _v5.b;
							return A2(
								$stil4m$elm_syntax$ParserFast$Good,
								A3(func, a, b, c),
								s3);
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$ParserFast$mapOrSucceed = F3(
	function (valueChange, _v0, fallback) {
		var parse = _v0.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var value = _v1.a;
					var s1 = _v1.b;
					return A2(
						$stil4m$elm_syntax$ParserFast$Good,
						valueChange(value),
						s1);
				} else {
					var firstCommitted = _v1.a;
					var x = _v1.b;
					return firstCommitted ? A2($stil4m$elm_syntax$ParserFast$Bad, true, x) : A2($stil4m$elm_syntax$ParserFast$Good, fallback, s0);
				}
			});
	});
function $stil4m$elm_syntax$Elm$Parser$Base$cyclic$moduleNameOrEmpty() {
	return A4(
		$stil4m$elm_syntax$ParserFast$map2OrSucceed,
		F2(
			function (head, tail) {
				return A2($elm$core$List$cons, head, tail);
			}),
		A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '.', $stil4m$elm_syntax$Elm$Parser$Tokens$typeName),
		$stil4m$elm_syntax$ParserFast$lazy(
			function (_v0) {
				return $stil4m$elm_syntax$Elm$Parser$Base$cyclic$moduleNameOrEmpty();
			}),
		_List_Nil);
}
try {
	var $stil4m$elm_syntax$Elm$Parser$Base$moduleNameOrEmpty = $stil4m$elm_syntax$Elm$Parser$Base$cyclic$moduleNameOrEmpty();
	$stil4m$elm_syntax$Elm$Parser$Base$cyclic$moduleNameOrEmpty = function () {
		return $stil4m$elm_syntax$Elm$Parser$Base$moduleNameOrEmpty;
	};
} catch ($) {
	throw 'Some top-level definitions from `Elm.Parser.Base` are causing infinite recursion:\n\n  ┌─────┐\n  │    moduleNameOrEmpty\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $stil4m$elm_syntax$Elm$Parser$Base$moduleName = A3(
	$stil4m$elm_syntax$ParserFast$map2WithRange,
	F3(
		function (range, head, tail) {
			return A2(
				$stil4m$elm_syntax$Elm$Syntax$Node$Node,
				range,
				A2($elm$core$List$cons, head, tail));
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeName,
	$stil4m$elm_syntax$Elm$Parser$Base$moduleNameOrEmpty);
var $stil4m$elm_syntax$Elm$Parser$Tokens$typeNameMapWithRange = function (rangeAndNameToRes) {
	return A3($stil4m$elm_syntax$ParserFast$ifFollowedByWhileMapWithRangeWithoutLinebreak, rangeAndNameToRes, $stil4m$elm_syntax$Char$Extra$unicodeIsUpperFast, $stil4m$elm_syntax$Char$Extra$unicodeIsAlphaNumOrUnderscoreFast);
};
var $stil4m$elm_syntax$Elm$Parser$Imports$importDefinition = A7(
	$stil4m$elm_syntax$ParserFast$map6WithStartLocation,
	F7(
		function (start, commentsAfterImport, mod, commentsAfterModuleName, maybeModuleAlias, maybeExposingList, commentsAfterEverything) {
			var modRange = mod.a;
			var endRange = function () {
				if (maybeModuleAlias.$ === 'Just') {
					var moduleAliasValue = maybeModuleAlias.a;
					var _v3 = moduleAliasValue.syntax;
					var range = _v3.a;
					return range;
				} else {
					if (maybeExposingList.$ === 'Just') {
						var exposingListValue = maybeExposingList.a;
						var _v5 = exposingListValue.syntax;
						var range = _v5.a;
						return range;
					} else {
						return modRange;
					}
				}
			}();
			return {
				comments: function () {
					var commentsBeforeAlias = A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterModuleName, commentsAfterImport);
					var commentsBeforeExposingList = function () {
						if (maybeModuleAlias.$ === 'Nothing') {
							return commentsBeforeAlias;
						} else {
							var moduleAliasValue = maybeModuleAlias.a;
							return A2($stil4m$elm_syntax$Rope$prependTo, moduleAliasValue.comments, commentsBeforeAlias);
						}
					}();
					return A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterEverything,
						function () {
							if (maybeExposingList.$ === 'Nothing') {
								return commentsBeforeExposingList;
							} else {
								var exposingListValue = maybeExposingList.a;
								return A2($stil4m$elm_syntax$Rope$prependTo, exposingListValue.comments, commentsBeforeExposingList);
							}
						}());
				}(),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					{end: endRange.end, start: start},
					{
						exposingList: A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.syntax;
							},
							maybeExposingList),
						moduleAlias: A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.syntax;
							},
							maybeModuleAlias),
						moduleName: mod
					})
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'import', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Base$moduleName,
	$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
	A5(
		$stil4m$elm_syntax$ParserFast$map3OrSucceed,
		F3(
			function (commentsBefore, moduleAliasNode, commentsAfter) {
				return $elm$core$Maybe$Just(
					{
						comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfter, commentsBefore),
						syntax: moduleAliasNode
					});
			}),
		A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'as', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
		$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameMapWithRange(
			F2(
				function (range, moduleAlias) {
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						_List_fromArray(
							[moduleAlias]));
				})),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$elm$core$Maybe$Nothing),
	A3($stil4m$elm_syntax$ParserFast$mapOrSucceed, $elm$core$Maybe$Just, $stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition, $elm$core$Maybe$Nothing),
	$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout);
var $stil4m$elm_syntax$Elm$Parser$Layout$endsTopIndented = function (parser) {
	return A3(
		$stil4m$elm_syntax$ParserFast$validateEndColumnIndentation,
		F2(
			function (column, indent) {
				return !(column - indent);
			}),
		'must be on top indentation',
		parser);
};
var $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict = $stil4m$elm_syntax$Elm$Parser$Layout$endsTopIndented($stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout);
var $stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedByComments = function (nextParser) {
	return A3(
		$stil4m$elm_syntax$ParserFast$map2,
		F2(
			function (commentsBefore, afterComments) {
				return A2($stil4m$elm_syntax$Rope$prependTo, afterComments, commentsBefore);
			}),
		$stil4m$elm_syntax$Elm$Parser$Layout$optimisticLayout,
		$stil4m$elm_syntax$Elm$Parser$Layout$onTopIndentationFollowedBy(nextParser));
};
var $stil4m$elm_syntax$Elm$Syntax$Module$EffectModule = function (a) {
	return {$: 'EffectModule', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClause = A5(
	$stil4m$elm_syntax$ParserFast$map4,
	F4(
		function (fnName, commentsAfterFnName, commentsAfterEqual, typeName_) {
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterEqual, commentsAfterFnName),
				syntax: _Utils_Tuple2(fnName, typeName_)
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Tokens$functionName,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	A2($stil4m$elm_syntax$ParserFast$symbolFollowedBy, '=', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Tokens$typeNameNode);
var $stil4m$elm_syntax$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(x);
				} else {
					var $temp$predicate = predicate,
						$temp$list = xs;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $stil4m$elm_syntax$Elm$Parser$Modules$whereBlock = A2(
	$stil4m$elm_syntax$ParserFast$followedBySymbol,
	'}',
	A2(
		$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
		'{',
		A5(
			$stil4m$elm_syntax$ParserFast$map4,
			F4(
				function (commentsBeforeHead, head, commentsAfterHead, tail) {
					var pairs = A2($elm$core$List$cons, head.syntax, tail.syntax);
					return {
						comments: A2(
							$stil4m$elm_syntax$Rope$prependTo,
							tail.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterHead,
								A2($stil4m$elm_syntax$Rope$prependTo, head.comments, commentsBeforeHead))),
						syntax: {
							command: A2(
								$elm$core$Maybe$map,
								$elm$core$Tuple$second,
								A2(
									$stil4m$elm_syntax$List$Extra$find,
									function (_v0) {
										var fnName = _v0.a;
										return fnName === 'command';
									},
									pairs)),
							subscription: A2(
								$elm$core$Maybe$map,
								$elm$core$Tuple$second,
								A2(
									$stil4m$elm_syntax$List$Extra$find,
									function (_v1) {
										var fnName = _v1.a;
										return fnName === 'subscription';
									},
									pairs))
						}
					};
				}),
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClause,
			$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
			$stil4m$elm_syntax$ParserWithComments$many(
				A2(
					$stil4m$elm_syntax$ParserFast$symbolFollowedBy,
					',',
					$stil4m$elm_syntax$Elm$Parser$Layout$maybeAroundBothSides($stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClause))))));
var $stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClauses = A3(
	$stil4m$elm_syntax$ParserFast$map2,
	F2(
		function (commentsBefore, whereResult) {
			return {
				comments: A2($stil4m$elm_syntax$Rope$prependTo, whereResult.comments, commentsBefore),
				syntax: whereResult.syntax
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'where', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Modules$whereBlock);
var $stil4m$elm_syntax$ParserFast$map7WithRange = F8(
	function (func, _v0, _v1, _v2, _v3, _v4, _v5, _v6) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		var parseC = _v2.a;
		var parseD = _v3.a;
		var parseE = _v4.a;
		var parseF = _v5.a;
		var parseG = _v6.a;
		return $stil4m$elm_syntax$ParserFast$Parser(
			function (s0) {
				var _v7 = parseA(s0);
				if (_v7.$ === 'Bad') {
					var committed = _v7.a;
					var x = _v7.b;
					return A2($stil4m$elm_syntax$ParserFast$Bad, committed, x);
				} else {
					var a = _v7.a;
					var s1 = _v7.b;
					var _v8 = parseB(s1);
					if (_v8.$ === 'Bad') {
						var x = _v8.b;
						return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
					} else {
						var b = _v8.a;
						var s2 = _v8.b;
						var _v9 = parseC(s2);
						if (_v9.$ === 'Bad') {
							var x = _v9.b;
							return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
						} else {
							var c = _v9.a;
							var s3 = _v9.b;
							var _v10 = parseD(s3);
							if (_v10.$ === 'Bad') {
								var x = _v10.b;
								return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
							} else {
								var d = _v10.a;
								var s4 = _v10.b;
								var _v11 = parseE(s4);
								if (_v11.$ === 'Bad') {
									var x = _v11.b;
									return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
								} else {
									var e = _v11.a;
									var s5 = _v11.b;
									var _v12 = parseF(s5);
									if (_v12.$ === 'Bad') {
										var x = _v12.b;
										return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
									} else {
										var f = _v12.a;
										var s6 = _v12.b;
										var _v13 = parseG(s6);
										if (_v13.$ === 'Bad') {
											var x = _v13.b;
											return A2($stil4m$elm_syntax$ParserFast$Bad, true, x);
										} else {
											var g = _v13.a;
											var s7 = _v13.b;
											return A2(
												$stil4m$elm_syntax$ParserFast$Good,
												A8(
													func,
													{
														end: {column: s7.col, row: s7.row},
														start: {column: s0.col, row: s0.row}
													},
													a,
													b,
													c,
													d,
													e,
													f,
													g),
												s7);
										}
									}
								}
							}
						}
					}
				}
			});
	});
var $stil4m$elm_syntax$Elm$Parser$Modules$effectModuleDefinition = A8(
	$stil4m$elm_syntax$ParserFast$map7WithRange,
	F8(
		function (range, commentsAfterEffect, commentsAfterModule, name, commentsAfterName, whereClauses, commentsAfterWhereClauses, exp) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					exp.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterWhereClauses,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							whereClauses.comments,
							A2(
								$stil4m$elm_syntax$Rope$prependTo,
								commentsAfterName,
								A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterModule, commentsAfterEffect))))),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Module$EffectModule(
						{command: whereClauses.syntax.command, exposingList: exp.syntax, moduleName: name, subscription: whereClauses.syntax.subscription}))
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'effect', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'module', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Base$moduleName,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Modules$effectWhereClauses,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition);
var $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule = function (a) {
	return {$: 'NormalModule', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$normalModuleDefinition = A5(
	$stil4m$elm_syntax$ParserFast$map4WithRange,
	F5(
		function (range, commentsAfterModule, moduleName, commentsAfterModuleName, exposingList) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					exposingList.comments,
					A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterModuleName, commentsAfterModule)),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Module$NormalModule(
						{exposingList: exposingList.syntax, moduleName: moduleName}))
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'module', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Base$moduleName,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition);
var $stil4m$elm_syntax$Elm$Syntax$Module$PortModule = function (a) {
	return {$: 'PortModule', a: a};
};
var $stil4m$elm_syntax$Elm$Parser$Modules$portModuleDefinition = A6(
	$stil4m$elm_syntax$ParserFast$map5WithRange,
	F6(
		function (range, commentsAfterPort, commentsAfterModule, moduleName, commentsAfterModuleName, exposingList) {
			return {
				comments: A2(
					$stil4m$elm_syntax$Rope$prependTo,
					exposingList.comments,
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						commentsAfterModuleName,
						A2($stil4m$elm_syntax$Rope$prependTo, commentsAfterModule, commentsAfterPort))),
				syntax: A2(
					$stil4m$elm_syntax$Elm$Syntax$Node$Node,
					range,
					$stil4m$elm_syntax$Elm$Syntax$Module$PortModule(
						{exposingList: exposingList.syntax, moduleName: moduleName}))
			};
		}),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'port', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	A2($stil4m$elm_syntax$ParserFast$keywordFollowedBy, 'module', $stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout),
	$stil4m$elm_syntax$Elm$Parser$Base$moduleName,
	$stil4m$elm_syntax$Elm$Parser$Layout$maybeLayout,
	$stil4m$elm_syntax$Elm$Parser$Expose$exposeDefinition);
var $stil4m$elm_syntax$Elm$Parser$Modules$moduleDefinition = A3($stil4m$elm_syntax$ParserFast$oneOf3, $stil4m$elm_syntax$Elm$Parser$Modules$normalModuleDefinition, $stil4m$elm_syntax$Elm$Parser$Modules$portModuleDefinition, $stil4m$elm_syntax$Elm$Parser$Modules$effectModuleDefinition);
var $stil4m$elm_syntax$Elm$Parser$Comments$moduleDocumentation = $stil4m$elm_syntax$Elm$Parser$Comments$declarationDocumentation;
var $stil4m$elm_syntax$Rope$ropeLikelyFilledToListInto = F2(
	function (initialAcc, ropeLikelyFilled) {
		ropeLikelyFilledToListInto:
		while (true) {
			if (ropeLikelyFilled.$ === 'Leaf') {
				var onlyElement = ropeLikelyFilled.a;
				return A2($elm$core$List$cons, onlyElement, initialAcc);
			} else {
				var left = ropeLikelyFilled.a;
				var right = ropeLikelyFilled.b;
				var $temp$initialAcc = A2($stil4m$elm_syntax$Rope$ropeLikelyFilledToListInto, initialAcc, right),
					$temp$ropeLikelyFilled = left;
				initialAcc = $temp$initialAcc;
				ropeLikelyFilled = $temp$ropeLikelyFilled;
				continue ropeLikelyFilledToListInto;
			}
		}
	});
var $stil4m$elm_syntax$Rope$toList = function (rope) {
	if (rope.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var ropeLikelyFilled = rope.a;
		return A2($stil4m$elm_syntax$Rope$ropeLikelyFilledToListInto, _List_Nil, ropeLikelyFilled);
	}
};
var $stil4m$elm_syntax$Elm$Parser$File$file = A5(
	$stil4m$elm_syntax$ParserFast$map4,
	F4(
		function (moduleDefinition, moduleComments, imports, declarations) {
			return {
				comments: $stil4m$elm_syntax$Rope$toList(
					A2(
						$stil4m$elm_syntax$Rope$prependTo,
						declarations.comments,
						A2(
							$stil4m$elm_syntax$Rope$prependTo,
							imports.comments,
							A2($stil4m$elm_syntax$Rope$prependTo, moduleComments, moduleDefinition.comments)))),
				declarations: declarations.syntax,
				imports: imports.syntax,
				moduleDefinition: moduleDefinition.syntax
			};
		}),
	$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedByWithComments($stil4m$elm_syntax$Elm$Parser$Modules$moduleDefinition),
	$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrictFollowedByComments(
		A4(
			$stil4m$elm_syntax$ParserFast$map2OrSucceed,
			F2(
				function (moduleDocumentation, commentsAfter) {
					return A2(
						$stil4m$elm_syntax$Rope$filledPrependTo,
						commentsAfter,
						$stil4m$elm_syntax$Rope$one(moduleDocumentation));
				}),
			$stil4m$elm_syntax$Elm$Parser$Comments$moduleDocumentation,
			$stil4m$elm_syntax$Elm$Parser$Layout$layoutStrict,
			$stil4m$elm_syntax$Rope$empty)),
	$stil4m$elm_syntax$ParserWithComments$many($stil4m$elm_syntax$Elm$Parser$Imports$importDefinition),
	$stil4m$elm_syntax$Elm$Parser$File$fileDeclarations);
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 'ExpectingKeyword', a: a};
};
var $elm$parser$Parser$ExpectingNumber = {$: 'ExpectingNumber'};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $stil4m$elm_syntax$ParserFast$ropeFilledToList = F2(
	function (problemToConvert, soFar) {
		switch (problemToConvert.$) {
			case 'ExpectingOneOf':
				var firstTry = problemToConvert.a;
				var secondTry = problemToConvert.b;
				var thirdTryUp = problemToConvert.c;
				return A2(
					$stil4m$elm_syntax$ParserFast$ropeFilledToList,
					firstTry,
					A2(
						$stil4m$elm_syntax$ParserFast$ropeFilledToList,
						secondTry,
						A3($elm$core$List$foldr, $stil4m$elm_syntax$ParserFast$ropeFilledToList, soFar, thirdTryUp)));
			case 'ExpectingNumber':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				return A2(
					$elm$core$List$cons,
					{col: col, problem: $elm$parser$Parser$ExpectingNumber, row: row},
					soFar);
			case 'ExpectingSymbol':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				var symbolString = problemToConvert.c;
				return A2(
					$elm$core$List$cons,
					{
						col: col,
						problem: $elm$parser$Parser$ExpectingSymbol(symbolString),
						row: row
					},
					soFar);
			case 'ExpectingAnyChar':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				return A2(
					$elm$core$List$cons,
					{
						col: col,
						problem: $elm$parser$Parser$Problem('expecting any char'),
						row: row
					},
					soFar);
			case 'ExpectingKeyword':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				var keywordString = problemToConvert.c;
				return A2(
					$elm$core$List$cons,
					{
						col: col,
						problem: $elm$parser$Parser$ExpectingKeyword(keywordString),
						row: row
					},
					soFar);
			case 'ExpectingCharSatisfyingPredicate':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				return A2(
					$elm$core$List$cons,
					{col: col, problem: $elm$parser$Parser$UnexpectedChar, row: row},
					soFar);
			case 'ExpectingStringSatisfyingPredicate':
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				return A2(
					$elm$core$List$cons,
					{
						col: col,
						problem: $elm$parser$Parser$Problem('expected string to pass validation'),
						row: row
					},
					soFar);
			default:
				var row = problemToConvert.a;
				var col = problemToConvert.b;
				var customMessage = problemToConvert.c;
				return A2(
					$elm$core$List$cons,
					{
						col: col,
						problem: $elm$parser$Parser$Problem(customMessage),
						row: row
					},
					soFar);
		}
	});
var $stil4m$elm_syntax$ParserFast$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.a;
			var finalState = _v1.b;
			return (!(finalState.offset - $elm$core$String$length(finalState.src))) ? $elm$core$Result$Ok(value) : $elm$core$Result$Err(
				_List_fromArray(
					[
						{col: finalState.col, problem: $elm$parser$Parser$ExpectingEnd, row: finalState.row}
					]));
		} else {
			var deadEnds = _v1.b;
			return $elm$core$Result$Err(
				A2($stil4m$elm_syntax$ParserFast$ropeFilledToList, deadEnds, _List_Nil));
		}
	});
var $stil4m$elm_syntax$Elm$Parser$parseToFile = function (input) {
	return A2($stil4m$elm_syntax$ParserFast$run, $stil4m$elm_syntax$Elm$Parser$File$file, input);
};
var $author$project$ElmSyntaxPrint$toString = function (print) {
	return $author$project$Print$toString(print);
};
var $author$project$Top$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(model, msg);
		if (_v0.a.$ === 'Normal') {
			var _v1 = _v0.a;
			var _v2 = _v0.b;
			var name = _v2.a;
			var val = _v2.b;
			var elmAstResult = $stil4m$elm_syntax$Elm$Parser$parseToFile(val);
			if (elmAstResult.$ === 'Err') {
				var _v4 = A2($elm$core$Debug$log, 'error', name);
				return _Utils_Tuple2($author$project$Top$Normal, $elm$core$Platform$Cmd$none);
			} else {
				var file = elmAstResult.a;
				var pretty = $author$project$ElmSyntaxPrint$toString(
					$author$project$ElmSyntaxPrint$module_(file));
				return _Utils_Tuple2(
					model,
					$author$project$Top$codeOutPort(
						_Utils_Tuple2(name, pretty)));
			}
		} else {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Top$main = $elm$core$Platform$worker(
	{init: $author$project$Top$init, subscriptions: $author$project$Top$subscriptions, update: $author$project$Top$update});
_Platform_export({'Top':{'init':$author$project$Top$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));