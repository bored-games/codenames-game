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




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
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
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
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

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
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



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
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
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
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
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
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
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Codenames$Card = F3(
	function (word, team, uncovered) {
		return {team: team, uncovered: uncovered, word: word};
	});
var $author$project$Codenames$Model = function (chat) {
	return function (currentTimer) {
		return function (blockKeyShortcuts) {
			return function (debugString) {
				return function (topMessage) {
					return function (toastMessages) {
						return function (toggleLightbox) {
							return function (toggleQR) {
								return function (toggleSidebar) {
									return function (toggleCustomWordsEntry) {
										return function (toggleSoundEffects) {
											return function (toggleSpymasterModal) {
												return function (toggleTeamModal) {
													return function (settings) {
														return function (redRemaining) {
															return function (blueRemaining) {
																return function (password) {
																	return function (customWordsString) {
																		return function (customWords) {
																			return function (wordlists) {
																				return function (allWords) {
																					return function (cards) {
																						return function (user) {
																							return function (users) {
																								return function (red_spymaster) {
																									return function (blue_spymaster) {
																										return function (status) {
																											return function (clueInProgress) {
																												return function (guessesInProgress) {
																													return {allWords: allWords, blockKeyShortcuts: blockKeyShortcuts, blueRemaining: blueRemaining, blue_spymaster: blue_spymaster, cards: cards, chat: chat, clueInProgress: clueInProgress, currentTimer: currentTimer, customWords: customWords, customWordsString: customWordsString, debugString: debugString, guessesInProgress: guessesInProgress, password: password, redRemaining: redRemaining, red_spymaster: red_spymaster, settings: settings, status: status, toastMessages: toastMessages, toggleCustomWordsEntry: toggleCustomWordsEntry, toggleLightbox: toggleLightbox, toggleQR: toggleQR, toggleSidebar: toggleSidebar, toggleSoundEffects: toggleSoundEffects, toggleSpymasterModal: toggleSpymasterModal, toggleTeamModal: toggleTeamModal, topMessage: topMessage, user: user, users: users, wordlists: wordlists};
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
						};
					};
				};
			};
		};
	};
};
var $author$project$Codenames$Status = F5(
	function (game_over, turn, text, clue, remaining_guesses) {
		return {clue: clue, game_over: game_over, remaining_guesses: remaining_guesses, text: text, turn: turn};
	});
var $author$project$Toast$Stack = F2(
	function (a, b) {
		return {$: 'Stack', a: a, b: b};
	});
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $author$project$Toast$initialState = A2(
	$author$project$Toast$Stack,
	_List_Nil,
	$elm$random$Random$initialSeed(0));
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $author$project$Wordlist$wordlistAdvanced = _List_Nil;
var $author$project$Wordlist$wordlistBasic = _List_fromArray(
	['hotdog', 'dance', 'cross', 'cotton', 'match', 'france', 'ball', 'bow', 'mercury', 'net', 'spider', 'cat', 'whip', 'stock', 'princess', 'check', 'bed', 'thief', 'park', 'bar', 'czech', 'thumb', 'plastic', 'leprechaun', 'bell', 'triangle', 'light', 'calf', 'straw', 'cloak', 'mexico', 'ship', 'wind', 'lion', 'london', 'fair', 'force', 'phoenix', 'buck', 'ice cream', 'van', 'drill', 'tube', 'link', 'seal', 'fish', 'iron', 'block', 'doctor', 'degree', 'roulette', 'moon', 'angel', 'shot', 'octopus', 'death', 'time', 'queen', 'circle', 'bark', 'figure', 'file', 'back', 'antarctica', 'dwarf', 'nurse', 'lemon', 'cap', 'belt', 'air', 'switch', 'board', 'eye', 'dice', 'kid', 'police', 'rock', 'screen', 'trap', 'drop', 'agent', 'rabbit', 'vet', 'date', 'chocolate', 'bridge', 'beach', 'cold', 'horn', 'bank', 'stick', 'paste', 'bugle', 'ruler', 'rome', 'battery', 'pound', 'spine', 'mount', 'jupiter', 'boulder', 'glove', 'buffalo', 'parachute', 'gas', 'key', 'casino', 'torch', 'pit', 'king', 'revolution', 'palm', 'ice', 'piano', 'code', 'crane', 'grass', 'pool', 'helicopter', 'mug', 'fan', 'saturn', 'canada', 'skyscraper', 'opera', 'nut', 'heart', 'theater', 'turkey', 'route', 'court', 'alps', 'round', 'orange', 'diamond', 'scale', 'well', 'band', 'train', 'poison', 'fighter', 'shop', 'shadow', 'rose', 'arm', 'ground', 'slip', 'duck', 'pirate', 'jet', 'comic', 'soul', 'strike', 'forest', 'capital', 'table', 'litter', 'car', 'grace', 'pumpkin', 'copper', 'egypt', 'suit', 'robin', 'charge', 'cricket', 'card', 'boom', 'platypus', 'missile', 'part', 'bomb', 'plane', 'hawk', 'sub', 'press', 'box', 'shark', 'stream', 'deck', 'hotel', 'cast', 'alien', 'pilot', 'state', 'fly', 'pitch', 'cook', 'moscow', 'tag', 'spot', 'kiwi', 'trip', 'spell', 'star', 'snowman', 'concert', 'web', 'hospital', 'scorpion', 'laser', 'pyramid', 'sock', 'hood', 'mine', 'penguin', 'spring', 'worm', 'india', 'temple', 'whale', 'fall', 'hollywood', 'fire', 'field', 'tower', 'wake', 'needle', 'centaur', 'change', 'bermuda', 'note', 'tablet', 'ring', 'horse', 'microscope', 'mammoth', 'europe', 'kangaroo', 'bottle', 'fence', 'wave', 'head', 'tie', 'olive', 'teacher', 'jack', 'satellite', 'limousine', 'knight', 'contract', 'scuba diver', 'plate', 'mouse', 'pistol', 'engine', 'string', 'knife', 'cell', 'organ', 'pass', 'stadium', 'sueprhero', 'foot', 'horseshoe', 'log', 'ketchup', 'scientist', 'lab', 'slug', 'himalayas', 'dinosaur', 'lawyer', 'australia', 'pie', 'march', 'africa', 'line', 'war', 'row', 'shakespeare', 'night', 'church', 'cover', 'jam', 'green', 'dragon', 'oil', 'new york', 'honey', 'loch ness', 'witch', 'spike', 'cliff', 'compound', 'crown', 'bill', 'cycle', 'berry', 'america', 'boot', 'chick', 'face', 'school', 'wall', 'atlantis', 'water', 'pin', 'lead', 'port', 'tooth', 'lock', 'pan', 'dog', 'post', 'fork', 'novel', 'club', 'aztec', 'yard', 'ambulance', 'ham', 'undertaker', 'flute', 'china', 'nail', 'day', 'model', 'crash', 'film', 'bug', 'smuggler', 'swing', 'sound', 'gold', 'olympus', 'mass', 'staff', 'greece', 'tap', 'shoe', 'pupil', 'hook', 'snow', 'glass', 'hole', 'eagle', 'unicorn', 'amazon', 'lap', 'washer', 'robot', 'pole', 'beijing', 'telescope', 'soldier', 'ivory', 'ghost', 'center', 'berlin', 'server', 'pants', 'vacuum', 'chair', 'washington', 'watch', 'disease', 'play', 'brush', 'bond', 'conductor', 'ray', 'germany', 'pipe', 'sink', 'maple', 'chest', 'dress', 'draft', 'mail', 'carrot', 'bolt', 'giant', 'millionaire', 'tail', 'tokyo', 'button', 'mint', 'england', 'hand', 'paper', 'tick', 'ninja', 'bat', 'marble', 'square', 'mouth', 'mole', 'plot', 'space', 'trunk', 'bear', 'beat', 'genius', 'spy', 'luck', 'life', 'game', 'embassy', 'apple', 'racket']);
var $author$project$Wordlist$wordlistColors = _List_fromArray(
	['red', 'orange', 'yellow', 'green', 'blue', 'black', 'white', 'cyan', 'pink', 'purple', 'grey', 'brown', 'magenta', 'tan', 'cyan', 'olive', 'maroon', 'navy', 'aquamarine', 'silver', 'gold', 'turquoise', 'lime', 'teal', 'indigo', 'violet']);
var $author$project$Wordlist$wordlistDeutsch = _List_fromArray(
	['Roulette', 'Osten', 'Drache', 'Afrika', 'Krieg', 'Alpen', 'Honig', 'Frankreich', 'Bombe', 'Winnetou', 'Kasino', 'Mexiko', 'Wolkenkratzer', 'Verein', 'Saturn', 'Feder', 'Alien', 'Hamburger', 'Peitsche', 'Berlin', 'Antarktis', 'Adler', 'Schneemann', 'Europa', 'Konzert', 'Loch Ness', 'Schokolade', 'Peking', 'Jet', 'Inka', 'Millionär', 'Becken', 'Dinosaurier', 'Optik', 'Pirat', 'Strasse', 'Hupe', 'Essen', 'Pinguin', 'Siegel', 'Spinne', 'Bart', 'Geschoss', 'Blüte', 'Botschaft', 'Moos', 'Pistole', 'Abgabe', 'Krankheit', 'Bahn', 'Spion', 'Tafel', 'Prinzessin', 'Bart', 'Genie', 'Quartett', 'Dieb', 'Torte', 'Oper', 'Tau', 'Ritter', 'Chemie', 'Stadion', 'Arm', 'Limousine', 'Linse', 'Geist', 'Kippe', 'Laster', 'Melone', 'Lakritze', 'Fuchs', 'Laser', 'Boot', 'Tod', 'Korn', 'Krankenhaus', 'Bande', 'Skelett', 'Mal', 'Oktopus', 'Batterie', 'Hubschrauber', 'Dame', 'Känguru', 'Pflaster', 'Mikroskop', 'Erde', 'Zentaur', 'Messe', 'Superheld', 'Ton', 'Teleskop', 'Römer', 'Fallschirm', 'Stamm', 'Schnabeltier', 'Brand', 'Olymp', 'Schild', 'Satellit', 'Lippe', 'Engel', 'Miene', 'Roboter', 'Kokos', 'Einhorn', 'Läufer', 'Hexe', 'Bund', 'Bergsteiger', 'Elf', 'Taucher', 'Iris', 'Gift', 'Gang', 'Brücke', 'Pfeife', 'Feuer', 'Kiel', 'Tisch', 'Star', 'Wal', 'Leiter', 'Mond', 'Ladung', 'Fisch', 'Bauer', 'Doktor', 'Strudel', 'Kirche', 'Bremse', 'Gürtel', 'Hahn', 'Zitrone', 'Kapele', 'Wind', 'Strauss', 'Löwe', 'Satz', 'Auge', 'Grund', 'Luft', 'Kater', 'Hase', 'Matte', 'Bank', 'Kerze', 'Gras', 'Wirtschaft', 'Auflauf', 'Dichtung', 'Zwerg', 'Gehalt', 'Wald', 'Chor', 'Auto', 'Feige', 'Burg', 'Erika', 'Apfel', 'Mangel', 'Öl', 'Rolle', 'Koch', 'Stock', 'Bär', 'Dietrich', 'Katze', 'Schule', 'Leben', 'Ente', 'Glück', 'Schotten', 'Riese', 'Mark', 'Gesicht', 'Lager', 'Strand', 'Fall', 'Hotel', 'Jura', 'Wasser', 'Niete', 'Papier', 'Geschirr', 'Wurm', 'Knie', 'Anwalt', 'Drossel', 'Forscher', 'Hering', 'Tanz', 'Sekretär', 'Karotte', 'Drucker', 'Ketschup', 'Blinker', 'Nacht', 'Stift', 'Meer', 'Flügel', 'Fuss', 'Schein', 'Maus', 'Funken', 'Messer', 'Bock', 'Theater', 'Po', 'Polizei', 'Atlas', 'Schiff', 'Stempel', 'Pilot', 'Schelle', 'Daumen', 'Leuchte', 'Lehrer', 'Umzug', 'Flasche', 'Finger', 'Tag', 'Riegel', 'König', 'Mast', 'Glas', 'Käfer', 'Königin', 'Bogen', 'Zahn', 'Wanze', 'Hund', 'Scheibe', 'Pferd', 'Schalter', 'Schuh', 'Schimmel', 'Stuhl', 'Demo', 'Krone', 'Bein', 'Eis', 'Börse', 'Gold', 'Takt', 'Gabel', 'Fliege', 'Zeit', 'Jäger', 'Flöte', 'Kunde', 'Fackel', 'Nuss', 'Schnee', 'Schlange', 'Elfenbein', 'Tempo', 'Soldat', 'Bach', 'Pyramide', 'Vorsatz', 'Schnur', 'Gericht', 'Stern', 'Kamm', 'Ring', 'Busch', 'Horn', 'Platte', 'Herz', 'Decke', 'Ball', 'Rücken', 'Kanal', 'Maler', 'Nadel', 'Heide', 'Linie', 'Boxer', 'Korb', 'Reif', 'Blau', 'Ausdruck', 'Taste', 'Zug', 'Schirm', 'Kiefer', 'Spiel', 'Washington', 'Fleck', 'Mini', 'Knopf', 'Gut', 'Mund', 'Kohle', 'Akt', 'Grad', 'Himalaja', 'Brause', 'Bett', 'Viertel', 'Wand', 'Rute', 'Turm', 'Bulle', 'Karte', 'Figur', 'Tor', 'Fest', 'Raute', 'Zoll', 'Kreuz', 'Loge', 'Netz', 'Mutter', 'Punkt', 'Riemen', 'Pass', 'Verband', 'Fläche', 'Hut', 'Loch', 'Watt', 'Glocke', 'Horst', 'Kraft', 'Luxemburg', 'Schloss', 'Birne', 'Maschine', 'Note', 'Welle', 'Film', 'Storm', 'Absatz', 'Haupt', 'Blatt', 'Pol', 'Mandel', 'Mittel', 'Indien', 'Jahr', 'Fessel', 'Leim', 'Schale', 'Seite', 'Aufzug', 'Bau', 'Quelle', 'Kreis', 'Harz', 'Bindung', 'Wurf', 'Uhr', 'Golf', 'New', 'York', 'Rost', 'Australien', 'Rost', 'Bayern', 'Nagel', 'Tokio', 'Toast', 'Ägypten', 'Zylinder', 'London', 'Muschel', 'Morgenstern', 'Würfel', 'Moskau', 'Weide', 'China', 'Kapitän', 'Shakespeare', 'Lösung', 'Hollywood', 'Rasen', 'Griechenland', 'Rock', 'Rom', 'Krebs', 'Hand', 'Flur', 'Ninja', 'Steuer', 'Brötchen', 'Zelle', 'Kiwi', 'Barren', 'Deutschland', 'Löffel', 'Staat', 'Futter', 'Amerikaner', 'Schuppen', 'Atlantis', 'Orange', 'England', 'Pension']);
var $author$project$Wordlist$wordlistEspanol = _List_fromArray(
	['Hollywood', 'Pantalla', 'Juego', 'Canica', 'Dinosaurio', 'Gato', 'Tono', 'Lazo', 'Grecia', 'Cubierta', 'Pico', 'Centro', 'Vacío', 'Unicornio', 'Enterrador', 'Calcetín', 'Lago Ness', 'Caballo', 'Berlin', 'Ornitorrinco', 'Puerto', 'Cofre', 'Caja', 'Compuesto', 'Nave', 'Reloj', 'Espacio', 'Flauta', 'Torre', 'Muerte', 'Pozo', 'Justo', 'Diente', 'Staff', 'Cuenta', 'Disparo', 'Rey', 'Sartén', 'Cuadrado', 'Búfalo', 'Científico', 'Pollo', 'Atlantis', 'Espía', 'Correo', 'Nuez', 'Registro', 'Pirata', 'Cara', 'Pegar', 'Enfermedad', 'Patio', 'Montar', 'Babosa', 'Dados', 'Plomo', 'Gancho', 'Zanahoria', 'Veneno', 'Acción', 'Pie', 'Antorcha', 'Brazo', 'Figura', 'Mina', 'Traje', 'Grúa', 'Beijing', 'Masa', 'Microscopio', 'Motor', 'China', 'Paja', 'Pantalón', 'Europa', 'Bota', 'Princesa', 'Liga', 'Suerte', 'Aceituna', 'Palma', 'Maestro', 'Pulgar', 'Pulpo', 'Capucha', 'Empate', 'Doctor', 'Despertar', 'Grillo', 'Millonario', 'Nueva York', 'Estado', 'Bermudas', 'Parque', 'Pavo', 'Chocolate', 'Viaje', 'Raqueta', 'Bate', 'Chorro', 'Shakespearei', 'Tornillo', 'Interruptor', 'Pared', 'Alma', 'Fantasma', 'Tiempo', 'Danza', 'Amazonas', 'Gracia', 'Moscú', 'Calabaza', 'Antártida', 'Látigo', 'Corazón', 'Mesa', 'Bola', 'Luchador', 'Frío', 'Día', 'Primavera', 'Encuentro', 'Diamante', 'Centauro', 'Marzo', 'Ruleta', 'Perro', 'Cruz', 'Onda', 'Pato', 'Viento', 'Lugar', 'Rascacielos', 'Papel', 'Manzana', 'Aceite', 'Cocinar', 'Volar', 'Reparto', 'Oso', 'Broche', 'Ladrón', 'Trompa', 'América', 'Novela', 'Célula', 'Arco', 'Modelo', 'Cuchillo', 'Caballero', 'Corte', 'Hierro', 'Ballena', 'Sombra', 'Contrato', 'Mercurio', 'Conductor', 'Sello', 'Carro', 'Anillo', 'Niño', 'Piano', 'Láser', 'Sonido', 'Polo', 'Superhéroe', 'Revolución', 'Hoyo', 'Gas', 'Vidrio', 'Washington', 'Corteza', 'Nieve', 'Marfil', 'Pipa', 'Tapa', 'Grado', 'Tokio', 'Iglesia', 'Pay', 'Tubo', 'Bloque', 'Cómico', 'Pez', 'Puente', 'Luna', 'Parte', 'Azteca', 'Contrabandista', 'Tren', 'Embajada', 'Pupila', 'Buzo', 'Hielo', 'Trampa', 'Código', 'Zapato', 'Servidor', 'Club', 'Fila', 'Pirámide', 'Insecto', 'Pingüino', 'Libra', 'Himalaya', 'Checa', 'Roma', 'Ojo', 'Tablero', 'Cama', 'Punto', 'Francia', 'Mamut', 'Algodón', 'Robin', 'Red', 'Corneta', 'Maple', 'Inglaterra', 'Campo', 'Robot', 'Trama', 'África', 'Etiqueta', 'Boca', 'Kiwi', 'Lunar', 'Escuela', 'Hundir', 'Pistola', 'Ópera', 'Menta', 'Raíz', 'Submarino', 'Corona', 'Lomo', 'Avión', 'México', 'Capa', 'Círculo', 'Pastilla', 'Australia', 'Verde', 'Egipto', 'Línea', 'Abogado', 'Bruja', 'Paracaídas', 'Choque', 'Oro', 'Nota', 'León', 'Plástico', 'Telaraña', 'Ambulancia', 'Hospital', 'Hechizo', 'Seguro', 'Agua', 'Londres', 'Casino', 'Ciclo', 'Bar', 'Acantilado', 'Redondo', 'Bmba', 'Gigante', 'Mano', 'ninja', 'Rosa', 'Resbalar', 'Limosina', 'Pase', 'Teatro', 'Plato', 'Satélite', 'Catsup', 'Hotel', 'Cola', 'Garrapata', 'Suelo', 'Policía', 'Enano', 'Ventilador', 'Vestido', 'Saturno', 'Pasto', 'Cepillo', 'Silla', 'Roca', 'Piloto', 'Telescopio', 'Archivo', 'Laboratorio', 'India', 'Regla', 'Clavo', 'Columpio', 'Olimpia', 'Cambio', 'Frcha', 'Corriente', 'Misil', 'Escala', 'Banda', 'Ángel', 'Prensa', 'Baya', 'Carta', 'Comprobar', 'Borrador', 'Cabeza', 'Regazo', 'Naranja', 'Helado', 'Película', 'Lavadora', 'Piscina', 'Tiburón', 'Camioneta', 'Cuerda', 'Ternera', 'Halcón', 'Águila', 'Aguja', 'Bosque', 'Dragón', 'Clave', 'Cinturón', 'Gorra', 'Taladro', 'Guante', 'Pasta', 'Otoño', 'Fuego', 'Araña', 'Espina', 'Soldado', 'Cuerno', 'Reina', 'Jamón', 'Litro', 'Vida', 'Templo', 'Conejo', 'Botón', 'Juego', 'Estrella', 'Júpiter', 'Veterinaria', 'Nioche', 'aire', 'Batería', 'Genio', 'Tienda', 'Botella', 'Estadio', 'Alien', 'Luz', 'Triángulo', 'Limón', 'Enfermera', 'Gota', 'Pista', 'Banco', 'Alemania', 'Lombriz', 'Rayo', 'Capital', 'Paro', 'Guerra', 'Concierto', 'Miel', 'Canadá', 'Ciervo', 'Mono de Nieve', 'Vencer', 'Atasco', 'Cobre', 'Playa', 'Campana', 'Duende', 'Fénix', 'Fuerza', 'Explosión', 'Tenedor', 'Alpes', 'Poste', 'Cerca', 'Canguro', 'Ratón', 'Taza', 'Herradura', 'Escorpión', 'Agente', 'Helicóptero', 'Agujero', 'Órgano', 'Gato', 'Carga']);
var $author$project$Wordlist$wordlistFrancais = _List_fromArray(
	['Accident', 'Achat', 'Acné', 'Action', 'Adolescent', 'Afrique', 'Aiguille', 'Allumer', 'Alpes', 'Alphabet', 'Altitude', 'Amérique', 'Ami', 'Amour', 'Ampoule', 'Anniversaire', 'Appétit', 'Araignée', 'Arbre', 'Arc', 'Arc-en-ciel', 'Argent', 'Arme', 'Armée', 'Ascenseur', 'Asie', 'Assis', 'Astronaute', 'Atchoum', 'Athlète', 'Atlantide', 'Aube', 'Australie', 'Avec', 'Aventure', 'Avion', 'Avocat', 'Bac', 'Baguette', 'Bain', 'Baiser', 'Balai', 'Balle', 'Ballon', 'Bambou', 'Banane', 'Bannir', 'Barbe', 'Barrière', 'Bas', 'Basket', 'Bateau', 'Bâton', 'Batterie', 'Bébé', 'Beethoven', 'Bête', 'Biberon', 'Bière', 'Blanc', 'blé', 'Bleu', 'Bob', 'Boisson', 'Boîte', 'Bombe', 'Bonbon', 'Bonnet', 'Bord', 'Bordeaux', 'Botte', 'Boue', 'Bougie', 'Boule', 'Bouteille', 'Bouton', 'Branche', 'Bras', 'Bravo', 'Bretagne', 'Brise', 'Brosse', 'Bruit', 'Brume', 'Brun', 'Bûche', 'Bulle', 'Bureau', 'But', 'Cabane', 'Cabine', 'Cacher', 'Cadeau', 'Cafard', 'Café', 'Caisse', 'Calculer', 'Calme', 'Caméra', 'Camion', 'Camping', 'Canada', 'Canard', 'Canette', 'Canine', 'Cap', 'Capitalisme', 'Car', 'Carotte', 'Carré', 'Carte', 'Carton', 'Casque', 'Casser', 'Cassette', 'Cauchemar', 'Cause', 'Ceinture', 'Cellule', 'Cercle', 'Chaîne', 'Chair', 'Chaise', 'Champ', 'Champion', 'Chant', 'Chapeau', 'Charbon', 'Charge', 'Chasse', 'Chat', 'Château', 'Chaud', 'Chaussure', 'Chauve', 'Chef', 'Chemise', 'Chêne', 'Cher', 'Cheval', 'Chevalier', 'Cheveu', 'Chien', 'Chiffre', 'Chine', 'Chocolat', 'Chômage', 'Ciel', 'Cil', 'Cinéma', 'Cire', 'Cirque', 'Citron', 'Clé', 'Clou', 'Clown', 'Coach', 'Coccinelle', 'Code', 'Cœur', 'Col', 'Colle', 'Colline', 'Colonne', 'Cône', 'Confort', 'Continu', 'Contre', 'Conversation', 'Copain', 'Coq', 'Coquillage', 'Corbeau', 'Corde', 'Corps', 'Côte', 'Coude', 'Couloir', 'Coup', 'Cour', 'Courant', 'Courrier', 'Cours', 'Course', 'Court', 'Couteau', 'Couvert', 'Couverture', 'Cowboy', 'Crac', 'Crayon', 'Crème', 'Critique', 'Crochet', 'Croix', 'Croûte', 'Cuillère', 'Cuir', 'Cuisine', 'Culotte', 'Cycle', 'Dard', 'Dé', 'Debout', 'Défaut', 'Dehors', 'Démocratie', 'Dent', 'Dentiste', 'Dessin', 'Devoir', 'Diamant', 'Dictionnaire', 'Dieu', 'Dinosaure', 'Discours', 'Disque', 'Dix', 'Docteur', 'Doigt', 'Domino', 'Dormir', 'Droit', 'Eau', 'Échec', 'Échelle', 'Éclair', 'École', 'Écran', 'Écraser', 'Écrit', 'Église', 'Égout', 'Électricité', 'Éléphant', 'Élève', 'Elfe', 'Empreinte', 'Enceinte', 'Épice', 'Épine', 'Erreur', 'Espace', 'Espion', 'Essence', 'État', 'Été', 'Étoile', 'Étranger', 'Éventail', 'Évolution', 'Explosoin', 'Extension', 'Face', 'Fan', 'Farce', 'Fatigue', 'Fauteuil', 'Fayot', 'Fenêtre', 'Fer', 'Fête', 'Feu', 'Feuille', 'Fidèle', 'Fil', 'Fille', 'Flamme', 'Flèche', 'Fleur', 'Fleuve', 'Fond', 'Football', 'Forêt', 'Forger', 'Foudre', 'Fouet', 'Four', 'Fourmi', 'Froid', 'Fromage', 'Front', 'Fruit', 'Fuir', 'Futur', 'Garçon', 'Gâteau', 'Gauche', 'Gaz', 'Gazon', 'Gel', 'Genou', 'Glace', 'Gomme', 'Gorge', 'Goutte', 'Grand', 'Grèce', 'Grenouille', 'Grippe', 'Gris', 'Gros', 'Groupe', 'Guitare', 'Hasard', 'Haut', 'Hélicoptère', 'Herbe', 'Heureux', 'Histoire', 'Hiver', 'Hôtel', 'Hugo', 'Huile', 'Humide', 'Humour', 'Indice', 'Internet', 'Inviter', 'Italie', 'Jacques', 'Jambe', 'Jambon', 'Jardin', 'Jaune', 'Jean', 'Jeanne', 'Jet', 'Jeu', 'Jogging', 'Jour', 'Journal', 'Jupiter', 'Kilo', 'Kiwi', 'Laine', 'Lait', 'Langue', 'Lapin', 'Latin', 'Laver', 'Lecteur', 'Léger', 'Lent', 'Lettre', 'Lien', 'Ligne', 'Linge', 'Lion', 'Lit', 'Livre', 'Loi', 'Long', 'Louis', 'Loup', 'Lumière', 'Lundi', 'Lune', 'Lunette', 'Machine', 'Macho', 'main', 'Maison', 'Maîtresse', 'Mal', 'Maladie', 'Maman', 'Mammouth', 'Manger', 'Marais', 'Marc', 'Marche', 'Mariage', 'Marie', 'Mariée', 'Marque', 'Marseille', 'Masse', 'Mer', 'Messe', 'Mètre', 'Métro', 'Miaou', 'Micro', 'Mieux', 'Mille', 'Mine', 'Miroir', 'Moderne', 'Moitié', 'Monde', 'Monstre', 'Montagne', 'Montre', 'Mort', 'Moteur', 'Moto', 'Mou', 'Mouche', 'Moulin', 'Moustache', 'Mouton', 'Mur', 'Muscle', 'Musique', 'Mystère', 'Nage', 'Nature', 'Neige', 'Neutre', 'New York', 'Nez', 'Nid', 'Ninja', 'Niveau', 'Noël', 'Nœud', 'Noir', 'Nous', 'Nuage', 'Nuit', 'Numéro', 'Œil', 'Œuf', 'Oiseau', 'Olympique', 'Ombre', 'Ongle', 'Or', 'Oral', 'Orange', 'Ordinateur', 'Ordre', 'Ordure', 'Oreille', 'Organe', 'Orgueil', 'Ours', 'Outil', 'Ouvert', 'Ovale', 'Pain', 'Palais', 'Panneau', 'Pantalon', 'Pantin', 'Papa', 'Papier', 'Papillon', 'Paradis', 'Parc', 'Paris', 'Parole', 'Partie', 'Passe', 'Pâte', 'Patin', 'Patte', 'Paul', 'Payer', 'Pêche', 'Peinture', 'Pendule', 'Penser', 'Personne', 'Petit', 'Peur', 'Philosophe', 'Photo', 'Phrase', 'Piano', 'Pièce', 'Pied', 'Pierre', 'Pile', 'Pilote', 'Pince', 'Pioche', 'Pion', 'Pirate', 'Pire', 'Piscine', 'Place', 'Plafond', 'Plage', 'Plaie', 'Plan', 'Planche', 'Planète', 'Plante', 'Plastique', 'Plat', 'Plat', 'Plomb', 'Plonger', 'Pluie', 'Poche', 'Poète', 'Poids', 'Poing', 'Point', 'Poivre', 'Police', 'Politique', 'Pollen', 'Polo', 'Pomme', 'Pompe', 'Pont', 'Population', 'Port', 'Porte', 'Portefeuille', 'Positif', 'Poste', 'Poubelle', 'Poule', 'Poupée', 'Pousser', 'Poussière', 'Pouvoir', 'Préhistoire', 'Premier', 'Présent', 'Presse', 'Prier', 'Princesse', 'Prise', 'Privé', 'Professeur', 'Psychologie', 'Public', 'Pull', 'Punk', 'Puzzle', 'Pyjama', 'Quatre', 'Quinze', 'Race', 'Radio', 'Raisin', 'Rap', 'Rayé', 'Rayon', 'Réfléchir', 'Reine', 'Repas', 'Reptile', 'Requin', 'Rêve', 'Riche', 'Rideau', 'Rien', 'Rire', 'Robinet', 'Roche', 'Roi', 'Rond', 'Rose', 'Roue', 'Rouge', 'Rouille', 'Roux', 'Russie', 'Sable', 'Sabre', 'Sac', 'Sain', 'Saison', 'Sale', 'Salle', 'Salut', 'Samu', 'Sandwich', 'Sang', 'Sapin', 'Satellite', 'Saumon', 'Saut', 'Savoir', 'Schtroumpf', 'Science', 'Scout', 'Sec', 'Seine', 'Sel', 'Sept', 'Serpent', 'Serrer', 'Sexe', 'Shampooing', 'Siècle', 'Siège', 'Sieste', 'Silhouette', 'Sirène', 'Ski', 'Soleil', 'Sommeil', 'Son', 'Sonner', 'Sorcière', 'Sourd', 'Souris', 'Sport', 'Star', 'Station', 'Stylo', 'Sur', 'Surface', 'Sushi', 'Swing', 'Tableau', 'Tache', 'Taille', 'Tante', 'Tapis', 'Tard', 'Taxi', 'Téléphone', 'Télévision', 'Temple', 'Temps', 'Tennis', 'Tête', 'Thé', 'Tigre', 'Tintin', 'Tissu', 'Titre', 'Titre', 'Toast', 'Toilette', 'Tokyo', 'Tombe', 'Ton', 'Top', 'Touche', 'Toujours', 'Tour', 'Tournoi', 'Tout', 'Trace', 'Train', 'Traîner', 'Transport', 'Travail', 'Trésor', 'Triangle', 'Triste', 'Trône', 'Troupeau', 'Tsar', 'Tube', 'Tuer', 'Tuer', 'Tupperware', 'Tuyau', 'Twitter', 'Type', 'Université', 'Vache', 'Vache', 'Vague', 'Vaisselle', 'Valeur', 'Ver', 'Verdict', 'Verre', 'Vers', 'Vert', 'Veste', 'Viande', 'Vide', 'Vie', 'Vieux', 'Ville', 'Vin', 'Vingt', 'Violon', 'Vipère', 'Vision', 'Vite', 'Vive', 'Vœu', 'Voile', 'Voisin', 'Voiture', 'Vol', 'Volume', 'Vote', 'Vouloir', 'Voyage', 'Zen', 'Zéro', 'Zodiaque', 'Zone', 'Zoo']);
var $author$project$Wordlist$wordlistHalloween = _List_fromArray(
	['abnormal', 'abominable', 'afraid', 'afterlife', 'agony ', 'alarming', 'alien', 'angel', 'apparition', 'autumn', 'axe ', 'banshee', 'basilisk', 'bat', 'beast', 'behemoth', 'black', 'blood ', 'bogeyman', 'bones', 'boo', 'broom', 'bury', 'cadaver', 'candy', 'carve', 'casket', 'cat', 'cauldron', 'cemetery', 'Cerberus', 'changeling', 'chill', 'cider', 'claws', 'clown', 'coffin', 'cold ', 'convulse ', 'corpse', 'costume', 'creak ', 'creepy', 'crone', 'crypt', 'dark', 'dead', 'demon ', 'devil ', 'dire', 'disease', 'disguise', 'disturbed ', 'donuts', 'Dracula', 'dragon', 'dread ', 'drown ', 'ecto-plasm', 'eerie', 'evil', 'eye', 'fangs', 'fatal ', 'fear ', 'fiend', 'fog', 'Frankenstein', 'freak', 'fright', 'gargoyle', 'ghastly', 'ghost ', 'ghoul', 'goblin', 'goo', 'gore ', 'grave', 'gruesome', 'Halloween', 'haunted', 'hayride', 'heart', 'Hell ', 'helpless', 'horror', 'howl', 'insane', 'jack-o\'-lantern', 'kill ', 'leaves', 'macabre', 'magic', 'mask', 'mausoleum', 'menace ', 'midnight', 'monster ', 'moon ', 'morbid', 'mummy', 'murder', 'mutant', 'mystery', 'mystical', 'night', 'occult', 'October', 'ogre', 'ominous', 'orange', 'owl', 'pagan', 'pain ', 'paranormal', 'party', 'phantasm', 'phantom', 'poltergeist', 'potion', 'pumpkin', 'putrid ', 'raven', 'reaper', 'rotten ', 'scare', 'scream ', 'shadow', 'shiver', 'silence', 'sinister ', 'skeleton ', 'skull', 'slime', 'specter', 'spell', 'spider', 'spirit', 'spooky', 'stab', 'stalk ', 'strange', 'strangle', 'suicide ', 'supernatural', 'superstition', 'sweets', 'tarantula', 'tense ', 'terrify ', 'thrill', 'tomb', 'torture ', 'treat', 'tremble', 'trick', 'troll', 'undead', 'unearth', 'vampire', 'villain', 'wand', 'warlock', 'web', 'weird', 'werewolf', 'wicked', 'witch', 'wizard', 'wraith', 'zombie ']);
var $author$project$Codenames$init = function (_v0) {
	return _Utils_Tuple2(
		$author$project$Codenames$Model(_List_Nil)(0)(false)('')('')($author$project$Toast$initialState)(false)(false)(false)(false)(false)(false)(true)(
			{customWords: true, override: false, spies: false})(0)(0)('')('')(_List_Nil)(
			_List_fromArray(
				[
					{include: true, key: 0, name: 'basic words', words: $author$project$Wordlist$wordlistBasic},
					{include: false, key: 1, name: 'advanced words', words: $author$project$Wordlist$wordlistAdvanced},
					{include: false, key: 2, name: 'color words', words: $author$project$Wordlist$wordlistColors},
					{include: false, key: 3, name: 'Halloween words', words: $author$project$Wordlist$wordlistHalloween},
					{include: false, key: 4, name: 'German words', words: $author$project$Wordlist$wordlistDeutsch},
					{include: false, key: 5, name: 'French words', words: $author$project$Wordlist$wordlistFrancais},
					{include: false, key: 6, name: 'Spanish words', words: $author$project$Wordlist$wordlistEspanol}
				]))(_List_Nil)(
			A2(
				$elm$core$List$repeat,
				25,
				A3($author$project$Codenames$Card, '', 0, false)))($elm$core$Maybe$Nothing)(_List_Nil)($elm$core$Maybe$Nothing)($elm$core$Maybe$Nothing)(
			A5($author$project$Codenames$Status, true, false, 'Connecting...', $elm$core$Maybe$Nothing, 0))('')(''),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Codenames$GetJSON = function (a) {
	return {$: 'GetJSON', a: a};
};
var $author$project$Codenames$KeyChanged = function (a) {
	return {$: 'KeyChanged', a: a};
};
var $author$project$Codenames$Ping = function (a) {
	return {$: 'Ping', a: a};
};
var $author$project$Codenames$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Codenames$inputPort = _Platform_incomingPort('inputPort', $elm$json$Json$Decode$value);
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
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
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Codenames$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($elm$time$Time$every, 30000, $author$project$Codenames$Ping),
				A2($elm$time$Time$every, 1000, $author$project$Codenames$Tick),
				$author$project$Codenames$inputPort($author$project$Codenames$GetJSON),
				$elm$browser$Browser$Events$onKeyUp(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Codenames$KeyChanged,
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)))
			]));
};
var $author$project$Codenames$AddToastMessage = function (a) {
	return {$: 'AddToastMessage', a: a};
};
var $author$project$Codenames$ClearUI = {$: 'ClearUI'};
var $author$project$Codenames$GetBoard = function (a) {
	return {$: 'GetBoard', a: a};
};
var $author$project$Codenames$GetChat = function (a) {
	return {$: 'GetChat', a: a};
};
var $author$project$Codenames$GetFlashMessage = function (a) {
	return {$: 'GetFlashMessage', a: a};
};
var $author$project$Codenames$GetPassword = function (a) {
	return {$: 'GetPassword', a: a};
};
var $author$project$Codenames$GetSpymasterModal = function (a) {
	return {$: 'GetSpymasterModal', a: a};
};
var $author$project$Codenames$GetSpymasters = function (a) {
	return {$: 'GetSpymasters', a: a};
};
var $author$project$Codenames$GetStatus = function (a) {
	return {$: 'GetStatus', a: a};
};
var $author$project$Codenames$GetUser = function (a) {
	return {$: 'GetUser', a: a};
};
var $author$project$Codenames$GetUsersList = function (a) {
	return {$: 'GetUsersList', a: a};
};
var $author$project$Codenames$NoOp = {$: 'NoOp'};
var $author$project$Codenames$PassTurn = {$: 'PassTurn'};
var $author$project$Toast$Success = F2(
	function (a, b) {
		return {$: 'Success', a: a, b: b};
	});
var $author$project$Codenames$ToggleQR = {$: 'ToggleQR'};
var $author$project$Codenames$ToggleSidebar = {$: 'ToggleSidebar'};
var $author$project$Toast$Temporary = {$: 'Temporary'};
var $author$project$Toast$Entered = {$: 'Entered'};
var $author$project$Toast$TransitionOut = function (a) {
	return {$: 'TransitionOut', a: a};
};
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$maxInt = 2147483647;
var $elm$random$Random$minInt = -2147483648;
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $author$project$Toast$getNewId = function (seed) {
	return A2(
		$elm$random$Random$step,
		A2($elm$random$Random$int, $elm$random$Random$minInt, $elm$random$Random$maxInt),
		seed);
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Toast$addToast_ = F5(
	function (removeBehaviour, _v0, tagger, toast, _v1) {
		var cfg = _v0.a;
		var model = _v1.a;
		var cmd = _v1.b;
		var _v2 = model.toastMessages;
		var toasts = _v2.a;
		var seed = _v2.b;
		var _v3 = $author$project$Toast$getNewId(seed);
		var newId = _v3.a;
		var newSeed = _v3.b;
		var task = function () {
			if (removeBehaviour.$ === 'Temporary') {
				return A2(
					$elm$core$Task$perform,
					function (_v5) {
						return tagger(
							$author$project$Toast$TransitionOut(newId));
					},
					$elm$core$Process$sleep(cfg.delay));
			} else {
				return $elm$core$Platform$Cmd$none;
			}
		}();
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					toastMessages: A2(
						$author$project$Toast$Stack,
						_Utils_ap(
							toasts,
							_List_fromArray(
								[
									_Utils_Tuple3(newId, $author$project$Toast$Entered, toast)
								])),
						newSeed)
				}),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[cmd, task])));
	});
var $author$project$Toast$addToast = $author$project$Toast$addToast_($author$project$Toast$Temporary);
var $author$project$Toast$Config = function (a) {
	return {$: 'Config', a: a};
};
var $author$project$Toast$config = $author$project$Toast$Config(
	{containerAttrs: _List_Nil, delay: 3000, itemAttrs: _List_Nil, transitionInAttrs: _List_Nil, transitionOutAttrs: _List_Nil, transitionOutDuration: 0});
var $author$project$Toast$containerAttrs = F2(
	function (attrs, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{containerAttrs: attrs}));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Toast$defaultContainerAttrs = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
		A2($elm$html$Html$Attributes$style, 'top', '0'),
		A2($elm$html$Html$Attributes$style, 'right', '0'),
		A2($elm$html$Html$Attributes$style, 'width', '100%'),
		A2($elm$html$Html$Attributes$style, 'max-width', '300px'),
		A2($elm$html$Html$Attributes$style, 'list-style-type', 'none'),
		A2($elm$html$Html$Attributes$style, 'padding', '0'),
		A2($elm$html$Html$Attributes$style, 'margin', '0')
	]);
var $author$project$Toast$defaultItemAttrs = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'margin', '1em 1em 0 1em'),
		A2($elm$html$Html$Attributes$style, 'max-height', '100px'),
		A2($elm$html$Html$Attributes$style, 'transition', 'max-height 0.6s, margin-top 0.6s')
	]);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Toast$defaultTransitionInAttrs = _List_fromArray(
	[
		$elm$html$Html$Attributes$class('animated bounceInRight')
	]);
var $author$project$Toast$defaultTransitionOutAttrs = _List_fromArray(
	[
		$elm$html$Html$Attributes$class('animated fadeOutRightBig'),
		A2($elm$html$Html$Attributes$style, 'max-height', '0'),
		A2($elm$html$Html$Attributes$style, 'margin-top', '0')
	]);
var $author$project$Toast$delay = F2(
	function (time, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{delay: time}));
	});
var $author$project$Toast$itemAttrs = F2(
	function (attrs, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{itemAttrs: attrs}));
	});
var $author$project$Toast$transitionInAttrs = F2(
	function (attrs, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{transitionInAttrs: attrs}));
	});
var $author$project$Toast$transitionOutAttrs = F2(
	function (attrs, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{transitionOutAttrs: attrs}));
	});
var $author$project$Toast$transitionOutDuration = F2(
	function (time, _v0) {
		var cfg = _v0.a;
		return $author$project$Toast$Config(
			_Utils_update(
				cfg,
				{transitionOutDuration: time}));
	});
var $author$project$Toast$defaultConfig = A2(
	$author$project$Toast$delay,
	5000,
	A2(
		$author$project$Toast$itemAttrs,
		$author$project$Toast$defaultItemAttrs,
		A2(
			$author$project$Toast$containerAttrs,
			$author$project$Toast$defaultContainerAttrs,
			A2(
				$author$project$Toast$transitionInAttrs,
				$author$project$Toast$defaultTransitionInAttrs,
				A2(
					$author$project$Toast$transitionOutAttrs,
					$author$project$Toast$defaultTransitionOutAttrs,
					A2($author$project$Toast$transitionOutDuration, 700, $author$project$Toast$config))))));
var $author$project$Codenames$toastConfig = A2($author$project$Toast$delay, 3300, $author$project$Toast$defaultConfig);
var $author$project$Codenames$addToast = F2(
	function (toast, _v0) {
		var model = _v0.a;
		var cmd = _v0.b;
		return A4(
			$author$project$Toast$addToast,
			$author$project$Codenames$toastConfig,
			$author$project$Codenames$AddToastMessage,
			toast,
			_Utils_Tuple2(model, cmd));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $elm$browser$Browser$Dom$blur = _Browser_call('blur');
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$Codenames$BoardInfo = F4(
	function (password, redRemaining, blueRemaining, cards) {
		return {blueRemaining: blueRemaining, cards: cards, password: password, redRemaining: redRemaining};
	});
var $author$project$Codenames$CardTwo = F3(
	function (word, team, id) {
		return {id: id, team: team, word: word};
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Codenames$decodeCard = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Codenames$CardTwo,
	A2($elm$json$Json$Decode$field, 'word', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'team',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Codenames$decodeCardList = $elm$json$Json$Decode$list($author$project$Codenames$decodeCard);
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Codenames$decodeBoardInfo = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Codenames$BoardInfo,
	A2($elm$json$Json$Decode$field, 'password', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'red_remaining', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'blue_remaining', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'board', $author$project$Codenames$decodeCardList));
var $author$project$Chat$Chatline = F5(
	function (room_name, user, message, timestamp, kind) {
		return {kind: kind, message: message, room_name: room_name, timestamp: timestamp, user: user};
	});
var $author$project$Chat$chatMessageToChatline = F4(
	function (room_name, user, message, timestamp) {
		return A5($author$project$Chat$Chatline, room_name, user, message, timestamp, 1);
	});
var $author$project$User$User = F7(
	function (username, nickname, color, team, score, is_admin, is_muted) {
		return {color: color, is_admin: is_admin, is_muted: is_muted, nickname: nickname, score: score, team: team, username: username};
	});
var $elm$json$Json$Decode$map7 = _Json_map7;
var $author$project$User$decodeUser = A8(
	$elm$json$Json$Decode$map7,
	$author$project$User$User,
	A2($elm$json$Json$Decode$field, 'username', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'nickname', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'color', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'team', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'is_admin', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'is_muted', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Chat$decodeChatline = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Chat$chatMessageToChatline,
	A2($elm$json$Json$Decode$field, 'room_name', $elm$json$Json$Decode$string),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$field, 'user', $author$project$User$decodeUser)),
	A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$string));
var $author$project$Codenames$JSONMessage = F2(
	function (action, content) {
		return {action: action, content: content};
	});
var $author$project$Codenames$decodeJSON = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Codenames$JSONMessage,
	A2($elm$json$Json$Decode$field, 'action', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'content', $elm$json$Json$Decode$value));
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Codenames$decodeSpymasters = A3(
	$elm$json$Json$Decode$map2,
	$elm$core$Tuple$pair,
	A2(
		$elm$json$Json$Decode$index,
		0,
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$index,
		1,
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)));
var $elm$json$Json$Decode$map5 = _Json_map5;
var $author$project$Codenames$decodeStatus = A6(
	$elm$json$Json$Decode$map5,
	$author$project$Codenames$Status,
	A2($elm$json$Json$Decode$field, 'game_over', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'turn', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'clue',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'remaining_guesses', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $author$project$User$HalfUser = F6(
	function (nickname, color, team, score, isadmin, ismuted) {
		return {color: color, isadmin: isadmin, ismuted: ismuted, nickname: nickname, score: score, team: team};
	});
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$User$halfUserDecoder = A7(
	$elm$json$Json$Decode$map6,
	$author$project$User$HalfUser,
	A2($elm$json$Json$Decode$field, 'nickname', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'color', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'team', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'is_admin', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'is_muted', $elm$json$Json$Decode$bool));
var $author$project$User$halfUserToUser = F2(
	function (username, _v0) {
		var nickname = _v0.nickname;
		var color = _v0.color;
		var team = _v0.team;
		var score = _v0.score;
		var isadmin = _v0.isadmin;
		var ismuted = _v0.ismuted;
		return A7($author$project$User$User, username, nickname, color, team, score, isadmin, ismuted);
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$User$decodeUsersList = A2(
	$elm$json$Json$Decode$map,
	$elm$core$Dict$map($author$project$User$halfUserToUser),
	$elm$json$Json$Decode$dict($author$project$User$halfUserDecoder));
var $elm$json$Json$Decode$decodeValue = _Json_run;
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
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$User$getUserByName = F2(
	function (searchstring, users) {
		if (searchstring.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var s = searchstring.a;
			return $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (x) {
						return _Utils_eq(x.nickname, s);
					},
					users));
		}
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$core$Basics$not = _Basics_not;
var $author$project$Codenames$isNotEmpty = function (str) {
	return !$elm$core$String$isEmpty(str);
};
var $elm$core$Debug$log = _Debug_log;
var $author$project$Codenames$maybeToggle = F2(
	function (key, wordlist) {
		return _Utils_eq(key, wordlist.key) ? _Utils_update(
			wordlist,
			{include: !wordlist.include}) : wordlist;
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Codenames$outputPort = _Platform_outgoingPort('outputPort', $elm$json$Json$Encode$string);
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Codenames$toCard = function (c) {
	return A3(
		$author$project$Codenames$Card,
		c.word,
		A2($elm$core$Maybe$withDefault, 0, c.team),
		!_Utils_eq(c.team, $elm$core$Maybe$Nothing));
};
var $elm$core$String$trim = _String_trim;
var $author$project$Toast$Leaving = {$: 'Leaving'};
var $author$project$Toast$Remove = function (a) {
	return {$: 'Remove', a: a};
};
var $author$project$Toast$update = F4(
	function (_v0, tagger, msg, model) {
		var cfg = _v0.a;
		var _v1 = model.toastMessages;
		var toasts = _v1.a;
		var seed = _v1.b;
		switch (msg.$) {
			case 'Add':
				var toast = msg.a;
				return A4(
					$author$project$Toast$addToast,
					$author$project$Toast$config,
					tagger,
					toast,
					_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
			case 'Remove':
				var targetId = msg.a;
				var newStack = A2(
					$elm$core$List$filter,
					function (_v3) {
						var id = _v3.a;
						var toast = _v3.b;
						var status = _v3.c;
						return !_Utils_eq(id, targetId);
					},
					toasts);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							toastMessages: A2($author$project$Toast$Stack, newStack, seed)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var targetId = msg.a;
				var newStack = A2(
					$elm$core$List$map,
					function (_v5) {
						var id = _v5.a;
						var status = _v5.b;
						var toast = _v5.c;
						return _Utils_eq(id, targetId) ? _Utils_Tuple3(id, $author$project$Toast$Leaving, toast) : _Utils_Tuple3(id, status, toast);
					},
					toasts);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							toastMessages: A2($author$project$Toast$Stack, newStack, seed)
						}),
					A2(
						$elm$core$Task$perform,
						function (_v4) {
							return tagger(
								$author$project$Toast$Remove(targetId));
						},
						$elm$core$Process$sleep(cfg.transitionOutDuration)));
		}
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Codenames$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'ToggleLightbox':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleLightbox: !model.toggleLightbox}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleQR':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleQR: !model.toggleQR}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleSidebar':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleSidebar: !model.toggleSidebar}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleSpymasterModal':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleSpymasterModal: !model.toggleSpymasterModal}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleTeamModal':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleTeamModal: !model.toggleTeamModal}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleCustomWordsEntry':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleCustomWordsEntry: !model.toggleCustomWordsEntry}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleSoundEffects':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleSoundEffects: !model.toggleSoundEffects}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleSpies':
					var oldSettings = model.settings;
					var newSettings = _Utils_update(
						oldSettings,
						{spies: !oldSettings.spies});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{settings: newSettings}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleOverride':
					var oldSettings = model.settings;
					var newSettings = _Utils_update(
						oldSettings,
						{override: !oldSettings.override});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{settings: newSettings}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleWordlist':
					var key = msg.a;
					var wordlists = A2(
						$elm$core$List$map,
						$author$project$Codenames$maybeToggle(key),
						model.wordlists);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{wordlists: wordlists}),
						$elm$core$Platform$Cmd$none);
				case 'ToggleCustomWords':
					var oldSettings = model.settings;
					var newSettings = _Utils_update(
						oldSettings,
						{customWords: !oldSettings.customWords});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{settings: newSettings}),
						$elm$core$Platform$Cmd$none);
				case 'SetCustomWords':
					var str = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{customWordsString: str}),
						$elm$core$Platform$Cmd$none);
				case 'SaveCustomWords':
					var customWords = $elm$core$Set$toList(
						$elm$core$Set$fromList(
							A2(
								$elm$core$List$filter,
								$author$project$Codenames$isNotEmpty,
								A2(
									$elm$core$List$map,
									$elm$core$String$trim,
									A2($elm$core$String$split, '\n', model.customWordsString)))));
					var customWordsString = A2($elm$core$String$join, '\n', customWords);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{customWords: customWords, customWordsString: customWordsString, toggleCustomWordsEntry: false}),
						$elm$core$Platform$Cmd$none);
				case 'CancelCustomWords':
					var customWordsString = A2($elm$core$String$join, '\n', model.customWords);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{customWordsString: customWordsString, toggleCustomWordsEntry: false}),
						$elm$core$Platform$Cmd$none);
				case 'PassTurn':
					return _Utils_Tuple2(
						model,
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('pass_turn')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$string(''))
													])))
										])))));
				case 'NewGame':
					return _Utils_Tuple2(
						model,
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('new_game')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$string(''))
													])))
										])))));
				case 'ClearUI':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toggleCustomWordsEntry: false, toggleLightbox: false, toggleQR: false, toggleSidebar: false}),
						A2(
							$elm$core$Task$attempt,
							function (_v1) {
								return $author$project$Codenames$NoOp;
							},
							$elm$browser$Browser$Dom$blur('custom_words_entry')));
				case 'BlockKeyShortcuts':
					var bool = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{blockKeyShortcuts: bool}),
						$elm$core$Platform$Cmd$none);
				case 'KeyChanged':
					var key = msg.a;
					var command = function () {
						if (model.blockKeyShortcuts) {
							if (key === 'Escape') {
								return $elm$core$Maybe$Just(
									A2($author$project$Codenames$update, $author$project$Codenames$ClearUI, model));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						} else {
							switch (key) {
								case ' ':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$PassTurn, model));
								case 'Escape':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$ClearUI, model));
								case 'q':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$ToggleQR, model));
								case 'Q':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$ToggleQR, model));
								case 's':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$ToggleSidebar, model));
								case 'S':
									return $elm$core$Maybe$Just(
										A2($author$project$Codenames$update, $author$project$Codenames$ToggleSidebar, model));
								default:
									return $elm$core$Maybe$Nothing;
							}
						}
					}();
					if (command.$ === 'Just') {
						var cmd = command.a;
						return cmd;
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 'GetUsersList':
					var json = msg.a;
					var _v5 = A2($elm$json$Json$Decode$decodeValue, $author$project$User$decodeUsersList, json);
					if (_v5.$ === 'Ok') {
						var usersList = _v5.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									users: $elm$core$Dict$values(usersList)
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing userlist JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetUser':
					var json = msg.a;
					var _v6 = A2($elm$json$Json$Decode$decodeValue, $author$project$User$decodeUser, json);
					if (_v6.$ === 'Ok') {
						var user = _v6.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									user: $elm$core$Maybe$Just(user)
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing user JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetSpymasters':
					var json = msg.a;
					var _v7 = A2($elm$json$Json$Decode$decodeValue, $author$project$Codenames$decodeSpymasters, json);
					if (_v7.$ === 'Ok') {
						var _v8 = _v7.a;
						var red_sm = _v8.a;
						var blue_sm = _v8.b;
						var users = model.users;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									blue_spymaster: A2($author$project$User$getUserByName, blue_sm, users),
									red_spymaster: A2($author$project$User$getUserByName, red_sm, users)
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing spymasters JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetSpymasterModal':
					var json = msg.a;
					var _v9 = A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$bool, json);
					if (_v9.$ === 'Ok') {
						var val = _v9.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{toggleSpymasterModal: val}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing spymasters JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetBoard':
					var json = msg.a;
					var _v10 = A2($elm$json$Json$Decode$decodeValue, $author$project$Codenames$decodeBoardInfo, json);
					if (_v10.$ === 'Ok') {
						var boardInfo = _v10.a;
						var newcards = A2($elm$core$List$map, $author$project$Codenames$toCard, boardInfo.cards);
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{blueRemaining: boardInfo.blueRemaining, cards: newcards, password: boardInfo.password, redRemaining: boardInfo.redRemaining}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Critical error getting board'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetChat':
					var json = msg.a;
					var _v11 = A2($elm$json$Json$Decode$decodeValue, $author$project$Chat$decodeChatline, json);
					if (_v11.$ === 'Ok') {
						var chatline = _v11.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									chat: A2($elm$core$List$cons, chatline, model.chat)
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing chat JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetStatus':
					var json = msg.a;
					var _v12 = A2($elm$json$Json$Decode$decodeValue, $author$project$Codenames$decodeStatus, json);
					if (_v12.$ === 'Ok') {
						var status = _v12.a;
						var turn = status.turn;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{status: status}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing status JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetFlashMessage':
					var json = msg.a;
					var _v13 = A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, json);
					if (_v13.$ === 'Ok') {
						var message = _v13.a;
						return A2(
							$author$project$Codenames$addToast,
							A2($author$project$Toast$Success, '', message),
							_Utils_Tuple2(
								_Utils_update(
									model,
									{debugString: message}),
								$elm$core$Platform$Cmd$none));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing Flash Message JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'GetPassword':
					var json = msg.a;
					var _v14 = A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, json);
					if (_v14.$ === 'Ok') {
						var password = _v14.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{password: password}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{debugString: 'Error parsing Password JSON'}),
							$elm$core$Platform$Cmd$none);
					}
				case 'AddToastMessage':
					var subMsg = msg.a;
					return A4($author$project$Toast$update, $author$project$Codenames$toastConfig, $author$project$Codenames$AddToastMessage, subMsg, model);
				case 'Tick':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{currentTimer: model.currentTimer + 1}),
						$elm$core$Platform$Cmd$none);
				case 'Ping':
					var newTime = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{currentTimer: model.currentTimer + 1}),
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('ping')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$string('ping'))
										])))));
				case 'GetJSON':
					var json = msg.a;
					var _v15 = A2($elm$json$Json$Decode$decodeValue, $author$project$Codenames$decodeJSON, json);
					if (_v15.$ === 'Ok') {
						var action = _v15.a.action;
						var content = _v15.a.content;
						switch (action) {
							case 'update_scoreboard':
								var $temp$msg = $author$project$Codenames$GetUsersList(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_user':
								var $temp$msg = $author$project$Codenames$GetUser(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_board':
								var $temp$msg = $author$project$Codenames$GetBoard(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_status':
								var $temp$msg = $author$project$Codenames$GetStatus(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_spymasters':
								var $temp$msg = $author$project$Codenames$GetSpymasters(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_spymaster_modal':
								var $temp$msg = $author$project$Codenames$GetSpymasterModal(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'connect_to_server':
								return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
							case 'update_chat':
								var $temp$msg = $author$project$Codenames$GetChat(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'player_chat_new_message':
								var $temp$msg = $author$project$Codenames$GetChat(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'system_chat_new_message':
								var $temp$msg = $author$project$Codenames$GetChat(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'system_chat_to_player_new_message':
								var $temp$msg = $author$project$Codenames$GetChat(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'update_flash_msg':
								var $temp$msg = $author$project$Codenames$GetFlashMessage(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'new_game':
								var $temp$msg = $author$project$Codenames$GetPassword(content),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							case 'ping':
								return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
							default:
								return _Utils_Tuple2(
									A2($elm$core$Debug$log, 'Error: unknown code in JSON message', model),
									$elm$core$Platform$Cmd$none);
						}
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									debugString: 'Bad JSON: ' + A2($elm$json$Json$Encode$encode, 0, json)
								}),
							$elm$core$Platform$Cmd$none);
					}
				case 'SetTeam':
					var team = msg.a;
					return _Utils_Tuple2(
						model,
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('set_team')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$int(team))
													])))
										])))));
				case 'SetSpymaster':
					var team = msg.a;
					return _Utils_Tuple2(
						model,
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('set_spymaster')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$int(team))
													])))
										])))));
				case 'UncoverCard':
					var id = msg.a;
					return _Utils_Tuple2(
						model,
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('uncover_card')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$object(
															_List_fromArray(
																[
																	_Utils_Tuple2(
																	'id',
																	$elm$json$Json$Encode$int(id)),
																	_Utils_Tuple2(
																	'override',
																	$elm$json$Json$Encode$bool(model.settings.override))
																])))
													])))
										])))));
				case 'SubmitClue':
					var guesses = model.guessesInProgress;
					var clue = model.clueInProgress;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{clueInProgress: '', guessesInProgress: ''}),
						$author$project$Codenames$outputPort(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'action',
											$elm$json$Json$Encode$string('game_action')),
											_Utils_Tuple2(
											'content',
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'action',
														$elm$json$Json$Encode$string('submit_clue')),
														_Utils_Tuple2(
														'content',
														$elm$json$Json$Encode$object(
															_List_fromArray(
																[
																	_Utils_Tuple2(
																	'clue',
																	$elm$json$Json$Encode$string(clue)),
																	_Utils_Tuple2(
																	'guesses',
																	$elm$json$Json$Encode$string(guesses))
																])))
													])))
										])))));
				case 'SetClue':
					var word = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{clueInProgress: word}),
						$elm$core$Platform$Cmd$none);
				case 'SetGuesses':
					var guesses = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{guessesInProgress: guesses}),
						$elm$core$Platform$Cmd$none);
				default:
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Codenames$BlockKeyShortcuts = function (a) {
	return {$: 'BlockKeyShortcuts', a: a};
};
var $author$project$Codenames$CancelCustomWords = {$: 'CancelCustomWords'};
var $author$project$Codenames$NewGame = {$: 'NewGame'};
var $author$project$Codenames$SaveCustomWords = {$: 'SaveCustomWords'};
var $author$project$Codenames$SetCustomWords = function (a) {
	return {$: 'SetCustomWords', a: a};
};
var $author$project$Codenames$ToggleCustomWords = {$: 'ToggleCustomWords'};
var $author$project$Codenames$ToggleCustomWordsEntry = {$: 'ToggleCustomWordsEntry'};
var $author$project$Codenames$ToggleLightbox = {$: 'ToggleLightbox'};
var $author$project$Codenames$ToggleOverride = {$: 'ToggleOverride'};
var $author$project$Codenames$ToggleSoundEffects = {$: 'ToggleSoundEffects'};
var $author$project$Codenames$ToggleSpies = {$: 'ToggleSpies'};
var $author$project$Codenames$ToggleSpymasterModal = {$: 'ToggleSpymasterModal'};
var $author$project$Codenames$ToggleTeamModal = {$: 'ToggleTeamModal'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Toast$genericToast = F3(
	function (variantClass, title, message) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('toasty-container'),
					$elm$html$Html$Attributes$class(variantClass)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h1,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('toasty-title')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					$elm$core$String$isEmpty(message) ? $elm$html$Html$text('') : A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('toasty-message')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(message)
						]))
				]));
	});
var $author$project$Toast$defaultView = function (toast) {
	switch (toast.$) {
		case 'Success':
			var title = toast.a;
			var message = toast.b;
			return A3($author$project$Toast$genericToast, 'toasty-success', title, message);
		case 'Warning':
			var title = toast.a;
			var message = toast.b;
			return A3($author$project$Toast$genericToast, 'toasty-warning', title, message);
		default:
			var title = toast.a;
			var message = toast.b;
			return A3($author$project$Toast$genericToast, 'toasty-error', title, message);
	}
};
var $author$project$Codenames$UncoverCard = function (a) {
	return {$: 'UncoverCard', a: a};
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Codenames$drawCard = F2(
	function (index, cards) {
		if (cards.b) {
			var c = cards.a;
			var cs = cards.b;
			return A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							'card_border team-' + ($elm$core$String$fromInt(
								function ($) {
									return $.team;
								}(c)) + (function ($) {
								return $.uncovered;
							}(c) ? ' uncovered' : ' covered'))),
							$elm$html$Html$Attributes$id(
							'c' + $elm$core$String$fromInt(index)),
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$UncoverCard(index))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card_top')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('spy')
												]),
											_List_Nil)
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('wordbox')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													function ($) {
														return $.word;
													}(c))
												]))
										]))
								]))
						])),
				A2($author$project$Codenames$drawCard, index + 1, cs));
		} else {
			return _List_Nil;
		}
	});
var $author$project$Codenames$ToggleWordlist = function (a) {
	return {$: 'ToggleWordlist', a: a};
};
var $elm$html$Html$li = _VirtualDom_node('li');
var $author$project$Codenames$drawWordlistToggle = function (wordlist) {
	return A2(
		$elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(''),
						$elm$html$Html$Events$onClick(
						$author$project$Codenames$ToggleWordlist(wordlist.key))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(
								'icon ' + (wordlist.include ? 'checked' : 'unchecked'))
							]),
						_List_Nil),
						$elm$html$Html$text('Use ' + wordlist.name)
					]))
			]));
};
var $author$project$Codenames$isSpymaster = F2(
	function (user, spymaster) {
		if (user.$ === 'Nothing') {
			return false;
		} else {
			var u = user.a;
			if (spymaster.$ === 'Nothing') {
				return false;
			} else {
				var s = spymaster.a;
				return _Utils_eq(s.nickname, u.nickname);
			}
		}
	});
var $elm$html$Html$blockquote = _VirtualDom_node('blockquote');
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $author$project$Codenames$lightboxInfo = function (password) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('instructions')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('A team game for 4+ people')
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Divide into two teams and select one player from each team to be the Spymaster. The Spymaster has access to the decrypted board showing which words belong to the team. Each turn, they will provide one word that is not on any card, along with one number, to connect as many words as possible for the team to guess. The number (plus one) determines the maximum number of words that team may guess this turn.')
								])),
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Each guess is completed by selecting a card, revealing to which team it belongs. If a team selects a word that does not belong to their team, their turn is over. At any time, a team can pass, and the other team\'s Spymaster begins their turn. When all of one team\'s words are found, the team wins. If the *Assassin* is selected, the team loses immediately.')
								]))
						]))
				])),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('right-side')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('./spymaster/index.html?passphrase=' + password),
							$elm$html$Html$Attributes$target('_blank'),
							$elm$html$Html$Attributes$class('spymaster')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('spymaster-preview')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Click here to access the Decryptor. ')
										])),
									A2($elm$html$Html$br, _List_Nil, _List_Nil),
									$elm$html$Html$text('Paste in the password (' + (password + ') to find the correct board.'))
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('legal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Legal & copyright')
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('This program is free software: you can redistribute it or modify it under the GNU General Public License, version 3+. The source code is available on Github. Enjoy!'),
									A2(
									$elm$html$Html$blockquote,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Copyright does not protect the idea for a game, its name or title, or the method or methods for playing it. Nor does copyright protect any idea, system, method, device, or trademark ma­terial involved in developing, merchandising, or playing a game.')
										]))
								]))
						]))
				]))
		]);
};
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Codenames$SetSpymaster = function (a) {
	return {$: 'SetSpymaster', a: a};
};
var $author$project$Codenames$SetTeam = function (a) {
	return {$: 'SetTeam', a: a};
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $author$project$Codenames$modalSpectators = F3(
	function (red_user, blue_user, all_users) {
		var spectators = A2(
			$elm$core$List$filter,
			function (x) {
				return (x.team !== 1) && (x.team !== 2);
			},
			all_users);
		return A2(
			$elm$core$String$join,
			', ',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.nickname;
				},
				spectators));
	});
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Codenames$modalUser = F5(
	function (user, spymaster, color, teamid, users) {
		var team_text = $elm$core$List$isEmpty(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.nickname;
				},
				A2(
					$elm$core$List$filter,
					function (x) {
						return _Utils_eq(x.team, teamid);
					},
					users))) ? 'No members' : A2(
			$elm$core$String$join,
			', ',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.nickname;
				},
				A2(
					$elm$core$List$filter,
					function (x) {
						return _Utils_eq(x.team, teamid);
					},
					users)));
		var spymaster_html = function () {
			if (spymaster.$ === 'Nothing') {
				return $elm$html$Html$text('No spymaster');
			} else {
				var u = spymaster.a;
				return $elm$html$Html$text(u.nickname);
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('modal_' + color)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pad')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h3,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(color + ' team')
								])),
							A2(
							$elm$html$Html$h4,
							_List_Nil,
							_List_fromArray(
								[spymaster_html])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(team_text)
								]))
						]))
				]));
	});
var $author$project$Codenames$showModal = F4(
	function (user, red_sm, blue_sm, users) {
		var spectator_button = function () {
			if (user.$ === 'Nothing') {
				return $elm$html$Html$text('');
			} else {
				var u = user.a;
				return ((u.team !== 1) && (u.team !== 2)) ? $elm$html$Html$text('') : A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$SetTeam(0))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Become spectator')
						]));
			}
		}();
		var red_button = function () {
			if (user.$ === 'Nothing') {
				return $elm$html$Html$text('');
			} else {
				var u = user.a;
				return (u.team === 1) ? (_Utils_eq(user, red_sm) ? $elm$html$Html$text('') : A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$SetSpymaster(1))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Become red spymaster')
						]))) : A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$SetTeam(1))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Join red team')
						]));
			}
		}();
		var blue_button = function () {
			if (user.$ === 'Nothing') {
				return $elm$html$Html$text('');
			} else {
				var u = user.a;
				return (u.team === 2) ? (_Utils_eq(user, blue_sm) ? $elm$html$Html$text('') : A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$SetSpymaster(1))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Become blue spymaster')
						]))) : A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Codenames$SetTeam(2))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Join blue team')
						]));
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('lightbox')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('modal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flex_row')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('flex_fill')
										]),
									_List_Nil),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('close_button'),
											$elm$html$Html$Events$onClick($author$project$Codenames$ToggleTeamModal)
										]),
									_List_Nil)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flex_container')
								]),
							_List_fromArray(
								[
									A5($author$project$Codenames$modalUser, user, red_sm, 'red', 1, users),
									A5($author$project$Codenames$modalUser, user, blue_sm, 'blue', 2, users),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('modal_spectators')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h3,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Spectators')
												])),
											$elm$html$Html$text(
											A3($author$project$Codenames$modalSpectators, red_sm, blue_sm, users))
										])),
									A2(
									$elm$html$Html$div,
									_List_Nil,
									_List_fromArray(
										[spectator_button, red_button, blue_button]))
								]))
						]))
				]));
	});
var $author$project$Codenames$SetClue = function (a) {
	return {$: 'SetClue', a: a};
};
var $author$project$Codenames$SetGuesses = function (a) {
	return {$: 'SetGuesses', a: a};
};
var $author$project$Codenames$SubmitClue = {$: 'SubmitClue'};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Codenames$spymasterModal = F5(
	function (user, red_sm, blue_sm, clueInProgress, password) {
		var is_red_sm = A2($author$project$Codenames$isSpymaster, user, red_sm);
		var is_blue_sm = A2($author$project$Codenames$isSpymaster, user, blue_sm);
		var header_msg = is_red_sm ? _List_fromArray(
			[
				$elm$html$Html$text('You are the red spymaster. '),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('./spymaster/index.html?passphrase=' + password),
						$elm$html$Html$Attributes$target('_blank')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Open Decryptor')
					]))
			]) : (is_blue_sm ? _List_fromArray(
			[
				$elm$html$Html$text('You are the blue spymaster. '),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('./spymaster/index.html?passphrase=' + password),
						$elm$html$Html$Attributes$target('_blank')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Open Decryptor')
					]))
			]) : _List_fromArray(
			[
				$elm$html$Html$text('You are not a spymaster!')
			]));
		var clue_html = (is_red_sm || is_blue_sm) ? _List_fromArray(
			[
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Clue')
					])),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('text'),
						$elm$html$Html$Events$onInput($author$project$Codenames$SetClue),
						$elm$html$Html$Events$onFocus(
						$author$project$Codenames$BlockKeyShortcuts(true)),
						$elm$html$Html$Events$onBlur(
						$author$project$Codenames$BlockKeyShortcuts(false)),
						$elm$html$Html$Attributes$placeholder('Enter a clue'),
						$elm$html$Html$Attributes$value(clueInProgress)
					]),
				_List_Nil),
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Guesses')
					])),
				A2(
				$elm$html$Html$select,
				_List_fromArray(
					[
						$elm$html$Html$Events$onInput($author$project$Codenames$SetGuesses)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value(''),
								$elm$html$Html$Attributes$disabled(true),
								$elm$html$Html$Attributes$selected(true)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Select a value')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('0')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('0')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('1')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('1')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('2')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('2')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('3')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('3')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('4')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('4')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('5')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('5')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('6')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('6')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('7')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('7')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('8')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('8')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('9')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('9 ')
							])),
						A2(
						$elm$html$Html$option,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value('infinity')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('∞')
							]))
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Codenames$SubmitClue)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Submit clue')
					]))
			]) : _List_Nil;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('lightbox')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('modal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flex_row')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('flex_fill')
										]),
									_List_fromArray(
										[
											A2($elm$html$Html$h3, _List_Nil, header_msg)
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('close_button'),
											$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSpymasterModal)
										]),
									_List_Nil)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flex_container')
								]),
							clue_html)
						]))
				]));
	});
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Toast$itemContainer = F4(
	function (_v0, tagger, _v1, toastView) {
		var cfg = _v0.a;
		var id = _v1.a;
		var status = _v1.b;
		var toast = _v1.c;
		var attrs = function () {
			if (status.$ === 'Entered') {
				return cfg.transitionInAttrs;
			} else {
				return cfg.transitionOutAttrs;
			}
		}();
		return _Utils_Tuple2(
			$elm$core$String$fromInt(id),
			A2(
				$elm$html$Html$li,
				_Utils_ap(
					cfg.itemAttrs,
					_Utils_ap(
						attrs,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								tagger(
									$author$project$Toast$TransitionOut(id)))
							]))),
				_List_fromArray(
					[
						toastView(toast)
					])));
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$html$Html$Keyed$ol = $elm$html$Html$Keyed$node('ol');
var $author$project$Toast$view = F4(
	function (cfg, toastView, tagger, _v0) {
		var toasts = _v0.a;
		var seed = _v0.b;
		var _v1 = cfg;
		var c = _v1.a;
		return $elm$core$List$isEmpty(toasts) ? $elm$html$Html$text('') : A2(
			$elm$html$Html$Keyed$ol,
			c.containerAttrs,
			A2(
				$elm$core$List$map,
				function (toast) {
					return A4($author$project$Toast$itemContainer, cfg, tagger, toast, toastView);
				},
				toasts));
	});
var $author$project$Codenames$view = function (model) {
	var wordlistToggles = A2($elm$core$List$map, $author$project$Codenames$drawWordlistToggle, model.wordlists);
	var right_turn_text_bottom = model.status.game_over ? '' : (((model.status.remaining_guesses === 1) ? '1 guess' : ($elm$core$String$fromInt(model.status.remaining_guesses) + ' guesses')) + ' remaining');
	var left_turn_text_bottom = model.status.game_over ? '' : 'Click here or press space to pass';
	var left_turn_text = model.status.game_over ? 'Select New Game to play again' : ((model.status.turn ? 'Red' : 'Blue') + ' team\'s turn');
	var addCards = function (cards) {
		return A2($author$project$Codenames$drawCard, 0, cards);
	};
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'lightbox' + (model.toggleLightbox ? ' show' : ' hidden')),
						$elm$html$Html$Events$onClick($author$project$Codenames$ToggleLightbox)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						$author$project$Codenames$lightboxInfo(model.password))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'lightbox' + (model.toggleQR ? ' show' : ' hidden')),
						$elm$html$Html$Events$onClick($author$project$Codenames$ToggleQR)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('qrcode')
							]),
						_List_Nil)
					])),
				model.toggleTeamModal ? A4($author$project$Codenames$showModal, model.user, model.red_spymaster, model.blue_spymaster, model.users) : $elm$html$Html$text(''),
				model.toggleSpymasterModal ? A5($author$project$Codenames$spymasterModal, model.user, model.red_spymaster, model.blue_spymaster, model.clueInProgress, model.password) : $elm$html$Html$text(''),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('debug')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('top')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('top_message')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.topMessage)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('toast_container')
							]),
						_List_fromArray(
							[
								A4($author$project$Toast$view, $author$project$Codenames$toastConfig, $author$project$Toast$defaultView, $author$project$Codenames$AddToastMessage, model.toastMessages)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'sidebar' + (model.toggleSidebar ? '' : ' hidden'))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$ul,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class(''),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSpies)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class(
														'icon ' + (model.settings.spies ? 'checked' : 'unchecked'))
													]),
												_List_Nil),
												$elm$html$Html$text('Show spies')
											]))
									])),
								A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class(''),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleOverride)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class(
														'icon ' + (model.settings.override ? 'checked' : 'unchecked'))
													]),
												_List_Nil),
												$elm$html$Html$text('Bypass turn control')
											]))
									])),
								A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('disabled'),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSoundEffects)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class(
														'icon ' + (model.toggleSoundEffects ? 'checked' : 'unchecked'))
													]),
												_List_Nil),
												$elm$html$Html$text('Enable sound effects')
											]))
									]))
							])),
						A2(
						$elm$html$Html$ul,
						_List_Nil,
						A2(
							$elm$core$List$append,
							wordlistToggles,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$li,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class(''),
													$elm$html$Html$Events$onClick($author$project$Codenames$ToggleCustomWords)
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$span,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class(
															'icon ' + (model.settings.customWords ? 'checked' : 'unchecked'))
														]),
													_List_Nil),
													$elm$html$Html$text('Use custom words')
												]))
										]))
								]))),
						A2(
						$elm$html$Html$ul,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class(''),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleCustomWordsEntry)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('icon edit')
													]),
												_List_Nil),
												$elm$html$Html$text('Edit custom wordlist')
											]))
									])),
								A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class(''),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSidebar)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('icon close')
													]),
												_List_Nil),
												$elm$html$Html$text('Close settings')
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'customWordsBar' + (model.toggleCustomWordsEntry ? '' : ' hidden'))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('textarea_area')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$id('custom_words_entry'),
										$elm$html$Html$Attributes$placeholder('Enter one word per line'),
										$elm$html$Html$Events$onInput($author$project$Codenames$SetCustomWords),
										$elm$html$Html$Attributes$value(model.customWordsString),
										$elm$html$Html$Events$onFocus(
										$author$project$Codenames$BlockKeyShortcuts(true)),
										$elm$html$Html$Events$onBlur(
										$author$project$Codenames$BlockKeyShortcuts(false))
									]),
								_List_Nil)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('button_area')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Codenames$SaveCustomWords)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Save')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Codenames$CancelCustomWords),
										$elm$html$Html$Attributes$class('button--cancel')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Cancel')
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('center')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$main_,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(
								_Utils_ap(
									model.status.turn ? 'red-turn' : 'blue-turn',
									model.settings.spies ? '' : ' hide_spies'))
							]),
						addCards(model.cards)),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('bottom')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_left bottom_no_stretch')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('settings_button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSidebar)
											]),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_left')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('turn_text button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$PassTurn)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(left_turn_text),
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('bottom_span')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(left_turn_text_bottom)
													]))
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_left cards_remaining')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('red_remaining')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$elm$core$String$fromInt(model.redRemaining) + ' remaining')
											])),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('blue_remaining')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$elm$core$String$fromInt(model.blueRemaining) + ' remaining')
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_left turn_info')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('turn_text button')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(model.status.text),
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('bottom_span')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(right_turn_text_bottom)
													]))
											]))
									])),
								(A2($author$project$Codenames$isSpymaster, model.user, model.red_spymaster) || A2($author$project$Codenames$isSpymaster, model.user, model.blue_spymaster)) ? A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_right bottom_no_stretch')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('spymaster_button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleSpymasterModal)
											]),
										_List_Nil)
									])) : $elm$html$Html$text(''),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_right bottom_no_stretch')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('new_game_button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$NewGame)
											]),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_right bottom_no_stretch')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('info_button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleLightbox)
											]),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bottom_right bottom_no_stretch')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('teams_button'),
												$elm$html$Html$Events$onClick($author$project$Codenames$ToggleTeamModal)
											]),
										_List_Nil)
									]))
							]))
					]))
			]));
};
var $author$project$Codenames$main = $elm$browser$Browser$element(
	{init: $author$project$Codenames$init, subscriptions: $author$project$Codenames$subscriptions, update: $author$project$Codenames$update, view: $author$project$Codenames$view});
_Platform_export({'Codenames':{'init':$author$project$Codenames$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));