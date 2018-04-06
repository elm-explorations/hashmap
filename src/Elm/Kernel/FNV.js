/*

*/

var _FNV_prime = 16777619;
var _FNV_offset = 2166136261;

function _FNV_hash(object) {
    switch (typeof object) {
    case 'string': return _FNV_hashString(object);
    case 'number': return _FNV_hashNum(object);
    case 'boolean': return object ? _FNV_hashNum(1) : _FNV_hashNum(0);
    case 'object': return _FNV_hashObj(object);
    default: return 0;
    }
}

function _FNV_hashString(str) {
    var current = _FNV_offset;

    for (var i = 0, len = str.length; i < len; i++) {
	current = (current ^ str.charCodeAt(i)) * _FNV_prime;
    }

    return current >>> 0;
}

function _FNV_hashNum(num) {
    return ((_FNV_offset ^ num) * _FNV_prime) >>> 0;
}

function _FNV_hashObj(obj) {
    var current = _FNV_offset;

    for (var key in obj) {
	current = (current ^ _FNV_hash(obj[key])) * _FNV_prime;
    }

    return current >>> 0;
}

