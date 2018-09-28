/*

*/

var _FNV_prime = 16777619;

function _FNV_hash(current, object) {
  switch (typeof object) {
  case 'string': return _FNV_hashString(current, object);
  case 'number': return _FNV_hashNum(current, object);
  case 'boolean': return object ? _FNV_hashNum(current, 1) : _FNV_hashNum(current, 0);
  case 'object': return _FNV_hashObj(current, object);
  default: return 0;
  }
}

function _FNV_hashString(current, str) {
  for (var i = 0, len = str.length; i < len; i++) {
    current = (current ^ str.charCodeAt(i)) * _FNV_prime;
  }

  return current >>> 0;
}

function _FNV_hashNum(current, num) {
  return ((current ^ num) * _FNV_prime) >>> 0;
}

function _FNV_hashObj(current, obj) {
  for (var key in obj) {
    current = _FNV_hash(current, obj[key]);
  }

  return current >>> 0;
}
