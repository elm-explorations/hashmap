var _Skinney$elm_hashmap_exploration$Native_FNV = function() {
    var fnvPrime = 16777619;
    var fnvOffset = 2166136261;

    function hash(object) {
	switch (typeof object) {
	case 'string': return hashString(object);
	case 'number': return hashNum(object);
	case 'boolean': return object ? hashNum(1) : hashNum(0);
	case 'object': return hashObj(object);
	default: return 0;
	}
    }

    function hashString(str) {
	var current = fnvOffset;

	for (var i = 0, len = str.length; i < len; i++) {
	    current = (current ^ str.charCodeAt(i)) * fnvPrime;
	}

	return current >>> 0;
    }

    function hashNum(num) {
	return ((fnvOffset ^ num) * fnvPrime) >>> 0;
    }

    function hashObj(obj) {
	var current = fnvOffset;

	for (var key in obj) {
	    current = (current ^ hash(obj[key])) * fnvPrime;
	}

	return current >>> 0;
    }

return {
    hash: hash
};

}();
