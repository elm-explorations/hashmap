var _Skinney$elm_hashmap_exploration$Native_FNV = function() {
    var fnvPrime = 16777619;

    function hash(current, object) {
	switch (typeof object) {
	case 'string': return hashString(current, object);
	case 'number': return hashNum(current, object);
	case 'boolean': return object ? hashNum(current, 1) : hashNum(current, 0);
	case 'object': return hashObj(current, object);
	default: return 0;
	}
    }

    function hashString(current, str) {
	for (var i = 0, len = str.length; i < len; i++) {
	    current = (current ^ str.charCodeAt(i)) * fnvPrime;
	}

	return current >>> 0;
    }

    function hashNum(current, num) {
	return ((current ^ num) * fnvPrime) >>> 0;
    }

    function hashObj(current, obj) {
	for (var key in obj) {
	    current = hash(current, obj[key]);
	}

	return current >>> 0;
    }

return {
    hash: F2(hash)
};

}();
