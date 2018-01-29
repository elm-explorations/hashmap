var _Skinney$elm_hashmap_exploration$Native_FNV = function() {
    var fnvPrime = Math.pow(2, 24) + Math.pow(2, 8) + 0x93;

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
	var current = 0;

	for (var i = 0, len = str.length; i < len; i++) {
	    current = (current ^ str.charCodeAt(i)) * fnvPrime;
	}

	return current | 0;
    }

    function hashNum(num) {
	return num * fnvPrime;
    }

    function hashObj(obj) {
	var keys = [];
	var current = 0;

	for (var key in obj) {
	    keys.push(key);
	}

	keys.sort();

	for (var idx = 0, len = keys.length; idx < len; idx++) {
	    current = (current ^ hash(obj[keys[idx]])) * fnvPrime;
	}

	return current | 0;
    }

return {
    hash: hash
};

}();
