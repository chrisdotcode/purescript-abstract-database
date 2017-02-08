'use strict';

exports.toISOString = function toISOString(date) {
	return date.toISOString();
};

function push(el, arr) {
	arr.push(el);
	return arr;
}

exports.objFold = function objFold(writeProp) {
	return function (obj) {
		return Object.keys.reduce(function(o, key) {
			if (!obj.hasOwnProperty(key)) { return; }

			return push(writeProp(key, obj[key]), o);
		}, []);
	};
};

exports.toString = function toString(obj) {
	return obj.toString();
};

exports.parseISODateTimeString = function parseISODateTimeString(Left) {
	return function(Right) {
	return function(datetimeString) {
		try {
			var parse = Date.parse(datetimeString);

			if (parse !== parse /* NaN */) {
				throw new Error("The given datetime string was not in the expected ISO8601 format.");
			}

			return Right(new Date(parse));
		} catch (err) {
			return Left(err.message);
		}
	}};
};
