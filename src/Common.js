"use strict";

var CryptoJS = require('crypto-js');

exports.hmacSha3 = function(msg, key) {
	var hmac  = CryptoJS.HmacSHA3(msg, key);
	//console.log("bla");
	//console.log(hmac.toString());
	//console.log(CryptoJS.enc.Utf8.stringify(hmac));
	return hmac.toString();
}

exports.setLocation = function(newLoc) {
	window.location = newLoc;
	return null;
}

exports.getAttr = function(attr) {
    return function(ob) {
        return function() {
            return ob.attr(attr);
        };
    };
};

exports.toHex = function(str) {
	var repr = parseInt(str, 16);
	return repr.toString(16);
}

exports.fromHex = function(str) {
	return CryptoJS.enc.Hex.parse(str);
}

exports.fromUtf8 = function(str) {
	return CryptoJS.enc.Utf8.parse(str);
}
