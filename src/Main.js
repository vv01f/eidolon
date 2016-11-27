"use strict";

var CryptoJS = require('crypto-js');

exports.hmacSha3 = function(msg, key) {
	var wamsg = CryptoJS.enc.Utf8.parse(msg);
	var wakey = CryptoJS.enc.Utf8.parse(key);
	var hmac  = CryptoJS.HmacSHA3(wamsg, wakey);
	return hmac.toString();
}
