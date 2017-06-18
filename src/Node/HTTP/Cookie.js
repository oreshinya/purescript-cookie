"use strict";

exports.responseCookies = function(res) {
  return res.getHeaders()['set-cookie'] || [];
}
