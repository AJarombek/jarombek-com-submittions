"use strict";

var _taylorSwift = require("./taylorSwift");

var ts = _interopRequireWildcard(_taylorSwift);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

console.info(ts.lyrics("Sparks Fly")); // Author: Andrew Jarombek
// Date: 11/10/2017
// Import the taylor swift module and call its function

// Alternative: import {lyrics} from './taylorSwift';
