// Author: Andrew Jarombek
// Date: 11/14/2017
// Look at the global object in Node.js

// In node 'this' will relate to the current module.  
// In this case the module is empty so it will return {}
console.info(this); 

/* In Node this code runs as: */

(function (exports, require, module, __filename, __dirname) {
    console.info(this);
});

// The global object in node is called 'global' and has propeties about the current architecute you can look at 
// The following results are from my PC 
global.process.arch; // x64
global.process.platform; // win32
global.process.env.OS; // Windows_NT
global.process.env.PROCESSOR_ARCHITECTURE; // AMD64
global.process.env.USERNAME; // Andy
global.process.pid; // 15088