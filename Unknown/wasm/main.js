/**
 * Main file for testing Web Assembly files
 * @author Andrew Jarombek
 * @since 12/31/2018
 */

const {execWat} = require('./wat-wasm');

// Basic addition operation in Web Assembly
execWat("wa/test.wat", (instance) => console.info(instance.exports.add(2, 2)));

execWat("wa/basics.wat", (instance) => console.info(instance.exports.calc_pace(2, 12, 31)));