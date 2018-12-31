/**
 * Run the Web Assembly function
 * @author Andrew Jarombek
 * @since 12/30/2018
 */

WebAssembly.instantiateStreaming(fetch('test.wasm'))
    .then(wasm => {
        console.info(wasm.instance.exports.add(2, 2));
    });
