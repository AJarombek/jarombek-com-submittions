// Author: Andrew Jarombek
// Date: 11/14/2017
// Look at the global object (window) in the browser

console.info(this);

console.info(window);

this.prompt("What is your name?");

// open() creates (and returns) a new window and document.write() creates a
// new HTML blueprint for the DOM
var newWindow = window.open();
newWindow.document.write("<p>Hello</p>");

// When you click on the new window, call close() which closes the window
// window object has click listener variables that you can assign functions 
// to.  onclick() is called when the user clicks on the window.
newWindow.onclick = function click() {
newWindow.close();
};