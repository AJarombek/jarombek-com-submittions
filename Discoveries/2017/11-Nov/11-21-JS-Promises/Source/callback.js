// Author: Andrew Jarombek
// Date: 11/20/2017
// Calling the google meow API with a simple http request with callback

var httpRequest = new XMLHttpRequest();

httpRequest.open('GET', 'https://www.googleapis.com/customsearch/v1?key=AIzaSyA2QIPJoGYMx_DuQH6wDqNG3AHXG7bcb94&cx=005720647135116730303:chkg-pnyocg&q=meow&prettyPrint=false', true);
httpRequest.send();

httpRequest.onreadystatechange = function() {
  
  if (httpRequest.status === 200 && httpRequest.readyState === 4) {
    var json = JSON.parse(httpRequest.response);
    
    for (let item of json.items) {
    	console.info(item.title);
    }
  }
}