// Author: Andrew Jarombek
// Date: 11/20/2017
// Calling the google meow API with a promise

function httpMeow() {
    return new Promise(function (resolve, reject) {
        var httpRequest = new XMLHttpRequest();
    
        httpRequest.open('GET', 'https://www.googleapis.com/customsearch/v1?key=AIzaSyA2QIPJoGYMx_DuQH6wDqNG3AHXG7bcb94' + 		
                        '&cx=005720647135116730303:chkg-pnyocg&q=meow&prettyPrint=false', true);
        httpRequest.onload = function success() {
            if (httpRequest.status === 200) {
            resolve(httpRequest.response);
            } else {
            reject("no meow was heard :(");
            }
        };
        httpRequest.onerror = function error() {
            reject("no meow was heard :(");
        };
        httpRequest.send();
    });
}
  
httpMeow().then(function(response) {
    var json = JSON.parse(response);
    
    for (let item of json.items) {
    console.info(item.title);
    }
}, function(err) {
    console.info(err);
});