// @author: Andrew Jarombek
// @since: 11/26/2017
// Create an API for the meow google search.  This will hold all the promise code

export function search(query = 'cat') {
    return new Promise(function (resolve, reject) {
        var httpRequest = new XMLHttpRequest();
    
        httpRequest.open('GET', 'https://www.googleapis.com/customsearch/v1?key=AIzaSyA2QIPJoGYMx_DuQH6wDqNG3AHXG7bcb94' + 		
                        '&cx=005720647135116730303:chkg-pnyocg&q=' + query + '&prettyPrint=false', true);
        httpRequest.onload = function success() {
            if (httpRequest.status === 200) {
            resolve(JSON.parse(httpRequest.response));
            } else {
            reject({items: [{title: "no meow was heard :("}]});
            }
        };
        httpRequest.onerror = function error() {
            reject({items: [{title: "no meow was heard :("}]});
        };
        httpRequest.send();
    });
}