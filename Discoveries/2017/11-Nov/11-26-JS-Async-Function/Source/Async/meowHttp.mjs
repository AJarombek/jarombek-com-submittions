// @author: Andrew Jarombek
// @since: 11/26/2017
// Create an API for the meow google search.  This will hold all the promise code.
// The .mjs extension tells node that this file is a module.

import {default as https} from 'https';

export function search(query = 'cat') {

    const url = 'https://www.googleapis.com/customsearch/v1?key=AIzaSyA2QIPJoGYMx_DuQH6wDqNG3AHXG7bcb94' + 		
                '&cx=005720647135116730303:chkg-pnyocg&q=' + query + '&prettyPrint=false';

    //console.info(https);
    return new Promise(function (resolve, reject) {

        https.get(url, res => {
            res.setEncoding('utf-8');
            let response = '';

            res.on('data', data => {
                response += data;
            });

            res.on('end', () => {
                resolve(JSON.parse(response));
            });
        });
    });
}