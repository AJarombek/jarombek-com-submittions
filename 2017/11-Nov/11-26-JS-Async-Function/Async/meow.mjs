// @author: Andrew Jarombek
// @since: 11/26/2017

import * as http from './meowHttp';

(async function search() {
    let catResult = await http.search();
    print(catResult);

    let meowResult = await http.search('meow');
    print(meowResult);
})();

function print(result) {
    for (let item of result.items) {
        console.info(item.title);
    }
}
