async(function *tasks() {
    yield httpGet("www.example.com/search/1");
    yield httpGet("www.example.com/search/2");
    yield httpGet("www.example.com/search/3");
})

function async(generator) {
    var iterator = generator();
    await(iterator.next())

    function await(promise) {
        promise.then(function() {
            await(iterator.next());
        })
    }
}