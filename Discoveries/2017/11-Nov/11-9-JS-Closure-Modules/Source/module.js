// Author: Andrew Jarombek
// Date: 11/10/2017
// Show the revealing module pattern and demonstrate closure

var taylorSwiftApi = function() {

  var lyric = {
    sparks: "I see sparks fly whenever you smile",
    mine: "You are the best thing thats ever been mine",
    dec: "I go back to Decemeber all the time",
    other: "I'm sorry, taylor can't pick up the phone right now"
  }
 
  function lyrics(song) {
    switch(song.toLowerCase()) {
        case "sparks fly":
        return lyric.sparks;
        case "mine":
        return lyric.mine;
        case "back to december":
        return lyric.dec;
        default:
        return lyric.other;
    }
  }
  
  return {
    lyrics: lyrics
  }
}();

console.info(taylorSwiftApi); // {lyrics: function}
console.info(taylorSwiftApi.lyric); // undefined

// Thanks to closure over the lexical scope in taylorSwiftApi, this incorrect 
// lyric.sparks will not be used when calling taylorSwiftApi.lyrics()
var lyric = {
  sparks: "I was enchanted to meet you"
};

let result = taylorSwiftApi.lyrics("Sparks Fly");
console.info(result); // I see sparks fly whenever you smile