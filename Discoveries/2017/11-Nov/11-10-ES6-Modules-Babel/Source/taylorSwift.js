// Author: Andrew Jarombek
// Date: 10/10/2017
// Create an API using ES6 modules

export function lyrics(song) {
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

var lyric = {
  sparks: "I see sparks fly whenever you smile",
  mine: "You are the best thing thats ever been mine",
  dec: "I go back to Decemeber all the time",
  other: "I'm sorry, taylor can't pick up the phone right now"
}