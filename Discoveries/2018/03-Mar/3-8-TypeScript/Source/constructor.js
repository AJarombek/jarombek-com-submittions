/**
 * The JavaScript version of the User class.  Note that in this class the instance variables are not private,
 * added complexity would be needed to make them private
 * @author Andrew Jarombek
 * @since 3/8/2018
 */

class User {
	
  constructor(username, first, last) {
  	this.username = username;
    this.first = first;
    this.last = last;
  }
  
  getUsername() {
  	return this.username;
  }
}

const user = new User('andy', 'Andrew', 'Jarombek');
console.info(user.getUsername());