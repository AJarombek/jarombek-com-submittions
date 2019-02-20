/**
 * Look at cool features available with TypeScript
 * @author Andrew Jarombek
 * @since 3/8/2018
 */

class User {

    // Access modifiers (public, private, protected in TS) on constructor variables automatically 
    // make them instance variables
    constructor(private username: string, private first: string, private last: string) { }

    public getUsername() {

        // Access the automatically created instance variable
        return this.username;
    }
}

const newUser: User = new User("andy", "Andrew", "Jarombek");
console.info(newUser.getUsername()); // andy