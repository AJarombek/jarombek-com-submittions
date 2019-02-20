package defaultmethods;

/**
 * @author Andrew Jarombek
 * @since 1/16/2018
 */
public class Cat implements LivingAnimal, Pet {

    private String name;
    private String owner;

    public Cat(String name) {
        this.name = name;
    }

    public Cat(String name, String owner) {
        this.name = name;
        this.owner = owner;
    }

    @Override
    public String aboutMe() {
        return "I am a cat named " + name;
    }

    @Override
    public String owner() {
        if (owner == null) {
            return "I have no owner!";
        } else {
            return "My owner is " + owner;
        }
    }

    @Override
    public void run() {
        System.out.println(name + " ran!");
    }

    @Override
    public void walk() {
        System.out.println(name + " walked!");
    }

    @Override
    public void sleep() {
        System.out.println(name + " slept!");
    }

    @Override
    public void eat() {
        System.out.println(name + " ate!");
    }

    @Override
    public String status() {
        return Pet.super.status();
    }
}
