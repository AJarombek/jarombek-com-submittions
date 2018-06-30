package reflection;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;

/**
 * Some basics of the Reflection API
 * @author Andrew Jarombek
 * @since 6/30/2018
 */
public class Reflect {

    /**
     * Perform some basic inspections on an object through reflection techniques
     * @param object an object to inspect
     */
    private static void inspect(Object object) {

        Class objectClass = object.getClass();

        String modifier = Modifier.toString(objectClass.getModifiers());
        System.out.println(modifier);

        Class superClass = objectClass.getSuperclass();
        System.out.println(superClass.toString());

        Optional<Method> method = getMethod(objectClass, "length", null);

        method.ifPresent(m -> System.out.println(m.toString()));
    }

    /**
     * Attempt to construct a new object reflectively with just the {@code Class} object
     * @param objectClass a class that we wish to construct an instance of
     * @return an {code Optional} with a new instance of a class if the construction is successful.
     * Otherwise an empty {@code Optional} is returned.
     */
    private static <T> Optional<T> constructReflectively(Class<T> objectClass) {
        String isPrimitive = objectClass.isPrimitive() ? "Constructing a Primitive" : "Constructing an Object";
        System.out.println(isPrimitive);

        var optionalConstructor = getConstructor(objectClass);

        if (optionalConstructor.isPresent()) {
            return getInstance(optionalConstructor.get());
        } else {
            return Optional.empty();
        }
    }

    /**
     * Get a constructor from a {@code Class} object
     * @param objectClass the class to look for the constructor on
     * @return an {code Optional} with a constructor if the constructor is found.
     * Otherwise an empty {@code Optional} is returned.
     */
    private static <T> Optional<Constructor<T>> getConstructor(Class<T> objectClass) {
        try {
            Constructor<T> constructor = objectClass.getDeclaredConstructor();
            return Optional.of(constructor);
        } catch (NoSuchMethodException e) {
            System.out.println("Error Getting Constructor with Reflection: " + e.getLocalizedMessage());
            return Optional.empty();
        }
    }

    /**
     * Create a new object instance with a constructor
     * @param constructor a constructor object instance from the reflection API to create a new instance with
     * @return an {code Optional} with an object instance if the constructor is invoked successfully.
     * Otherwise an empty {@code Optional} is returned.
     */
    private static <T> Optional<T> getInstance(Constructor<T> constructor) {
        try {
            var instance = constructor.newInstance();
            return Optional.of(instance);
        } catch (Exception e) {
            System.out.println("Error Getting Instance with Reflection: " + e.getLocalizedMessage());
            return Optional.empty();
        }
    }

    /**
     * Print out all the methods on an object
     * @param object an object to search for methods on
     */
    private static void listMethods(Object object) {
        Class objectClass = object.getClass();
        Method[] methods = objectClass.getMethods();

        Arrays.stream(methods).forEach(System.out::println);
    }

    /**
     * Get a method from a class with specific parameters
     * @param objectClass the class to look for a method on
     * @param name what the method is defined as
     * @param parameterTypes types of all the parameters for a method
     * @return an {code Optional} with a method if the object contains the method.
     * Otherwise an empty {@code Optional} is returned.
     */
    private static Optional<Method> getMethod(Class<?> objectClass, String name, Class<?>... parameterTypes) {
        try {
            Method method = objectClass.getMethod(name, parameterTypes);
            return Optional.of(method);
        } catch (NoSuchMethodException e) {
            return Optional.empty();
        }
    }

    public static void main(String... args) {

        String year = String.valueOf(1995);
        inspect(year);
        listMethods(year);

        /* Reflectively construct a instance of String */
        var optionalString = constructReflectively(String.class);

        optionalString.ifPresent(str -> System.out.println(str.length()));

        /* Reflectively construct a instance of primitive int */
        var optionalInteger = constructReflectively(int.class);

        optionalInteger.ifPresent(i -> System.out.println(i.intValue()));

        /* Reflectively construct a instance of ArrayList */
        var optionalArrayList = constructReflectively(ArrayList.class);

        if (optionalArrayList.isPresent()) {
            var arrayList = (ArrayList<String>) optionalArrayList.get();
            arrayList.add("hi");
            arrayList.add("what is up");
            System.out.println(arrayList.toString());
        }
    }
}