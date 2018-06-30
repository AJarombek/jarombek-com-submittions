package reflection;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Optional;

public class Reflect {

    private static void reflect(Object object) {

        Class objectClass = object.getClass();
        Optional<Method> method = getMethod(objectClass, "length", null);

        method.ifPresent(m -> System.out.println(m.toString())); // public int java.lang.String.length()

        String modifier = Modifier.toString(objectClass.getModifiers());
        System.out.println(modifier); // public final

        Class superClass = objectClass.getSuperclass();
        System.out.println(superClass.toString()); // class java.lang.Object
    }

    private static Optional<Method> getMethod(Class<?> objectClass, String name, Class<?>... parameterTypes) {
        try {
            Method method = objectClass.getMethod(name, parameterTypes);
            return Optional.of(method);
        } catch (NoSuchMethodException e) {
            return Optional.empty();
        }
    }

    public static void main(String... args) {
        reflect(String.valueOf(1995));

        String str = "hey";
        int length = str.length();
    }
}