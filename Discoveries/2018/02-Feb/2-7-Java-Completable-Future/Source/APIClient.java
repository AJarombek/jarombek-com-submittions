package completablefuture;

import java.util.concurrent.CompletableFuture;

/**
 * @author Andrew Jarombek
 * @since 2/7/2018
 */
public class APIClient {

    private static LanguageAPI languageAPI = new LanguageAPI();

    /**
     * Get a language asynchronously.  When the language object is finally received, print it
     * @param name - the language name
     * @return the CompletableFuture
     */
    public static CompletableFuture<Void> getLanguageAsync(String name) {

        // Start up a new thread and perform the language APIs getLanguage() method asynchronously
        CompletableFuture<Language> futureLanguage = CompletableFuture.supplyAsync(() -> languageAPI.getLanguage(name));

        // Print out the Language once the futures value is supplied
        return futureLanguage.thenAccept(System.out::println);
    }

    public static void main(String... args) {

        // Get the Java language asynchronously
        CompletableFuture<Void> future = getLanguageAsync("Java");
        System.out.println("Finding Java...");

        // Wait for the future to complete before ending the Java process
        future.join();
    }
}
