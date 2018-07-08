package java8;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

public class Regex {

    public static void main(String... args) {

        /* Basic Regex pattern matching and finding for existence */

        String dateRegex = "\\d{1,2}/\\d{1,2}/\\d{4}";

        String today = "7/8/2018";
        String tomorrow = "Tomorrow is 7/9/2018.";
        String endOfYear = "12/31/2018";
        String myBirthday = "Feb. 26, 2018";

        // Calling string.matches(regex) gives the same result as Pattern.matches(regex, string)
        // These can be further optimized
        assert today.matches(dateRegex);
        assert !tomorrow.matches(dateRegex);
        assert Pattern.matches(dateRegex, endOfYear);
        assert !Pattern.matches(dateRegex, myBirthday);

        // Compile the regular expression into a pattern before matching the pattern.  This reduces the work
        // needed to perform each Regex match
        Pattern datePattern = Pattern.compile(dateRegex);

        Matcher todayMatcher = datePattern.matcher(today);
        Matcher tomorrowMatcher = datePattern.matcher(tomorrow);
        Matcher endOfYearMatcher = datePattern.matcher(endOfYear);
        Matcher myBirthdayMatcher = datePattern.matcher(myBirthday);

        assert todayMatcher.find();
        assert tomorrowMatcher.find();
        assert endOfYearMatcher.find();
        assert !myBirthdayMatcher.find();

        /* Looping through RegEx matches */

        String catStatements = "I really like cats.  Cats, cats, CATS!  \n" +
                "I wish I had a cat, I would name it Cat.";

        String catRegex = "[Cc][Aa][Tt][Ss]?";

        Pattern catPattern = Pattern.compile(catRegex);
        Matcher catMatcher = catPattern.matcher(catStatements);

        List<String> catList = new ArrayList<>();

        // Search for matches of the regex until all are found
        while (catMatcher.find()) {
            // matcher.group() returns the string of the previous match
            catList.add(catMatcher.group());
        }

        assert catList.size() == 6;
        assert catList.get(0).equals("cats");
        assert catList.get(3).equals("CATS");
        assert catList.get(5).equals("Cat");

        /* Looping through Regex Grouping Captures */

        String topLanguages = "Top 5 Favorite Programming Languages (as of 7/8/2018) \n" +
                "1. Java 2. JavaScript 3. Python 4. Swift 5. PHP";

        String languageRegex = "(\\d)\\. (\\w*)";

        Pattern languagePattern = Pattern.compile(languageRegex);
        Matcher languageMatcher = languagePattern.matcher(topLanguages);

        Map<String, String> languageMap = new HashMap<>();

        while (languageMatcher.find()) {
            languageMap.put(languageMatcher.group(1), languageMatcher.group(2));
        }

        assert languageMap.size() == 5;
        assert languageMap.get("1").equals("Java");
        assert languageMap.get("2").equals("JavaScript");
        assert languageMap.get("3").equals("Python");
        assert languageMap.get("4").equals("Swift");
        assert languageMap.get("5").equals("PHP");
    }
}
