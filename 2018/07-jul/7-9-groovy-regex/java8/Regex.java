package java8;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

public class Regex {

    public static void main(String... args) {

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

        Pattern datePattern = Pattern.compile(dateRegex);
        Matcher todayMatcher = datePattern.matcher(today);
        Matcher tomorrowMatcher = datePattern.matcher(tomorrow);
        Matcher endOfYearMatcher = datePattern.matcher(endOfYear);
        Matcher myBirthdayMatcher = datePattern.matcher(myBirthday);

        assert todayMatcher.find();
        assert tomorrowMatcher.find();
        assert endOfYearMatcher.find();
        assert !myBirthdayMatcher.find();
    }
}
