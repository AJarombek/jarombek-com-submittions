import java.text.Normalizer;

/**
 * Java Unicode Support
 * @author Andrew Jarombek
 * @since 10/4/2018
 */

public class Unicode {

    public static void main(String... args) {
        String b = "beyonce\u0301";
        String b2 = "beyonc\u00E9";

        assert b.length() == 8;
        assert b2.length() == 7;

        assert !b.equals(b2);

        // Normalize both strings using NFC
        String normalized_b = Normalizer.normalize(b, Normalizer.Form.NFC);
        String normalized_b2 = Normalizer.normalize(b, Normalizer.Form.NFC);

        assert normalized_b.length() == 7;
        assert normalized_b2.length() == 7;

        assert normalized_b.equals(normalized_b2);

        String smiley = "😊";
        assert smiley.length() == 2;

        // In order to get the proper length of unicode,
        // length must be measured by unicode code point instead of character.
        assert smiley.codePointCount(0, smiley.length()) == 1;
    }
}