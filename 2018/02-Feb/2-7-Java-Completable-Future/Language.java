package completablefuture;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.time.LocalDate;

/**
 * The Language object
 * @author Andrew Jarombek
 * @since 2/7/2018
 */
@Data
@AllArgsConstructor
public class Language {

    private String name;
    private LocalDate created;
    private String version;
    private String paradigm;
}
