package api;

import java.time.Duration;
import java.util.List;

public class TransactionManager {

    public static void main(String... args) {
        var exercise = List.of(Run.createNow("2.0", Duration.ofMinutes(20).plusSeconds(26), Run.Surface.SAND));
    }
}
