import api.Exercise;
import api.Run;
import api.Ski;
import bchain.BlockChain;
import bchain.SimpleBlockChain;

import java.time.Duration;
import java.util.List;

public class TransactionManager {

    public static void main(String... args) {
        var exercise = List.of(Run.createNow(2.0, Duration.ofMinutes(20).plusSeconds(26), Run.Surface.SAND),
                Ski.createNow(1.5, Duration.ofMinutes(31), Ski.Type.Nordic));

        System.out.println(exercise.toString());

        BlockChain<Exercise> blockChain = new SimpleBlockChain<>();
        var transaction = blockChain.add(Run.createNow(2.0,
                Duration.ofMinutes(26).plusSeconds(22), Run.Surface.SAND));

        transaction.ifPresent(t -> System.out.println(t.toString()));

        System.out.println(blockChain.toString());
    }
}
