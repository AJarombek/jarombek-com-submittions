import exercise.Exercise;
import exercise.Run;
import exercise.Ski;
import bchain.Block;
import bchain.BlockChain;
import bchain.SimpleBlockChain;

import java.time.Duration;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import java.util.Optional;

/**
 * Main method to initialize a blockChain instance and add transactions to it
 * @author Andrew Jarombek
 * @since 5/19/2018
 */
public class TransactionManager {

    public static void main(String... args) {

        // Create a new blockchain and add a transaction to it
        BlockChain<Exercise> blockChain = new SimpleBlockChain<>();
        var transaction1 = blockChain.add(Run.createNow(2.0,
                Duration.ofMinutes(26).plusSeconds(22), Run.Surface.SAND));

        // Print out the transaction that went through
        transaction1.ifPresent(System.out::println);

        // Print out the current state of the blockchain
        System.out.println("Current BlockChain: " + blockChain.toString());

        // Create a list of exercises to add to the blockchain
        var transaction2 = List.of(
                Run.createNow(2.0, Duration.ofMinutes(20).plusSeconds(26), Run.Surface.SAND),
                Ski.createNow(1.5, Duration.ofMinutes(31), Ski.Type.Nordic),
                Run.create(4.27, Duration.ofMinutes(34).plusSeconds(18),
                        LocalDate.of(2018, Month.MAY, 18)),
                Run.create(3.37, Duration.ofMinutes(28).plusSeconds(17),
                        LocalDate.of(2018, Month.MAY, 17), Run.Surface.SAND),
                Run.create(4.22, Duration.ofMinutes(36).plusSeconds(5),
                        LocalDate.of(2018, Month.MAY, 16), Run.Surface.SAND)
        );

        System.out.println("Exercises to be Added: " + transaction2.toString());

        // Add all the exercises to the blockchain
        blockChain.addAll(transaction2);

        System.out.println("Current BlockChain: " + blockChain.toString());

        // Add another exercise to the blockchain
        var transaction3 = blockChain.add(Run.create(3, Duration.ofMinutes(24).plusSeconds(31),
                LocalDate.of(2018, Month.MAY, 19), Run.Surface.SAND));

        transaction3.ifPresent(System.out::println);

        System.out.println("Current BlockChain: " + blockChain.toString());

        // Find a transaction on the blockchain
        transaction3.ifPresent(t -> {
            Optional<Block.Transaction<Exercise>> transaction = blockChain.find(t.getId());
            transaction.ifPresent(System.out::println);
        });
    }
}
