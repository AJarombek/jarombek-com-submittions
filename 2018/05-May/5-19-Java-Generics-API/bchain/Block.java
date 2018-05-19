package bchain;

import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import static java.util.stream.Collectors.toMap;

/**
 * Class representing a block in the blockchain.  This class is private final - it
 * can not be extended.  From an outside view it is also immutable - the transactions
 * and previous block fields can't be changed in the lifetime of the {@code Block} instance.
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see BlockChain
 */
public final class Block<V> {

    /**
     * A map of all the transactions in this block.  Once the object is created, this {@code Map}
     * can't be modified.  The key is the hash code for the transaction and the value is the
     * transaction instance.
     */
    private Map<String, Transaction<V>> transactions;

    /**
     * A reference to the previous block in the blockchain.  This field is also immutable.
     */
    private Block<V> prevBlock;

    /**
     * Package-private constructor for a new block.  It creates an empty transaction log and the
     * previous block set to the argument passed in
     * @param prevBlock - the previous block in the blockchain
     */
    Block(Block<V> prevBlock) {
        transactions = new HashMap<>();
        this.prevBlock = prevBlock;
    }

    /**
     * Package-private Static factory method for creating a {@code Block}.  It creates an empty
     * transaction log and the previous block set to the argument passed in
     * @param prevBlock - the previous block in the blockchain
     * @return a new {@code Block} instance
     */
    static <V> Block<V> create(Block<V> prevBlock) {
        return new Block<>(prevBlock);
    }

    /**
     * A private constructor of the {@code Block} instance.
     * @param transactions - a map of transactions to initialize the instance with.
     * @param prevBlock - the previous block in the blockchain
     */
    private Block(Map<String, Transaction<V>> transactions, Block<V> prevBlock) {
        this.transactions = transactions;
        this.prevBlock = prevBlock;
    }

    /**
     * Create a new block based on the existing block plus a new transaction with
     * the value passed in.
     * @param value - the value of a new transaction.
     * @param prevBlock - the previous block in the blockchain
     * @return a new {@code Block}.
     */
    Block <V> concatTransaction(V value, Block<V> prevBlock) {

        // Create a new instance with the contents of the existing transactions map
        var map = transactions.entrySet().stream()
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue));

        Transaction.create(value).ifPresent(transaction -> map.put(String.valueOf(transaction.hashCode()), transaction));

        return new Block<>(map, prevBlock);
    }

    /**
     * Get the last transaction in this block.  This will also be the newest transaction.
     * @return an {@code Optional} instance containing a transaction if this block has one
     * or more transactions, otherwise an empty {@code Optional}.
     */
    Optional<Transaction<V>> lastTransaction() {
        int skip = (transactions.size() == 0) ? 0 : transactions.size() - 1;
        return transactions.values().stream().skip(skip).findFirst();
    }

    /**
     * Get a transaction based on a given identifier
     * @param id - identifier of a transaction.
     * @return an {@code Optional} instance containing a transaction if it exists,
     * otherwise an empty {@code Optional}.
     */
    Optional<Transaction<V>> findTransaction(long id) {
        return transactions.values().stream().filter(t -> t.id == id).findFirst();
    }

    /**
     * Check if a transaction with a given identifier exists in the block
     * @param id - identifier of this transaction.
     * @return a {@code boolean} value of {@code true} if the transaction exists,
     * {@code false} otherwise.
     */
    boolean containsTransaction(long id) {
        return transactions.values().stream().anyMatch(t -> t.id == id);
    }

    /**
     * Get the number of transactions in the block.
     * @return number of transactions
     */
    int length() {
        return transactions.entrySet().size();
    }

    /**
     * public static nested class that represents transactions in the blockchain.  This
     * is used as a helper class for {@code Block} since each block contains a list of
     * transactions.  This class helps contain the details of the transaction.
     */
    public static class Transaction<V> {

        /**
         * The value of the transaction.  This is the data that users wanted to save to
         * the blockchain.  The value is immutable
         */
        private V value;

        /**
         * The creation data of the transaction.  This date is immutable.
         */
        private LocalTime creationDate;

        /**
         * The identifier of this transaction.  It is immutable and based off the hash code
         * of the {@code value} and {@code creationDate}.
         */
        private long id;

        private Transaction(V value) {
            this.value = value;
            this.creationDate = LocalTime.now();
            id = Objects.hash(this.value, this.creationDate);
        }

        /**
         * static factory method to create a transaction
         * @param value - the value to be saved with the transaction
         * @return an {@code Optional} instance containing a transaction if the argument passed
         * in wasn't null, otherwise an empty {@code Optional}.
         */
        static <V> Optional<Transaction<V>> create(V value) {
            var optionalValue = Optional.ofNullable(value);

            if (optionalValue.isPresent()) {
                return Optional.of(new Transaction<>(value));
            } else {
                return Optional.empty();
            }
        }

        /**
         * Find the hash code for this object
         * @return the hashed result
         * @see Object#hashCode()
         */
        @Override
        public int hashCode() {
            return Objects.hash(value, creationDate, id);
        }

        /**
         * Determine if another object is equal to this object
         * @param obj - another object to compare to
         * @return a {@code boolean} of whether the objects are equal
         * @see Object#equals(Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (!(obj instanceof Transaction))
                return false;
            var transaction = (Transaction) obj;
            return Objects.equals(transaction.value, value) &&
                    Objects.equals(transaction.creationDate, creationDate) &&
                    Objects.equals(transaction.id, id);
        }

        /**
         * Generate a {@code String} representation of this object
         * @return {@code String} representation
         * @see Object#toString()
         */
        @Override
        public String toString() {
            return "[ " + value.toString() + ", " + creationDate.toString() + ", " + id + " ]";
        }

        /* Getters */

        /**
         * Get the value of the transaction
         * @return value based on the generic type of the transaction
         */
        public V getValue() {
            return value;
        }

        /**
         * Get the date a which the transaction was created
         * @return {@code LocalDate} of the transactions creation
         */
        public LocalTime getCreationDate() {
            return creationDate;
        }

        /**
         * Get the identifier of the transaction
         * @return identifier of the transaction in the form of a {@code long}.
         */
        public long getId() {
            return id;
        }
    }

    /**
     * Find the hash code for this object
     * @return the hashed result
     * @see Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash(transactions, prevBlock);
    }

    /**
     * Determine if another object is equal to this object
     * @param obj - another object to compare to
     * @return a {@code boolean} of whether the objects are equal
     * @see Object#equals(Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Block))
            return false;
        var block = (Block) obj;
        return Objects.equals(block.transactions, transactions) &&
                Objects.equals(block.prevBlock, prevBlock);
    }

    /**
     * Generate a {@code String} representation of this object
     * @return {@code String} representation
     * @see Object#toString()
     */
    @Override
    public String toString() {
        if (Optional.ofNullable(prevBlock).isPresent()) {
            return "[ " + transactions.toString() + ", prev: " + prevBlock.toString() + " ]";
        } else {
            return "[ " + transactions.toString() + " ]";
        }
    }

    /* Getter */

    /**
     * Get the previous block in the blockchain.
     * @return the previous {@code Block}
     */
    public Block<V> getPrevBlock() {
        return prevBlock;
    }
}
