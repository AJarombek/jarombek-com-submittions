package bchain;

import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static java.util.stream.Collectors.toMap;

public final class Block<V> {

    private Map<String, Transaction<V>> transactions;
    private Block<V> prevBlock;

    Block(Block<V> prevBlock) {
        transactions = new HashMap<>();
        this.prevBlock = prevBlock;
    }

    static <V> Block<V> create(Block<V> prevBlock) {
        return new Block<>(prevBlock);
    }

    private Block(Map<String, Transaction<V>> transactions) {
        this.transactions = transactions;
    }

    Block<V> concatTransaction(V value) {
        var map = transactions.entrySet().stream().collect(toMap(Map.Entry::getKey, Map.Entry::getValue));

        Transaction.create(value).ifPresent(transaction -> map.put(String.valueOf(transaction.hashCode()), transaction));

        return new Block<>(map);
    }

    Optional<Transaction<V>> lastTransaction() {
        int skip = (transactions.size() == 0) ? 0 : transactions.size() - 1;
        return transactions.values().stream().skip(skip).findFirst();
    }

    Optional<Transaction<V>> findTransaction(long id) {
        return transactions.values().stream().filter(t -> t.id == id).findFirst();
    }

    boolean containsTransaction(long id) {
        return transactions.values().stream().anyMatch(t -> t.id == id);
    }

    int length() {
        return transactions.entrySet().size();
    }

    public static class Transaction<V> {
        private V value;
        private LocalTime creationDate;
        private long id;

        private Transaction(V value) {
            this.value = value;
            this.creationDate = LocalTime.now();
            id = Objects.hash(this.value, this.creationDate);
        }

        static <V> Optional<Transaction<V>> create(V value) {
            var optionalValue = Optional.ofNullable(value);

            if (optionalValue.isPresent()) {
                return Optional.of(new Transaction<>(value));
            } else {
                return Optional.empty();
            }
        }

        @Override
        public int hashCode() {
            return Objects.hash(value, creationDate, id);
        }

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

        @Override
        public String toString() {
            return "[ " + value.toString() + ", " + creationDate.toString() + ", " + id + " ]";
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(transactions, prevBlock);
    }

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

    @Override
    public String toString() {
        if (Optional.ofNullable(prevBlock).isPresent()) {
            return "[ " + transactions.toString() + ", prev: " + prevBlock.toString() + " ]";
        } else {
            return "[ " + transactions.toString() + " ]";
        }
    }
}
