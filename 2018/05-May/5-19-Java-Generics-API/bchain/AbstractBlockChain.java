package bchain;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

public abstract class AbstractBlockChain<E> implements BlockChain<E> {

    private int hashCode;

    @Override
    public abstract Optional<Block.Transaction<E>> add(E el);

    @Override
    public abstract Optional<Block.Transaction<E>> find(long id);

    @Override
    public abstract void addAll(Iterable<? extends E> elements);

    abstract List<Block<E>> getBlockList();

    @Override
    public int hashCode() {
        var result = hashCode;

        if (result == 0) {
            result = getBlockList().hashCode();
        }

        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof AbstractBlockChain))
            return false;
        var blockChain = (AbstractBlockChain) obj;
        return Objects.equals(blockChain.getBlockList(), getBlockList());
    }

    @Override
    public String toString() {
        return getBlockList().toString();
    }
}
