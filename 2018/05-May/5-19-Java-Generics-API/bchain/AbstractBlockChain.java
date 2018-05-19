package bchain;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Skeletal Implementation of the {@code BlockChain} interface.  Implementation details
 * for the overridden {@code Object} methods are defined here, while the underlying
 * blockchain structure is implemented in the child classes.
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see BlockChain
 * @see SimpleBlockChain
 */
public abstract class AbstractBlockChain<E> implements BlockChain<E> {

    /**
     * Cache the result of the {@code hashCode()} method
     */
    private int hashCode;

    /* Pass along to the child classes */

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract Optional<Block.Transaction<E>> add(E el);

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract Optional<Block.Transaction<E>> find(long id);

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract void addAll(Iterable<? extends E> elements);

    /**
     * A package-private method to return a list of all the blocks in the blockchain
     * @return a {@code List} of {@code Block} instances in the blockchain
     */
    abstract List<Block<E>> getBlockList();

    /**
     * Find the hash code for this object
     * @return the hashed result
     * @see Object#hashCode()
     */
    @Override
    public int hashCode() {
        var result = hashCode;

        if (result == 0) {
            result = getBlockList().hashCode();
            hashCode = result;
        }

        return result;
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
        if (!(obj instanceof AbstractBlockChain))
            return false;
        var blockChain = (AbstractBlockChain) obj;
        return Objects.equals(blockChain.getBlockList(), getBlockList());
    }

    /**
     * Generate a {@code String} representation of this object
     * @return {@code String} representation
     * @see Object#toString()
     */
    @Override
    public String toString() {
        return getBlockList().toString();
    }
}
