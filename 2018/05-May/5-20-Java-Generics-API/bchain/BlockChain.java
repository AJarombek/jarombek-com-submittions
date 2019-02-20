package bchain;

import java.util.Optional;

/**
 * Interface to set up a BlockChain
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see AbstractBlockChain
 * @see SimpleBlockChain
 */
public interface BlockChain<E> {

    /**
     * Add an element to the blockchain and return its transaction
     * @param el - the element to be added
     * @return an {@code Optional} value of the transaction added to the blockchain
     */
    Optional<Block.Transaction<E>> add(E el);

    /**
     * Find a transaction on the blockchain given its {@code id}
     * @param id - the identifier of a transaction
     * @return an {@code Optional} value of the transaction found on the blockchain
     */
    Optional<Block.Transaction<E>> find(long id);

    /**
     * Add a bunch of elements to the blockchain
     * @param elements - a collection of elements to be added to the blockchain.  The parameter
     *                 type is {@code Iterable}, so any class that implements the {@code Iterable}
     *                 interface can be used as an argument.  This includes any class that extends
     *                 {@code Collection}.  The generic type of the {@code Iterable} can be the
     *                 generic type of the blockchain or any subclass.
     */
    void addAll(Iterable<? extends E> elements);
}
