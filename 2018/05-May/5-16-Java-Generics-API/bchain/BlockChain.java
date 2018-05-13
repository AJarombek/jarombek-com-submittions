package bchain;

import java.util.Optional;

public interface BlockChain<E> {

    Optional<Block.Transaction<E>> add(E el);
    Optional<Block.Transaction<E>> find(long id);
}
