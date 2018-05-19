package bchain;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * A simple implementation of the blockchain.  This class is private final - it can not
 * be extended.
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see AbstractBlockChain
 * @see BlockChain
 * @see Block
 */
public final class SimpleBlockChain<E> extends AbstractBlockChain<E> {

    /**
     * A list of the blocks in the blockchain.  This is the main underlying data
     * structure for the {@code SimpleBlockChain}.  Note: the {@code Block} in the
     * {@code blockList} are immutable
     */
    private List<Block<E>> blockList;

    /**
     * Set the max block size for the blockchain.  Once this limit is exceeded a
     * new block will be initialized.
     */
    private int maxBlockSize = 5;

    public SimpleBlockChain() {
        blockList = new ArrayList<>(List.of(new Block<>(null)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Block.Transaction<E>> add(E el) {

        // Get the latest block in the blockList.  This is the block we will add to if it isn't full
        var latestBlock = blockList.get(blockList.size() - 1);

        // Reference to the previous block in the blockchain
        var prevBlock = latestBlock.getPrevBlock();

        // If the block isn't full, create a new block that replicates the old one except with
        // the new transaction.  Replace the existing block with this new one.  Otherwise, create a
        // brand new block and add it to the list.
        if (latestBlock.length() < maxBlockSize) {
            System.out.println("Concat with Existing Block");
            latestBlock = latestBlock.concatTransaction(el, prevBlock);

            blockList.remove(blockList.size() - 1);
            blockList.add(latestBlock);

            return latestBlock.lastTransaction();
        } else {
            System.out.println("Creating a New Block");
            Block<E> block = Block.create(latestBlock);
            block = block.concatTransaction(el, prevBlock);

            blockList.add(block);

            return block.lastTransaction();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Block.Transaction<E>> find(long id) {

        // First filter down the blockList to the block that contains the transaction
        Optional<Block<E>> containingBlock = blockList.stream()
                .filter(block -> block.containsTransaction(id)).findFirst();

        // If a block containing the transaction is found, return the transaction.
        // Otherwise return an empty optional.
        if (containingBlock.isPresent()) {
            return containingBlock.get().findTransaction(id);
        } else {
            return Optional.empty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addAll(Iterable<? extends E> elements) {
        elements.forEach(this::add);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    List<Block<E>> getBlockList() {
        return blockList;
    }
}
