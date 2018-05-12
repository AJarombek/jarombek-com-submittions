package bchain;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public final class SimpleBlockChain<E> extends AbstractBlockChain<E> {

    private List<Block<E>> blockList;
    private int maxBlockSize = 5;

    public SimpleBlockChain() {
        blockList = new ArrayList<>(List.of(new Block<>(null)));
    }

    @Override
    public Optional<Block.Transaction<E>> add(E el) {
        var lastBlock = blockList.get(blockList.size() - 1);
        System.out.println("Last Block: " + lastBlock.toString());

        if (lastBlock.length() < maxBlockSize) {
            System.out.println("Concat with Existing Block");
            lastBlock = lastBlock.concatTransaction(el);

            blockList.remove(blockList.size() - 1);
            blockList.add(lastBlock);

            return lastBlock.lastTransaction();
        } else {
            System.out.println("Creating a New Block");
            Block<E> block = Block.create(lastBlock);
            block = block.concatTransaction(el);

            blockList.add(block);

            return block.lastTransaction();
        }
    }

    @Override
    public Optional<Block.Transaction<E>> find(long id) {
        Optional<Block<E>> containingBlock = blockList.stream()
                .filter(block -> block.containsTransaction(id)).findFirst();

        if (containingBlock.isPresent()) {
            return containingBlock.get().findTransaction(id);
        } else {
            return Optional.empty();
        }
    }

    @Override
    List<Block<E>> getBlockList() {
        return blockList;
    }
}
