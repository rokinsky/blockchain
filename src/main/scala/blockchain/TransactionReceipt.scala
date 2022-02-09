package blockchain

import blockchain.Transaction.Miner
import cats.syntax.functorFilter.*

final case class TransactionReceipt(
  txrBlock: Hash,
  txrProof: MerkleProof[Transaction],
)

object TransactionReceipt:
  def validateReceipt(transactionReceipt: TransactionReceipt, blockHeader: BlockHeader): Boolean =
    transactionReceipt.txrBlock == blockHeader.hash &&
      MerkleProof.verifyProof(blockHeader.txRoot, transactionReceipt.txrProof)

  def of(block: Block, transaction: Transaction): Option[TransactionReceipt] =
    HashTree
      .buildTree(block.blockHdr.coinbase :: block.blockTxs)
      .flatMap(MerkleProof.buildProof(transaction, _))
      .map(TransactionReceipt(block.blockHdr.hash, _))

  def mineTransactions(miner: Miner, parent: Hash, transactions: List[Transaction]): (Block, List[TransactionReceipt]) =
    val block = Block.mineBlock(miner, parent, transactions)
    (block, transactions.mapFilter(TransactionReceipt.of(block, _)))
