package blockchain

import blockchain.Blockchain.{Amount, Miner}
import cats.syntax.foldable.*
import cats.syntax.functorFilter.*

import scala.annotation.tailrec

object Blockchain:
  type Address = Hash
  type Amount = Int
  type Miner = Address

  val Coin: Amount = 1000
  val Difficulty = 5
  val BlockReward: Amount = 50 * Coin

  def mineBlock(miner: Miner, parent: Hash, transactions: List[Transaction]): Block =
    val coinbase = Transaction.coinbaseTx(miner)
    val txRoot = HashTree.buildTree(coinbase :: transactions).fold(0x00000000)(_.treeHash)

    @tailrec
    def mine(nonce: Hash): Block =
      val blockHeader = BlockHeader(parent, coinbase, txRoot, nonce)
      if BlockHeader.validNonce(blockHeader) then Block(blockHeader, transactions) else mine(nonce + 1)

    mine(0x00000000)

  def validChain(blocks: List[Block]): Boolean =
    verifyChain(blocks).isDefined

  def verifyChain(blocks: List[Block]): Option[Hash] =
    blocks.reverse.foldM(0)((parent, block) => verifyBlock(block, parent))

  def verifyBlock(block: Block, parent: Hash): Option[Hash] =
    for
      tree <- HashTree.buildTree(block.header.coinbase :: block.transactions)
      if BlockHeader.validNonce(block.header) &&
        block.header.parent == parent &&
        block.header.txRoot == tree.treeHash
    yield block.hash

  def validateReceipt(transactionReceipt: TransactionReceipt, blockHeader: BlockHeader): Boolean =
    transactionReceipt.blockHash == blockHeader.hash &&
      MerkleProof.verifyProof(blockHeader.txRoot, transactionReceipt.proof)

  def mineTransactions(miner: Miner, parent: Hash, transactions: List[Transaction]): (Block, List[TransactionReceipt]) =
    val block = Blockchain.mineBlock(miner, parent, transactions)
    (block, transactions.mapFilter(TransactionReceipt.of(block, _)))
