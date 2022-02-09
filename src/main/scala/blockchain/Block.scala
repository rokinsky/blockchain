package blockchain

import blockchain.Transaction.{Address, Miner}
import blockchain.Hashable.given_Hashable_Int
import blockchain.{BlockHeader, Transaction}
import cats.syntax.foldable.*

import scala.annotation.tailrec

final case class Block(blockHdr: BlockHeader, blockTxs: List[Transaction])

object Block:
  def mineBlock(miner: Miner, parent: Hash, blockTxs: List[Transaction]): Block =
    val coinbase = Transaction.coinbaseTx(miner)
    val txRoot = HashTree.buildTree(coinbase :: blockTxs).fold(0)(_.treeHash)

    @tailrec
    def auxMine(hash: Hash): Block =
      val blockHdr = BlockHeader(
        parent = parent,
        coinbase = coinbase,
        txRoot = txRoot,
        nonce = hash,
      )
      if BlockHeader.validNonce(blockHdr) then Block(blockHdr, blockTxs) else auxMine(hash + 1)

    auxMine(0)

  def validChain(blocks: List[Block]): Boolean =
    verifyChain(blocks).isDefined

  def verifyChain(blocks: List[Block]): Option[Hash] =
    blocks.reverse.foldM(0)((acc, cur) => verifyBlock(cur, acc))

  def verifyBlock(block: Block, parentHash: Hash): Option[Hash] =
    for {
      tree <- HashTree.buildTree(block.blockHdr.coinbase :: block.blockTxs)
      if BlockHeader.validNonce(block.blockHdr) &&
         block.blockHdr.parent == parentHash &&
         block.blockHdr.txRoot == tree.treeHash
    } yield block.hash

  given Hashable[Block] with
    extension(a: Block) def hash: Hash =
      a.blockHdr.hash
