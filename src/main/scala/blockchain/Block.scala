package blockchain

import blockchain.Transaction.{Address, Miner}
import blockchain.{BlockHeader, Transaction}
import cats.Show
import cats.syntax.foldable.*
import cats.syntax.show.*

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

final case class Block(blockHdr: BlockHeader, blockTxs: List[Transaction])

object Block:
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
    blocks.reverse.foldM(0)((acc, cur) => verifyBlock(cur, acc))

  def verifyBlock(block: Block, parent: Hash): Option[Hash] =
    for
      tree <- HashTree.buildTree(block.blockHdr.coinbase :: block.blockTxs)
      if BlockHeader.validNonce(block.blockHdr) &&
         block.blockHdr.parent == parent &&
         block.blockHdr.txRoot == tree.treeHash
    yield block.hash

  given Hashable[Block] with
    extension(a: Block) def hash: Hash =
      a.blockHdr.hash

  given Show[Block] with
    def show(block: Block): String =  PPrint.pprV(List(
      block.blockHdr.show,
      (block.blockHdr.coinbase :: block.blockTxs).show
    ))
