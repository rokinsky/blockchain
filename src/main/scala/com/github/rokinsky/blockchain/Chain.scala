package com.github.rokinsky.blockchain

import cats.syntax.foldable.*
import cats.syntax.functorFilter.*
import com.github.rokinsky.blockchain.Chain.{Address, Amount, Miner}

import scala.annotation.tailrec

final case class Chain(
  chain:               List[Block],
  currentTransactions: List[Transaction],
  difficulty:          Int,
  blockReward:         Amount
)

object Chain:
  type Address = Hash
  type Amount  = Int
  type Miner   = Address

  extension (blockchain: Chain)
    def addTransaction(transaction: Transaction): Chain =
      blockchain.copy(currentTransactions = transaction :: blockchain.currentTransactions)

    def lastHash: Hash =
      blockchain.chain.headOption.fold(0x0000000)(_.hash)

    def mineBlock(miner: Miner): Chain =
      val coinbase = Transaction.coinbaseTx(miner, blockchain.blockReward)
      val txRoot   = HashTree.of(coinbase :: blockchain.currentTransactions).fold(0x00000000)(_.treeHash)
      val parent   = lastHash

      @tailrec
      def mine(nonce: Hash): Block = // proof of work
        val blockHeader = BlockHeader(parent, coinbase, txRoot, nonce, blockchain.difficulty)
        if blockHeader.isValid then Block(blockHeader, blockchain.currentTransactions) else mine(nonce + 1)

      val block = mine(0x00000000)
      blockchain.copy(currentTransactions = Nil, chain = block :: blockchain.chain)

    def isValid: Boolean =
      verifyChain.isDefined

    def verifyChain: Option[Hash] =
      blockchain.chain.reverse.foldM(0)((parent, block) => block.verify(parent))

    def receipts: List[(Block, List[TransactionReceipt])] =
      blockchain.chain.map(block => (block, block.transactions.mapFilter(TransactionReceipt.of(block, _))))

  object Defaults {
    val Difficulty = 5
    val BlockReward: Amount = 50 * Coin
  }

  val Coin: Amount = 1000

  def of(miner: Miner, difficulty: Int = Defaults.Difficulty, blockReward: Amount = Defaults.BlockReward): Chain =
    Chain(Nil, Nil, difficulty, blockReward).mineBlock(miner)
