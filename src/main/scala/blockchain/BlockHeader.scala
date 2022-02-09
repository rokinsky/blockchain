package blockchain

import blockchain.Hashable.hashableSeqA
import blockchain.Transaction
import blockchain.Transaction.{Amount, Coin}
import cats.syntax.semigroup.*

final case class BlockHeader(
  parent: Hash,
  coinbase: Transaction,
  txRoot: Hash, // root of the Merkle tree
  nonce: Hash
)

object BlockHeader:
  val Difficulty = 5
  val BlockReward: Amount = 50 * Coin

  def validNonce(blockHeader: BlockHeader): Boolean =
    blockHeader.hash % math.pow(2, Difficulty) == 0

  given Hashable[BlockHeader] with
    extension(a: BlockHeader) def hash: Hash =
      List(a.parent, a.coinbase.hash, a.txRoot, a.nonce).hash
