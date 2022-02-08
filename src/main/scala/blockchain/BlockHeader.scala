package blockchain

import blockchain.Hashable.given_Hashable_Int
import blockchain.Transaction
import blockchain.Transaction.{Amount, Coin, given_Hashable_Transaction}
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
    blockHeader.hash % (2 ^ Difficulty) == 0

  given Hashable[BlockHeader] with
    extension(a: BlockHeader) def hash: Hash =
      a.parent |+| a.coinbase.hash |+| a.txRoot |+| a.nonce
