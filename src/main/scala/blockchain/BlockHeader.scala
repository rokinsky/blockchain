package blockchain

import blockchain.Hash.given_Show_Hash
import blockchain.Hashable.hashableSeqA
import blockchain.PPrint.given_Show_String_A
import blockchain.Transaction
import blockchain.Blockchain.{Amount, Coin}
import cats.Show
import cats.syntax.semigroup.*
import cats.syntax.show.*

final case class BlockHeader(
  parent: Hash,
  coinbase: Transaction,
  txRoot: Hash, // root of the Merkle tree
  nonce: Hash
)

object BlockHeader:

  def validNonce(blockHeader: BlockHeader): Boolean =
    blockHeader.hash % math.pow(2, Blockchain.Difficulty) == 0

  given Hashable[BlockHeader] with
    extension(a: BlockHeader) def hash: Hash =
      List(a.parent, a.coinbase.hash, a.txRoot, a.nonce).hash

  given Show[BlockHeader] with
    def show(blockHeader: BlockHeader): String = PPrint.pprV(List(
      ("hash", blockHeader.hash).show,
      ("parent", blockHeader.parent).show,
      ("miner", blockHeader.coinbase.receiver).show,
      ("root", blockHeader.txRoot).show,
      ("nonce", blockHeader.nonce.toString).show
    ))
