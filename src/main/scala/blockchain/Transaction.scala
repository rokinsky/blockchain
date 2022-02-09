package blockchain

import blockchain.BlockHeader.BlockReward
import blockchain.Hashable.hashableSeqA
import blockchain.Transaction.{Address, Amount}
import cats.syntax.semigroup.*

final case class Transaction(txFrom: Address, txTo: Address, txAmount: Amount)

object Transaction:
  type Address = Hash
  type Amount = Int
  type Miner = Address

  val Coin: Amount = 1000

  def coinbaseTx(miner: Miner): Transaction =
    Transaction(txFrom = 0, txTo = miner, txAmount = BlockReward)

  given Hashable[Transaction] with
    extension(a: Transaction) def hash: Hash =
      List(a.txFrom, a.txTo, a.txAmount).hash
