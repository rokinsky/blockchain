package blockchain

import blockchain.BlockHeader.BlockReward
import blockchain.PPrint.given_Show_String_A
import blockchain.Hash.given_Show_Hash
import blockchain.Hashable.hashableSeqA
import blockchain.Transaction.{Address, Amount}
import cats.Show
import cats.syntax.show.*
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

  given Show[List[Transaction]] with
    def show(transactions: List[Transaction]): String =
      PPrint.pprV(transactions.map(_.show))

  given Show[Transaction] with
    def show(transaction: Transaction): String = PPrint.pprH(List(
      "Tx#",
      transaction.hash.show,
      ("from", transaction.txFrom).show,
      ("to", transaction.txTo).show,
      ("amount", transaction.txAmount.toString).show
    ))
