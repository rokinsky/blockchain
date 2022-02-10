package blockchain

import blockchain.Blockchain.{Address, Amount, BlockReward, Miner}
import blockchain.Hash.given_Show_Hash
import blockchain.Hashable.hashableSeqA
import blockchain.PPrint.given_Show_String_A
import cats.Show
import cats.syntax.semigroup.*
import cats.syntax.show.*

final case class Transaction(sender: Address, receiver: Address, amount: Amount)

object Transaction:

  def coinbaseTx(miner: Miner): Transaction =
    Transaction(sender = 0, receiver = miner, amount = BlockReward)

  given Hashable[Transaction] with
    extension(a: Transaction) def hash: Hash =
      List(a.sender, a.receiver, a.amount).hash

  given Show[List[Transaction]] with
    def show(transactions: List[Transaction]): String =
      PPrint.pprV(transactions.map(_.show))

  given Show[Transaction] with
    def show(transaction: Transaction): String = PPrint.pprH(List(
      "Tx#",
      transaction.hash.show,
      ("from", transaction.sender).show,
      ("to", transaction.receiver).show,
      ("amount", transaction.amount.toString).show
    ))
