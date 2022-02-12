package com.github.rokinsky.blockchain

import cats.Show
import cats.syntax.semigroup.*
import cats.syntax.show.*
import com.github.rokinsky.blockchain.Blockchain.{Address, Amount, BlockReward, Miner}
import com.github.rokinsky.blockchain.Hash.given_Show_Hash
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import com.github.rokinsky.blockchain.PPrint.given_Show_String_A

final case class Transaction(sender: Address, receiver: Address, amount: Amount)

object Transaction:

  def coinbaseTx(miner: Miner): Transaction =
    Transaction(sender = 0, receiver = miner, amount = BlockReward)

  given Hashable[Transaction] with
    extension(transaction: Transaction) def hash: Hash =
      List(transaction.sender, transaction.receiver, transaction.amount).hash

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
