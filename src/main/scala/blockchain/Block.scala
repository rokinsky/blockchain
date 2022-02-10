package blockchain

import blockchain.{BlockHeader, Transaction}
import cats.Show
import cats.syntax.show.*

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

final case class Block(header: BlockHeader, transactions: List[Transaction])

object Block:
  given Hashable[Block] with
    extension(a: Block) def hash: Hash =
      a.header.hash

  given Show[Block] with
    def show(block: Block): String =  PPrint.pprV(List(
      block.header.show,
      (block.header.coinbase :: block.transactions).show
    ))
