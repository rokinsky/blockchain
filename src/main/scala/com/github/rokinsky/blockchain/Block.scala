package com.github.rokinsky.blockchain

import cats.Show
import cats.syntax.show.*

import scala.annotation.tailrec
import scala.util.Properties.lineSeparator as EOL

final case class Block(header: BlockHeader, transactions: List[Transaction])

object Block:
  extension (block: Block)
    def verify(parent: Hash): Option[Hash] =
      for
        tree <- HashTree.of(block.header.coinbase :: block.transactions)
        if block.header.isValid &&
          block.header.parent == parent &&
          block.header.txRoot == tree.treeHash
      yield block.hash

  given Hashable[Block] with
    extension (block: Block)
      def hash: Hash =
        block.header.hash

  given Show[Block] with
    def show(block: Block): String = PPrint.pprV(
      List(
        block.header.show,
        (block.header.coinbase :: block.transactions).show
      )
    )
