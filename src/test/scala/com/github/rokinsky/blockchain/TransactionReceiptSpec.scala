package com.github.rokinsky.blockchain

import cats.syntax.option.*
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class TransactionReceiptSpec extends AnyFlatSpec:
  "test1" should "be correct" in new Scope:
    val (block, receipt :: Nil) = Blockchain.mineTransactions(
      charlie,
      block1.hash,
      List(tx1)
    )
    val actual   = Blockchain.validateReceipt(receipt, block.header)
    val expected = true

    expected shouldEqual actual
