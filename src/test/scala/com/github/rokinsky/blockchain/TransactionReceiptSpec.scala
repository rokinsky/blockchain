package com.github.rokinsky.blockchain

import cats.syntax.option.*
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class TransactionReceiptSpec extends AnyFlatSpec:
  "test1" should "be correct" in new Scope:
    val (block, receipt :: Nil) = chain.receipts.head

    val actual   = receipt.isValid(block.header)
    val expected = true

    expected shouldEqual actual
