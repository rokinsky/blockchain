package com.github.rokinsky.blockchain

import cats.syntax.option.*
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class BlockSpec extends AnyFlatSpec:
  "test1" should "be correct" in new Scope:
    val expected = Block(
      BlockHeader(0x00000000, Transaction(0x00000000, 0x7203d9df, 50000), 0x5b10bd5d, 18),
      Nil
    )
    val actual = block0

    expected shouldEqual actual

  "test2" should "be correct" in new Scope:
    val expected = Block(
      BlockHeader(0x70b432e0, Transaction(0x00000000, 0x790251e0, 50000), 0x5ea7a6f0, 0),
      Nil
    )
    val actual = block1

    expected shouldEqual actual

  "test3" should "be correct" in new Scope:
    val expected = Block(
      BlockHeader(0x2f83ae40, Transaction(0x00000000, 0x5303a90e, 50000), 0x8abe9e15, 3),
      List(Transaction(0x790251e0, 0xb1011705, 1000))
    )
    val actual = block2

    expected shouldEqual actual

  "test4" should "be correct" in new Scope:
    val expected = 0x0dbea380.some
    val actual = Blockchain.verifyChain(List(block2, block1, block0))

    expected shouldEqual actual

  "test5" should "be correct" in new Scope:
    val expected = none[Hash]
    val actual = Blockchain.verifyChain(List(block1, block2))

    expected shouldEqual actual
