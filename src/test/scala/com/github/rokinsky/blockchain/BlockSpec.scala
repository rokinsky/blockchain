package com.github.rokinsky.blockchain

import cats.syntax.option.*
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class BlockSpec extends AnyFlatSpec:
  "test1" should "be correct" in new Scope:
    val expected = 0x0dbea380.some
    val actual   = chain.verifyChain

    expected shouldEqual actual

  "test2" should "be correct" in new Scope:
    val expected = none[Hash]
    val actual   = chain.copy(chain = List(block1, block2)).verifyChain

    expected shouldEqual actual
