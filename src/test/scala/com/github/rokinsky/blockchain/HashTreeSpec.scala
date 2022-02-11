package com.github.rokinsky.blockchain

import cats.syntax.option.*
import cats.syntax.show.*
import com.github.rokinsky.blockchain.HashTree.given_Show_HashTree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class HashTreeSpec extends AnyFlatSpec:
  "test1" should "be correct" in {
    val expected = s"""0x2e1cc0e4 -
                     |  0xfbfe18ac -
                     |    0x6600a107 -
                     |      0x00000066 'f'
                     |      0x00000075 'u'
                     |    0x62009aa7 -
                     |      0x00000062 'b'
                     |      0x00000061 'a'
                     |  0xd11bea20 +
                     |    0x7200b3e8 +
                     |      0x00000072 'r'
                     |""".stripMargin.some
    val actual = HashTree.buildTree("fubar".toList).map(_.show)

    expected shouldEqual actual
  }

  "test2" should "be correct" in {
    val expected = s"""0x00000061 'a'
                     |""".stripMargin.some
    val actual = HashTree.buildTree("a".toList).map(_.show)

    expected shouldEqual actual
  }

  "test3" should "be correct" in {
    val expected = s"""0x6600a0f8 +
                     |  0x00000066 'f'
                     |""".stripMargin
    val actual = HashTree.drawTree(HashTree.twig(HashTree.leaf('f')))

    expected shouldEqual actual
  }
