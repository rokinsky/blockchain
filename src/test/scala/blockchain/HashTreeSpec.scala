package blockchain

import blockchain.HashTree
import blockchain.HashTree.given_Show_HashTree
import cats.syntax.option.*
import cats.syntax.show.*
import org.junit.Assert.*
import org.junit.Test

class HashTreeSpec:
  @Test def t1(): Unit =
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

    assertEquals(expected, actual)
