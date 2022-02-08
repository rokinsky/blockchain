package blockchain

import blockchain.HashTree
import blockchain.Hashable.hashableSeqA
import blockchain.Transaction.Coin
import cats.syntax.option.*
import org.junit.Assert.*
import org.junit.Test

class BlockSpec:
  val tx1: Transaction = Transaction(txFrom = "Alice".toList.hash, txTo = "Bob".toList.hash, txAmount = 1 * Coin)
  val block0: Block = Block.mineBlock("Satoshi".toList.hash, 0, Nil)
  val genesis: Block = block0
  val block1: Block = Block.mineBlock("Alice".toList.hash, genesis.hash, Nil)
  val block2: Block = Block.mineBlock("Charlie".toList.hash, block1.hash, List(tx1))
  val chain = List(block2, block1, block0)

  println(s"$block0, $block1, $block2")

  @Test def t1(): Unit =
    val expected = Block(
      BlockHeader(parent = 797158976, Transaction(0, 1392748814, 50000), 0, 3),
      List(Transaction(2030195168, 0, 1000))
    )
    val actual = Block.mineBlock("Charlie".toList.hash, block1.hash, List(tx1))

    assertEquals(expected, actual)

  @Test def t2(): Unit =
    val expected = 0x0dbea380.some
    val actual = Block.verifyChain(chain)

    assertEquals(expected, actual)

  @Test def t3(): Unit =
    val expected = none[Hash]
    val actual = Block.verifyChain(List(block1, block2))

    assertEquals(expected, actual)
