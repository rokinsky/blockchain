package blockchain

import blockchain.HashTree
import blockchain.Hashable.hashableSeqA
import cats.syntax.option.*
import org.junit.Assert.*
import org.junit.Test

class BlockSpec:
  // TODO: move to Scope
  val alice: Hash = "Alice".toList.hash
  val bob: Hash = "Bob".toList.hash
  val charlie: Hash = "Charlie".toList.hash
  val satoshi: Hash = "Satoshi".toList.hash

  val tx1: Transaction = Transaction(alice, bob, 1 * Blockchain.Coin)
  val block0: Block = Blockchain.mineBlock(satoshi, 0, Nil)
  val genesis: Block = block0
  val block1: Block = Blockchain.mineBlock(alice, genesis.hash, Nil)
  val block2: Block = Blockchain.mineBlock(charlie, block1.hash, List(tx1))

  @Test def t1(): Unit =
    val expected = Block(
      BlockHeader(0x00000000, Transaction(0x00000000, 0x7203d9df, 50000), 0x5b10bd5d, 18),
      Nil
    )
    val actual = block0

    assertEquals(expected, actual)

  @Test def t2(): Unit =
    val expected = Block(
      BlockHeader(0x70b432e0, Transaction(0x00000000, 0x790251e0, 50000), 0x5ea7a6f0, 0),
      Nil
    )
    val actual = block1

    assertEquals(expected, actual)

  @Test def t3(): Unit =
    val expected = Block(
      BlockHeader(0x2f83ae40, Transaction(0x00000000, 0x5303a90e, 50000), 0x8abe9e15, 3),
      List(Transaction(0x790251e0, 0xb1011705, 1000))
    )
    val actual = block2

    assertEquals(expected, actual)

  @Test def t4(): Unit =
    val expected = 0x0dbea380.some
    val actual = Blockchain.verifyChain(List(block2, block1, block0))

    assertEquals(expected, actual)

  @Test def t5(): Unit =
    val expected = none[Hash]
    val actual = Blockchain.verifyChain(List(block1, block2))

    assertEquals(expected, actual)
