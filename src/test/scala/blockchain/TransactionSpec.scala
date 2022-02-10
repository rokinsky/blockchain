package blockchain

import blockchain.HashTree
import blockchain.Hashable.hashableSeqA
import cats.syntax.option.*
import cats.syntax.show.*
import org.junit.Assert.*
import org.junit.Test

class TransactionSpec:
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

  @Test def tt(): Unit =
    val expected = """hash: 0x70b432e0
                     |parent: 0x00000000
                     |miner: 0x7203d9df
                     |root: 0x5b10bd5d
                     |nonce: 18
                     |Tx# 0x5b10bd5d from: 0x00000000 to: 0x7203d9df amount: 50000
                     |hash: 0x2f83ae40
                     |parent: 0x70b432e0
                     |miner: 0x790251e0
                     |root: 0x5ea7a6f0
                     |nonce: 0
                     |Tx# 0x5ea7a6f0 from: 0x00000000 to: 0x790251e0 amount: 50000
                     |hash: 0x0dbea380
                     |parent: 0x2f83ae40
                     |miner: 0x5303a90e
                     |root: 0x8abe9e15
                     |nonce: 3
                     |Tx# 0xbcc3e45a from: 0x00000000 to: 0x5303a90e amount: 50000
                     |Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000""".stripMargin
    val actual = PPrint.pprV(List(block0, block1, block2))

    assertEquals(expected, actual)
