package blockchain

import blockchain.HashTree
import blockchain.Hashable.hashableSeqA
import blockchain.Transaction.Coin
import cats.syntax.option.*
import org.junit.Assert.*
import org.junit.Test

class TransactionReceiptSpec:
  // TODO: move to Scope
  val alice: Hash = "Alice".toList.hash
  val bob: Hash = "Bob".toList.hash
  val charlie: Hash = "Charlie".toList.hash
  val satoshi: Hash = "Satoshi".toList.hash

  val tx1: Transaction = Transaction(alice, bob, 1 * Coin)
  val block0: Block = Block.mineBlock(satoshi, 0, Nil)
  val genesis: Block = block0
  val block1: Block = Block.mineBlock(alice, genesis.hash, Nil)
  val block2: Block = Block.mineBlock(charlie, block1.hash, List(tx1))

  @Test def t1(): Unit =
    val (block, receipt :: Nil) = TransactionReceipt.mineTransactions(
      charlie,
      block1.hash,
      List(tx1)
    )
    val actual = TransactionReceipt.validateReceipt(receipt, block.blockHdr)
    val expected = true

    assertEquals(expected, actual)
