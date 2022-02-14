package com.github.rokinsky.blockchain

import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A

class Scope:
  val alice:   Hash = "Alice".toList.hash
  val bob:     Hash = "Bob".toList.hash
  val charlie: Hash = "Charlie".toList.hash
  val satoshi: Hash = "Satoshi".toList.hash

  val tx1: Transaction = Transaction(alice, bob, 1 * Chain.Coin)

  def chain: Chain =
    Chain.of(satoshi).mineBlock(alice).addTransaction(tx1).mineBlock(charlie)

  val block2 :: block1 :: block0 :: Nil = chain.chain
