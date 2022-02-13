package com.github.rokinsky.blockchain

import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A

class Scope:
  val alice:   Hash = "Alice".toList.hash
  val bob:     Hash = "Bob".toList.hash
  val charlie: Hash = "Charlie".toList.hash
  val satoshi: Hash = "Satoshi".toList.hash

  val tx1:     Transaction = Transaction(alice, bob, 1 * Blockchain.Coin)
  val block0:  Block       = Blockchain.mineBlock(satoshi, 0, Nil)
  val genesis: Block       = block0
  val block1:  Block       = Blockchain.mineBlock(alice, genesis.hash, Nil)
  val block2:  Block       = Blockchain.mineBlock(charlie, block1.hash, List(tx1))
