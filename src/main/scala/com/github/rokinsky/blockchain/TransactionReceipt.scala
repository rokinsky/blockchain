package com.github.rokinsky.blockchain

import com.github.rokinsky.blockchain.Blockchain.Miner

final case class TransactionReceipt(
  blockHash: Hash,
  proof: MerkleProof[Transaction],
)

object TransactionReceipt:
  def of(block: Block, transaction: Transaction): Option[TransactionReceipt] =
    for
      tree <- HashTree.of(block.header.coinbase :: block.transactions)
      proof <- MerkleProof.of(transaction, tree)
    yield TransactionReceipt(block.header.hash, proof)
