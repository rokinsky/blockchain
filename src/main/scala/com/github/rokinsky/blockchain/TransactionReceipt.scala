package com.github.rokinsky.blockchain

import com.github.rokinsky.blockchain.Blockchain.Miner

final case class TransactionReceipt(
  blockHash: Hash,
  proof: MerkleProof[Transaction],
)

object TransactionReceipt:
  def of(block: Block, transaction: Transaction): Option[TransactionReceipt] =
    HashTree
      .buildTree(block.header.coinbase :: block.transactions)
      .flatMap(MerkleProof.buildProof(transaction, _))
      .map(TransactionReceipt(block.header.hash, _))
