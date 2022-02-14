package com.github.rokinsky.blockchain

import com.github.rokinsky.blockchain.Chain.Miner

final case class TransactionReceipt(
  blockHash: Hash,
  proof:     MerkleProof[Transaction],
)

object TransactionReceipt:
  extension (transactionReceipt: TransactionReceipt)
    def isValid(blockHeader: BlockHeader): Boolean =
      transactionReceipt.blockHash == blockHeader.hash &&
        MerkleProof.verifyProof(blockHeader.txRoot, transactionReceipt.proof)

  def of(block: Block, transaction: Transaction): Option[TransactionReceipt] =
    for
      tree  <- HashTree.of(block.header.coinbase :: block.transactions)
      proof <- MerkleProof.of(transaction, tree)
    yield TransactionReceipt(block.header.hash, proof)
