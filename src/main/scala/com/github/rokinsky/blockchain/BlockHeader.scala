package com.github.rokinsky.blockchain

import cats.Show
import cats.syntax.semigroup.*
import cats.syntax.show.*
import com.github.rokinsky.blockchain.Blockchain.{Amount, Coin}
import com.github.rokinsky.blockchain.Hash.given_Show_Hash
import com.github.rokinsky.blockchain.Hashable.given_Hashable_Seq_A
import com.github.rokinsky.blockchain.PPrint.given_Show_String_A

final case class BlockHeader(
  parent:   Hash,
  coinbase: Transaction,
  txRoot:   Hash, // root of the Merkle tree
  nonce:    Hash
)

object BlockHeader:

  def validNonce(blockHeader: BlockHeader): Boolean =
    blockHeader.hash % math.pow(2, Blockchain.Difficulty) == 0

  given Hashable[BlockHeader] with
    extension (blockHeader: BlockHeader)
      def hash: Hash =
        List(blockHeader.parent, blockHeader.coinbase.hash, blockHeader.txRoot, blockHeader.nonce).hash

  given Show[BlockHeader] with
    def show(blockHeader: BlockHeader): String = PPrint.pprV(
      List(
        ("hash", blockHeader.hash).show,
        ("parent", blockHeader.parent).show,
        ("miner", blockHeader.coinbase.receiver).show,
        ("root", blockHeader.txRoot).show,
        ("nonce", blockHeader.nonce.toString).show
      )
    )
