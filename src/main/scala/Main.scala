import blockchain.HashTree
import cats.Show.catsShowForChar

@main def hello(): Unit =
  println(HashTree.buildTree("fubar".toList).map(HashTree.drawTree))
  println(msg)

def msg = "I was compiled by Scala 3. :)"
