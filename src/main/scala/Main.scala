import blockchain.HashTree

@main def hello(): Unit =
  println(HashTree.buildTree("fubar".toList))
  println(msg)

def msg = "I was compiled by Scala 3. :)"
