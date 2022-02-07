import blockchain.HashTree
import blockchain.HashTree.given_Show_HashTree
import cats.Show.catsShowForChar
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Applicative, Functor}

object Main extends IOApp.Simple {
  def app[F[_]: Console: Applicative: Functor]: F[Unit] =
    HashTree
      .buildTree("fubar".toList)
      .traverse_(tree => Console[F].println(tree.show))

  val run: IO[Unit] = app[IO]

  def msg = "I was compiled by Scala 3. :)"
}
