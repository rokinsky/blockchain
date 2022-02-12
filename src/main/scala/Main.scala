
import cats.Show.catsShowForChar
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{Applicative, Functor, Monad}
import com.github.rokinsky.blockchain.{HashTree, MerkleProof}

object Main extends IOApp.Simple {
  def app[F[_]: Console: Monad]: F[Unit] =
    for
      _ <- HashTree
        .of("fubar".toList)
        .traverse_(tree => Console[F].println(tree.show))
      _ <- HashTree
        .of("bitcoin".toList)
        .map(tree => MerkleProof.merklePaths('i', tree))
        .traverse_(merklePaths =>
          merklePaths.traverse(merklePath =>
            Console[F].println(MerkleProof.showMerklePath(merklePath))
          )
        )
    yield ()

  val run: IO[Unit] = app[IO]
}
