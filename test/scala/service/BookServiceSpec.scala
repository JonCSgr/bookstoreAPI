package service

import booksdb.{BookPatch, Bookdb}
import cats.effect.{Blocker, IO}
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._


class BookServiceSpec extends AnyWordSpecLike with Matchers {
  implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

  implicit private val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", // driver classname
    "jdbc:postgresql:booksdb", // connect URL (driver-specific)
    "postgres", // user
    "123", // password
    Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
  )

  private val bookdb = new Bookdb()
  private val bookService = new BookService(bookdb)
  private val testBook = BookPatch(Some("Donnie Darko"), Some(1990), Some(1))

  "BookService" should {
    "coordinate patchquery " in {
      forAll { (title: Option[String], year: Option[Int], cost: Option[Int]) =>
        bookService.patchBook("1", testBook).map { strings => if (title.isEmpty && year.isEmpty && cost.isEmpty) (strings should have size 0) else (strings.size shouldBe >=(0)) }
      }
    }
  }
}
