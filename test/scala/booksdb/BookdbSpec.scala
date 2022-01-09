package booksdb

import cats.effect.{Blocker, IO}
import doobie.Transactor
import doobie.util.ExecutionContexts
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{forAll, _}


class BookdbSpec extends AnyWordSpecLike with Matchers with doobie.scalatest.IOChecker {
  implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

  override def transactor: doobie.Transactor[IO] =
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver", // driver classname
      "jdbc:postgresql:booksdb", // connect URL (driver-specific)
      "postgres", // user
      "123", // password
      Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
    )

  private val bookdb = new Bookdb
  "Bookdb" should {
    "check the query" in {
      val bookdb = new Bookdb
      check(bookdb.queryForBooks)
    }
    /*  "check the query for specific year range in a book [MIN,MAX]" in {
      val bookdb = new Bookdb
      check(bookdb.olderThan(1900 to 2000))
    }
*/
    "check the query for counting total books" in {
      val bookdb = new Bookdb
      check(bookdb.countBooks)
    }

    "check the query for updating a book" in {
      val bookdb = new Bookdb
      check(bookdb.updateBookQuery("", "", 0, 0))
    }

    "check the query for inserting book" in {
      val bookdb = new Bookdb
      check(bookdb.addBookQuery(Book("", "", 0, 0)))
    }

    "check the query for deleting book" in {
      val bookdb = new Bookdb
      check(bookdb.deleteBookQuery(""))
    }

    /*
    "check the query for patching a book" in {
      val bookPatch = BookPatch(Some(""), None, None)
      check(bookdb.patchBookQuery("", bookPatch))
    }

    "check the query for patching a book only year parameter" in {
      val bookPatch = BookPatch(None, Some(21), None)
      check(bookdb.patchBookQuery("", bookPatch))
    }
*/

    "check the patch query " in {
      forAll { (title: Option[String], year: Option[Int], cost: Option[Int]) =>
        whenever(title.isDefined || year.isDefined || cost.isDefined) {
          check(bookdb.patchBookQuery("", BookPatch(title, year, cost)))
        }
      }
    }
  }
}