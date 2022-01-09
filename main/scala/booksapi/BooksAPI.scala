package booksapi

import booksdb.{Book, BookPatch, Bookdb}
import cats.effect._
import cats.syntax.all._
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import io.circe.generic.auto._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import service.BookService
import sttp.tapir._
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.http4s.SwaggerHttp4s

import scala.concurrent.ExecutionContext

object BooksAPI extends IOApp {
  private implicit val ioConcurrent: ConcurrentEffect[IO] = IO.ioConcurrentEffect
  implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

  val transactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      be <- Blocker[IO] // our blocking EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver", // driver classname
        "jdbc:postgresql:booksdb", // connect URL (driver-specific)
        "postgres", // user
        "123", // password
        ce, // await connection here
        be // execute JDBC operations here
      )
    } yield xa


  //GET /books?year=...&limit=...
  private val getBooksEndPoint: Endpoint[(Option[Int], Option[Int], Option[Int]), String, List[Book], Any]
  =
    endpoint.get
      .in("books")
      .in(query[Option[Int]]("year"))
      .in(query[Option[Int]]("limit"))
      .in(query[Option[Int]]("cost"))
      .errorOut(stringBody)
      .out(jsonBody[List[Book]])

  //GET /book/UID
  private val getBookEndpoint: Endpoint[String, String, Book, Any]
  =
    endpoint.get
      .in("books")
      .in(path[String]("bookId"))
      .errorOut(stringBody)
      .out(jsonBody[Book])

  private val addBookEndpoint: Endpoint[Book, String, String, Any] =
    endpoint.post
      .in("books") // -> localhost:8080/books/add
      .in(jsonBody[Book]
        .description("The book to add")
        .example(Book("oXod33", "Donnie Darko", 1999, 30)))
      .errorOut(stringBody)
      .out(jsonBody[String])

  private val updateBookEndpoint: Endpoint[(String, Book), String, String, Any] =
    endpoint.put
      .in("books")
      .in(path[String]("bookId"))
      .in(jsonBody[Book])
      .errorOut(stringBody)
      .out(jsonBody[String])

  private val deleteBookEndpoint: Endpoint[String, String, String, Any] =
    endpoint.delete
      .in("books")
      .in(path[String]("bookId"))
      .errorOut(stringBody)
      .out(jsonBody[String])

  private val patchBookEndpoint: Endpoint[(String, BookPatch), String, String, Any] =
    endpoint.patch
      .in("books")
      .in(path[String]("bookId"))
      .in(jsonBody[BookPatch])
      .errorOut(stringBody)
      .out(jsonBody[String])

  override def run(args: List[String]): IO[ExitCode] = transactor.use { implicit xa =>
    val bookdbAdmin = new Bookdb()
    val bookService = new BookService(bookdbAdmin)

    val getBooksRoute = getBooksEndPoint.serverLogic[IO] {
      case (year: Option[Int], limit: Option[Int], cost: Option[Int]) =>
        bookService.myBooks1.map(_.map(_
          .filter(book => if (year.isDefined) book.year == year.get else if (cost.isDefined) book.cost == cost.get else true)))
    }

    val getBookRoute = getBookEndpoint.serverLogic[IO] {
      id =>
        bookService.myBooks1.map(_.map(_ //something => something (duplicate something)
          //something => something (duplicate something) // MAP -> IO , FLATMAP -> EITHER OF LIST OF BOOKS | FUNCTION FROM A LIST OF BOOKS TO EITHER OF BOOK
          .collectFirst { //MAP :         Either List of Books    | Function from a list of books to a book
            case book if book.id == id => book
          }.toRight("no books found")).flatten)
    }

    val addBookRoute = addBookEndpoint.serverLogic[IO] {
      book =>
        bookService.insertDoTransact(book).map(ids => ids.headOption.toRight("book not inserted"))
    }

    val updateBookRoute = updateBookEndpoint.serverLogic[IO] {
      case (id, book) =>
        bookService.updateDoTransact(id, book.title, book.year, book.cost).map(ids => ids.headOption.toRight("book not updated"))
    }

    val patchBookRoute = patchBookEndpoint.serverLogic[IO] {
      case (id, bookPatch: BookPatch) =>
        bookService.patchBook(id, bookPatch).map(ids => ids.headOption.toRight("book not patched"))
    }

    val deleteBookRoute = deleteBookEndpoint.serverLogic[IO] {
      id =>
        bookService.deleteDoTransact(id).map(ids => ids.headOption.toRight("book not deleted"))
    }

    val booksRoute = Http4sServerInterpreter().toRoutes(List(getBooksRoute, getBookRoute, addBookRoute, updateBookRoute, deleteBookRoute, patchBookRoute))

    val docs = OpenAPIDocsInterpreter()
      .toOpenAPI(List(getBooksEndPoint, getBookEndpoint, addBookEndpoint, updateBookEndpoint, deleteBookEndpoint, patchBookEndpoint), "My Bookshop", "1.0")
      .toYaml

    val openApiRoute = new SwaggerHttp4s(docs).routes
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(8080, "localhost")
      .withHttpApp((openApiRoute <+> booksRoute).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
