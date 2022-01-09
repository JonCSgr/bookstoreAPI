package service

import booksdb.{Book, BookPatch, Bookdb}
import cats.effect._
import cats.implicits.catsSyntaxEitherId
import doobie._
import doobie.implicits._
import cats.syntax.all._

//dependencie injection,single responsibility, solid

class BookService[F[_]](bookDbAdmin: Bookdb)(implicit contextShift: ContextShift[F], xa: Transactor[F], b: BracketThrow[F]) {

  def patchBook(id: String, bookPatch: BookPatch): F[List[String]] = {
    if (bookPatch.title.isEmpty && bookPatch.year.isEmpty && bookPatch.cost.isEmpty)
      b.pure(Nil)
    else
      patchDoTransact(id, bookPatch)
  }

  def insertDoTransact(book: Book): F[List[String]] =
    bookDbAdmin.addBookQuery(book).withGeneratedKeys[String]("id").compile.toList.transact(xa)

  def deleteDoTransact(id: String): F[List[String]] =
    bookDbAdmin.deleteBookQuery(id).withGeneratedKeys[String]("id").compile.toList.transact(xa)

  def updateDoTransact(id: String, title: String, year: Int, cost: Int): F[List[String]] =
    bookDbAdmin.updateBookQuery(id, title, year, cost).withGeneratedKeys[String]("id").compile.toList.transact(xa)

  def patchDoTransact(id: String, bookPatch: BookPatch): F[List[String]] =
    bookDbAdmin.patchBookQuery(id, bookPatch).withGeneratedKeys[String]("id").compile.toList.transact(xa)

  val myBooks1: F[Either[String, List[Book]]] = bookDbAdmin.queryForBooks.to[List].transact(xa).map(_.asRight[String])
}












