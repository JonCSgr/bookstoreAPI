package booksdb

import doobie._
import doobie.implicits._


case class Book(id: String, title: String, year: Int, cost: Int)

final case class BookPatch(title: Option[String], year: Option[Int], cost: Option[Int])

class Bookdb {

  private implicit val bigIntGet: Get[BigInt] = Get[Long].map(BigInt.apply)

  def addBookQuery(id: String, title: String, year: Int, cost: Int): Update0 =
    sql"insert into books (id, title, year, cost) values ($id, $title,$year,$cost)".update

  def addBookQuery(book: Book): Update0 =
    sql"insert into books (id, title, year, cost) values (${book.id}, ${book.title},${book.year},${book.cost})".update

  def updateBookQuery(id: String, title: String, year: Int, cost: Int): doobie.Update0 =
    sql"""
    update books set title = $title, year = $year, cost =  $cost where id = $id""".update

  def patchBookQuery(id: String, bookPatch: BookPatch): Update0 = {
    val setBooksFragment = sql"update books set "
    val updateTitleFragment = if (bookPatch.title.isDefined) fr"title = ${bookPatch.title.get}" else fr""
    val titleComma = if ((bookPatch.year.isDefined || bookPatch.cost.isDefined) && bookPatch.title.isDefined) fr"," else fr""
    val updateYearFragment = if (bookPatch.year.isDefined) fr"year = ${bookPatch.year.get}" else fr""
    val yearComma = if (bookPatch.cost.isDefined && bookPatch.year.isDefined) fr"," else fr""
    val updateCostFragment = if (bookPatch.cost.isDefined) fr"cost = ${bookPatch.cost.get}" else fr""
    val whereBooksFragment = fr"where books.id = $id"
    val patchBooksQuery = setBooksFragment ++ updateTitleFragment ++ titleComma ++ updateYearFragment ++ yearComma ++ updateCostFragment ++ whereBooksFragment
    patchBooksQuery.update
  }

  def deleteBookQuery(id: String): doobie.Update0 =
    sql"DELETE FROM books WHERE id = $id".update

  def retrieveInfo(id: String): doobie.Query0[Book] = sql"""
                                                           |select title, year, cost
                                                           |from books
                                                           |where id = ${id}
                                                           | """.stripMargin.query[Book]

  def countBooks: doobie.Query0[BigInt] =
    sql"""
    select count(*) from books""".query

  val queryForBooks: doobie.Query0[Book] = sql"select id, title, year, cost from books".query[Book]


}