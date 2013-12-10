package com.condenast.ml.knowledge

import scala.math.Ordering.Implicits._
import scala.collection.mutable.PriorityQueue

case class Term(var docCount: Int, var count: Int)
case class Keyword(var docCount: Int, var termCount: Int, var terms: Map[String, TermWeight])
case class TermWeight(docCount: Int, count: Int, var score: Double = 0.0)

class DefaultRepository extends Repository {

  private var termMap: Map[String, Term] = Map()
  private var keywordMap: Map[String, Keyword] = Map()
  private var docCount = 0

  def addDoc(docId: String) = docCount += 1

  def increaseDocCountForKeyword(keyword: String): Int = {
    keywordMap.get(keyword) match {
      case None => keywordMap = keywordMap + (keyword -> Keyword(1, 0, Map()))
      case Some(Keyword(d, t, tl)) => keywordMap = keywordMap + (keyword -> Keyword(d + 1, t, tl))
    }
    keywordMap(keyword).docCount
  }

  def increaseTermCount(term: String, count: Int): Int = {
    termMap.get(term) match {
      case None => termMap = termMap + (term -> Term(1, count))
      case Some(Term(d, c)) => termMap = termMap + (term -> Term(d + 1, c + count))
    }
    termMap(term).count
  }

  def increaseTermCountForKeyword(keyword: String)(term: String, count: Int): Int = {
    keywordMap.get(keyword) match {
      case None => keywordMap = keywordMap + (keyword -> Keyword(1, count, Map(term -> TermWeight(1, count))))
      case Some(Keyword(d, t, tm)) =>
        tm.get(term) match {
          case None => keywordMap = keywordMap + (keyword -> Keyword(d, t + count, tm + (term -> TermWeight(1, count))))
          case Some(TermWeight(td, tc, ts)) => keywordMap = keywordMap + (keyword -> Keyword(d, t + count, tm + (term -> TermWeight(td + 1, tc + count, ts))))
        }
    }
    keywordMap(keyword).terms(term).count
  }

  def termForKeywordStaticstic(keyword: String, term: String): (Int, Int) =
    keywordMap.get(keyword) match {
      case None => (0, 0)
      case Some(Keyword(d, t, tm)) =>
        tm.get(term) match {
          case None => (0, 0)
          case Some(TermWeight(td, tc, ts)) => (td, tc)
        }
    }

  def keywordStatistic(keyword: String): (Int, Int) =
    keywordMap.get(keyword) match {
      case None => (0, 0)
      case Some(Keyword(d, t, tm)) => (d, t)
    }

  def termStatistic(term: String): (Int, Int) =
    termMap.get(term) match {
      case None => (0, 0)
      case Some(Term(d, c)) => (d, c)
    }

  def totalDocCount = docCount
  def keywords: Iterable[String] = keywordMap.keys
  def terms: Iterable[String] = termMap.keys

  def calibrate = {
    val mi: (Double, Double, Double, Double, Double) => Double = { (n, n11, n10, n01, n00) =>
      val l2: Double => Double = x => scala.math.log(x) / scala.math.log(2)
      (n11 / n) * l2((n * n11) / ((n11 + n10) * (n11 + n01))) +
        (n01 / n) * l2((n * n01) / ((n01 + n00) * (n01 + n11))) +
        (n10 / n) * l2((n * n10) / ((n11 + n10) * (n10 + n00))) +
        (n00 / n) * l2((n * n00) / ((n01 + n00) * (n10 + n00)))
    }

    val score: ((String, TermWeight)) => Double = t => t._2.score

    // for each keyword, reduce noise by removing statistically irrelevant terms
    keywordMap map {
      case (kkk, keyword) =>
        val termPriorities = new PriorityQueue[(String, TermWeight)]()(Ordering.by(score))

        // calculate MI for each term and sort them from highest to lowest
        keyword.terms map {
          case (t, tw) =>
            val term = termMap(t)
            val n = docCount
            val n11 = tw.docCount
            val n10 = term.docCount - tw.docCount
            val n01 = keyword.docCount - tw.docCount
            val n00 = docCount - term.docCount - keyword.docCount + tw.docCount
            tw.score = if (n01 != 0) mi(n, n11, n10, n01, n00) else 0.99999
            termPriorities.enqueue(t -> tw)
        }

        // pull the highest scored terms, up to 1,000 terms, from the priority queue
        var newTerms = Map[String, TermWeight]()
        val i = termPriorities.iterator
        var c = 0
        while (i.hasNext && c < 1000) {
          val (t, tw) = termPriorities.dequeue
          newTerms = newTerms + (t -> tw)
          c += 1
        }

        keyword.terms = newTerms
    }
  }

  override def toString: String = {
    "DocCount=%s\nTerms=%s\nKeywords=%s\n".format(docCount, termMap.size, keywordMap.size)
  }
}

object DefaultRepository {

  def apply() = new DefaultRepository

  def apply(repositoryDir: String): Repository = {
    val repository = new DefaultRepository

    repository.keywordMap = Map()

    Storage.loadItems(repositoryDir + "/index") foreach { item =>
      item match {
        case ValueItem("docCount", values) => repository.docCount = values(0).toInt
        case CollectionEntryItem(values) =>
          if (values.length != 2)
            throw new RuntimeException("Invalid index file: " + values)
          val k = values(0)
          val fileName = "k_" + values(1).toInt + ".txt"
          val keyword = Keyword(0, 0, Map())

          Storage.loadItems(repositoryDir + "/" + fileName).foreach { keywordItem =>
            keywordItem match {
              case ValueItem("docCount", values) => keyword.docCount = values(0).toInt
              case ValueItem("termCount", values) => keyword.termCount = values(0).toInt
              case CollectionEntryItem(values) => keyword.terms = keyword.terms + (values(0) -> TermWeight(values(1).toInt, values(2).toInt))
            }
          }

          repository.keywordMap = repository.keywordMap + (k -> keyword)
      }
    }

    repository.termMap = Map()
    Storage.loadItems(repositoryDir + "/terms.txt") foreach { item =>
      item match {
        case CollectionEntryItem(values) =>
          val t = values(0)
          val term = Term(values(1).toInt, values(2).toInt)
          repository.termMap = repository.termMap + (t -> term)
      }
    }

    repository.calibrate
    save(repository, "/tmp/repo_calibrated")
    repository
  }

  def save(repository: DefaultRepository, repositoryDir: String): Unit = {
    var index = 1
    var items = List[FileItem](ValueItem("docCount", Array[String](repository.docCount.toString)))

    repository.keywordMap foreach { case (k, keyword) =>
      items = CollectionEntryItem(Array[String](k, index.toString)) :: items

      var keywordItems = List[FileItem](
        ValueItem("docCount", Array[String](keyword.docCount.toString)),
        ValueItem("termCount", Array[String](keyword.termCount.toString)))

      val score: ((String, TermWeight)) => Double = t => t._2.score
      val termPriorities = new PriorityQueue[(String, TermWeight)]()(Ordering.by(score))
      keyword.terms map {
        case (t, w) => termPriorities.enqueue((t, w))
      }

      val i = termPriorities.iterator
      while (i.hasNext) {
        termPriorities.dequeue match {
          case (t, TermWeight(d, c, s)) =>
            val entry = if (s > 0.0)
              CollectionEntryItem(Array[String](t, d.toString, c.toString, "%15.14f".format(s)))
            else
              CollectionEntryItem(Array[String](t, d.toString, c.toString))
            keywordItems = entry :: keywordItems
        }
      }
      Storage.saveItems(repositoryDir + "/k_" + index + ".txt", keywordItems.reverse)
      index += 1
    }

    Storage.saveItems(repositoryDir + "/index", items.reverse)

    // term file
    items = List[FileItem]()
    repository.termMap map { case (t, Term(d, c)) => items = CollectionEntryItem(Array[String](t, d.toString, c.toString)) :: items }
    Storage.saveItems(repositoryDir + "/terms.txt", items.reverse)
  }

}