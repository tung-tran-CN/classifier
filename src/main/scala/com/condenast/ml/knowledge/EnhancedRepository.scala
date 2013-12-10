package com.condenast.ml.knowledge

import scala.math.Ordering.Implicits._
import scala.collection.mutable.PriorityQueue
import com.condenast.ml.corpus.Source
import com.condenast.ml.corpus.SourceDocument
import com.condenast.ml.util.Logger

case class ETerm(var docCount: Int, var count: Int, var tdocs: Map[String, Int])
case class EKeyword(var docCount: Int, var termCount: Int, var terms: Map[String, ETermWeight], var kdocs: Set[String])
case class ETermWeight(var docCount: Int, var count: Int, var score: Double = 0.0)
case class Document(var sqrtSum: Double, var terms: Map[String, Double])

class EnhancedRepository extends DefaultRepository {
  private var termMap: Map[String, ETerm] = Map()
  private var keywordMap: Map[String, EKeyword] = Map()
  private var docCount = 0

  def train(source: Source): Repository = {
    val it = source.iterator
    var docMap = Map[String, Document]()

    while (it.hasNext) {
      val doc = it.next
      docCount += 1
      val dc = Document(0.0, Map())
      docMap = docMap + (doc.id -> dc)
      if (docCount % 500 == 0) Logger().debug("Reading doc " + doc.id)

      doc.getKeywords foreach { k =>
        val keyword = keywordMap.get(k) match {
          case None => EKeyword(0, 0, Map(), Set())
          case Some(e) => e
        }

        keyword.docCount += 1
        keyword.kdocs = keyword.kdocs + doc.id

        doc.getTerms foreach { t =>
          val tk = keyword.terms.get(t._1) match {
            case None => ETermWeight(0, 0)
            case Some(tw) => tw
          }

          tk.docCount += 1
          tk.count += t._2
          keyword.termCount += t._2

          keyword.terms = keyword.terms + (t._1 -> tk)
        }

        keywordMap = keywordMap + (k -> keyword)
      }

      doc.getTerms foreach { t =>
        val term = termMap.get(t._1) match {
          case None => ETerm(0, 0, Map())
          case Some(et) => et
        }
        term.docCount += 1
        term.count += t._2
        term.tdocs = term.tdocs + (doc.id -> t._2)

        termMap = termMap + (t._1 -> term)
        dc.terms = dc.terms + (t._1 -> t._2)
        dc.sqrtSum += t._2 * t._2
      }

      dc.sqrtSum = Math.sqrt(dc.sqrtSum)
    }

    // step 2 -- calculating dij
    docMap foreach {
      case (id, doc) =>
        doc.terms foreach {
          case (t, d) =>
            var vd: Double = log2(d + 1) * log2(docCount.floatValue / termMap(t).docCount)
            vd = vd / doc.sqrtSum
            doc.terms = doc.terms + (t -> vd) // update doc term with dij
        }
    }

    val s2 = System.currentTimeMillis

    // step 3 -- calculate and save term probability
    keywordMap foreach {
      case (k, keyword) =>
        val excluded = excludedList(docMap.keys, keyword.kdocs)

        keyword.terms foreach {
          case (t, weight) =>
            val s1 = System.currentTimeMillis
            val term = termMap(t)
            val alpha = 1.0
            val ex2 = excludedDocsNotInTerm(term.tdocs.keys, excluded)
            val dd = ex2.foldLeft[Double](alpha) { (d, id) =>
              val doc = docMap(id)
              doc.terms.get(t) match {
                case None => d
                case Some(vd) => d + vd
              }
            }

            val alpha2: Double = termMap.keys.size
            val dd2 = ex2.foldLeft[Double](alpha2) { (d, id) =>
              val doc = docMap(id)
              doc.terms.keys.foldLeft[Double](0.0) { (d, t) =>
                d + doc.terms(t)
              }
            }
            // store the value temporarily for each term
            weight.score = log2(dd / dd2)
        }

        val twci = keyword.terms.values.foldLeft[Double](0.0) { (d, weight) => d + weight.score }
        val s3 = System.currentTimeMillis
        Logger().debug("TP k=%s time=%d twci=%15.14f".format(k, (s3 - s2) / 1000, twci))

        keyword.terms foreach { case (t, weight) => weight.score = weight.score / twci }
    }
    Logger().debug("END training " + docCount)

    this
  }

  private def excludedDocsNotInTerm(l1: Iterable[String], l2: Iterable[String]) =
    l1 filter { t => !(l2.dropWhile(s => s != t) isEmpty) }
   
  private def excludedList(l1: Iterable[String], l2: Iterable[String]) =
    l1 filter { t => l2.dropWhile(s => s != t) isEmpty }

  private def log2(x: Double): Double = scala.math.log(x) / scala.math.log(2)

  override def addDoc(docId: String) = docCount += 1

  override def increaseTermCount(term: String, count: Int): Int = {
    throw new UnsupportedOperationException
  }

  override def increaseTermCountForKeyword(keyword: String)(term: String, count: Int): Int = {
    throw new UnsupportedOperationException
  }

  override def increaseDocCountForKeyword(keyword: String): Int = {
    throw new UnsupportedOperationException
  }

  override def termForKeywordStaticstic(keyword: String, term: String): (Int, Int) =
    keywordMap.get(keyword) match {
      case None => (0, 0)
      case Some(EKeyword(d, t, tm, dl)) =>
        tm.get(term) match {
          case None => (0, 0)
          case Some(ETermWeight(td, tc, ts)) => (td, tc)
        }
    }

  override def keywordStatistic(keyword: String): (Int, Int) =
    keywordMap.get(keyword) match {
      case None => (0, 0)
      case Some(EKeyword(d, t, tm, dl)) => (d, t)
    }

  override def termStatistic(term: String): (Int, Int) =
    termMap.get(term) match {
      case None => (0, 0)
      case Some(ETerm(d, c, dl)) => (d, c)
    }

  override def totalDocCount = docCount
  override def keywords: Iterable[String] = keywordMap.keys
  override def terms: Iterable[String] = termMap.keys

}

object EnhancedRepository {
  def apply() = new EnhancedRepository

  def save(repository: EnhancedRepository, repositoryDir: String) {
    Logger().debug("keywords=%d terms=%d\n".format(repository.keywordMap.keys.size, repository.termMap.keys.size))
    var index = 1
    var items = List[FileItem](ValueItem("docCount", Array[String](repository.docCount.toString)))

    repository.keywordMap foreach {
      case (k, keyword) =>
        items = items :+ CollectionEntryItem(Array[String](k, index.toString))

        var keywordItems = List[FileItem](
          ValueItem("docCount", Array[String](keyword.docCount.toString)),
          ValueItem("termCount", Array[String](keyword.termCount.toString)))

        val score: ((String, ETermWeight)) => Double = t => t._2.score
        val termPriorities = new PriorityQueue[(String, ETermWeight)]()(Ordering.by(score))
        keyword.terms map {
          case (t, w) => termPriorities.enqueue((t, w))
        }

        val i = termPriorities.iterator
        while (i.hasNext) {
          termPriorities.dequeue match {
            case (t, ETermWeight(d, c, s)) =>
              val entry = if (s > 0.0)
                CollectionEntryItem(Array[String](t, d.toString, c.toString, "%15.14f".format(s)))
              else
                CollectionEntryItem(Array[String](t, d.toString, c.toString))
              keywordItems = keywordItems :+ entry
          }
        }
        Logger().debug("Writing terms for keyword=%s index=%d".format(k, index))
        Storage.saveItems(repositoryDir + "/k_" + index + ".txt", keywordItems)
        index += 1
    }

    Storage.saveItems(repositoryDir + "/index", items)

    // term file
    items = List[FileItem]()
    repository.termMap map { case (t, ETerm(d, c, dl)) => items = items :+ CollectionEntryItem(Array[String](t, d.toString, c.toString)) }
    Storage.saveItems(repositoryDir + "/terms.txt", items)
  }
}