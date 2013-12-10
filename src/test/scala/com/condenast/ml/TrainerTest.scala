package com.condenast.ml

import org.scalatest.FunSuite
import scala.collection.mutable.Map
import com.condenast.ml.corpus.Source
import com.condenast.ml.corpus.SourceDocument
import com.condenast.ml.classifier.BayesClassifier
import com.condenast.ml.knowledge.EnhancedRepository
import com.condenast.ml.util.Logger
import com.condenast.ml.util.LoggingLevel

class TranerTest extends FunSuite {
  test("Train and test a Bayer Classifier") {
    val repository = EnhancedRepository()
    val classifier = BayesClassifier()
    classifier.train(TestSource, repository)
    
    Logger.setLevel(LoggingLevel.DEBUG)
    val testData = generateTest("Chinese Chinese Chinese Tokyo Japan")
    val p1 = classifier.keywordProbabilitySmallScale(testData, "yes", repository)
    val p2 = classifier.keywordProbabilitySmallScale(testData, "no", repository)
    Logger().info("yes=%8.6f no=%8.6f".format(p1, p2))
    assert(p1 > p2, p1 + " is not greater than " + p2)
  }
 
  def generateTest(line: String): List[(String, Int)] = {
    val termMap = Map[String, Int]()
    line.split("\\s+").view.foreach { t =>
      termMap.get(t) match {
        case None => termMap(t) = 1
        case Some(v) => termMap(t) = v + 1
      }
    }
    termMap.view.toList
  }
}

object TestSource extends Source {
  val source = List(TestSourceDoc("Chinese Beijing Chinese", "yes"),
    TestSourceDoc("Chinese Chinese Shanghai", "yes"),
    TestSourceDoc("Chinese Macao", "yes"),
    TestSourceDoc("Tokyo Japan Chinese", "no"))
  def iterator: Iterator[SourceDocument] = source.iterator
}

case class TestSourceDoc(line: String, val keyword: String) extends SourceDocument {
  val termMap = Map[String, Int]()
  line.split("\\s+").view.foreach { t =>
    termMap.get(t) match {
      case None => termMap(t) = 1
      case Some(v) => termMap(t) = v + 1
    }
  }

  def id: String = "0"
  def valid: Boolean = true
  def getKeywords: List[String] = List(keyword)
  def getTerms: List[(String, Int)] = termMap.view.toList
}