package com.condenast.ml.classifier

import scala.collection.mutable.Map
import com.condenast.ml.corpus.Source
import com.condenast.ml.knowledge.Repository
import com.condenast.ml.util.Logger

abstract class BayesClassifier {

  def train(source: Source, repository: Repository): Repository = {
    val it = source.iterator
    while (it.hasNext) {
      val doc = it.next
      if (doc.valid) {
        repository.addDoc(doc.id)

        val keywords = doc.getKeywords
        keywords foreach { k => repository.increaseDocCountForKeyword(k) }

        doc.getTerms foreach { t =>
          keywords foreach { k => repository.increaseTermCountForKeyword(k)(t._1, t._2) }
          repository.increaseTermCount(t._1, t._2)
        }
      }
    }
    repository
  }

  def keywordProbability(features: List[(String, Int)], keyword: String, repository: Repository): Double
  def keywordProbabilitySmallScale(features: List[(String, Int)], keyword: String, repository: Repository): Double
}

object BayesClassifier {
  def apply() = new MultinomialBayesClassifier()
}

class MultinomialBayesClassifier extends BayesClassifier {

  override def keywordProbability(features: List[(String, Int)], keyword: String, repository: Repository): Double = {
    val l2: Double => Double = x => scala.math.log(x) / scala.math.log(2)
    val p = features.foldLeft[Double](0.0) { (v, t) =>
      val p = t._2 * l2(termProbabilityMultinomial(repository, t._1, keyword))
      Logger().debug("P(%s|%s) = %20.15f - %d".format(t._1, keyword, v + p, t._2))
      v + p
    }
    val c = repository.keywordStatistic(keyword)._1.floatValue / repository.totalDocCount
    Logger().debug("%s: %20.15f + %20.15f".format(keyword, scala.math.log(c), p))
    l2(c) + p
  }

  override def keywordProbabilitySmallScale(features: List[(String, Int)], keyword: String, repository: Repository): Double = {
    val p = features.foldLeft[Double](1.0) { (v, t) =>
      val p = (1 to t._2).foldLeft[Double](1.0) { (v2, _) => v2 * termProbabilityMultinomial(repository, t._1, keyword) }
      Logger().debug("P(%s|%s) = %20.15f - %d".format(t._1, keyword, v * p, t._2))
      v * p
    }
    val c = repository.keywordStatistic(keyword)._1.floatValue / repository.totalDocCount
    Logger().debug("%s: %20.15f + %20.15f".format(keyword, c, p))
    c * p
  }

  private def termProbabilityMultinomial(repository: Repository, term: String, keyword: String): Double = {
    val tk = repository.termForKeywordStaticstic(keyword, term) match {
      case (0, 0) => 0
      case (d, c) => c
    }
    (tk + 1).floatValue / (repository.keywordStatistic(keyword)._2 + repository.terms.size)
  }
}

class BernoulliBayesClassifier extends BayesClassifier {

  override def keywordProbability(features: List[(String, Int)], keyword: String, repository: Repository): Double = {

    val docProbBernoulli: (List[(String, Int)], String) => Double = { (l, k) =>
      val p = l.foldLeft[Double](0.0) { (v, t) =>
        v + scala.math.log(termProbabilityBernoulli(repository, t._1, k))
      }
      val c = repository.keywordStatistic(k)._1.floatValue / repository.totalDocCount
      Logger().debug("%s: %20.15f + %20.15f".format(k, scala.math.log(c), p))
      scala.math.log(c) + p
    }

    docProbBernoulli(features, keyword)
  }

  override def keywordProbabilitySmallScale(features: List[(String, Int)], keyword: String, repository: Repository): Double = {

    val docProbBernoulli: (List[(String, Int)], String) => Double = { (l, k) =>
      val p = l.foldLeft[Double](1.0) { (v, t) =>
        v * termProbabilityBernoulli(repository, t._1, k)
      }

      val f = l map { case (t, c) => t }
      val ex = repository.terms filter { t => !f.contains(t) }
      val p2 = ex.foldLeft[Double](1.0) { (v, t) =>
        v * (1 - termProbabilityBernoulli(repository, t, k))
      }

      val c = repository.keywordStatistic(k)._1.floatValue / repository.totalDocCount

      Logger().debug("%s: %20.15f * %20.15f * %20.15f: %d %d".format(k, c, p, p2, repository.keywordStatistic(k)._1, repository.totalDocCount))
      c * p * p2
    }

    docProbBernoulli(features, keyword)
  }

  private def termProbabilityBernoulli(repository: Repository, term: String, keyword: String): Double = {
    val tk = repository.termForKeywordStaticstic(keyword, term) match {
      case (0, 0) => 0
      case (d, c) => d
    }
    val p = (tk + 1).floatValue / (repository.keywordStatistic(keyword)._1 + 2)
    p
  }
}