package com.condenast.ml.classifier

import scala.collection.immutable.List
import com.condenast.ml.knowledge.Repository

class TWCNBayesClassifier extends BayesClassifier {

  def keywordProbability(features: List[(String, Int)], keyword: String, repository: Repository): Double = {
    0.0d
  }

  def keywordProbabilitySmallScale(features: List[(String, Int)], keyword: String, repository: Repository): Double = {
    keywordProbability(features, keyword, repository)
  }
  
  private def termProbability(repository: Repository, term: String, keyword: String): Double = {
    var dij: Double = repository.termStatistic(term) match {
      case (0, 0) => 0
      case (d, c) => d
    }
    
    dij = scala.math.log(dij + 1)
    
    0.0
    //(tk + 1).floatValue / (repository.keywordStatistic(keyword)._2 + repository.terms.size)
  }
}