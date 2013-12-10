package com.condenast.ml

import scala.collection.mutable.Map
import com.condenast.ml.classifier.BayesClassifier
import com.condenast.ml.knowledge.DefaultRepository
import com.condenast.ml.knowledge.EnhancedRepository
import com.condenast.ml.knowledge.Repository
import scala.util.Sorting
import com.condenast.ml.util.Logger
import com.condenast.ml.util.LoggingLevel
import com.condenast.ml.corpus.SolrSource

object DefaultTrainingTest {

  def _main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("No arguments")
      System.exit(1)
    }

    Logger.setLevel(LoggingLevel.DEBUG)

    args(0) match {
      //case "solr" => testFeatureSet(solrSource.readDoc(solrSource.findDoc(args(1))), repository)
      case "train" => train(args(1)) 
      case "test" => testFeatureSet(readTestData(args(2)), loadRepository(args(1)))
      case "class" => testFeatureSet(readTestData(args(2)), loadRepository(args(1)), args(3))
      case "load" => loadRepository(args(1))
      case _ => throw new IllegalArgumentException
   }
  }
  
  def train(repoDir: String): Unit = {
    val repository = DefaultRepository()
    val classifier = BayesClassifier()
    classifier.train(SolrSource("/Users/ttran2/tmp/qa_index", "content_terms"), repository)
    //classifier.train(SolrSource("/Users/ttran2/tmp/qa_index", "headertext_t"), repository)
    
    if (repoDir != null)
      DefaultRepository.save(repository, repoDir)
  }

  def loadRepository(repoDir: String): Repository = {
	val repository = DefaultRepository(repoDir)
	println(repository.toString)
	repository
  }
  
  def testFeatureSet(features: List[(String, Int)], repository: Repository) = {

    val classifier = BayesClassifier()
    
    val result: Array[(String, Double)] = 
      repository.keywords map { k => (k, classifier.keywordProbability(features, k, repository)) } toArray

    Sorting.quickSort(result)(new Ordering[(String, Double)] {
      def compare(a: (String, Double), b: (String, Double)) = a._2 compare b._2
    })

    for (r <- result)
      Logger().info("%s: %30.28f".format(r._1, r._2))
  }
  
  def testFeatureSet(features: List[(String, Int)], repository: Repository, keyword: String) = {

    val classifier = BayesClassifier()
    
    repository.keywords.find { k => k == keyword} match {
      case None => println(keyword + "isn't a valid keyword")
      case Some(k) =>       
      	val p = classifier.keywordProbability(features, k, repository)
      	Logger().info("%s: %30.28f".format(k, p))
    }
  }
  
  def readTestData(fname: String): List[(String, Int)] = {
	  SolrSource.loadFeatures(fname)
  }
}