package com.condenast.ml

import org.apache.lucene.index._
import org.apache.lucene.store._
import org.apache.lucene.document._
import java.io.File
import scala.collection.immutable.SortedSet
import scala.collection.mutable.Map
import scala.util.Sorting

object SavedClassifier {

  val TERM_FIELD = "content_terms" // "bodytext_plain_t_m"
  def _main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("No doc specified")
      return
    }
    val reader =
//      IndexReader.open(FSDirectory.open(new File("/Users/ttran2/Developer/workspace/hg/ttran2_vf-poc-related_articles/common/services/solr3/distros/vanityfair/data/index")));
      IndexReader.open(FSDirectory.open(new File("/Users/ttran2/Developer/workspace/hg/ttran2_vf-poc-solr-enhanced-index/common/services/solr3/distros/vanityfair/data/index")));

    val total = reader.maxDoc
    var termSet = SortedSet[String]()
    val termEnum = reader.terms(new Term(TERM_FIELD))
    while (termEnum.next) {
      val term = termEnum.term;
      if (term.field == TERM_FIELD)
        termSet = termSet + term.text
    }

    var termMap: Map[String, Int] = Map()
    var termKeywordMap: Map[(String, String), Int] = Map()
    var docWithKeywordCountMap: Map[String, Int] = Map()
    var docCount = 0
    var target = -1

    (1 until total) foreach { i =>
      if (!reader.isDeleted(i)) {
        val doc: Document = reader.document(i)
        val url = doc.get("pagepath_s")
        var t = true
        if (url == args(0)) target = i
        //else t = false;
        
        val tags = doc.getValues("tagid_s_m")
        if (t && tags != null && tags.length > 0) {
          //println("DOC=" + doc.get("pagepath_s"))
          docCount = docCount + 1
          
          // Increase the doc with keyword count
          tags.view foreach {t => 
          	docWithKeywordCountMap.get(t) match {
          	  case None => docWithKeywordCountMap(t) = 1
          	  case Some(v) => docWithKeywordCountMap(t) = v + 1
          	}
          }
          
          val tfvs: Array[TermFreqVector] = reader.getTermFreqVectors(i);
          if (tfvs != null) {
            for (tfv <- tfvs) {
              val terms = tfv.getTerms
              for ((term, j) <- terms.view.zipWithIndex
                if (termSet.contains(term))
              ) {
            	  val f: Int = tfv.getTermFrequencies()(j)
                  //println("t=" + term + " j=" + j + ", " + f)
            	  tags.view foreach {t => 
            	    termKeywordMap.get((term, t)) match {
            	      case None => termKeywordMap((term, t)) = 1
            	      case Some(v) => termKeywordMap((term, t)) = v + 1 
            	    } 
            	  }
            	  
            	  termMap.get(term) match {
            	    case None => termMap(term) = f
            	    case Some(v) => termMap(term) = v + f
            	  }
                }
            }
          }
        }
      }
    }
    
    //docWithKeywordCountMap.keys foreach {k => println("KEY " + k + ":" + docWithKeywordCountMap.get(k).get)}
    //termKeywordMap.keys foreach {k => println(k + ": " + termKeywordMap.get(k).get)}
    
    val termProb: (String, String)=>Double = {(t,k) => 
      //println("Calculating " + t + " " + k)
      val prob = termKeywordMap.get((t,k)) match {
        case None => 0
        case Some(v) => v / docWithKeywordCountMap.get(k).get
      } 
      if (prob == 0) 0.0
      
      val total = termMap.get(t) match {
        case None => 0
        case Some(v) => v
      }
      
      if (total == 0) 0.0
      val weight = 1
      val ap = 0.5
/*
      if (t=="Kate") {
    	  println("t=" + t + ", k=" + k);
    	  println("total=" + total + ", prob=" + prob)
    	  println(((weight * ap) + (total * prob)) / (weight + total))
      }
       
*/
      ((weight * ap) + (total * prob)) / (weight + total)
    }
    
    val docProb: (Set[String],String)=>Double = {(set,k) =>
      val p = set.foldRight[Double](0.0) {(t,v) => 
       // printf("t=%s, v=%20.19f, result=%20.17f\n", t, v, scala.math.log(termProb(t,k)))
        v + scala.math.log(termProb(t,k))
      }
      val c = docWithKeywordCountMap.get(k).get.floatValue / docCount    
      scala.math.log(c) + p
    }
    
    val readTestData: String=>Set[String] = {fname =>
      var set: Set[String] = Set()
      scala.io.Source.fromFile(fname).getLines().foreach { s =>
      	val l = s.split("\\s+")
      	for (t <- l) set = set + t
      }
      set
    }
    
    val readFromSolr: Int=>Set[String] = {docid =>
      var set: Set[String] = Set()
      val tfv: TermFreqVector = reader.getTermFreqVector(docid, TERM_FIELD);
      if (tfv != null) {
        for (term <- tfv.getTerms)
          set = set + term
      }
      set
    }      
    
    println(target + ": " + args(0))
    val test = readFromSolr(target)
    println(test)
    //val test = readTestData("/tmp/test_doc")
    
    val result: Array[(String,Double)] = docWithKeywordCountMap.keys map {k => (k, docProb(test, k)) } toArray

object po extends Ordering[(String, Double)] {
  def compare(a:(String, Double), b:(String, Double)) = a._2 compare b._2
}
    Sorting.quickSort(result)(po)
    for (r <- result)
      printf("%s: %20.17f\n", r._1, r._2)
  }
}