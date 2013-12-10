package com.condenast.ml.corpus

import scala.collection.Iterator
import org.apache.lucene.document.Document
import org.apache.lucene.index.TermFreqVector
import org.apache.lucene.index.IndexReader
import org.apache.lucene.store.FSDirectory
import java.io.File
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.PorterStemFilter
import org.apache.lucene.analysis.StopFilter
import org.apache.solr.analysis.TrimFilter
import org.apache.lucene.analysis.StopAnalyzer
import org.apache.solr.analysis.PatternReplaceFilter
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.util.Version._
import java.util.regex.Pattern
import org.apache.lucene.analysis.CharReader
import java.io.StringReader
import org.apache.lucene.analysis.tokenattributes.TermAttribute
import org.apache.solr.analysis.LegacyHTMLStripCharFilter
import java.io.Reader
import com.condenast.ml.util.Logger

class SolrSource(directoryName: String, val fieldName: String) extends Source {
  val reader = IndexReader.open(FSDirectory.open(new File(directoryName)))
  Logger().debug("Total doc =" + reader.maxDoc)
  var cursor: Int = 1
  var fieldTerms = List[(String,Int)]()

  def readDoc(docId: String): SourceDocument = {
    val id = docId.toInt
    if (id > reader.maxDoc - 1) 
      throw new IllegalArgumentException
    
    if (reader.isDeleted(id))
      throw new RuntimeException
      
    new SolrDoc(reader.document(id), docId, terms(id))
  }
  
  private def terms(index: Int): List[(String, Int)] = {
	val tfv = reader.getTermFreqVector(index, fieldName)
    tfv match {
      case null => Nil
      case _ => tfv.getTerms.view.zipWithIndex map { t => (t._1, tfv.getTermFrequencies()(t._2)) } toList
    }
  }
  
  // Horrible method. to be replaced with search
  def findDoc(path: String): Int = {
    var target = -1
    (1 until reader.maxDoc) foreach {i =>
      if (!reader.isDeleted(i)) {
        val doc: Document = reader.document(i)
        val url = doc.get("pagepath_s")
        if (url == path) target = i
      }
    }	
    target
  }
  
  object SolrIterator extends Iterator[SourceDocument] {
	var max = reader.maxDoc - 1
	var count = 0

    def hasNext = {
      while (count < max && cursor < reader.maxDoc - 1 && reader.isDeleted(cursor)) cursor += 1
      count < max && cursor < reader.maxDoc - 1
    }
    
    def next = {
      if (count < max && cursor < reader.maxDoc - 1) {
        val doc = new SolrDoc(reader.document(cursor), cursor.toString, terms(cursor))
        cursor += 1
        count += 1
        doc
      }  
      else
        throw new RuntimeException("Solr doc is not available")
    }
  }
  
  object ValidDocOnlySolrIterator extends Iterator[SourceDocument] {

    def hasNext = cursor < reader.maxDoc - 1
    def next = {
      while (hasNext && reader.isDeleted(cursor)) cursor += 1      
      if (hasNext) new SolrDoc(reader.document(cursor), cursor.toString, terms(cursor)) else null
    }
  }

  def iterator: Iterator[SourceDocument] = {
    val i = SolrIterator
    i.max = 10000
    i
  }
}

object SolrSource {
  
  def apply(directoryName: String, fieldName: String) = {
    new SolrSource(directoryName, fieldName)
  }
  
  def loadFeatures(fname: String): List[(String, Int)] = {
    val al = new org.apache.lucene.analysis.Analyzer {
      override def tokenStream(field: String, reader: Reader): TokenStream = {

        val r = new LegacyHTMLStripCharFilter(CharReader.get(reader))

        new PorterStemFilter(
          new StopFilter(LUCENE_31,
            new TrimFilter(
              new PatternReplaceFilter(
                //new PatternReplaceFilter(
                new PatternReplaceFilter(new StandardTokenizer(LUCENE_31, r), Pattern.compile("(&amp;#|;|-)"), " ", true),
                //	Pattern.compile("([^\\w&amp;&amp;[^\\s]])"), "", true),
                Pattern.compile("(\\s+)"), " ", true), true),
            StopAnalyzer.ENGLISH_STOP_WORDS_SET))
      }
    }

    var m: Map[String, Int] = Map()
    scala.io.Source.fromFile(fname)(scala.io.Codec.ISO8859).getLines().foreach { line =>
      try {
        var stream: TokenStream = al.tokenStream("content_terms", new StringReader(line))
        while (stream.incrementToken()) {
          val attr = stream.getAttribute(classOf[TermAttribute])
          val term = attr.term()
          m.get(term) match {
            case None => m = m + (term -> 1)
            case Some(v) => m = m + (term -> (v + 1))
          }
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
    m.view.toList
  }
}

class SolrDoc(doc: Document, val id: String, val terms: List[(String, Int)]) extends SourceDocument {
  //val id = doc.get("uid")
  val keywords:Array[String] = doc.getValues("tagid_s_m")
  val valid = true
  
  def getKeywords: List[String] = keywords.toList
  def getTerms: List[(String, Int)] = terms
}