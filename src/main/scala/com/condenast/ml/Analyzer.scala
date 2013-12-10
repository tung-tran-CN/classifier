/*package com.condenast.ml

import org.apache.lucene.util.Version._
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.TokenStream
import java.io.StringReader
import org.apache.lucene.analysis.tokenattributes.TermAttribute
import org.apache.lucene.analysis.ReusableAnalyzerBase
import java.io.Reader
import org.apache.solr.analysis.HTMLStripCharFilter
import org.apache.lucene.analysis.StopFilter
import org.apache.solr.analysis.TrimFilter
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.StopAnalyzer
import org.apache.lucene.analysis.CharReader
import org.apache.solr.analysis.PatternReplaceCharFilter
import java.util.regex.Pattern
import org.apache.solr.analysis.PatternReplaceFilter
import org.apache.lucene.analysis.PorterStemFilter
import scala.collection.mutable.Map

object Analyzer {

  def main(args: Array[String]) = {
    val set = readTestData("/tmp/test_data")
	set foreach { s=> println(s) }
  }

  def analyzeText(text: String): Set[String] = {
    var set: Set[String] = Set()
    val al = new MyAnalyzer()
      var stream: TokenStream = al.tokenStream("content_terms", new StringReader(text))
      while (stream.incrementToken()) {
        val attr = stream.getAttribute(classOf[TermAttribute])
        val term = attr.term()
        val f = attr.termLength()
        println("TERM: " + term + " L=" + f)
        set = set + term
      }
    set
  }

  def readTestData(fname: String): List[(String,Int)] = {
    var m: Map[String,Int] = Map()
    val al = new MyAnalyzer()
    scala.io.Source.fromFile(fname).getLines().foreach { line =>
      var stream: TokenStream = al.tokenStream("content_terms", new StringReader(line))
      while (stream.incrementToken()) {
        val attr = stream.getAttribute(classOf[TermAttribute])
        val term = attr.term()
        m.get(term) match {
          case None => m(term) = 1
          case Some(v) => m(term) = v + 1
        }
      }
    }
    m.view.toList
  }
}

class MyAnalyzer extends org.apache.lucene.analysis.Analyzer {
  override def tokenStream(field: String, reader: Reader): TokenStream = {
    val r = new HTMLStripCharFilter(CharReader.get(reader))

    new PorterStemFilter(
      new StopFilter(LUCENE_31,
        new TrimFilter(
          new PatternReplaceFilter(
            //new PatternReplaceFilter(
            new PatternReplaceFilter(new StandardTokenizer(LUCENE_31, r), Pattern.compile("(&amp;#|;|-)"), " ", true),
            //	Pattern.compile("([^\\w&amp;&amp;[^\\s]])"), "", true),
            Pattern.compile("(\\s+)"), " ", true), true),
        StopAnalyzer.ENGLISH_STOP_WORDS_SET)
      )
  }
}*/