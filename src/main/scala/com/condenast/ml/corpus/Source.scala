package com.condenast.ml.corpus

import scala.collection.immutable.Iterable

trait SourceDocument {
  /**
   * 
   */
  def id: String
  /**
   * 
   */
  def valid: Boolean
  /**
   * 
   */	
  def getKeywords: List[String]
  /**
   * 
   */
  def getTerms: List[(String, Int)]
}

trait Source extends Iterable[SourceDocument] {
  def readDoc(docId: String): SourceDocument
}