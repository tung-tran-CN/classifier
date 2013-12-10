package com.condenast.ml.knowledge

import scala.collection.Iterable
import java.io.BufferedWriter
import java.io.FileWriter

trait Repository {
  /**
   *
   */
  def addDoc(docId: String): Unit
  /**
   *
   */
  def increaseDocCountForKeyword(keyword: String): Int
  /**
   *
   */
  def increaseTermCount(term: String, count: Int): Int
  /**
   *
   */
  def increaseTermCountForKeyword(keyword: String)(term: String, count: Int): Int
  /**
   * return (docCount, occurrences) for a given term in doc associated with the keyword
   */
  def termForKeywordStaticstic(keyword: String, term: String): (Int, Int)
  /**
   * return (doc count, total term count) for a given keyword
   */
  def keywordStatistic(keyword: String): (Int, Int)
  /**
   * return (doc count, total term count) for a given term
   */
  def termStatistic(term: String): (Int, Int)
  /**
   *
   */
  def totalDocCount: Int
  /**
   * 
   */
  def keywords: Iterable[String]
  /**
   * 
   */
  def terms: Iterable[String]
}