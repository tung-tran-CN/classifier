package com.condenast.ml.knowledge

import java.io.BufferedWriter
import java.io.FileWriter

abstract class FileItem
case class ValueItem(name: String, values: Array[String]) extends FileItem
case class CollectionEntryItem(values: Array[String]) extends FileItem

object Storage {

  def saveItems(fileName: String, items: List[FileItem]): Unit = {
    val arrayToString: Array[String] => String = { values =>
      val sv = values.toList.foldLeft[String]("") { (v, t) =>
        v + "," + t
      }
      if (sv.length > 0) sv.substring(1) else sv
    }

    val buff = new BufferedWriter(new FileWriter(fileName))
    items foreach { item =>
      item match {
        case ValueItem(n, values) =>
          buff.write(n + "=" + arrayToString(values) + "\n")
        case CollectionEntryItem(values) =>
          buff.write("$=" + arrayToString(values) + "\n")
      }
    }
    buff.close
  }

  def loadItems(fileName: String): List[FileItem] = {
    var m: List[FileItem] = List()
    scala.io.Source.fromFile(fileName).getLines().foreach { line =>
      try {
        val c = line.split("=")
        if (c.length == 2) {
          val cv = c(1).split(",")
          val i: FileItem = c(0) match {
            // collection item
            case "$" => CollectionEntryItem(cv)
            case v => ValueItem(v, cv)
          }
          m = i :: m
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
    m
  }
}