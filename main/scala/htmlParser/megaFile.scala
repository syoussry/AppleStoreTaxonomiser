package htmlParser

import scala.io.Source
import java.io.{File, PrintWriter}

object megaFile {

  // creates a megafile of all the IDS in ONE category
  // pattern: name!#!ID
  def categoryAppsIDS(category: String) = {

    val path:String = "crawlData/" + category + "/"
    val files:Array[File] = new java.io.File(path + "apps/").listFiles
    val fileName:String = path + "megafiles/"

    val writer = new PrintWriter(new File(fileName + category + ".csv"))

    files.foreach(file =>
      writer.write(Source.fromFile(file).getLines().mkString("\n") +
        System.getProperty("line.separator"))
    )

    writer.close()

  }

}
