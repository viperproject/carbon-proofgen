package viper.carbon.proofgen.util

import java.io.File
import java.net.URL
import java.nio.file.Path

object FileUtil {

  def listOfSubdirectories(directoryPath: Path): Option[List[String]] = {
    try {
      Some(directoryPath.toFile.listFiles().filter(_.isDirectory).map(_.getName).toList)
    } catch {
      case _: Exception => None
    }
  }

  def deleteFilesInDirectory(directoryPath: Path, pred: String => Boolean) : Unit = {
    val subFiles = directoryPath.toFile.listFiles()
    if(subFiles != null) {
      subFiles.filter(f => pred(f.getName)).foreach(_.delete())
    }
  }

}
