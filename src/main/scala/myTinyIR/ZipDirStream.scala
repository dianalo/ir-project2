package myTinyIR

import util.Try
import util.Failure
import util.Success
import io.Source
import java.io.File
import java.io.InputStream
import java.util.zip.ZipFile
import java.util.zip.ZipEntry
import scala.collection.JavaConversions._

// create a document stream out of all files in a all zip files
// that are found in a given directory
//
class ZipDirStream (dirpath: String, extension: String = "") 
extends DirStream (dirpath,extension) {

  override def length : Int =
    ziplist.map(new ZipStream(_,extension).length).sum
  
  override def stream : Stream[InputStream] =  
    ziplist.map(new ZipStream(_,extension).stream).reduceRight(_ append _)
  
  val ziplist = new File(dirpath)
      .listFiles.filter(isZipFile(_))        
  	  .map(z => z.getAbsolutePath).sorted.toList
  
  private def isZipFile(f: File) = f.getName.endsWith(".zip")
}

