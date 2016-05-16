package com.faacets
package polyta

object ResourceListing {

  import java.net.{URL, URLDecoder}
  import java.io.File
  import java.util.jar.JarFile
  import collection.JavaConverters._

/** List directory contents for a resource folder. Not recursive.
  * This is basically a brute-force implementation.
  * Works for regular files and also JARs.
  * 
  * @author Greg Briggs
  * @param clazz Any java class that lives in the same place as the resources you want.
  * @param path Should end with "/", but not start with one.
  * @return Just the name of each member item, not the full paths.
  * @throws URISyntaxException 
  * @throws IOException 
  */
  def list(clazz: Class[_], path: String): Seq[String] = {
    def getResourceListingJar(dirURL: URL): Seq[String] = {
      //strip out only the JAR file
      val jarPath = dirURL.getPath.substring(5, dirURL.getPath.indexOf("!"))
      val jar = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
      val entries = jar.entries.asScala //gives ALL entries in jar
      (entries.map(_.getName).filter(_.startsWith(path)).map { name =>
        val entry = name.substring(path.length)
        val checkSubdir = entry.indexOf("/")
        if (checkSubdir >= 0)
          // if it is a subdirectory, we just return the directory name
          entry.substring(0, checkSubdir)
        else
          entry
      }).toSet.toSeq
    }
    Option(clazz.getClassLoader.getResource(path)) match {
      case Some(dirURL) if dirURL.getProtocol == "file" =>
        /* A file path: easy enough */
        return new File(dirURL.toURI).list.toSeq
      case None =>
        /* 
         * In case of a jar file, we can't actually find a directory.
         * Have to assume the same jar as clazz.
         */
        val me: String = clazz.getName.replace(".", "/")+".class"
        getResourceListingJar(clazz.getClassLoader.getResource(me))
      case Some(dirURL) if dirURL.getProtocol == "jar" =>
        getResourceListingJar(dirURL)
      case Some(dirURL) =>         
        throw new UnsupportedOperationException("Cannot list files for URL " + dirURL)
    }
  }

}
