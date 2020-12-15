package start

import days._
import org.clapper.classutil.{ClassFinder, ClassInfo}

import scala.io.Source
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe

object Advent extends App {
  val classes = getClassList.sorted

  for (className <- classes) {
    if (!(className equals "days.Day")) {
      val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
      val module = runtimeMirror.staticModule(className)
      val obj = runtimeMirror.reflectModule(module)
      val day:Day = obj.instance.asInstanceOf[Day]
      println(className.substring(5).replace("$", ""))
      day.run()
      println("----------------------------")
    }
  }

  def readFile(file: String): List[String] = {
    val source = Source.fromFile("resources/" + file + ".txt")
    val lines = source.getLines().toList
    source.close()
    return lines
  }

  def getClassList: ListBuffer[String] = {
    var pluginList = ListBuffer[String]()
    val classPath = List(Day01.getClass.getProtectionDomain.getCodeSource.getLocation.getPath).map(new File(_))
    val finder = ClassFinder(classPath)
    val classes = finder.getClasses()
    val plugins = ClassFinder.concreteSubclasses("days.Day", classes)

    while (plugins.hasNext) {
      val plugin = plugins.next()
      pluginList.addOne(plugin.name)
    }
    pluginList
  }
}
