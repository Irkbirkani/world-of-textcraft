import scala.io.StdIn._

val a = readLine("Array").toArray
val b = readLine("Check")

def mod (a:Array[Int], num:Int):Int = {
  for(i<-0 to a.length-1;if i%nums != 0) yield i*3
  a.foreach(_%nums)
  a.sum
}
