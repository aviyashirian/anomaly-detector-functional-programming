import scala.io.Source

class TimeSeries(csvFileName: String) {

  val data: scala.collection.mutable.Map[String, Vector[Double]] = scala.collection.mutable.Map()
  var features: Vector[String] = Vector()

  val source = Source.fromFile(csvFileName)
  source.getLines().zipWithIndex.foreach({ case (line, i) => {
    val elems: Array[String] = line.split(",")

    if (i == 0) {
      features = features ++ elems.toVector
      features.foreach(f => {
        data(f) = Vector()
      })
    } else {
      elems.zipWithIndex.foreach({ case (value, j) => {
        val f = features(j)
        val newA: Vector[Double] = data.getOrElse(f, Vector.empty[Double]) :+ value.toDouble
        data.update(f, newA)
      }
      })
    }
  }
  })
  source.close()


  // given name of a feature return in O(1) its value series
  def getValues(feature: String): Option[Vector[Double]] = {
    return data.get(feature)
  }

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    try {
      val values = getValues(feature).get
      val element = values.apply(timeStep)
      Some(element)
    } catch {
      case e: Exception => None
    }
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature: String, r: Range): Option[Vector[Double]] = {
    try {
      val values = getValues(feature).get

      if(values.size <= r.end || r.start < 0) {
        return None
      }
      val sliced = values.slice(r.start, r.end + 1)
      Some(sliced)
    } catch {
      case e: Exception => None
    }
  }


}
