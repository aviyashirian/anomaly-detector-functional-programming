
object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    return Util.learnZ(normal.features, normal)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    return Util.detectZ(model, test)
  }
}
