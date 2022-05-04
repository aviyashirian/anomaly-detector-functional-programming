
trait AnomalyDetector {
  def learn(normal:TimeSeries):Map[String,String]
  def detect(model:Map[String,String],test:TimeSeries):Vector[(String, Int)] // shorter syntax for Tuple2[Sting,Int]
}
