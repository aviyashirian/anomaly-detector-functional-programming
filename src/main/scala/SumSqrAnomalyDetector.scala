
import scala.collection.mutable

object SumSqrAnomalyDetector extends  AnomalyDetector {
    val threshold = 0.9

    override def learn(normal: TimeSeries): Map[String, String] = {
        return Util.learnSumSqr(Util.correlatedFeatures(normal, threshold), normal)
    }

    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
        return Util.detectSumSqr(model, test)
    }
}
