
import scala.collection.mutable

object HybridAnomalyDetector extends  AnomalyDetector {

    override def learn(normal: TimeSeries): Map[String, String] = {
        val correlations = Util.featuresCorrelation(normal)

        // linear reg
        val linear = Util.learnLinear(correlations.filter({ case (i, j, pearson) => pearson >= 0.9}), normal).map({case (k,v) => (k,"linear " + v) })
        
        // sum sqr reg
        val sumSqr = Util.learnSumSqr(correlations.filter({ case (i, j, pearson) => pearson > 0.5 && pearson < 0.9}), normal).map({case (k,v) => (k,"sumsqr " + v) })
        
        // z reg
        var zFeatNames = correlations.filter({ case (i, j, pearson) => pearson <= 0.5 }).map({ case (i, j, pearson) => i }).toVector
        zFeatNames = zFeatNames :+ normal.features.apply(normal.features.size - 1)
        val z = Util.learnZ(zFeatNames, normal).map({case (k,v) => (k,"z " + v) })   
        
        return linear ++ sumSqr ++ z
    }

    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
        val linear = model.filter({ case(k,v) => v.startsWith("linear ") }).map({ case(k,v) => (k, v.split(" ").apply(1)) })
        val sumSqr = model.filter({ case(k,v) => v.startsWith("sumsqr ") }).map({ case(k,v) => (k, v.split(" ").apply(1)) })
        val z = model.filter({ case(k,v) => v.startsWith("z ") }).map({ case(k,v) => (k, v.split(" ").apply(1)) })

        val detectedLinear = Util.detectLinear(linear, test)
        var detectedSumsqr = Util.detectSumSqr(sumSqr, test)
        detectedSumsqr = detectedSumsqr.sortBy({ case (k,v) => v }).reverse.groupBy({ case (k,v) => k }).map({ case (k, arr) => arr.head }).toVector
        val detectedZ = Util.detectZ(z, test)

        
        return detectedLinear ++ detectedSumsqr ++ detectedZ
    }

}
