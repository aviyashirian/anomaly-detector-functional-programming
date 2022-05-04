import java.util.Arrays
object Util {

	// max
	def max[A](elements: List[A], maxFunc: (A, A) => Int): A = elements match {
		case Nil => throw new RuntimeException("max of empty list")
		case head :: Nil => head
		case head :: tail => 
			val maxTail = max(tail, maxFunc)
			if (maxFunc(head, maxTail) >= 0) head else maxTail
	}

	// map
	def map[A, B, C](elements: List[A], aToB: A => B, bToC: B => C): List[C] = {
		if (elements == Nil) return Nil
		return elements.map(aToB).map(bToC)
	}

	// isSorted
	def isSorted[A](elements: List[A], sortFunc: (A, A) => Boolean): Boolean = {
		if (elements == Nil) return true
		if (elements.tail == Nil) return true
		if (!isSorted(elements.tail, sortFunc)) return false
		return sortFunc(elements.head, elements.tail.head)
	}

	// probs
	def probs(elements: Array[Double]): Array[Double] = {
		val size = elements.size.doubleValue
		val grouped = elements.groupBy(identity).mapValues(_.size)
		return elements.map(grouped(_)/size)
	}

	// entropy
	def entropy(elements: Array[Double]): Double = {
		def log2(num: Double): Double = {
			return Math.log10(num) / Math.log10(2)
		}
		
		def singleEnt(num: Double): Double = {
			return num * log2(num)
		}
		
		return -1 * elements.zip(probs(elements).map(singleEnt)).distinct.map({ case (x,y) => y }).sum
	}

	// mu
	def mu(elements: Array[Double]): Double = {
		return elements.zip(probs(elements)).distinct.map({ case (x,y) => x * y }).sum
	}

	// variance
	def variance(elements: Array[Double]): Double = {
		val m = mu(elements)

		return elements.zip(probs(elements)).distinct.map({ case (xi, pi) => pi * Math.pow(xi - m, 2) }).sum
	}

	// zscore
	def zscore(elements: Array[Double], x: Double): Double = {
		val m = mu(elements)
		val devi = Math.sqrt(variance(elements))

		return (x - m) / devi
	}

	// cov
	def cov(x: Array[Double], y: Array[Double]): Double = {
		val xy = x.zip(y).map({ case (x,y) => x * y })
		val m_xy = mu(xy)
		val m_x = mu(x)
		val m_y = mu(y)

		return m_xy - m_x * m_y
	}

	// pearson
	def pearson(x: Array[Double], y: Array[Double]): Double = {
		val devi_x = Math.sqrt(variance(x))
		val devi_y = Math.sqrt(variance(y))

		return cov(x, y) / (devi_x * devi_y)
	}

	def featuresCorrelation(ts: TimeSeries): Array[(String, String, Double)] = {
		var result = Array[(String, String, Double)]()
		val features = ts.features

		for (feaI <- 0 to ts.features.size - 2) {
			val featureIValues = ts.getValues(features.apply(feaI)).get.toArray
			
			for (feaJ <- feaI + 1 to ts.features.size - 1) {
				val featureJValues = ts.getValues(features.apply(feaJ)).get.toArray
				val pearson = Math.abs(Util.pearson(featureIValues, featureJValues))

				result = result :+ (features.apply(feaI), features.apply(feaJ), pearson)
			}
		}

		return result
	}

	def correlatedFeatures(ts: TimeSeries, threshold: Double): Array[(String, String, Double)] = {
		val grouped = Util.featuresCorrelation(ts)
					.filter({ case (i, j, pearson) => pearson >= threshold})
					.groupBy({ case (i, j, pearson) => i})
					
		var result = Array[(String, String, Double)]()
		grouped.values.foreach(arr => {
			val maxPearson: Double = arr.map({ case (i, j, pearson) => pearson}).max
			result = result :+ arr.find({ case (i, j, pearson) => pearson == maxPearson}).get
		})

		return result
	}

	def dist(p1: Point, p2: Point): Double = {
		return Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
	}

	def sqrSum(pi: Point, points: Vector[Point]): Double = {
		return points.map(pj => Math.pow(Util.dist(pi, pj), 2)).sum
	}

	def learnZ(features: Vector[String], ts: TimeSeries): Map[String, String] = {
		var map = Map[String, String]()

		features.foreach(featName => {
			val values = ts.getValues(featName).get.toArray
			var maxZ = 0.0
			values.foreach(value => {
				val z = Math.abs(Util.zscore(values, value))
				if(z > maxZ) {
					maxZ = z
				}
			})
			map = map.updated(featName, maxZ.toString)
		})

		return map
	}

	def detectZ(model: Map[String, String], test: TimeSeries): Vector[(String, Int)]  = {
		var results: Vector[(String, Int)] = Vector()
		model.keys.foreach(feat => {
			val values = test.getValues(feat).get.toArray
			val maxZ = model.get(feat).get.toDouble
			values.zipWithIndex.foreach({case (value, i) => {
				val z = Math.abs(Util.zscore(values, value))
				if(z > maxZ) {
					results = results :+ (feat,i)
				}
			}})
		})
		return results
	}

	def learnLinear(correlatedFeatures: Array[(String, String, Double)], ts: TimeSeries): Map[String, String] = {
		var map = Map[String, String]()

		correlatedFeatures.foreach({ case (firstFeatName, secondFeatName, pearson) => {
			val firstFeatValues = ts.getValues(firstFeatName).get
			val secondFeatValues = ts.getValues(secondFeatName).get
			val points = firstFeatValues.zip(secondFeatValues).map({case (x,y) => new Point(x,y)})
			val line = new Line(points.toArray)
			val maxDistanceFromLine = points.map(p => line.dist(p)).max

			map = map.updated(
				firstFeatName + "," + secondFeatName,
				line.a + "," + line.b + "," + maxDistanceFromLine
			)
		}})

		return map
	}

	def detectLinear(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
		var results: Vector[(String, Int)] = Vector()
		model.foreach({ case (correlatedFeatures: String, info: String) => {
			val firstFeat = correlatedFeatures.split(",").apply(0)
			val secondFeat = correlatedFeatures.split(",").apply(1)
			val lineA = info.split(",").apply(0).toDouble
			val lineB = info.split(",").apply(1).toDouble
			val maxDist = info.split(",").apply(2).toDouble
			
			val firstFeatValues = test.getValues(firstFeat).get.toArray
			val secondFeatValues = test.getValues(secondFeat).get.toArray

			firstFeatValues.zip(secondFeatValues).zipWithIndex.foreach({case ((firstFeatVal,secondFeatVal),i) => {
				val dist = Math.abs((lineA * firstFeatVal + lineB) - secondFeatVal)
				if (dist > maxDist) {
					// This is an anomaly
					results = results :+ (correlatedFeatures,i)
				}
			}})
		}})

		return results
	}

	def learnSumSqr(correlatedFeatures: Array[(String, String, Double)], ts: TimeSeries): Map[String, String] = {
		var map = Map[String, String]()

		correlatedFeatures.foreach({ case (firstFeatName, secondFeatName, pearson) => {
			val firstFeatValues = ts.getValues(firstFeatName).get
			val secondFeatValues = ts.getValues(secondFeatName).get
			val points = firstFeatValues.zip(secondFeatValues).map({case (x,y) => new Point(x,y)})
			val line = new Line(points.toArray)
			val maxSqrSum = points.map(pi => Util.sqrSum(pi, points)).max

			map = map.updated(
				firstFeatName + "," + secondFeatName,
				line.a + "," + line.b + "," + maxSqrSum
			)
		}})

		return map
	}

	def detectSumSqr(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
		var results: Vector[(String, Int)] = Vector()
		model.foreach({ case (correlatedFeatures: String, info: String) => {
			val firstFeat = correlatedFeatures.split(",").apply(0)
			val secondFeat = correlatedFeatures.split(",").apply(1)
			val lineA = info.split(",").apply(0).toDouble
			val lineB = info.split(",").apply(1).toDouble
			val maxSqrSum = info.split(",").apply(2).toDouble
			
			val firstFeatValues = test.getValues(firstFeat).get.toArray
			val secondFeatValues = test.getValues(secondFeat).get.toArray

			val points = firstFeatValues.zip(secondFeatValues).map({case (x, y) => new Point(x,y)}).toVector

			points.zipWithIndex.foreach({case (pi,i) => {
				val sqrSum = Util.sqrSum(pi, points)
				if (sqrSum > maxSqrSum) {
					// This is an anomaly
					results = results :+ (correlatedFeatures,i)
				}}
			})
		}})

	
		return results
	}
}
