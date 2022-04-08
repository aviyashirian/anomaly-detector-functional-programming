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
}
