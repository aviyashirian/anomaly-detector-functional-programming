
object MainTrain {

  // a simple test for time series
  def testTimeSeries():Unit={
    val ts=new TimeSeries("train.csv")
    val a=Vector(1,2,3,4)
    val b=Vector(3,4,5)


    if(ts.getValues("E")!=None)
      println("when key does not exist you should return None (-1)")
    if(!ts.getValues("A").get.sameElements(a) && !ts.getValues("A").get.isInstanceOf[Vector[Double]])
      println("problem with getValues (-2)")
    if(!ts.getValues("B",1 to 3).get.sameElements(b))
      println("problem with getValues with range (-3)")
    if(ts.getValues("B",0 to 4)!=None || ts.getValues("B",-1 to 3)!=None)
      println("when index is out of bounds you should return None (-1)")
    if(ts.getValue("C",2).get!=5)
      println("problem with get Value (-2)")
    if(ts.getValue("D",5)!=None)
      println("when index is out of bounds you should return None (-1)")
  }


  // ZAnomalyDetector test
  def testZAD():Unit={
    val ts=new TimeSeries("train2.csv")
    val model=ZAnomalyDetector.learn(ts)
    val r0=ZAnomalyDetector.detect(model,ts)
    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=ZAnomalyDetector.detect(model,new TimeSeries("test2.csv"))
    if(r1.length!=1)
      println("there should be exactly one anomaly reported here (-10)")
    if(r1(0)._1!="A" || r1(0)._2!=19)
      println("wrong anomaly detected (-10)")

  }

  // LinearRegAnomalyDetector test
  def testLRAD():Unit={
    val ts=new TimeSeries("train3.csv")
    val model=LinearRegAnomalyDetector.learn(ts)
    val r0=LinearRegAnomalyDetector.detect(model,ts)
    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=LinearRegAnomalyDetector.detect(model,new TimeSeries("test3.csv"))
    if(r1.length!=2)
      println("wrong number of reported anomalies (-10)")
    if(!r1.contains(("A,B",5)) || !r1.contains(("C,D",13)))
      println("wrong anomalies reported (-10)")
  }

  // SumSqrAnomalyDetector test
  def testSSD():Unit={
    val ts=new TimeSeries("train3.csv")
    val model=SumSqrAnomalyDetector.learn(ts)
    val r0=SumSqrAnomalyDetector.detect(model,ts)
    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=SumSqrAnomalyDetector.detect(model,new TimeSeries("test3.csv"))
    if(r1.length!=2)
      println("wrong number of reported anomalies (-10)")
    if(!r1.contains(("A,B",5)) || !r1.contains(("A,B",18)))
      println("wrong anomalies reported (-10)")
  }

  // HybridAnomalyDetector test
  def testHAD():Unit={
    val ts=new TimeSeries("train4.csv")
    val model=HybridAnomalyDetector.learn(ts)
    val r0=HybridAnomalyDetector.detect(model,ts)

    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=HybridAnomalyDetector.detect(model,new TimeSeries("test4.csv"))
    // println(r1)

    if(r1.length!=5)
      println("wrong number of reported anomalies (-10)")
    if(!r1.contains(("E",24)) || !r1.contains(("B",10)) || !r1.contains(("D",15)) || !r1.contains(("A,B",10)) || !r1.contains(("C,D",15)))
      println("wrong anomalies reported (-10)")
  }

  def main(args: Array[String]): Unit = {

    testTimeSeries()
    testZAD()
    testLRAD()
    testSSD()
    testHAD()

    println("done")
  }
}
