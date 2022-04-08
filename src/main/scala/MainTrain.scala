object MainTrain {

  def main(args: Array[String]): Unit = {
    val nums: List[Int] = List(1, 2, 3, 4)
    if(Util.max(nums,(x:Int,y:Int)=>x-y)!=4)
      println("max does not return the max value  for list of ints(-5)")

    val names:List[String]= List("Hello","World","Scala","JVM","Functional","Programming")
    if(!Util.max(names,(x:String,y:String)=>x.compareTo(y)).equals("World"))
      println("max does not return the max value  for list of strings(-5)")


    var students = Util.map(nums,(x:Int)=>x*2,(y:Int)=>"student "+y)
    var b=true
    var i=0
    students.foreach(s=>{
      i+=2
      if(!s.equals("student "+i))
        b=false
    })
    if(!b)
      println("map does not return a correct list (-5)")

    if(!Util.isSorted(nums,(x:Int,y:Int)=>x<=y))
      println("wrong result for isSorted (-5)")
    if(Util.isSorted(nums,(x:Int,y:Int)=>x>=y))
      println("wrong result for isSorted (-5)")

    val vs=Array(14.0,14.0,1.0,2.0) // values
    val ps=Array(0.5,0.5,0.25,0.25) // probabilities
    if(!Util.probs(vs).sameElements(ps))
      println("wrong probabilities returned (-5)")


    var xs = Array(1.0,2.0,3.0,4.0,5.0,6.0)

    val x=Util.entropy(xs)
    val v=Util.entropy(vs)
    if(x!=Util.entropy(xs) || v!=Util.entropy(vs))
      println("no RT for entropy function (-5)")

    if(x<2.584 || x>2.585)
      println("wrong result for entropy (-5)")
    if(v<1.5 || v>1.501)
      println("wrong result for entropy (-5)")

    xs=Array(1.0, 1.0, 3.0, 4.0, 4.0)
    val m=Util.mu(xs)
    val vari=Util.variance(xs)
    if(m<2.6 || m>2.61)
      println("wrong result for mu function (-5)")
    else{
      if(vari<1.839 || vari>1.841)
        println("wrong result for variance function (-4)")

    }

    if(m!=Util.mu(xs))
      println("mu function is not RT (-5)")
    else
    if(vari!=Util.variance(xs))
      println("variance function is not RT (-5)")

    xs=Array(1.0,1.0,3.0,4.0,4.0)
    val z=Util.zscore(xs,3.0)

    if(z<(0.294)-0.001 || z>(0.294)+0.001)
      println("wrong result for z score function (-5)")


    xs=Array(1.0,2.0,3.0,4.0,5.0)
    var ys=Array(3.0,6.0,9.0,12.0,15.0)
    if(Util.pearson(xs,ys)<1.0-0.0001 )// result should be 1 or very close to 1
      println("wrong result for pearson function (-5)")

    ys=Array(-3.0,-6.0,-9.0,-12.0,-15.0)
    if(Util.pearson(xs,ys)> (-1.0+0.0001) )// result should be -1 or very close to -1
      println("wrong result for pearson function (-5)")

    ys=Array(3.0,-6.0,9.0,-12.0,15.0)
    if(Util.pearson(xs,ys)> (0.2601+0.0001) || Util.pearson(xs,ys)<(0.2601-0.0001))
      println("wrong result for pearson function (-5)")

    val pnts=Array(new Point(0,0.1),new Point(1,2.01),new Point(5.1,10))
    val l=new Line(pnts)
    if(l.a>1.94+0.01 || l.a<1.94-0.01)
      println("wrong result for line.a (-5)")
    if(l.b>0.085+0.001 || l.b<0.085-0.001)
      println("wrong result for line.b (-5)")

    if(l.f(4)>7.85+0.01 || l.f(4)<7.85-.01)
      println("wrong result for line.f (-5)")
    if(l.dist(new Point(4,8))>0.14+0.01 || l.dist(new Point(4,8))<0.14-.01)
      println("wrong result for line.dist (-5)")

    println("done")
  }
}
