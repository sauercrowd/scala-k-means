import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

trait ClusterComparable{
  def getComparableVal(): Double
}

class ClusterObject(x: Double)  extends ClusterComparable{
  def getComparableVal() = x 
}

case class ClusterField{
  private var elements:        ArrayBuffer[ClusterComparable] = new ArrayBuffer[ClusterComparable]()
  private var clusterElements: ListBuffer[Cluster]            = new ListBuffer[Cluster]()

  def getElementVal(i: Int): Double = elements(i).getComparableVal()
}


case class Cluster{
  private var mean:     Double = 0
  private var variance: Double = 0
  private var elements: ListBuffer[Int] = new ListBuffer[Int]()
  
  def getMean() = mean
  def getVariance() = variance
  def addElement(el: Int){
    elements += el
  }

  def getElements() = elements
  def removeElement(el: Int){
    elements -= el
  }

  def recalculate() : Unit = {
    val calcVariance = (mean: Double, x: Double) => (mean - x) * (mean - x)

    // calculate mean
    var sum:Double = 0
    for(el <- elements){
      sum = sum + el.getComparableVal()
    }
    mean = sum / elements.length

    // calculate cluster variance
    variance = 0
    for(el <- elements){
      variance = variance + calcVariance(mean, el.getComparableVal())
    }
  }
  
}

object kmeans{

  def main(args: Array[String]): Unit = {
    var testData: Array[ClusterObject] = new Array[ClusterObject](10)
    print("[")
    for( i <- 0 until testData.length){
      testData(i) = new ClusterObject(i.toDouble)
      print(i.toDouble.toString+", ")
    }
    println("]")

    lloyd(testData,3)
  }

  def getInitialCluster(dataset: Array[_ <: ClusterComparable], k: Int): Array[Cluster] = {
    //elements that need to be distributed after elementsPerCluster
    var leftover = dataset.length % k
    // how many alements should a cluster contain
    val elementsPerCluster:Int = dataset.length / k
    var initialDistribution:Array[Cluster] = new Array[Cluster](k)

    // how many elements were used for the current mean value
    var i:Int            = 0
    //which mean Index is currently calculated
    var clusterIndex:Int = 0
    // is a leftover Value used
    var usedLeftOver     = false

    for( x <- dataset){
      if(i == 0){
        initialDistribution(clusterIndex) = new Cluster()
      }
      //are there already enough values for the current mean calculation used
      if(i < elementsPerCluster){
        initialDistribution(clusterIndex).addElement(x)
        i = i + 1
      // if there are leftovers which need to be distributed, add them
      }else if(leftover > 0 && i < elementsPerCluster + 1){
        initialDistribution(clusterIndex).addElement(x)
        i = i + 1
        leftover = leftover - 1
        usedLeftOver = true
      }
      // check if the current mean calculation is finished, reset the parameters and it the mean to the return array
      if(( i  == elementsPerCluster && leftover == 0) || ( i == elementsPerCluster + 1 && usedLeftOver )){
        i = 0
        usedLeftOver = false
        initialDistribution(clusterIndex).recalculate()
        clusterIndex = clusterIndex + 1
      } 
    }
    return initialDistribution
  }
  
  def getClusterIndex(clusters: Array[Cluster], x: Double) : Int = {
    val variance = (mean: Double, x: Double) => ( mean - x ) * ( mean - x )

    var minVariance 	  = variance(clusters(0).getMean(), x)
    var minVarianceIndex  = 0

    for(i <- 1 until clusters.length){
      if(variance(clusters(i).getMean(),x) < minVariance){
        minVariance = variance(clusters(i).getMean(),x)
        minVarianceIndex = i
      }
    }
    return minVarianceIndex
  }

  def printClusterDistribution(clusters: Array[Cluster], iteration: Int){
    for(i <- 0 until clusters.length){
      print("Cluster "+i.toString+" elements: [")
      for(k <- clusters(i).getElements()){
        print(k.getComparableVal().toString+", ")
      }
      println("]")
    }
  }

  def lloyd(dataset: Array[_ <: ClusterComparable], k: Int): Array[Cluster] = {
    val clusters = getInitialCluster(dataset, k)
    var i = 1

    while(true){
      var finish = true
      //iterate through clusters
      for(i <- 0 until clusters.length){
        println("I: "+i.toString())
        for(el <- elements){
          println("Checking Cluster "+i.toString()+" Element "+el.getComparableVal())
          clusters(i).removeElement(el)
          clusters(i).recalculate()
          val minimalVarianceIndex = getClusterIndex(clusters, el.getComparableVal())
          clusters(minimalVarianceIndex).addElement(el)
          if(i != minimalVarianceIndex){
            finish=false
          }
        }
      }
      if(finish){
        return clusters
      }
      printClusterDistribution(clusters, i)
      i = i + 1
    }
    return null
  }
}
