import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

trait ClusterComparable{
  def getComparableVal(): Double
}

class ClusterObject(x: Double)  extends ClusterComparable{
  def getComparableVal() = x 
}

case class ClusterField () {
  private var elements:        ArrayBuffer[ClusterComparable] = new ArrayBuffer[ClusterComparable]()
  private var clusterElements: ListBuffer[Cluster]            = new ListBuffer[Cluster]()

  def getElementIndex(a: ClusterComparable) = elements.indexOf(a)
  def getElementVal(i: Int): Double = elements(i).getComparableVal()
  def addElement(el: ClusterComparable) : Int = {
    elements+=el
    return elements.length - 1
  }
  def getElements() = elements
  def removeElement(i: Int) : Boolean = {
    if(elements.length <= i) return false
    elements.remove(i)
    return true
  }
  def addCluster(c: Cluster) : Unit = {
    clusterElements += c
  }
  def getClusters() = clusterElements.toList
}


case class Cluster (){
  private var mean:     Double = 0
  private var variance: Double = 0
  private var elements: ListBuffer[Int] = new ListBuffer[Int]()
  
  def addElement(el: Int){
    elements += el
  }

  def containsElement(el: Int) = elements.contains(el)

  def getElements() = elements
  def removeElement(el: Int){
    elements -= el
  }

  def getVarianceDiff(field: ClusterField, i: Int): Double = {
    val calcVariance = (mean: Double, x: Double) => (mean - x) * (mean - x)

    var sum:Double       = 0
    var wasAlreadyInCalc = false

    for(el <- elements){
      if( el == i) wasAlreadyInCalc = true
      sum = sum + field.getElementVal(el)
    }
    var tmpmean = ( sum + field.getElementVal(i) ) / ( elements.length + 1 )
    if(wasAlreadyInCalc) tmpmean = sum / elements.length

    var tmpvariance:Double = 0
    for(el <- elements){
      if( el != i ) tmpvariance = tmpvariance + calcVariance(tmpmean, field.getElementVal(el))
    }
    return math.abs(tmpvariance - variance)
  }

}

object kmeans{

  def main(args: Array[String]): Unit = {
    var testData: Array[ClusterObject] = new Array[ClusterObject](10)

    for( i <- 0 until testData.length){
      testData(i) = new ClusterObject(i.toDouble)
    }

    lloyd(testData,3)
  }

  def getInitialCluster(dataset: Array[_ <: ClusterComparable], k: Int): ClusterField = {
    //elements that need to be distributed after elementsPerCluster
    var leftover = dataset.length % k
    // how many alements should a cluster contain
    val elementsPerCluster:Int = dataset.length / k
    var initialDistribution:ClusterField = new ClusterField

    // how many elements were used for the current mean value
    var i:Int            = 0
    // is a leftover Value used
    var usedLeftOver     = false
    var cl:Cluster       = null
    for( x <- dataset){
      if(i == 0){
        cl = new Cluster()
      }
      //are there already enough values for the current mean calculation used
      if(i < elementsPerCluster){
        val fullIndex = initialDistribution.addElement(x)
        cl.addElement(fullIndex)
        i = i + 1
      // if there are leftovers which need to be distributed, add them
      }else if(leftover > 0 && i < elementsPerCluster + 1){
        val fullIndex = initialDistribution.addElement(x)
        cl.addElement(fullIndex)
        i = i + 1
        leftover = leftover - 1
        usedLeftOver = true
      }
      // check if the current mean calculation is finished, reset the parameters and it the mean to the return array
      if(( i  == elementsPerCluster && leftover == 0) || ( i == elementsPerCluster + 1 && usedLeftOver )){
        i = 0
        usedLeftOver = false
        //println("[DEBUG] Cluster no "+ i.toString + "clusterElements length: " + cl.copy().getElements.length.toString)
        //cl.recalculate()
        initialDistribution.addCluster(cl)
      } 
    }
    return initialDistribution
  }
  
  def printClusterDistribution(field: ClusterField, iteration: Int){
    var i = 0
    for(cl <- field.getClusters()){
      print("Cluster "+i.toString+" elements: [")
//      print("Length: "+cl.getElements().length.toString)
      for(k <- cl.getElements()){
        print(k.toString+", ")
      }
      println("]")
      i = i + 1
    }
  }

  def lloyd(dataset: Array[_ <: ClusterComparable], k: Int): ClusterField = {
    val clusters = getInitialCluster(dataset, k)
    printClusterDistribution(clusters, 0)
    var iteration = 0

    //as long as the cluster improves, continue to run
    var hasNotImproved = false
    while(!hasNotImproved){
      hasNotImproved = true
      //iterate through all available elements
      for(el <- clusters.getElements()){
        // get the index for the element, since these are stored in the clusters
        var elIndex:Int            = clusters.getElementIndex(el)
        // cluster in Which the element was before
        var beforeClusterIndex:Int = -1
        // cluster index which got currently checked
        var clusterIndex:Int       = 0
        // cluster which got the minimal variance difference
        var clusterMinIndex:Int    = -1
        // the actual difference of that
        var minDiff:Double         = -1
        // iterate through all clusters
        for(cl <- clusters.getClusters()){
           if(cl.containsElement(elIndex)) beforeClusterIndex = clusterIndex
           val diff = cl.getVarianceDiff(clusters, elIndex)
           if(clusterMinIndex == -1 || diff < clusterMinIndex){
             minDiff         = diff
             clusterMinIndex = clusterIndex
           }
           clusterIndex = clusterIndex + 1
        }
        if(beforeClusterIndex != -1 && beforeClusterIndex != clusterMinIndex){
          hasNotImproved=false
          println("Found a better cluster!")
          var clusterIndex = 0
          for(cl <- clusters.getClusters()){
            if(clusterIndex == beforeClusterIndex) cl.removeElement(elIndex)
            if(clusterIndex == clusterMinIndex)    cl.addElement(elIndex)
            clusterIndex = clusterIndex + 1
          }
        }
        
      }
    }
  printClusterDistribution(clusters, 1)
  return null
  }
}
