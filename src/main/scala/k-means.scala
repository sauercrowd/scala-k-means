import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


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
    // sum all elements up wich are currently in the cluster, set a marker if the elements which needs to be check is already in here
    for(el <- elements){
      if( el == i) wasAlreadyInCalc = true
      sum = sum + field.getElementVal(el)
    }

    // if the cluster got no elements or the element which is checked is already in, return 0.0 for variance
    if(wasAlreadyInCalc && elements.length == 1 || !wasAlreadyInCalc && elements.length == 0) return 0.0    
    var tmpsum:Double = 0
    // if it is already in cluster, tmpsum is the sum without that value, if it isnt, tmpsum is the sum with that value
    if(wasAlreadyInCalc){
       tmpsum = sum - field.getElementVal(i)
    }else{
       tmpsum = sum + field.getElementVal(i)
    }

    var mean1:Double = 0 //with new value
    var mean2:Double = 0 //withoyt new value

    // if it was already in here, mean1 is just sum divided by elements, and mean2 tmpsum divided by elements minus1
    if(wasAlreadyInCalc){
      mean1 = sum / elements.length
      mean2 = tmpsum / (elements.length - 1)
    }else{
    //if it wasnt here, mean1 is tmpsum (sum with new value) divided by length plus one, and mean2 just sum divided by elements
      mean1 = tmpsum / ( elements.length + 1)
      mean2 = sum / elements.length
    }
    

    var variance1:Double = 0
    var variance2:Double = 0
    for(el <- elements){
      variance1 = variance1 + calcVariance(mean1, field.getElementVal(el))
      variance2 = variance2 + calcVariance(mean2, field.getElementVal(el))
    }

    variance1 = variance1 + calcVariance(mean1, field.getElementVal(i))

    return math.abs(variance1 - variance2)
  }

}

object kmeans{

  def main(args: Array[String]): Unit = {
    var testData: Array[ClusterObject] = new Array[ClusterObject](10)

    testData(0) = new ClusterObject(1.1)
    testData(1) = new ClusterObject(1.3)
    testData(2) = new ClusterObject(1.8)
    testData(3) = new ClusterObject(4)
    testData(4) = new ClusterObject(7)
    testData(5) = new ClusterObject(8.1)
    testData(6) = new ClusterObject(7.5)
    testData(7) = new ClusterObject(3)
    testData(8) = new ClusterObject(10)
    testData(9) = new ClusterObject(20)

    lloyd(testData,3)
  }

  def shuffleDistribution(dataset: Array[_ <: ClusterComparable]) : Array[ClusterComparable] = {
    var r = new ArrayBuffer[ClusterComparable]()
    for(el <- dataset){
      if(r.length == 0){
        r += el
      }else{
        val rnd = Random.nextInt(r.length+1)
        if(rnd == r.length) r += el
        else r.insert(rnd, el)
      }
    }
    return r.toArray
  }

  def getInitialCluster(d: Array[_ <: ClusterComparable], k: Int): ClusterField = {
    val dataset = shuffleDistribution(d)
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
        initialDistribution.addCluster(cl)
      } 
    }
    return initialDistribution
  }
  
  def printClusterDistribution(field: ClusterField, iteration: Int){
    var i = 0
    println("\nAfter Iteration: "+iteration.toString)
    for(cl <- field.getClusters()){
      print("Cluster "+i.toString+" elements: [")
      val els = cl.getElements()
      print(field.getElementVal(els(0)).toString)
      for(k <- 1 until els.length){
        print(", "+field.getElementVal(els(k)).toString)
      }
      println("]")
      i = i + 1
    }
  }

  def lloyd(dataset: Array[_ <: ClusterComparable], k: Int): ClusterField = {
    print("Input:\n["+dataset(0).getComparableVal())

    for(i <- 1 until dataset.length){
      print(", " + dataset(i).getComparableVal())
    }
    println("]\n")
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
           if(clusterMinIndex == -1 || diff < minDiff){
             minDiff         = diff
             clusterMinIndex = clusterIndex
           }
           clusterIndex = clusterIndex + 1
        }
        if(beforeClusterIndex != -1 && beforeClusterIndex != clusterMinIndex){
          hasNotImproved=false
          var clusterIndex = 0
          for(cl <- clusters.getClusters()){
            if(clusterIndex == beforeClusterIndex) cl.removeElement(elIndex)
            if(clusterIndex == clusterMinIndex)    cl.addElement(elIndex)
            clusterIndex = clusterIndex + 1
          }
        }
        
      }
      iteration = iteration + 1
    }
  printClusterDistribution(clusters, iteration)
  return clusters
  }
}
