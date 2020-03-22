package genetics
import genetics.genes.{Gene, _}
import genetics.geometry._



object GeneticAlgorithm
{

  def generateIndividual(sampleChromosome:List[Gene]):Animal = {
    var currentChromosome = List.empty[Gene]
    for (i <- 1 to sampleChromosome.length)
    {currentChromosome = currentChromosome :+ new Gene(math.random())}

    var currentAnimal = new Animal(currentChromosome.toList)
    /* Animals are generated with 0.0 fitness*/
    currentAnimal

  }
  def fitnessStatus(population: Array[Animal]):Unit= {
    var avg=0.0
    var iter = 0
    for (i<-population)
      {
        avg=avg + i.fitness
      iter = iter +1
      }
    println(avg/iter + " Is the average fitness of this generation")
    println(population.last.fitness + "The highest fitness rating so far")

  }
  def mutateAnimal(animal: Animal):Animal= {
    animal.genes.map(x=>
      if(math.random()>=0.5)
      {math.round(x.geneValue + x.geneValue *.1)}
      else
      {math.round(x.geneValue - x.geneValue *.1)}
    )
    animal
  }
  def NewGeneration(oldGeneration:Array[Animal]):Array[Animal] = {
    var newGeneration=new Array[Animal](oldGeneration.length)
    newGeneration(0)=oldGeneration.last
    newGeneration(1)=mutateAnimal(oldGeneration.last)
    newGeneration(2)=mutateAnimal(oldGeneration.last)
    newGeneration(3)=mutateAnimal(oldGeneration.apply(oldGeneration.length-1))
    for (i <- 4 to oldGeneration.length-1)
    {
      newGeneration(i)= generateIndividual(oldGeneration.last.genes)
    }
    newGeneration

  }



  def geneticAlgorithm[T](fitnessFunction:(T)=> Double ,chromosomeDecoder:(List[Gene])=> T,sampleChromosome:List[Gene]): T =
    {
      val maxPopSize:Int = 20
      var currentPopulation:Array[Animal] = Array.empty[Animal]
      for(i <- 1 to maxPopSize)
        {currentPopulation= currentPopulation :+ generateIndividual(sampleChromosome)}


      for(individual <- currentPopulation)
        {
          var potentialSolution:T = chromosomeDecoder(individual.genes) /* generates a solution based of the individuals unique genes" */
          individual.fitness = fitnessFunction(potentialSolution)       /* Assigns a fitness rating based on the how the how the solution held up to the test conditions */
        }
      currentPopulation = currentPopulation.sortBy(_.fitness)
      /*the array is now in order from least fit to most fit 0 ti 1*/
      /*fitnessStatus(currentPopulation)*/


      for(i <- 0 until 90000) {
        /*println("A new Generation is born!")
        println("_____________________________")*/
        currentPopulation = NewGeneration(currentPopulation)
        for (individual <- currentPopulation) {
          var potentialSolution: T = chromosomeDecoder(individual.genes) /* generates a solution based of the individuals unique genes" */
          individual.fitness = fitnessFunction(potentialSolution) /* Assigns a fitness rating based on the how the how the solution held up to the test conditions */
        }
        currentPopulation = currentPopulation.sortBy(_.fitness)
        /*fitnessStatus(currentPopulation)*/
      }

      chromosomeDecoder(currentPopulation.last.genes)

    }





  def linearReg1stParam(scatterPlot: Array[Point]):Line => Double =
  {
    input:Line =>
    {
      var distanceError:Double = 0.0
      for(i <- scatterPlot )
      {
        distanceError= distanceError + math.abs( i.y-input.evaluate(i.x))

      }
      distanceError
      -(math.atan(distanceError)/(math.Pi/2))+1
    }

  }
  def linearReg2ndParam():List[Gene] => Line =
  {
    param:List[Gene]=>
    {val geneList = param.toArray

      val slope = math.tan((geneList.apply(0).geneValue - .05)*(math.Pi))
      val yIntercept = math.tan((geneList.apply(1).geneValue - .05)*(math.Pi))
      val outputLine = new Line(slope,yIntercept)
      outputLine
    }

  }
  def linearReg3rdParam():List[Gene] = List[Gene](new Gene(420),new Gene(420))

  /*return a line which minimizes the sum of the y distances of the points to the line.*/
   def linearRegression(scatterPlot:List[Point]):Line =
  {
    val samplepoints:Array[Point] = scatterPlot.toArray
  geneticAlgorithm[Line](linearReg1stParam(samplepoints),linearReg2ndParam(),linearReg3rdParam())







  }

 def main(args: Array[String]): Unit = {
    val input:List[Point] = List(new Point(1,1),new Point(2,2))
    val answer = linearRegression(input)
    println(answer.slope+" "+answer.yIntercept)
  }



}
