package ksai.core.classification

import breeze.collection.mutable.SparseArray
import ksai.math.Distribution

sealed trait Model

case object GENERAL extends Model

case object MULTINOMIAL extends Model

case object BERNOULLI extends Model


case class NaiveBayes(
                       serialVersionUID: Long = 1L,
                       EPSILON: Double = 1E-20,
                       model: Model,
                       classCount: Int, //The number of classes.
                       independentVariablesCount: Int, //The number of independent variables.
                       priori: Array[Double] = Array.emptyDoubleArray, //A priori probabilities of each class.
                       prob: Array[Array[Distribution]] = Array.empty, //The conditional distribution for general purpose naive Bayes classifier.
                       sigma: Double = 1.0,
                       predefinedPriori: Boolean = false, //True if the priori probabilities are pre defined by user.
                       instanceCount: Int = 0, //The total number of instances.
                       instancesInEachClass: Array[Int] = Array.emptyIntArray, //The number of instances in each class.
                       termsInEachClass: Array[Int] = Array.emptyIntArray, //The number of terms of each class.
                       ntc: Array[Array[Int]] = Array.empty, //The number of each term in each class.
                       condprob: Array[Array[Double]] = Array.empty //The log conditional probabilities for document classification.
                     ) extends OnlineClassifier[Array[Double]]
  with SoftClassifier[Array[Double]] with Serializable {

  /**
    * Online learning of naive Bayes classifier on a sequence,
    * which is modeled as a bag of words. Note that this method is NOT
    * applicable for naive Bayes classifier with general generation model.
    *
    * @param instance training instance.
    * @param label    training label in [0, k), where k is the number of classes.
    */

  override def learn(instance: Array[Double], label: Int): Unit = {
    if (instance.length != independentVariablesCount) throw new IllegalArgumentException("Invalid input vector size: " + instance.length)

    model match {
      case GENERAL => throw new UnsupportedOperationException("General-mode Naive Bayes classifier doesn't support online learning.")
      case MULTINOMIAL =>
        for {
          iterator <- 0 until independentVariablesCount
        } yield {
          ntc(label)(iterator) + instance(iterator)
          termsInEachClass(label) + instance(iterator)
        }
        instancesInEachClass(label) + 1
        copy(instanceCount = instanceCount + 1)
        update()
      case BERNOULLI =>
        for {
          iterator <- 0 until independentVariablesCount
        } yield {
          if (instance(iterator) > 0) {
            ntc(label)(iterator) + 1
          }
        }
        instancesInEachClass(label) + 1
        copy(instanceCount = instanceCount + 1)
        update()


    }
  }

  /**
    * Online learning of naive Bayes classifier on a sequence,
    * which is modeled as a bag of words. Note that this method is NOT
    * applicable for naive Bayes classifier with general generation model.
    *
    * @param instance training instance in sparse format.
    * @param label training label in [0, k), where k is the number of classes.
    */
  def learn(instance:SparseArray[Double], label: Int): Unit = {
    model match {
      case GENERAL =>
        throw new UnsupportedOperationException("General-mode Naive Bayes classifier doesn't support online learning.")
      case MULTINOMIAL =>
        for(iterator <- instance.index) {
          ntc(label)(iterator) + instance.valueAt(iterator)
          termsInEachClass(label) + instance.valueAt(iterator)
        }
        instanceCount + 1
        instancesInEachClass(label) + 1
        update()
      case BERNOULLI =>
        for(iterator <- instance.index) {
          if(instance.valueAt(iterator) > 0) {
            ntc(label)(iterator) + 1
          }
        }
        instanceCount + 1
        instancesInEachClass(label) + 1
        update()
    }
  }


  /**
    * Online learning of naive Bayes classifier on sequences,
    * which are modeled as a bag of words. Note that this method is NOT
    * applicable for naive Bayes classifier with general generation model.
    *
    * @param instances training instances.
    * @param labels    training labels in [0, k), where k is the number of classes.
    */
  def learn(instances: Array[Array[Double]], labels: Array[Int]) {
    model match {
      case GENERAL => throw new UnsupportedOperationException("General-mode Naive Bayes classifier doesn't support online learning.")
      case MULTINOMIAL =>
        for {
          outerIterator <- 0 until instances.length
        } yield {
          if (instances(outerIterator).length != independentVariablesCount) {
            throw new IllegalArgumentException("Invalid input vector size: " + instances(outerIterator).length)
          }
          for {
            innerIterator <- 0 until independentVariablesCount
          } yield {
            this.ntc(labels(outerIterator))(innerIterator) + instances(outerIterator)(innerIterator)
            this.termsInEachClass(labels(outerIterator)) + instances(outerIterator)(innerIterator)
          }
          this.instanceCount + 1
          this.instancesInEachClass(labels(outerIterator)) + 1
        }
      case BERNOULLI =>
        for {
          outerIterator <- 0 until instances.length
        } yield {
          if (instances(outerIterator).length != independentVariablesCount) {
            throw new IllegalArgumentException("Invalid input vector size: " + instances(outerIterator).length)
          }
          for {
            innerIterator <- 0 until independentVariablesCount
          } yield {
            if (instances(outerIterator)(innerIterator) > 0) {
              this.ntc(labels(outerIterator))(innerIterator) + 1
            }
          }

          this.instanceCount + 1
          this.instancesInEachClass(labels(outerIterator)) + 1
        }
    }

    update
  }


  /**
    * Update conditional probabilities.
    */
  def update(): Unit = {
    if (!predefinedPriori) for (iterator <- 0 until classCount ){
      priori(iterator) = (instancesInEachClass(iterator)+ EPSILON) / (instanceCount + classCount * EPSILON)
    }

    model match {
      case MULTINOMIAL =>
        for(outerIterator <- 0 until classCount) {
          for(innerIterator <- 0 until independentVariablesCount) {
            condprob(outerIterator)(innerIterator) = (ntc(outerIterator)(innerIterator) + sigma) / (termsInEachClass(outerIterator) + sigma * independentVariablesCount)
          }
        }
      case _ =>
        for(outerIterator <- 0 until classCount) {
          for(innerIterator <- 0 until independentVariablesCount) {
            condprob(outerIterator)(innerIterator) = (ntc(outerIterator)(innerIterator) + sigma) / (termsInEachClass(outerIterator) + sigma * 2)
          }
        }
    }
  }

  /**
    * Predict the class of an instance.
    *
    * @param instances the instance to be classified.
    * @return the predicted class label. For MULTINOMIAL and BERNOULLI models,
    * returns -1 if the instance does not contain any feature words.
    */
  override def predict(instances: Array[Double]):Int = {
    predict(instances, Array.emptyDoubleArray)
  }

  /**
    * Predict the class of an instance.
    *
    * @param instances the instance to be classified.
    * @param posteriori the array to store a posteriori probabilities on output.
    * @return the predicted class label. For MULTINOMIAL and BERNOULLI models,
    * returns -1 if the instance does not contain any feature words.
    */
  override def predict(instances:Array[Double], posteriori:Array[Double]):Int = {
    if (instances.length != independentVariablesCount) {
      throw new IllegalArgumentException(String.format("Invalid input vector size: " + instances.length))
    }

    if (posteriori.nonEmpty && posteriori.length != classCount) {
      throw new IllegalArgumentException(String.format(s"Invalid posteriori vector size: ${posteriori.length}, expected: $classCount"))
    }

    var label = -1
    var max = Double.NegativeInfinity
    var `any` = model match {
      case GENERAL => true
      case _ => false
    }

    for (outerIterator <- 0 until classCount) {

      var logprob = Math.log(priori(outerIterator))

      for (innerIterator <- 0 until independentVariablesCount) {
        model match {
          case GENERAL => logprob += prob(outerIterator)(innerIterator).logp(instances(innerIterator))
          case MULTINOMIAL =>
            if(instances(innerIterator) > 0) {
              logprob += instances(innerIterator) * Math.log(condprob(outerIterator)(innerIterator))
              `any` = true
            }
          case BERNOULLI =>
            if(instances(outerIterator) > 0){
              logprob += Math.log(condprob(outerIterator)(innerIterator))
              `any` = true
            } else {
              logprob += Math.log(1.0 - condprob(outerIterator)(innerIterator))
            }
        }
      }

      if(logprob > max && `any`) {
        max = logprob
        label = outerIterator
      }

      if (posteriori.nonEmpty) {
        posteriori(outerIterator) = logprob
      }
    }

    if(posteriori.nonEmpty && `any`) {
      val posterioriSum = posteriori.map( value => Math.exp(value - max)).sum
      for(iterator <- posteriori.indices) {
        posteriori(iterator) = posteriori(iterator) / posterioriSum
      }
    }
    label
  }

  /**
    * Predict the class of an instance.
    *
    * @param instance the instance to be classified.
    * @return the predicted class label. For MULTINOMIAL and BERNOULLI models,
    *         returns -1 if the instance does not contain any feature words.
    */
  def predict(instance: SparseArray[Double]): Int ={
    predict(instance, Array.emptyDoubleArray)
  }

  /**
    * Predict the class of an instance.
    *
    * @param instance the instance to be classified.
    * @param posteriori the array to store a posteriori probabilities on output.
    * @return the predicted class label. For MULTINOMIAL and BERNOULLI models,
    *         returns -1 if the instance does not contain any feature words.
    */
  def predict(instance: SparseArray[Double] , posteriori: Array[Double]):Int = {
    if (posteriori.nonEmpty && posteriori.length != classCount) {
      throw new IllegalArgumentException(String.format(s"Invalid posteriori vector size: ${posteriori.length}, expected: $classCount"))
    }

    var label = -1
    var max = Double.NegativeInfinity
    var `any` = model match {
      case GENERAL => true
      case _ => false
    }

    for (outerIterator <- 0 until classCount) {
      var logprob = Math.log(priori(outerIterator))

      for (innerIterator <- instance.index){
        model match {
          case GENERAL => logprob += prob(outerIterator)(innerIterator).logp(instance.valueAt(innerIterator))
          case MULTINOMIAL =>
            if(instance.valueAt(innerIterator) > 0) {
              logprob += instance.valueAt(innerIterator) * Math.log(condprob(outerIterator)(innerIterator))
              `any` = true
            }
          case BERNOULLI =>
            if(instance.valueAt(innerIterator) > 0){
              logprob += instance.valueAt(innerIterator) * Math.log(condprob(outerIterator)(innerIterator))
              `any` = true
            } else {
              logprob += Math.log(1.0 - condprob(outerIterator)(innerIterator))
            }
        }
      }

      if(logprob > max && `any`) {
        max = logprob
        label = outerIterator
      }

      if(posteriori.nonEmpty) {
        posteriori(outerIterator) = logprob
      }
    }

    if(posteriori.nonEmpty && `any`) {
      val posterioriSum = posteriori.map( value => Math.exp(value - max)).sum
      for(iterator <- posteriori.indices) {
        posteriori(iterator) = posteriori(iterator) / posterioriSum
      }
    }
    label

  }



}

object NaiveBayes {

  def apply(priori: Array[Double], condprob: Array[Array[Distribution]]) = {

    if (priori.length != condprob.length) {
      throw new IllegalArgumentException("The number of priori probabilities and that of the classes are not same.")
    }
    val sum = priori.map {
      value =>
        if (value <= 0.0 || value >= 1) {
          throw new IllegalArgumentException("Invalid priori probability: " + value)
        } else value
    }.sum

    if (Math.abs(sum - 1.0) > 1E-10) {
      throw new IllegalArgumentException("The sum of priori probabilities is not one: " + sum)
    }

    new NaiveBayes(model = GENERAL, classCount = priori.length, independentVariablesCount = condprob(0).length, priori = priori,
      prob = condprob, predefinedPriori = true)

  }

  def apply(model: Model, classCount: Int, independentVariablesCount: Int) = {
    new NaiveBayes(model = model, classCount = classCount, independentVariablesCount = independentVariablesCount)
  }

  def apply(model: Model, classCount: Int, independentVariablesCount: Int, sigma: Double) = {

    if (classCount < 2) {
      throw new IllegalArgumentException("Invalid number of classes: " + classCount)
    }

    if (independentVariablesCount <= 0) {
      throw new IllegalArgumentException("Invalid dimension: " + independentVariablesCount)
    }

    if (sigma < 0) {
      throw new IllegalArgumentException("Invalid add-k smoothing parameter: " + sigma)
    }

    new NaiveBayes(model = model, classCount = classCount, independentVariablesCount = independentVariablesCount,
      sigma = sigma, priori = new Array[Double](classCount), instancesInEachClass = new Array[Int](classCount),
      termsInEachClass = new Array[Int](classCount), ntc = Array.ofDim[Int](classCount, independentVariablesCount),
      condprob = Array.ofDim[Double](classCount, independentVariablesCount))

  }

  def apply(model: Model, priori: Array[Double], independentVariablesCount: Int) = {
    new NaiveBayes(model = model, independentVariablesCount = independentVariablesCount, priori = priori, classCount = priori.length)
  }

  def apply(model: Model, priori: Array[Double], independentVariablesCount: Int, sigma: Double) = {
    if (independentVariablesCount <= 0) {
      throw new IllegalArgumentException("Invalid dimension: " + independentVariablesCount)
    }

    if (sigma < 0) {
      throw new IllegalArgumentException("Invalid add-k smoothing parameter: " + sigma)
    }

    if (priori.length < 2) {
      throw new IllegalArgumentException("Invalid number of classes: " + priori.length)
    }
    val sum = priori.map {
      value =>
        if (value <= 0.0 || value >= 1) {
          throw new IllegalArgumentException("Invalid priori probability: " + value)
        } else value
    }.sum

    if (Math.abs(sum - 1.0) > 1E-10) {
      throw new IllegalArgumentException("The sum of priori probabilities is not one: " + sum)
    }

    new NaiveBayes(model = model, classCount = priori.length, independentVariablesCount = independentVariablesCount,
      sigma = sigma, priori = priori, predefinedPriori = true, instancesInEachClass = new Array[Int](priori.length),
      termsInEachClass = new Array[Int](priori.length), ntc = Array.ofDim[Int](priori.length, independentVariablesCount),
      condprob = Array.ofDim[Double](priori.length, independentVariablesCount))
  }


}


case class NaiveBayesTrainer(
                              model: Model,
                              classCount: Int, //The number of classes.
                              independentVariablesCount: Int, //The number of independent variables.
                              priori: Array[Double] = Array.emptyDoubleArray, //A priori probabilities of each class.
                              sigma: Double = 1.0
                            ) {
  require(sigma < 0 , "Invalid add-k smoothing parameter: " + sigma)

  def train(x: Array[Array[Double]], y: Array[Int]): NaiveBayes = {
    val bayes = NaiveBayes(model = model, classCount = classCount,
        independentVariablesCount = independentVariablesCount, sigma = sigma, priori = priori)
    bayes.learn(x, y)
    bayes
  }
}

object NaiveBayesTrainer {

  def apply(model: Model, classCount: Int, independentVariablesCount: Int) = {
    if (classCount < 2) throw new IllegalArgumentException("Invalid number of classes: " + classCount)
    if (independentVariablesCount <= 0) throw new IllegalArgumentException("Invalid dimension: " + independentVariablesCount)
    commonApply(model, classCount, independentVariablesCount, Array.emptyDoubleArray)
  }

  def apply(model: Model, classCount: Int, independentVariablesCount: Int, priori: Array[Double]) = {
    if (independentVariablesCount <= 0) {
      throw new IllegalArgumentException("Invalid dimension: " + independentVariablesCount)
    }

    if (priori.length < 2) {
      throw new IllegalArgumentException("Invalid number of classes: " + priori.length)
    }

    val sum = priori.map {
      value =>
        if (value <= 0.0 || value >= 1) {
          throw new IllegalArgumentException("Invalid priori probability: " + value)
        } else value
    }.sum

    if (Math.abs(sum - 1.0) > 1E-10) {
      throw new IllegalArgumentException("The sum of priori probabilities is not one: " + sum)
    }

    commonApply(model, priori.length, independentVariablesCount, priori)

  }

  def commonApply(model: Model, classCount: Int, independentVariablesCount: Int, priori: Array[Double]) = {
    new NaiveBayesTrainer(model, classCount, independentVariablesCount, priori)
  }
}


