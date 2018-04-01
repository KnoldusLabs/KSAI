package ksai.core.classification

/**
  * Classifier with online learning capability. Online learning is a model of
  * induction that learns one instance at a time. More formally, an online
  * algorithm proceeds in a sequence of trials.
  *
  * @tparam A
  */
trait OnlineClassifier[A] extends Classifier[A]{

  /**
    * Online update the classifier with a new training instance.
    * In general, this method may be NOT multi-thread safe.
    *
    * @param instance training instance.
    * @param label training label.
    */

  def learn(instance: A, label: Int)
}
