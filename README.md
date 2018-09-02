KSAI
=====

KSAI is a machine learning library contains various algorithms such as classification, regression, clustering and many others. It is an attempt to build machine learning algorithms with the language Scala. The library Breeze, which is built on scala is getting used for doing the mathematical functionalities. KSAI is inspired from a java based ml library called SMILE.

KSAI mainly used scala’s in built case classes, Future and some of the other cool feature that it has. It has also used Akka in some places. In order to start exploring the library the test cases might be good start. Right now it might not be that easy to use the library with limited documentation and unclear api, however the committers will update them in the near future.


Installation
-------------

You can use the libraries through Maven central repository by adding the following to your project pom.xml file

```
    <dependency>
        <groupId>io.github.knolduslabs.ksai</groupId>
 	    <artifactId>ksai</artifactId>
 	    <version>0.1</version>
    </dependency>
```

Or in build.sbt

```
    libraryDependencies += "io.github.knolduslabs.ksai" %% "ksai" % "0.1"
```

Algorithms
--------------

Below are some of the algorithms that has been implemented

* **Neural Network**
* **Association Rule**
* **Decision Tree**
* **Random Forest**
* **KNN**
* **K-Means**
* **Naive Bayes**
* **Logistic Regression**
* **PCA**
* **LDA**
* **Single Noise Ratio**
* **Sum Square Ratio**

Committers
-------------

* *Pranjut Gogoi*
* *Girish Bharti*
* *Anuj Saxena*
* *Nitin Aggarwal*
* *Akshansh Jain*
* *Shubham Aggarwal*
