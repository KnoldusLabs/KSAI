package ksai.data.parser

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source


class ARFFParserTest extends FlatSpec with Matchers{


  "ARFFParser" should "fill up the Attributes from the ARFF file" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val attributes = ARFFParser.parse(arffFile)
    println(attributes.attributes)
    println()
    println(attributes.target)
    println()
    println(attributes.data)
    println()
    println(attributes.nominals)
    println()
    println(attributes.relation)

    assert(attributes.attributes != Nil)
  }

  it should "generate random values for weight" in {

  }


}
