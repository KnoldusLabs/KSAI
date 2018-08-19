package ksai.core.association.totalsupporttree

class Node(
            /**
              * The id of the item.
              */
            val id: Int = -1,

            /**
              * The support associated with the itemset represented by the node.
              */
            var support: Int = 0,

            /**
              * The set of children nodes.
              */
            var maybeChildren: Option[Array[Node]] = None)

object Node {

  /**
    * Constructor.
    *
    * @param id The id of the item.
    */
  def apply(id: Int = -1): Node = new Node(id)
}
