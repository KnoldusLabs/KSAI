package ksai.core.association

class Node(var parent: Option[Node] = None,
           var next: Option[Node] = None,
           var children: Option[java.util.HashMap[Int, Node]] = None,
           var id: Int = -1,
           var count: Int = 0) {

  def add(index: Int, end: Int, itemset: Array[Int], support: Int, fPTree: FPTree): Unit = {
    if (index < end) {
      val childrenNodes: java.util.HashMap[Int, Node] = children.fold(new java.util.HashMap[Int, Node])(identity)

      val maybeChild: Option[Node] = Option(childrenNodes.get(itemset(index)))

      maybeChild.fold(append(index, end, itemset, support, fPTree)) { child =>
        child.count += support
        child.add(index + 1, end, itemset, support, fPTree)
      }
    }
  }

  def append(index: Int, end: Int, itemset: Array[Int], support: Int, fPTree: FPTree): Unit = {
    if (index < end) {

      val childrenNodes: java.util.HashMap[Int, Node] = children.fold(new java.util.HashMap[Int, Node])(identity)

      if (index >= fPTree.maxItemSetSize) fPTree.maxItemSetSize += 1

      val item = itemset(index)

      val parentNode = if (id < 0) None else Some(this)
      val child = new Node(id = item, count = support, parent = parentNode)

      child.addToHeaderTable(fPTree)

      childrenNodes.put(item, child)
      children = Some(childrenNodes)

      child.append(index + 1, end, itemset, support, fPTree)
    }
  }

  def addToHeaderTable(fPTree: FPTree): Unit = {
    next = fPTree.headerTable(fPTree.order(id)).maybeNode
    fPTree.headerTable(fPTree.order(id)).maybeNode = Some(this)
  }

}
