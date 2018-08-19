package ksai.core.association.fptree

class HeaderTableItem(
                       /**
                         * The item identifier.
                         */
                       val id: Int,

                       /**
                         * The forward link to the first node in the link list of nodes.
                         */
                       var maybeNode: Option[Node] = None,

                       /**
                         * the support of single item.
                         */
                       val count: Int = 0)
