package ksai.core.association.fptree

case class ItemSet(
                    /**
                      * The set of items.
                      */
                    items: Array[Int],

                    /**
                      * The associated support of the item set.
                      */
                    support: Int)
