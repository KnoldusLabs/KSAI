package ksai.core.association

import java.util.Comparator

object HeaderTableItemComparator extends Comparator[HeaderTableItem] {

  override def compare(headerTableItem1: HeaderTableItem, headerTableItem2: HeaderTableItem) = headerTableItem2.count - headerTableItem1.count
}