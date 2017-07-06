/**
  * Created by fang on 7/5/17.
  */
private[this] sealed abstract class ExpandType
private[this] object ExpandTypeTop extends ExpandType
private[this] object ExpandTypeBottom extends ExpandType
private[this] object ExpandTypeSide extends ExpandType


class Layout(val text: Array[String], val alignPos: Int = 0) {
  val height: Int = text.length
  val width: Int = text.head.length

  override def toString: String = text.mkString("\n")

  def appendedWith(other: Layout): Layout = {
    val newLeft = this.expandTop(other.heightAboveAlignPos - this.heightAboveAlignPos)
      .expandBottom(other.heightBelowAlignPos - this.heightBelowAlignPos)
    val newRight = other.expandTop(this.heightAboveAlignPos - other.heightAboveAlignPos)
      .expandBottom(this.heightBelowAlignPos - other.heightBelowAlignPos)
    new Layout(newLeft.text.zip(newRight.text).map(e => e._1 + e._2), alignPos)
  }

  def topOn(other: Layout): Layout = {
    val newTop = this.expandSide(other.width - this.width)
    val newBottom = other.expandSide(this.width - other.width)
    val div = "-" * newTop.width
    new Layout((newTop.text :+ div) ++ newBottom.text, newTop.height)
  }

  private def expand(margin: Int, expandType: ExpandType): Layout =
    if (margin <= 0) {
      this
    } else {
      expandType match {
        case ExpandTypeTop | ExpandTypeBottom =>
          val padding = Array.fill(margin)(" " * width)
          if (expandType == ExpandTypeTop) {
            new Layout(padding ++ text, alignPos + margin)
          } else {
            new Layout(text ++ padding, alignPos)
          }
        case ExpandTypeSide =>
          val leftMargin: Int = margin / 2
          val rightMargin: Int = margin - leftMargin
          val newText = text.map(" " * leftMargin + _ + " " * rightMargin)
          new Layout(newText, alignPos)
      }
    }

  private def expandTop(numRows: Int): Layout = expand(numRows, ExpandTypeTop)
  private def expandBottom(numRows: Int): Layout = expand(numRows, ExpandTypeBottom)
  private def expandSide(numCols: Int): Layout = expand(numCols, ExpandTypeSide)

  private def heightAboveAlignPos: Int = alignPos
  private def heightBelowAlignPos: Int = height - 1 - alignPos
}
