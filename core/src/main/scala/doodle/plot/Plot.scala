package doodle.plot

import cats._
import cats.data.NonEmptyList
import doodle.core.Point
import cats.syntax.all._

sealed abstract class Plot extends Product with Serializable {
  import Plot._

  def boundingBox: BoundingBox

  def and(that: Plot): Plot =
    this match {
      case And(plots) => And(that :: plots)
      case other      => And(NonEmptyList.of(that, other))
    }
}
object Plot {
  final case class ScatterPlot[C[_]](data: C[Point], reducible: Reducible[C])
      extends Plot {
    val boundingBox: BoundingBox = BoundingBox.fromPoints(data)(reducible)
  }
  final case class Interpolation[C[_]](data: C[Point], reducible: Reducible[C])
      extends Plot {
    val boundingBox: BoundingBox = BoundingBox.fromPoints(data)(reducible)
  }
  final case class And(plots: NonEmptyList[Plot]) extends Plot {
    val boundingBox = plots.reduceMap(plot => plot.boundingBox)
  }
}
