package doodle.plot

import doodle.core.Point
import cats.Reducible
import cats.syntax.all._
import cats.kernel.Semigroup

final case class BoundingBox(
    left: Double,
    top: Double,
    right: Double,
    bottom: Double
)
object BoundingBox {
  implicit val boundingBoxSemigroup: Semigroup[BoundingBox] =
    new Semigroup[BoundingBox] {
      def combine(x: BoundingBox, y: BoundingBox): BoundingBox = {
        BoundingBox(
          x.left.min(y.left),
          x.top.max(y.top),
          x.right.max(y.right),
          x.bottom.min(y.bottom)
        )
      }
    }

  def apply(point: Point): BoundingBox =
    BoundingBox(point.x, point.y, point.x, point.y)

  def fromPoints[C[_]: Reducible](points: C[Point]): BoundingBox =
    points.reduceMap(pt => BoundingBox(pt))
}
