/*
 * Copyright 2022 Creative Scala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package doodle.plot

import cats.Reducible
import cats.kernel.Semigroup
import cats.syntax.all._
import doodle.core.Point

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
