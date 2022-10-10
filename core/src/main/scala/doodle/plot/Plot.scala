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

import cats._
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.all._
import doodle.core.Point

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
  final case class ScatterPlot[C[_]: Reducible](data: C[Point]) extends Plot {
    val boundingBox: BoundingBox = BoundingBox.fromPoints(data)
  }
  final case class Interpolation[C[_]: Reducible](data: C[Point]) extends Plot {
    val boundingBox: BoundingBox = BoundingBox.fromPoints(data)
  }
  final case class And(plots: NonEmptyList[Plot]) extends Plot {
    val boundingBox = plots.reduceMap(plot => plot.boundingBox)
  }

  /** Create a scatterplot from data. */
  def scatterPlot[C[_]: Reducible](data: C[Point]): ScatterPlot[C] =
    ScatterPlot(data)

  /** Create a plot that interplates a curve between data points. */
  def interpolation[C[_]: Reducible](data: C[Point]): Interpolation[C] =
    Interpolation(data)

  /** Create a plot that interpolates a curve from data points sampled from the
    * given function in the range from start to end.
    */
  def function(start: Double, stop: Double)(
      f: Double => Double
  ): Interpolation[NonEmptyVector] = {
    val x0 = start.min(stop)
    val x1 = start.max(stop)
    val range = x1 - x0

    val data =
      for (x <- 0.to(100).map(x => (x.toDouble / 100.0) * range + x0))
        yield Point(x, f(x))

    interpolation(NonEmptyVector.fromVectorUnsafe(data.toVector))
  }
}
