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
