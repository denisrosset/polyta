package com.faacets
package polyta
package solvers

package com.faacets
package polyta
package solvers

import scala.{specialized => sp}

trait PolytopeConverter[@sp(Double) A] {
  def solveH[M, V](polyhedron: VPolyhedron[M, V, A]): HPolyhedron[M, V, A]
  def solveV[M, V](polyhedron: HPolyhedron[M, V, A]): VPolyhedron[M, V, A]
}
