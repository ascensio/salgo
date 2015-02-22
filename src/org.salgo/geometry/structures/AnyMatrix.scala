package org.salgo.geometry.structures

trait AnyMatrix {
  def rowDimension: Int
  def columnDimension: Int
  def isSquare = this.rowDimension == this.rowDimension
}


