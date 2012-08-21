package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
 import sys._

case class Person(age: Int, firstName: String, lastName: String)

object BasicListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.max
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1.length,l2.length) match{
      case (0 , _) => l2
      case (_ , 0) => l1
      case (_, _) => (l1,l2).zipped.map( _ + _ )
    }
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    l.toList.foldLeft(List[Int]())((m,n) => sumOfTwo(m,n))
  }

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]]  = {
    val partitioned = persons.sortBy(_.age).partition( a => a.age < 18) 
    val boysNames = partitioned._1.sortBy(_.age).map(_.firstName)
    val menNames  = partitioned._2.sortBy(_.age).map(_.firstName)
    List(boysNames,menNames)
  }
}