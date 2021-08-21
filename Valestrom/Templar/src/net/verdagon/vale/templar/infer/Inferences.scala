package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer.{CoordTemplataType, ITemplataType, IntegerTemplataType, KindTemplataType, MutabilityTemplataType, OwnershipTemplataType, PackTemplataType, PermissionTemplataType, PrototypeTemplataType, StringTemplataType, VariabilityTemplataType}
import net.verdagon.vale.templar.IRuneT
import net.verdagon.vale.templar.templata.{CoordListTemplata, CoordTemplata, ITemplata, IntegerTemplata, KindTemplata, MutabilityTemplata, OwnershipTemplata, PermissionTemplata, PrototypeTemplata, StringTemplata, VariabilityTemplata}
import net.verdagon.vale.templar.types.InterfaceTT
import net.verdagon.vale.{vassert, vcurious, vfail, vwat}

case class Inferences(
    typeByRune: Map[IRuneT, ITemplataType], // Here for doublechecking
    boundsByRune: Map[IRuneT, Vector[InterfaceTT]],
    templatasByRune: Map[IRuneT, ITemplata],
    possibilitiesByRune: Map[IRuneT, Vector[ITemplata]]) {
  override def hashCode(): Int = vcurious()

  def addBound(rune: IRuneT, interface: InterfaceTT): Inferences = {
    // If theres a declared type, make sure it matches.
    // SolverKindRune wouldnt have a type for example.
    vassert(typeByRune.get(rune).contains(KindTemplataType))

    val existingBoundsForThisRune: Vector[InterfaceTT] = boundsByRune.getOrElse(rune, Vector[InterfaceTT]())
    val newBoundsForThisRune: Vector[InterfaceTT] = existingBoundsForThisRune :+ interface

    Inferences(
      typeByRune,
      boundsByRune + (rune -> newBoundsForThisRune),
      templatasByRune,
      possibilitiesByRune)
  }
  def addConclusion(rune: IRuneT, templata: ITemplata): Inferences = {
    templatasByRune.get(rune) match {
      case None =>
      case Some(existingConclusion) => vassert(templata == existingConclusion)
    }

    // If theres a declared type, make sure it matches.
    // SolverKindRune wouldnt have a type for example.
    (typeByRune.get(rune), templata) match {
      case (None, _) =>
      case (Some(CoordTemplataType), CoordTemplata(_)) =>
      case (Some(StringTemplataType), StringTemplata(_)) =>
      case (Some(KindTemplataType), KindTemplata(_)) =>
      case (Some(IntegerTemplataType), IntegerTemplata(_)) =>
      case (Some(MutabilityTemplataType), MutabilityTemplata(_)) =>
      case (Some(VariabilityTemplataType), VariabilityTemplata(_)) =>
      case (Some(OwnershipTemplataType), OwnershipTemplata(_)) =>
      case (Some(PermissionTemplataType), PermissionTemplata(_)) =>
      case (Some(PrototypeTemplataType), PrototypeTemplata(_)) =>
      case (Some(PackTemplataType(CoordTemplataType)), CoordListTemplata(_)) =>
      case _ => vwat()
    }

    Inferences(
      typeByRune,
      boundsByRune,
      templatasByRune + (rune -> templata),
      possibilitiesByRune - rune)
  }
  def addPossibilities(rune: IRuneT, possibilities: Vector[ITemplata]): Inferences = {
    if (possibilities.size == 0) {
      vwat()
    } else if (possibilities.size == 1) {
      addConclusion(rune, possibilities.head)
    } else {
      vassert(!templatasByRune.contains(rune))
      possibilitiesByRune.get(rune) match {
        case None =>
        case Some(existingPossibilities) => vassert(possibilities == existingPossibilities)
      }
      Inferences(
        typeByRune,
        boundsByRune,
        templatasByRune,
        possibilitiesByRune + (rune -> possibilities))
    }
  }
  // Returns an Inferences without this rune, and gives all the possibilities for that rune
  def pop(rune: IRuneT): (Inferences, Vector[ITemplata]) = {
    val inferencesWithoutThatRune = Inferences(typeByRune, boundsByRune, templatasByRune, possibilitiesByRune - rune)
    (inferencesWithoutThatRune, possibilitiesByRune(rune))
  }
}

case class InferencesBox(var inferences: Inferences) {
  override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  def templatasByRune: Map[IRuneT, ITemplata] = inferences.templatasByRune
  def possibilitiesByRune: Map[IRuneT, Vector[ITemplata]] = inferences.possibilitiesByRune

  def addConclusion(rune: IRuneT, templata: ITemplata): Unit = {
    inferences = inferences.addConclusion(rune, templata)
  }
  def addPossibilities(rune: IRuneT, possibilities: Vector[ITemplata]): Unit = {
    inferences = inferences.addPossibilities(rune, possibilities)
  }
  // Returns an Inferences without this rune, and gives all the possibilities for that rune
  def pop(rune: IRuneT): Vector[ITemplata] = {
    val (newInferences, result) = inferences.pop(rune)
    inferences = newInferences
    result
  }
}
