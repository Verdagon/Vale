package net.verdagon.vale.solver

import com.sun.tools.javac.code.TypeTag
import net.verdagon.vale.{solver, vassert, vassertSome, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//trait IRunePuzzler[Rule] {
//  def getPuzzles(rulexAR: Rule): Array[Array[Int]]
//}
//
//// Let's move this out to templar once we've finished migrating templar to it
//// Would use Arrays, but scala cant instantiate arrays of generics =\
//object TemplarPuzzler {
//  def apply[Rule, RuneID, RuleID, Literal, Lookup](
//    inputRule: Rule
//  ): Vector[Vector[RuneID]] = {
//    inputRule match {
//      case LiteralAR(range, canonicalResultRune, value) => {
//        Vector(Vector())
//      }
//      case LookupAR(range, canonicalResultRune, name) => {
//        Vector(Vector())
//      }
//      case IsConcreteAR(range, canonicalArgRune) => {
//        Vector(Vector(canonicalArgRune))
//      }
//      case IsInterfaceAR(range, canonicalArgRune) => {
//        Vector(Vector(canonicalArgRune))
//      }
//      case IsStructAR(range, canonicalArgRune) => {
//        Vector(Vector(canonicalArgRune))
//      }
//      case CoerceToCoord(range, coordRune, kindRune) => {
//        Vector(Vector(kindRune))
//      }
//      case CallAR(range, canonicalResultRune, canonicalTemplateRune, canonicalArgRunes) => {
//        Vector(Vector(canonicalResultRune, canonicalTemplateRune), (Vector(canonicalTemplateRune) ++ canonicalArgRunes).toVector)
//      }
//      case CoordComponentsAR(range, canonicalCoordRune, canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune) => {
//        Vector(Vector(canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune), Vector(canonicalCoordRune))
//      }
//      case CoordListAR(range, canonicalResultRune, canonicalElementRunes) => {
//        Vector(Vector(canonicalResultRune), canonicalElementRunes.toVector)
//      }
//      case AugmentAR(range, canonicalResultRune, literal, canonicalInnerRune) => {
//        Vector(Vector(canonicalResultRune), Vector(canonicalInnerRune))
//      }
//      case IsaAR(range, canonicalSubRune, canonicalInterfaceRune) => {
//        Vector(Vector(vimpl()))
//      }
//      case KindComponentsAR(range, canonicalKindRune, canonicalMutabilityRune) => {
//        Vector(Vector(canonicalKindRune), Vector(canonicalMutabilityRune))
//      }
//      case ManualSequenceAR(range, canonicalResultRune, canonicalElementRunes) => {
//        Vector(Vector(canonicalResultRune), canonicalElementRunes.toVector)
//      }
//      case OneOfAR(range, canonicalResultRune, possibilities) => {
//        Vector(Vector(canonicalResultRune))
//      }
//      case PrototypeAR(range, canonicalResultRune, name, canonicalParameterRunes, canonicalReturnRune) => {
//        Vector(Vector(canonicalResultRune), canonicalParameterRunes.toVector :+ canonicalReturnRune)
//      }
//      case RepeaterSequenceAR(range, canonicalResultRune, canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune) => {
//        Vector(Vector(canonicalResultRune), Vector(canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune))
//      }
//      case _ => vfail()
//    }
//  }
//}

//object Optimizer {
//  def canonicalizeRule[RuneID, RuleID, Literal, Lookup](
//    inputRule: IRulexAR[TentativeRune, RuleID, Literal, Lookup],
//    runeToCanonicalRune: TentativeRune => Int
//  ): IRulexAR[Int, RuleID, Literal, Lookup] = {
//    inputRule match {
//      case LiteralAR(range, uncanonicalResultRune, value) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        LiteralAR(range, canonicalResultRune, value)
//      }
//      case LookupAR(range, uncanonicalResultRune, name) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        LookupAR(range, canonicalResultRune, name)
//      }
//      case IsConcreteAR(range, uncanonicalRune) => {
//        val canonicalRune = runeToCanonicalRune(uncanonicalRune)
//        IsConcreteAR(range, canonicalRune)
//      }
//      case IsInterfaceAR(range, uncanonicalRune) => {
//        val canonicalRune = runeToCanonicalRune(uncanonicalRune)
//        IsInterfaceAR(range, canonicalRune)
//      }
//      case IsStructAR(range, uncanonicalRune) => {
//        val canonicalRune = runeToCanonicalRune(uncanonicalRune)
//        IsStructAR(range, canonicalRune)
//      }
//      case CoerceToCoord(range, uncanonicalCoordRune, uncanonicalKindRune) => {
//        val canonicalCoordRune = runeToCanonicalRune(uncanonicalCoordRune)
//        val canonicalKindRune = runeToCanonicalRune(uncanonicalKindRune)
//        CoerceToCoord(range, canonicalCoordRune, canonicalKindRune)
//      }
//      case CallAR(range, uncanonicalResultRune, uncanonicalTemplateRune, uncanonicalArgRunes) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalTemplateRune = runeToCanonicalRune(uncanonicalTemplateRune)
//        val canonicalArgRunes = uncanonicalArgRunes.map(runeToCanonicalRune).toArray
//        CallAR(range, canonicalResultRune, canonicalTemplateRune, canonicalArgRunes)
//      }
//      case CoordComponentsAR(range, uncanonicalCoordRune, uncanonicalOwnershipRune, uncanonicalPermissionRune, uncanonicalKindRune) => {
//        val canonicalCoordRune = runeToCanonicalRune(uncanonicalCoordRune)
//        val canonicalOwnershipRune = runeToCanonicalRune(uncanonicalOwnershipRune)
//        val canonicalPermissionRune = runeToCanonicalRune(uncanonicalPermissionRune)
//        val canonicalKindRune = runeToCanonicalRune(uncanonicalKindRune)
//        CoordComponentsAR(
//          range, canonicalCoordRune, canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune)
//      }
//      case CoordListAR(range, uncanonicalResultRune, uncanonicalElementRunes) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalElementRunes = uncanonicalElementRunes.map(runeToCanonicalRune).toArray
//        CoordListAR(range, canonicalResultRune, canonicalElementRunes)
//      }
//      case AugmentAR(range, uncanonicalResultRune, literal, uncanonicalInnerRune) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalInnerRune = runeToCanonicalRune(uncanonicalInnerRune)
//        solver.AugmentAR(range, canonicalResultRune, literal, canonicalInnerRune)
//      }
//      case IsaAR(range, uncanonicalSubRune, uncanonicalInterfaceRune) => {
//        val canonicalSubRune = runeToCanonicalRune(uncanonicalSubRune)
//        val canonicalInterfaceRune = runeToCanonicalRune(uncanonicalInterfaceRune)
//        IsaAR(range, canonicalSubRune, canonicalInterfaceRune)
//      }
//      case KindComponentsAR(range, uncanonicalKindRune, uncanonicalMutabilityRune) => {
//        val canonicalKindRune = runeToCanonicalRune(uncanonicalKindRune)
//        val canonicalMutabilityRune = runeToCanonicalRune(uncanonicalMutabilityRune)
//        KindComponentsAR(range, canonicalKindRune, canonicalMutabilityRune)
//      }
//      case ManualSequenceAR(range, uncanonicalResultRune, uncanonicalElementRunes) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalElementRunes = uncanonicalElementRunes.map(runeToCanonicalRune).toArray
//        ManualSequenceAR(range, canonicalResultRune, canonicalElementRunes)
//      }
//      case OneOfAR(range, uncanonicalResultRune, possibilities) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        OneOfAR(range, canonicalResultRune, possibilities)
//      }
//      case PrototypeAR(range, uncanonicalResultRune, name, uncanonicalParameterRunes, uncanonicalReturnRune) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalParameterRunes = uncanonicalParameterRunes.map(runeToCanonicalRune).toArray
//        val canonicalReturnRune = runeToCanonicalRune(uncanonicalReturnRune)
//        PrototypeAR(range, canonicalResultRune, name, canonicalParameterRunes, canonicalReturnRune)
//      }
//      case RepeaterSequenceAR(range, uncanonicalResultRune, uncanonicalMutabilityRune, uncanonicalVariabilityRune, uncanonicalSizeRune, uncanonicalElementRune) => {
//        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
//        val canonicalMutabilityRune = runeToCanonicalRune(uncanonicalMutabilityRune)
//        val canonicalVariabilityRune = runeToCanonicalRune(uncanonicalVariabilityRune)
//        val canonicalSizeRune = runeToCanonicalRune(uncanonicalSizeRune)
//        val canonicalElementRune = runeToCanonicalRune(uncanonicalElementRune)
//        RepeaterSequenceAR(range, canonicalResultRune, canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune)
//      }
//      case _ => vfail()
//    }
//  }
//}
