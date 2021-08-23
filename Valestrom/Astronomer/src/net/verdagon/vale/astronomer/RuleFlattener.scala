package net.verdagon.vale.astronomer

import net.verdagon.vale.scout._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{AugmentAR, BuiltinCallAR, CallAR, CoordComponentsAR, CoordListAR, IRulexAR, IsaAR, KindComponentsAR, LiteralAR, LookupAR, ManualSequenceAR, OrAR, PrototypeAR, RepeaterSequenceAR, RuleOptimizer, RuneWorld, RuneWorldBuilder, RuneWorldSolverState, TentativeRune}
import net.verdagon.vale.{solver, vassert, vassertSome, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RuleFlattener {
  def flattenAndCompileRules(
    rulesS: Vector[IRulexSR]):
  (Map[IRuneA, Int], Map[IRuneA, ITemplataType], RuneWorldSolverState[RangeS, ILiteralAR, ILookupAR]) = {
    val tentativeRuneToType = mutable.HashMap[TentativeRune, ITemplataType]()
    val runeAToTentativeRune = mutable.HashMap[IRuneA, TentativeRune]()
    val unsimplifiedWorld =
      RuneWorldBuilder[RangeS, ILiteralAR, ILookupAR](
        ArrayBuffer(), 0, mutable.HashMap())
    rulesS.foreach(translateRule(unsimplifiedWorld, runeAToTentativeRune, tentativeRuneToType, _))
    val (runeToCanonicalRune, solverState) = RuleOptimizer.compileRules(unsimplifiedWorld)

    val runeAToType =
      runeAToTentativeRune.map({ case (runeA, originalRune) =>
        tentativeRuneToType.get(originalRune) match {
          case None => List()
          case Some(tyype) => List(runeA -> tyype)
        }
      }).flatten.toMap

    val runeAToRune =
      runeAToTentativeRune.mapValues(runeToCanonicalRune).toMap

    (runeAToRune, runeAToType, solverState)
  }

  def addRune(
    output: RuneWorldBuilder[RangeS, ILiteralAR, ILookupAR],
    runeToIndex: mutable.HashMap[IRuneA, TentativeRune],
    runeA: IRuneA
  ): TentativeRune = {
    runeToIndex.get(runeA) match {
      case None => {
        val runeIndex = output.addRune()
        runeToIndex.put(runeA, runeIndex)
        runeIndex
      }
      case Some(runeIndex) => {
        runeIndex
      }
    }
  }
  def translateRule(
    output: RuneWorldBuilder[RangeS, ILiteralAR, ILookupAR],
    runeToIndex: mutable.HashMap[IRuneA, TentativeRune],
    runeToType: mutable.HashMap[TentativeRune, ITemplataType],
    rulexS: IRulexSR
  ): TentativeRune = {
    rulexS match {
      case TypedSR(range, runeS, tyype) => {
        val resultRune = addRune(output, runeToIndex, Astronomer.translateRune(runeS))
        runeToType.put(resultRune, Astronomer.translateRuneType(tyype))
        resultRune
      }
      case EqualsSR(range, left, right) => {
        val leftRuneAR = translateRule(output, runeToIndex, runeToType, left)
        val rightRuneAR = translateRule(output, runeToIndex, runeToType, right)
        output.noteRunesEqual(leftRuneAR, rightRuneAR)
      }
      case RuneSR(range, runeS) => {
        val runeA = Astronomer.translateRune(runeS)
        runeToIndex.get(runeA) match {
          case None => {
            val runeIndex = output.addRune()
            runeToIndex.put(runeA, runeIndex)
            runeIndex
          }
          case Some(runeIndex) => {
            runeIndex
          }
        }
      }
      case NameSR(range, nameS) => {
        val nameA = Astronomer.translateImpreciseName(nameS)
        val resultRune = output.addRune()
        output.addRule(LookupAR(range, resultRune, NameAR(nameA)))
        resultRune
      }
      case AbsoluteNameSR(range, nameS) => {
        val nameA = Astronomer.translateName(nameS)
        val resultRune = output.addRune()
        output.addRule(LookupAR(range, resultRune, AbsoluteNameAR(nameA)))
        resultRune
      }
      case OwnershipSR(range, ownership) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, OwnershipAR(ownership)))
        resultRune
      }
      case PermissionSR(range, permission) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, PermissionAR(permission)))
        resultRune
      }
      case MutabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, MutabilityAR(m)))
        resultRune
      }
      case IntSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, IntAR(value)))
        resultRune
      }
      case StringSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, StringAR(value)))
        resultRune
      }
      case VariabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, VariabilityAR(m)))
        resultRune
      }
      case ManualSequenceSR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(ManualSequenceAR(range, resultRune, m.map(translateRule(output, runeToIndex, runeToType, _)).toArray))
        resultRune
      }
      case RepeaterSequenceSR(range, mutability, variability, size, element) => {
        val resultRune = output.addRune()
        output.addRule(RepeaterSequenceAR(range, resultRune, translateRule(output, runeToIndex, runeToType, mutability), translateRule(output, runeToIndex, runeToType, variability), translateRule(output, runeToIndex, runeToType, size), translateRule(output, runeToIndex, runeToType, element)))
        resultRune
      }
      case InterpretedSR(range, ownership, permission, inner) => {
        val resultRune = output.addRune()
        output.addRule(
          AugmentAR(
            range,
            resultRune,
            Vector(OwnershipAR(ownership), PermissionAR(permission)),
            translateRule(output, runeToIndex, runeToType, inner)))
        resultRune
      }
      case CallSR(range, template, args) => {
        val resultRune = output.addRune()
        output.addRule(CallAR(range, resultRune, translateRule(output, runeToIndex, runeToType, template), args.map(translateRule(output, runeToIndex, runeToType, _)).toArray))
        resultRune
      }
      case ComponentsSR(range, typedSR, componentsAR) => {
        val runeIndex = translateRule(output, runeToIndex, runeToType, typedSR)
        typedSR.tyype match {
          case CoordTypeSR => {
            val Vector(ownershipAR, /*locationRune, regionRune,*/ permissionAR, kindAR) = componentsAR
            val ownershipRune = translateRule(output, runeToIndex, runeToType, ownershipAR)
            val permissionRune = translateRule(output, runeToIndex, runeToType, permissionAR)
            val kindRune = translateRule(output, runeToIndex, runeToType, kindAR)
            output.addRule(CoordComponentsAR(range, runeIndex, ownershipRune, permissionRune, kindRune))
          }
          case KindTypeSR => {
            val Vector(mutabilityAR) = componentsAR
            val kindRune = output.addRune()
            val mutabilityRune = translateRule(output, runeToIndex, runeToType, mutabilityAR)
            output.addRule(KindComponentsAR(range, kindRune, mutabilityRune))
          }
        }
        runeIndex
      }
      case OrSR(range, possibilities) => {
        vimpl()
        //        OrAR(range, possibilities.map(translateRule(output, runeToIndex, runeToType, _)))
      }
      case BuiltinCallSR(range, name, args) => {
        vimpl() // depends on name
        //        val resultRune = output.addRune()
        //        BuiltinCallAR(range, resultRune, translateRule(output, runeToIndex, runeToType, name), args.map(translateRule(output, runeToIndex, runeToType, _)))
        //        vimpl() // we should split apart the various builtins, so we can know the correct puzzles
        //        resultRune
      }
      case other => vimpl(other.toString)
    }
  }
}
