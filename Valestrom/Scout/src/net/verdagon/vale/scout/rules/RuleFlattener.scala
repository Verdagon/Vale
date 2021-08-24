package net.verdagon.vale.scout.rules

import net.verdagon.vale.scout._
import net.verdagon.vale.solver._
import net.verdagon.vale.vimpl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RuleFlattener {
  // Returns:
  // - Map of scout rune to rune index
  // - The initially known types of some runes
  // - The solver state, which can be used to solve
  def flattenAndCompileRules(
    knowableRunesFromAbove: Set[IRuneS],
    rulesS: Vector[IRulexSR]):
  (Map[IRuneS, Int], Map[IRuneS, ITypeSR], RuneWorldSolverState[RangeS, ILiteralSR, ILookupSR]) = {
    val tentativeRuneToType = mutable.HashMap[TentativeRune, ITypeSR]()
    val runeSToTentativeRune = mutable.HashMap[IRuneS, TentativeRune]()
    val unsimplifiedWorld =
      RuneWorldBuilder[RangeS, ILiteralSR, ILookupSR](
        ArrayBuffer[IRulexAR[TentativeRune, RangeS, ILiteralSR, ILookupSR]](),
        0,
        mutable.HashMap[TentativeRune, TentativeRune]())
    rulesS.foreach(translateRule(knowableRunesFromAbove, unsimplifiedWorld, runeSToTentativeRune, tentativeRuneToType, _))
    val (runeToCanonicalRune, solverState) =
      RuneWorldOptimizer.optimize(
        unsimplifiedWorld,
        (rulexAR: IRulexAR[Int, RangeS, ILiteralSR, ILookupSR]) => {
          TemplarPuzzler.apply(rulexAR)
        })

    val runeSToType =
      runeSToTentativeRune.map({ case (runeS, originalRune) =>
        tentativeRuneToType.get(originalRune) match {
          case None => List()
          case Some(tyype) => List(runeS -> tyype)
        }
      }).flatten.toMap

    val runeSToRune =
      runeSToTentativeRune.mapValues(runeToCanonicalRune).toMap

    (runeSToRune, runeSToType, solverState)
  }

  def addRune(
    output: RuneWorldBuilder[RangeS, ILiteralSR, ILookupSR],
    runeToIndex: mutable.HashMap[IRuneS, TentativeRune],
    runeS: IRuneS
  ): TentativeRune = {
    runeToIndex.get(runeS) match {
      case None => {
        val runeIndex = output.addRune()
        runeToIndex.put(runeS, runeIndex)
        runeIndex
      }
      case Some(runeIndex) => {
        runeIndex
      }
    }
  }

  def translateRule(
    knowableRunesFromAbove: Set[IRuneS],
    output: RuneWorldBuilder[RangeS, ILiteralSR, ILookupSR],
    runeToIndex: mutable.HashMap[IRuneS, TentativeRune],
    runeToType: mutable.HashMap[TentativeRune, ITypeSR],
    rulexS: IRulexSR
  ): TentativeRune = {
    rulexS match {
      case TypedSR(range, runeS, tyype) => {
        val resultRune = addRune(output, runeToIndex, runeS)
        runeToType.put(resultRune, tyype)
        resultRune
      }
      case EqualsSR(range, left, right) => {
        val leftRuneAR = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, left)
        val rightRuneAR = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, right)
        output.noteRunesEqual(leftRuneAR, rightRuneAR)
      }
      case RuneSR(range, runeS) => {
        if (knowableRunesFromAbove.contains(runeS)) {
          val resultRune = output.addRune()
          output.addRule(LookupAR(range, resultRune, ParentRuneLookupSR(runeS)))
          resultRune
        } else {
          runeToIndex.get(runeS) match {
            case None => {
              val runeIndex = output.addRune()
              runeToIndex.put(runeS, runeIndex)
              runeIndex
            }
            case Some(runeIndex) => {
              runeIndex
            }
          }
        }
      }
      case NameSR(range, nameS) => {
        val resultRune = output.addRune()
        output.addRule(LookupAR(range, resultRune, NameLookupSR(nameS)))
        resultRune
      }
      case AbsoluteNameSR(range, nameS) => {
        val resultRune = output.addRune()
        output.addRule(LookupAR(range, resultRune, AbsoluteNameLookupSR(nameS)))
        resultRune
      }
      case OwnershipSR(range, ownership) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, OwnershipLiteralSR(ownership)))
        resultRune
      }
      case PermissionSR(range, permission) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, PermissionLiteralSR(permission)))
        resultRune
      }
      case MutabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, MutabilityLiteralSR(m)))
        resultRune
      }
      case IntSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, IntLiteralSR(value)))
        resultRune
      }
      case StringSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, StringLiteralSR(value)))
        resultRune
      }
      case VariabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(LiteralAR(range, resultRune, VariabilityLiteralSR(m)))
        resultRune
      }
      case ManualSequenceSR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(ManualSequenceAR(range, resultRune, m.map(translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, _)).toArray))
        resultRune
      }
      case RepeaterSequenceSR(range, mutability, variability, size, element) => {
        val resultRune = output.addRune()
        output.addRule(RepeaterSequenceAR(range, resultRune, translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, mutability), translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, variability), translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, size), translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, element)))
        resultRune
      }
      case InterpretedSR(range, ownership, permission, inner) => {
        val resultRune = output.addRune()
        output.addRule(
          AugmentAR(
            range,
            resultRune,
            Vector(OwnershipLiteralSR(ownership), PermissionLiteralSR(permission)),
            translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, inner)))
        resultRune
      }
      case CallSR(range, template, args) => {
        val resultRune = output.addRune()
        output.addRule(CallAR(range, resultRune, translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, template), args.map(translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, _)).toArray))
        resultRune
      }
      case ComponentsSR(range, typedSR, componentsAR) => {
        val runeIndex = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, typedSR)
        typedSR.tyype match {
          case CoordTypeSR => {
            val Vector(ownershipAR, /*locationRune, regionRune,*/ permissionAR, kindAR) = componentsAR
            val ownershipRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, ownershipAR)
            val permissionRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, permissionAR)
            val kindRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, kindAR)
            output.addRule(CoordComponentsAR(range, runeIndex, ownershipRune, permissionRune, kindRune))
          }
          case KindTypeSR => {
            val Vector(mutabilityAR) = componentsAR
            val kindRune = output.addRune()
            val mutabilityRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, mutabilityAR)
            output.addRule(KindComponentsAR(range, kindRune, mutabilityRune))
          }
        }
        runeIndex
      }
      case OrSR(range, possibilities) => {
        val resultRune = output.addRune()
        val literals =
          if (possibilities.forall({ case OwnershipSR(_, _) => true case _ => false})) {
            possibilities.map({ case OwnershipSR(_, ownership) => OwnershipLiteralSR(ownership) })
          } else {
            vimpl()
          }
        output.addRule(OneOfAR(range, resultRune, literals.toArray))
        resultRune
      }
      case BuiltinCallSR(range, name, argsAR) => {
        name match {
          case "passThroughIfConcrete" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, argAR)
            output.addRule(IsConcreteAR(range, argRune))
            argRune
          }
          case "passThroughIfInterface" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, argAR)
            output.addRule(IsInterfaceAR(range, argRune))
            argRune
          }
          case "passThroughIfStruct" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, argAR)
            output.addRule(IsStructAR(range, argRune))
            argRune
          }
          case "toRef" => {
            val Vector(argAR) = argsAR
            val coordRune = output.addRune()
            val kindRune = translateRule(knowableRunesFromAbove, output, runeToIndex, runeToType, argAR)
            output.addRule(CoerceToCoord(range, coordRune, kindRune))
            coordRune
          }
        }
      }
      case other => vimpl(other.toString)
    }
  }
}
