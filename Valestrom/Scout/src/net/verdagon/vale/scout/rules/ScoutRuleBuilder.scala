package net.verdagon.vale.scout.rules

import net.verdagon.vale.scout.{AbsoluteNameSR, CallSR, IRuneS, IntSR, InterpretedSR, ManualSequenceSR, MutabilitySR, NameSR, OwnershipSR, PermissionSR, RangeS, RepeaterSequenceSR, RuneSR, StringSR, VariabilitySR}
import net.verdagon.vale.solver.{AugmentAR, Builder, CallAR, CoerceToCoord, CoordComponentsAR, IRulexAR, IsConcreteAR, IsInterfaceAR, IsStructAR, KindComponentsAR, LiteralAR, LookupAR, ManualSequenceAR, OneOfAR, RepeaterSequenceAR, TentativeRune, World}
import net.verdagon.vale.{vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class ScoutWorld(
  runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
  tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
  tentativeRuneToCanonicalRune: mutable.HashMap[TentativeRune, Int],
  world: World[IRuneS, RangeS, ILiteralSR, ILookupSR]
)

object ScoutRuleBuilder {
  def apply(): ScoutRuleBuilder = {
    ScoutRuleBuilder(
      mutable.HashMap[IRuneS, TentativeRune](),
      mutable.HashMap[TentativeRune, ITypeSR](),
      Builder[RangeS, ILiteralSR, ILookupSR](
        ArrayBuffer[IRulexAR[TentativeRune, RangeS, ILiteralSR, ILookupSR]](),
        0,
        mutable.HashMap[TentativeRune, TentativeRune]()))
  }
}

case class ScoutRuleBuilder(
    runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
    tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
    builder: Builder[RangeS, ILiteralSR, ILookupSR]) {

  // This will map an IRuneS to a tentative rune.
  // This is useful because a lot of the scout's AST thinks in terms of IRuneS.
  // They can't think in terms of canonical runes (ints) yet because those are
  // only known after we assemble all the AST.
  def nameTentativeRune(rangeS: RangeS, knowableRunesFromAbove: Set[IRuneS], runeS: IRuneS, tentativeRune: TentativeRune) = {
    builder.noteRunesEqual(tentativeRune, addRune(rangeS, knowableRunesFromAbove, runeS))
    runeS
  }

  def translateRule(
    knowableRunesFromAbove: Set[IRuneS],
    rulexS: IRulexSR
  ): TentativeRune = {
    rulexS match {
      case TypedSR(range, runeS, tyype) => {
        val resultRune = addRune(range, knowableRunesFromAbove, runeS)
        tentativeRuneToType.get(resultRune) match {
          case None =>
          case Some(existingType) => {
            vfail("Conflict on rune: " + runeS + " inferred types " + existingType + " then " + tyype)
          }
        }
        tentativeRuneToType.put(resultRune, tyype)
        resultRune
      }
      case EqualsSR(range, left, right) => {
        val leftRuneAR = translateRule(knowableRunesFromAbove, left)
        val rightRuneAR = translateRule(knowableRunesFromAbove, right)
        builder.noteRunesEqual(leftRuneAR, rightRuneAR)
      }
      case RuneSR(range, runeS) => {
        addRune(range, knowableRunesFromAbove, runeS)
      }
      case NameSR(range, nameS) => {
        val resultRune = builder.addRune()
        builder.addRule(LookupAR(range, resultRune, NameLookupSR(nameS)))
        resultRune
      }
      case AbsoluteNameSR(range, nameS) => {
        val resultRune = builder.addRune()
        builder.addRule(LookupAR(range, resultRune, AbsoluteNameLookupSR(nameS)))
        resultRune
      }
      case OwnershipSR(range, ownership) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, OwnershipLiteralSR(ownership)))
        resultRune
      }
      case PermissionSR(range, permission) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, PermissionLiteralSR(permission)))
        resultRune
      }
      case MutabilitySR(range, m) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, MutabilityLiteralSR(m)))
        resultRune
      }
      case IntSR(range, value) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, IntLiteralSR(value)))
        resultRune
      }
      case StringSR(range, value) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, StringLiteralSR(value)))
        resultRune
      }
      case VariabilitySR(range, m) => {
        val resultRune = builder.addRune()
        builder.addRule(LiteralAR(range, resultRune, VariabilityLiteralSR(m)))
        resultRune
      }
      case ManualSequenceSR(range, m) => {
        val resultRune = builder.addRune()
        builder.addRule(ManualSequenceAR(range, resultRune, m.map(translateRule(knowableRunesFromAbove, _)).toArray))
        resultRune
      }
      case RepeaterSequenceSR(range, mutability, variability, size, element) => {
        val resultRune = builder.addRune()
        builder.addRule(RepeaterSequenceAR(range, resultRune, translateRule(knowableRunesFromAbove, mutability), translateRule(knowableRunesFromAbove, variability), translateRule(knowableRunesFromAbove, size), translateRule(knowableRunesFromAbove, element)))
        resultRune
      }
      case InterpretedSR(range, ownership, permission, inner) => {
        val resultRune = builder.addRune()
        builder.addRule(
          AugmentAR(
            range,
            resultRune,
            Vector(OwnershipLiteralSR(ownership), PermissionLiteralSR(permission)),
            translateRule(knowableRunesFromAbove, inner)))
        resultRune
      }
      case CallSR(range, template, args) => {
        val resultRune = builder.addRune()
        builder.addRule(CallAR(range, resultRune, translateRule(knowableRunesFromAbove, template), args.map(translateRule(knowableRunesFromAbove, _)).toArray))
        resultRune
      }
      case ComponentsSR(range, typedSR, componentsAR) => {
        val runeIndex = translateRule(knowableRunesFromAbove, typedSR)
        typedSR.tyype match {
          case CoordTypeSR => {
            val Vector(ownershipAR, /*locationRune, regionRune,*/ permissionAR, kindAR) = componentsAR
            val ownershipRune = translateRule(knowableRunesFromAbove, ownershipAR)
            val permissionRune = translateRule(knowableRunesFromAbove, permissionAR)
            val kindRune = translateRule(knowableRunesFromAbove, kindAR)
            builder.addRule(CoordComponentsAR(range, runeIndex, ownershipRune, permissionRune, kindRune))
          }
          case KindTypeSR => {
            val Vector(mutabilityAR) = componentsAR
            val kindRune = builder.addRune()
            val mutabilityRune = translateRule(knowableRunesFromAbove, mutabilityAR)
            builder.addRule(KindComponentsAR(range, kindRune, mutabilityRune))
          }
        }
        runeIndex
      }
      case OrSR(range, possibilities) => {
        val resultRune = builder.addRune()
        val literals =
          if (possibilities.forall({ case OwnershipSR(_, _) => true case _ => false})) {
            possibilities.map({ case OwnershipSR(_, ownership) => OwnershipLiteralSR(ownership) })
          } else {
            vimpl()
          }
        builder.addRule(OneOfAR(range, resultRune, literals.toArray))
        resultRune
      }
      case BuiltinCallSR(range, name, argsAR) => {
        name match {
          case "passThroughIfConcrete" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, argAR)
            builder.addRule(IsConcreteAR(range, argRune))
            argRune
          }
          case "passThroughIfInterface" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, argAR)
            builder.addRule(IsInterfaceAR(range, argRune))
            argRune
          }
          case "passThroughIfStruct" => {
            val Vector(argAR) = argsAR
            val argRune = translateRule(knowableRunesFromAbove, argAR)
            builder.addRule(IsStructAR(range, argRune))
            argRune
          }
          case "toRef" => {
            val Vector(argAR) = argsAR
            val coordRune = builder.addRune()
            val kindRune = translateRule(knowableRunesFromAbove, argAR)
            builder.addRule(CoerceToCoord(range, coordRune, kindRune))
            coordRune
          }
        }
      }
      case other => vimpl(other.toString)
    }
  }

  def addRune(
    range: RangeS,
    knowableRunesFromAbove: Set[IRuneS],
    runeS: IRuneS
  ): TentativeRune = {
    runeSToTentativeRune.get(runeS) match {
      case None => {
        val resultRune = builder.addRune()
        val runeIndex =
          if (knowableRunesFromAbove.contains(runeS)) {
            builder.addRule(LookupAR(range, resultRune, ParentRuneLookupSR(runeS)))
            resultRune
          } else {
            builder.addRune()
          }
        runeSToTentativeRune.put(runeS, runeIndex)
        runeIndex
      }
      case Some(runeIndex) => {
        runeIndex
      }
    }
  }
}
