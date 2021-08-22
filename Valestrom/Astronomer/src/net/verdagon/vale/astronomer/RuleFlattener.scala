package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.{AbsoluteNameSR, CallSR, IntSR, InterpretedSR, ManualSequenceSR, MutabilitySR, NameSR, OwnershipSR, PermissionSR, RepeaterSequenceSR, RuneSR, StringSR, VariabilitySR}
import net.verdagon.vale.scout.rules.{BoolTypeSR, BuiltinCallSR, ComponentsSR, CoordTypeSR, EqualsSR, FunctionTypeSR, IRulexSR, ITypeSR, IntTypeSR, KindTypeSR, LocationTypeSR, MutabilityTypeSR, OrSR, OwnershipTypeSR, PermissionTypeSR, PrototypeTypeSR, StringTypeSR, TypedSR, VariabilityTypeSR}
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class RuneWorldBuilder(
  rules: mutable.ArrayBuffer[IRulexAR],
  var nextRuneIndex: Int,
  runeToIndex: mutable.HashMap[IRuneA, Int],
  redundantRuneToOriginalRune: mutable.HashMap[Int, Int],
  runeToType: mutable.HashMap[Int, ITemplataType],
) {
  def addRule(rule: IRulexAR): Int = {
    val ruleIndex = rules.size
    rules += rule
    ruleIndex
  }
  def addRune(): Int = {
    val ruleIndex = nextRuneIndex
    nextRuneIndex = nextRuneIndex + 1
    ruleIndex
  }
  def addRune(runeA: IRuneA): Int = {
    runeToIndex.get(runeA) match {
      case None => {
        val runeIndex = addRune()
        runeToIndex.put(runeA, runeIndex)
        runeIndex
      }
      case Some(runeIndex) => {
        runeIndex
      }
    }
  }
  def noteRunesEqual(left: Int, right: Int): Unit = {
    val earlierRune = Math.min(left, right)
    val laterRune = Math.max(left, right)
    redundantRuneToOriginalRune.put(laterRune, earlierRune)
  }
}

case class RuneWorld(
  rules: Array[IRulexAR],

  // For example, if rule 7 says:
  //   1 = Ref(2, 3, 4, 5)
  // then 2, 3, 4, 5 together could solve the rule, or 1 could solve the rule.
  // In other words, the two sets of runes that could solve the rule are:
  // - [1]
  // - [2, 3, 4, 5]
  // Here we have two "puzzles". The runes in a puzzle are called "pieces".
  // Puzzles are identified up-front by Astronomer.

  // This tracks, for each puzzle, what rule does it refer to
  puzzleToRule: Array[Int],
  // This tracks, for each puzzle, what rules does it have
  puzzleToRunes: Array[Array[Int]],

  // For every rule, this is which puzzles can solve it.
  ruleToPuzzles: Array[Array[Int]],

  // For every rune, this is which puzzles it participates in.
  runeToPuzzles: Array[Array[Int]],

  // For every rune, which other rune might describe the interface that it must
  // inherit from.
  kindRuneToBoundingInterfaceRune: Array[Int])

case class RuneWorldSolverState(
  runeWorld: RuneWorld, // immutable

  // Together, these basically form a Array[Vector[Int]]
  puzzleToNumUnknownRunes: Array[Int],
  puzzleToUnknownRunes: Array[Array[Int]],
  // This is the puzzle's index in the below numUnknownsToPuzzle map.
  puzzleToIndexInNumUnknowns: Array[Int],

  // Together, these basically form a Array[Vector[Int]]
  // which will have five elements: 0, 1, 2, 3, 4
  // At slot 4 is all the puzzles that have 4 unknowns left
  // At slot 3 is all the puzzles that have 3 unknowns left
  // At slot 2 is all the puzzles that have 2 unknowns left
  // At slot 1 is all the puzzles that have 1 unknowns left
  // At slot 0 is all the puzzles that have 0 unknowns left
  // We will:
  // - Move a puzzle from one set to the next set if we solve one of its runes
  // - Solve any puzzle that has 0 unknowns left
  numUnknownsToNumPuzzles: Array[Int],
  numUnknownsToPuzzle: Array[Array[Int]]
) {
  override def hashCode(): Int = vfail() // is mutable, should never be hashed

  def deepClone(): RuneWorldSolverState = {
    RuneWorldSolverState(
      runeWorld,
      puzzleToNumUnknownRunes.clone(),
      puzzleToUnknownRunes.map(_.clone()).clone(),
      puzzleToIndexInNumUnknowns.clone(),
      numUnknownsToNumPuzzles.clone(),
      numUnknownsToPuzzle.map(_.clone()).clone())
  }
}

object RuleFlattener {
  def flattenAndCompileRules(
    rulesS: Vector[IRulexSR]):
  (Map[IRuneA, Int], Map[IRuneA, ITemplataType], RuneWorldSolverState) = {
    val unsimplifiedWorld = RuneWorldBuilder(ArrayBuffer(), 0, mutable.HashMap(), mutable.HashMap(), mutable.HashMap())
    rulesS.foreach(translateRule(unsimplifiedWorld, _))
//    (0 until unsimplifiedWorld.nextRuneIndex).foreach(i => {
//      if (!unsimplifiedWorld.redundantRuneToOriginalRune.contains(i)) {
//        unsimplifiedWorld.redundantRuneToOriginalRune.put(i, i)
//      }
//    })
    // Right now, the original runes are spaced out. Even if we have 8 original runes, their numbers might be
    // 3, 6, 14, 16, 19, 24, 27, 30.
    // Let's re-number them so they're all dense. These will be the "canonical" runes.
    var nextCanonicalRune = 0
    val runeToCanonicalRune = mutable.HashMap[Int, Int]()
    (0 until unsimplifiedWorld.nextRuneIndex).foreach(rune => {
      unsimplifiedWorld.redundantRuneToOriginalRune.get(rune) match {
        case None => {
          // This rune isnt redundant with anything, give it its own canonical rune.
          val canonicalRune = nextCanonicalRune
          nextCanonicalRune = nextCanonicalRune + 1
          runeToCanonicalRune.put(rune, canonicalRune)
        }
        case Some(originalRune) => {
          // The originalRune should be less than this rune, so it should already have a canonical one assigned.
          val canonicalRune = vassertSome(runeToCanonicalRune.get(originalRune))
          runeToCanonicalRune.put(rune, canonicalRune)
        }
      }
    })

    val originalRuneToCanonicalRune =
      unsimplifiedWorld.runeToIndex.mapValues(runeToCanonicalRune).toMap
    val originalRuneToType =
      unsimplifiedWorld.runeToIndex.map({ case (runeA, originalRune) => {
        unsimplifiedWorld.runeToType.get(originalRune) match {
          case None => List()
          case Some(tyype) => List(runeA -> tyype)
        }
      }
      }).flatten.toMap

    val solverState = compile(unsimplifiedWorld.rules, runeToCanonicalRune, nextCanonicalRune)
    (originalRuneToCanonicalRune, originalRuneToType, solverState)
  }

  def translateRule(
    output: RuneWorldBuilder,
    rulexS: IRulexSR
  ): Int = {
    rulexS match {
      case EqualsSR(range, left, right) => {
        val leftRuneAR = translateRule(output, left)
        val rightRuneAR = translateRule(output, right)
        if (leftRuneAR == rightRuneAR) {
          leftRuneAR
        } else {
          output.noteRunesEqual(leftRuneAR, rightRuneAR)
          leftRuneAR
        }
      }
      case ComponentsSR(range, typedSR, componentsAR) => {
        val runeIndex = translateRule(output, typedSR)
        typedSR.tyype match {
          case CoordTypeSR => {
            val Vector(ownershipAR, /*locationRune, regionRune,*/ permissionAR, kindAR) = componentsAR
            val ownershipRune = translateRule(output, ownershipAR)
            val permissionRune = translateRule(output, permissionAR)
            val kindRune = translateRule(output, kindAR)
            output.addRule(CoordComponentsAR(range, runeIndex, ownershipRune, permissionRune, kindRune))
          }
          case KindTypeSR => {
            val Vector(mutabilityAR) = componentsAR
            val kindRune = output.addRune()
            val mutabilityRune = translateRule(output, mutabilityAR)
            output.addRule(KindComponentsAR(range, kindRune, mutabilityRune))
          }
        }
        runeIndex
      }
      case OrSR(range, possibilities) => {
        vimpl()
        //        OrAR(range, possibilities.map(translateRule(output, _)))
      }
      case BuiltinCallSR(range, name, args) => {
        vimpl() // depends on name
        //        val resultRune = output.addRune()
        //        BuiltinCallAR(range, resultRune, translateRule(output, name), args.map(translateRule(output, _)))
        //        vimpl() // we should split apart the various builtins, so we can know the correct puzzles
        //        resultRune
      }
      case RuneSR(range, runeS) => {
        val runeA = Astronomer.translateRune(runeS)
        output.runeToIndex.get(runeA) match {
          case None => {
            val runeIndex = output.addRune()
            output.runeToIndex.put(runeA, runeIndex)
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
        output.addRule(NameAR(range, resultRune, nameA))
        resultRune
      }
      case OwnershipSR(range, ownership) => {
        val resultRune = output.addRune()
        output.addRule(OwnershipAR(range, resultRune, ownership))
        resultRune
      }
      case PermissionSR(range, permission) => {
        val resultRune = output.addRune()
        output.addRule(PermissionAR(range, resultRune, permission))
        resultRune
      }
      case InterpretedSR(range, ownership, permission, inner) => {
        val resultRune = output.addRune()
        output.addRule(InterpretedAR(range, resultRune, ownership, permission, translateRule(output, inner)))
        resultRune
      }
      case AbsoluteNameSR(range, nameS) => {
        val nameA = Astronomer.translateName(nameS)
        val resultRune = output.addRune()
        output.addRule(AbsoluteNameAR(range, resultRune, nameA))
        resultRune
      }
      case CallSR(range, template, args) => {
        val resultRune = output.addRune()
        output.addRule(CallAR(range, resultRune, translateRule(output, template), args.map(translateRule(output, _)).toArray))
        resultRune
      }
      case MutabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(MutabilityAR(range, resultRune, m))
        resultRune
      }
      case VariabilitySR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(VariabilityAR(range, resultRune, m))
        resultRune
      }
      case ManualSequenceSR(range, m) => {
        val resultRune = output.addRune()
        output.addRule(ManualSequenceAR(range, resultRune, m.map(translateRule(output, _)).toArray))
        resultRune
      }
      case RepeaterSequenceSR(range, mutability, variability, size, element) => {
        val resultRune = output.addRune()
        output.addRule(RepeaterSequenceAR(range, resultRune, translateRule(output, mutability), translateRule(output, variability), translateRule(output, size), translateRule(output, element)))
        resultRune
      }
      case IntSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(IntAR(range, resultRune, value))
        resultRune
      }
      case StringSR(range, value) => {
        val resultRune = output.addRune()
        output.addRule(StringAR(range, resultRune, value))
        resultRune
      }
      case TypedSR(range, runeS, tyype) => {
        val resultRune = output.addRune(Astronomer.translateRune(runeS))
        output.runeToType.put(resultRune, Astronomer.translateRuneType(tyype))
        resultRune
      }
      case other => vimpl(other.toString)
    }
  }

  def compileRule(
    inputRule: IRulexAR,
    runeToCanonicalRune: mutable.HashMap[Int, Int]
  ): (IRulexAR, Array[Array[Int]]) = {
    inputRule match {
      case IntAR(range, uncanonicalResultRune, value) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (IntAR(range, canonicalResultRune, value), Array(Array()))
      }
      case LocationAR(range, uncanonicalResultRune, location) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (LocationAR(range, canonicalResultRune, location), Array(Array()))
      }
      case MutabilityAR(range, uncanonicalResultRune, mutability) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (MutabilityAR(range, canonicalResultRune, mutability), Array(Array()))
      }
      case NameAR(range, uncanonicalResultRune, name) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (NameAR(range, canonicalResultRune, name), Array(Array()))
      }
      case OwnershipAR(range, uncanonicalResultRune, ownership) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (OwnershipAR(range, canonicalResultRune, ownership), Array(Array()))
      }
      case PermissionAR(range, uncanonicalResultRune, permission) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (PermissionAR(range, canonicalResultRune, permission), Array(Array()))
      }
      case StringAR(range, uncanonicalResultRune, value) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (StringAR(range, canonicalResultRune, value), Array(Array()))
      }
      case VariabilityAR(range, uncanonicalResultRune, variability) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (VariabilityAR(range, canonicalResultRune, variability), Array(Array()))
      }
      case AbsoluteNameAR(range, uncanonicalResultRune, name) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (AbsoluteNameAR(range, canonicalResultRune, name), Array(Array()))
      }
      case BoolAR(range, uncanonicalResultRune, value) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        (BoolAR(range, canonicalResultRune, value), Array(Array()))
      }
      case BuiltinCallAR(range, uncanonicalResultRune, name, args) => {
        vimpl() // split into actual things
        //            val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        //            (BoolAR(range, canonicalResultRune, value), Array(Array()))
      }
      case CallAR(range, uncanonicalResultRune, uncanonicalTemplateRune, uncanonicalArgRunes) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalTemplateRune = runeToCanonicalRune(uncanonicalTemplateRune)
        val canonicalArgRunes = uncanonicalArgRunes.map(runeToCanonicalRune).toArray
        (CallAR(range, canonicalResultRune, canonicalTemplateRune, canonicalArgRunes), Array(Array(canonicalResultRune), canonicalArgRunes))
      }
      case CoordComponentsAR(range, uncanonicalCoordRune, uncanonicalOwnershipRune, uncanonicalPermissionRune, uncanonicalKindRune) => {
        val canonicalCoordRune = runeToCanonicalRune(uncanonicalCoordRune)
        val canonicalOwnershipRune = runeToCanonicalRune(uncanonicalOwnershipRune)
        val canonicalPermissionRune = runeToCanonicalRune(uncanonicalPermissionRune)
        val canonicalKindRune = runeToCanonicalRune(uncanonicalKindRune)
        (
          CoordComponentsAR(
            range, canonicalCoordRune, canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune),
          Array(
            Array(canonicalOwnershipRune, canonicalPermissionRune, canonicalKindRune),
            Array(canonicalCoordRune)))
      }
      case CoordListAR(range, uncanonicalResultRune, uncanonicalElementRunes) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalElementRunes = uncanonicalElementRunes.map(runeToCanonicalRune).toArray
        (
          CoordListAR(range, canonicalResultRune, canonicalElementRunes),
          Array(Array(canonicalResultRune), canonicalElementRunes))
      }
      case InterpretedAR(range, uncanonicalResultRune, ownership, permission, uncanonicalInnerRune) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalInnerRune = runeToCanonicalRune(uncanonicalInnerRune)
        (InterpretedAR(range, canonicalResultRune, ownership, permission, canonicalInnerRune), Array(Array(canonicalResultRune), Array(canonicalInnerRune)))
      }
      case IsaAR(range, uncanonicalSubRune, uncanonicalInterfaceRune) => {
        val canonicalSubRune = runeToCanonicalRune(uncanonicalSubRune)
        val canonicalInterfaceRune = runeToCanonicalRune(uncanonicalInterfaceRune)
        (IsaAR(range, canonicalSubRune, canonicalInterfaceRune), Array(Array(vimpl())))
      }
      case KindComponentsAR(range, uncanonicalKindRune, uncanonicalMutabilityRune) => {
        val canonicalKindRune = runeToCanonicalRune(uncanonicalKindRune)
        val canonicalMutabilityRune = runeToCanonicalRune(uncanonicalMutabilityRune)
        (
          KindComponentsAR(range, canonicalKindRune, canonicalMutabilityRune),
          Array(Array(canonicalKindRune), Array(canonicalMutabilityRune)))
      }
      case ManualSequenceAR(range, uncanonicalResultRune, uncanonicalElementRunes) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalElementRunes = uncanonicalElementRunes.map(runeToCanonicalRune).toArray
        (
          ManualSequenceAR(range, canonicalResultRune, canonicalElementRunes),
          Array(Array(canonicalResultRune), canonicalElementRunes))
      }
      case OrAR(range, possibilities) => {
        vimpl()
        //            val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)

      }
      case PrototypeAR(range, uncanonicalResultRune, name, uncanonicalParameterRunes, uncanonicalReturnRune) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalParameterRunes = uncanonicalParameterRunes.map(runeToCanonicalRune).toArray
        val canonicalReturnRune = runeToCanonicalRune(uncanonicalReturnRune)
        (
          PrototypeAR(range, canonicalResultRune, name, canonicalParameterRunes, canonicalReturnRune),
          Array(Array(canonicalResultRune), canonicalParameterRunes :+ canonicalReturnRune))
      }
      case RepeaterSequenceAR(range, uncanonicalResultRune, uncanonicalMutabilityRune, uncanonicalVariabilityRune, uncanonicalSizeRune, uncanonicalElementRune) => {
        val canonicalResultRune = runeToCanonicalRune(uncanonicalResultRune)
        val canonicalMutabilityRune = runeToCanonicalRune(uncanonicalMutabilityRune)
        val canonicalVariabilityRune = runeToCanonicalRune(uncanonicalVariabilityRune)
        val canonicalSizeRune = runeToCanonicalRune(uncanonicalSizeRune)
        val canonicalElementRune = runeToCanonicalRune(uncanonicalElementRune)
        (
          RepeaterSequenceAR(range, canonicalResultRune, canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune),
          Array(Array(canonicalResultRune), Array(canonicalMutabilityRune, canonicalVariabilityRune, canonicalSizeRune, canonicalElementRune)))
      }
      case _ => vfail()
    }
  }

  def compile(
    inputRules: mutable.ArrayBuffer[IRulexAR],
    runeToCanonicalRune: mutable.HashMap[Int, Int],
    numCanonicalRunes: Int,
  ): RuneWorldSolverState = {
    val newRulesAndPuzzles = inputRules.map(compileRule(_, runeToCanonicalRune)).toArray
    val (newRules, ruleIndexToPuzzles) = newRulesAndPuzzles.unzip
    val kindRuneToBoundingInterfaceRuneAsMap =
      newRules.collect({ case IsaAR(_, sub, interface) => (sub, interface) }).toMap
    val kindRuneToBoundingInterfaceRune =
      (0 until numCanonicalRunes).map(rune => kindRuneToBoundingInterfaceRuneAsMap.getOrElse(rune, -1)).toArray

    val puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns = ArrayBuffer[(Int, Array[Int], Int)]()
    val numUnknownsToPuzzles =
      Array(
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int](),
        ArrayBuffer[Int]())
    val ruleToPuzzles = newRules.map(_ => ArrayBuffer[Int]())
    val runeToPuzzles = (0 until numCanonicalRunes).map(_ => ArrayBuffer[Int]()).toArray
    ruleIndexToPuzzles.zipWithIndex.foreach({ case (puzzlesForRule, ruleIndex) =>
      puzzlesForRule.foreach(puzzleUnknownRunes => {
        val puzzle = puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns.size

        val indexInNumUnknowns = numUnknownsToPuzzles(puzzleUnknownRunes.length).length
        numUnknownsToPuzzles(puzzleUnknownRunes.length) += puzzle

        val thing = (ruleIndex, puzzleUnknownRunes, indexInNumUnknowns)
        puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns += thing
        ruleToPuzzles(ruleIndex) += puzzle
        puzzleUnknownRunes.foreach(unknownRune => {
          runeToPuzzles(unknownRune) += puzzle
        })
      })
    })

    val numPuzzles = puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns.size
    val puzzleToRule = puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns.map(_._1)
    val puzzleToUnknownRunes = puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns.map(_._2)
    val puzzleToIndexInNumUnknowns = puzzlesToRuleAndUnknownRunesAndIndexInNumUnknowns.map(_._3)

    val runeWorld =
      RuneWorld(
        newRules,
        puzzleToRule.toArray,
        puzzleToUnknownRunes.map(_.clone()).toArray,
        ruleToPuzzles.map(_.toArray),
        runeToPuzzles.map(_.toArray),
        kindRuneToBoundingInterfaceRune)
    RuneWorldSolverState(
      runeWorld,
      puzzleToUnknownRunes.map(_.length).toArray,
      puzzleToUnknownRunes.map(_.clone()).toArray,
      puzzleToIndexInNumUnknowns.toArray.clone(),
      numUnknownsToPuzzles.map(_.length).toArray,
      numUnknownsToPuzzles.map(puzzles => {
        // Fill in the rest with -1s
        puzzles ++= (0 until (numPuzzles - puzzles.length)).map(_ => -1)
        vassert(puzzles.length == numPuzzles)
        puzzles.toArray.clone()
      }).toArray)
  }
}
