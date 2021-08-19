package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{ITemplexS, RangeS}
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.vale.templar.citizen.{AncestorHelper, StructTemplar}
import net.verdagon.vale.templar.env.{IEnvironment, ILookupContext, TemplataLookupContext}
import net.verdagon.vale.templar.infer.{IInfererDelegate, _}
import net.verdagon.vale.templar.infer.infer.{IInferSolveResult, InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{IProfiler, vassert, vassertSome, vfail, vimpl}

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class ConstructingRuneWorldTR(
  rules: mutable.ArrayBuffer[IRulexTR],

  runeToIndex: mutable.HashMap[IRuneA, Int],

  // For example, if rule 7 says:
  //   1 = Ref(2, 3, 4, 5)
  // then 2, 3, 4, 5 together could solve the rule, or 1 could solve the rule.
  // In other words, the two sets of runes that could solve the rule are:
  // - [1]
  // - [2, 3, 4, 5]
  // Here we have two "puzzles". The runes in a puzzle are called "pieces".
  // Puzzles are identified up-front by Astronomer.

  puzzleToRunes: mutable.ArrayBuffer[Vector[Int]],
  puzzleToRule: mutable.ArrayBuffer[Int],

  runeToPuzzles: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]
) {
  def addRule(rule: IRulexTR): Int = {
    val ruleIndex = rules.size
    rules += rule
    ruleIndex
  }
  def addRune(rune: IRuneA): Int = {
    val ruleIndex = runeToIndex.size
    runeToIndex.put(rune, ruleIndex)
    runeToPuzzles += mutable.ArrayBuffer()
    ruleIndex
  }
  def addRune(): Int = {
    val ruleIndex = runeToIndex.size
    runeToPuzzles += mutable.ArrayBuffer()
    ruleIndex
  }
  def addPuzzle(ruleIndex: Int, runes: Vector[Int]): Int = {
    val puzzleIndex = puzzleToRunes.size
    vassert(puzzleToRunes.size == puzzleToRule.size)
    puzzleToRunes += runes
    puzzleToRule += ruleIndex
    runes.foreach(rune => runeToPuzzles(rune) += puzzleIndex)
    puzzleIndex
  }

  def build(): RuneWorldTR = {
    RuneWorldTR(
      rules.toArray,
      puzzleToRunes.map(_.toArray).toArray,
      puzzleToRule.toArray,
      runeToPuzzles.map(_.toArray).toArray)
  }
}

class InferTemplar(
    opts: TemplarOptions,
    profiler: IProfiler,
    delegate: IInfererDelegate[IEnvironment, Temputs]) {
  private def solve(
    env: IEnvironment,
    state: Temputs,
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneA, ITemplataType],
    localRunes: Set[IRuneA],
    invocationRange: RangeS,
    directInputs: Map[IRuneA, ITemplata],
    paramAtoms: Vector[AtomAP],
    maybeParamInputs: Option[Vector[ParamFilter]],
    checkAllRunesPresent: Boolean,
  ): (IInferSolveResult) = {
    profiler.newProfile("infer", "", () => {
      val output = ConstructingRuneWorldTR(ArrayBuffer(), mutable.HashMap(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer())
      rules.map(translateRule(output, _))
      val rulesTR = output.build()


      Inferer.solve[IEnvironment, Temputs](
        profiler,
        delegate,
        env,
        state,
        rulesTR,
        typeByRune.map({ case (key, value) => NameTranslator.translateRune(key) -> value }),
        localRunes.map(NameTranslator.translateRune),
        invocationRange,
        directInputs.map({ case (key, value) => NameTranslator.translateRune(key) -> value }),
        paramAtoms,
        maybeParamInputs,
        checkAllRunesPresent)
    })
  }

  // No incoming types needed (like manually specified template args, or argument coords from a call).
  // This is for when we want to figure out the types for an ordinary function like
  //   fn sum(a: Int, b: Int)Int { }
  // which, remember, actually *does* have rules:
  //   fn sum
  //   rules(#1 = Int, #2 = Int, #3 = Int)
  //   (a: #1, b: #2) #3 { ...}
  def inferOrdinaryRules(
    env0: IEnvironment,
    temputs: Temputs,
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneA, ITemplataType],
    localRunes: Set[IRuneA],
  ): (Map[IRuneT, ITemplata]) = {
    profiler.childFrame("inferOrdinaryRules", () => {
      solve(env0, temputs, rules, typeByRune, localRunes, RangeS.internal(-13337), Map(), Vector.empty, None, true) match {
        case (InferSolveSuccess(inferences)) => {
          (inferences.templatasByRune)
        }
        case (isf@InferSolveFailure(_, _, _, _, range, _, _)) => {
          throw CompileErrorExceptionT(RangedInternalErrorT(range, "Conflict in determining ordinary rules' runes: " + isf))
        }
      }
    })
  }

  def inferFromExplicitTemplateArgs(
    env0: IEnvironment,
    temputs: Temputs,
    identifyingRunes: Vector[IRuneA],
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneA, ITemplataType],
    localRunes: Set[IRuneA],
    patterns1: Vector[AtomAP],
    maybeRetRune: Option[IRuneA],
    invocationRange: RangeS,
    explicits: Vector[ITemplata],
  ): (IInferSolveResult) = {
    profiler.childFrame("inferFromExplicitTemplateArgs", () => {
      if (identifyingRunes.size != explicits.size) {
        throw CompileErrorExceptionT(RangedInternalErrorT(invocationRange, "Wrong number of template args!"))
      }

      solve(
        env0,
        temputs,
        rules,
        typeByRune,
        localRunes,
        invocationRange,
        identifyingRunes.zip(explicits).toMap,
        patterns1,
        None,
        true)
    })
  }

  def inferFromArgCoords(
    env0: IEnvironment,
    temputs: Temputs,
    identifyingRunes: Vector[IRuneA],
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneA, ITemplataType],
    localRunes: Set[IRuneA],
    patterns1: Vector[AtomAP],
    maybeRetRune: Option[IRuneA],
    invocationRange: RangeS,
    alreadySpecifiedTemplateArgs: Vector[ITemplata],
    patternInputCoords: Vector[ParamFilter]
  ): (IInferSolveResult) = {
    profiler.childFrame("inferFromArgCoords", () => {
      solve(
        env0,
        temputs,
        rules,
        typeByRune,
        localRunes,
        invocationRange,
        // Note: this two things we're zipping are of different length, that's fine.
        identifyingRunes.zip(alreadySpecifiedTemplateArgs).toMap,
        patterns1,
        Some(patternInputCoords),
        true)
    })
  }

  def translateRule(
    output: ConstructingRuneWorldTR,
    rulexA: IRulexAR
  ): Int = {
    rulexA match {
      case EqualsAR(range, left, right) => {
        val leftRuneTR = translateRule(output, left)
        val rightRuneTR = translateRule(output, right)
        val ruleIndex = output.addRule(EqualsTR(range, leftRuneTR, rightRuneTR))
        output.addPuzzle(ruleIndex, Vector(leftRuneTR))
        output.addPuzzle(ruleIndex, Vector(rightRuneTR))
      }
      case TemplexAR(templex) => translateTemplex(output, templex)
      case ComponentsAR(range, tyype, componentsAR) => {
        tyype match {
          case CoordTemplataType => {
            val Vector(ownershipAR, /*locationRune, regionRune,*/ permissionAR, kindAR) = componentsAR
            val coordRune = output.addRune()
            val ownershipRune = translateRule(output, ownershipAR)
            val permissionRune = translateRule(output, permissionAR)
            val kindRune = translateRule(output, kindAR)
            val ruleIndex = output.addRule(CoordComponentsTR(range, coordRune, ownershipRune, permissionRune, kindRune))
            output.addPuzzle(ruleIndex, Vector(coordRune))
            output.addPuzzle(ruleIndex, Vector(ownershipRune, permissionRune, kindRune))
            coordRune
          }
          case KindTemplataType => {
            val Vector(mutabilityAR) = componentsAR
            val kindRune = output.addRune()
            val mutabilityRune = translateRule(output, mutabilityAR)
            val ruleIndex = output.addRule(KindComponentsTR(range, kindRune, mutabilityRune))
            output.addPuzzle(ruleIndex, Vector(kindRune))
            kindRune
          }
        }
      }
      case OrAR(range, possibilities) => {
        vimpl()
        //        OrTR(range, possibilities.map(translateRule(output, _)))
      }
      case CallAR(range, name, args, resultType) => {
        val resultRune = output.addRune()
        BuiltinCallTR(range, resultRune, name, args.map(translateRule(output, _)), resultType)
        vimpl() // we should split apart the various builtins, so we can know the correct puzzles
        resultRune
      }
      //      case CoordListAR(rules) => CoordListTR(rules.map(translateRule))
      case _ => vimpl()
    }
  }

  def translateTemplex(
    output: ConstructingRuneWorldTR,
    templexA: ITemplexA
  ): Int = {
    templexA match {
      case RuneAT(range, rune, resultType) => {
        output.addRune(rune)
      }
      case NameAT(range, name, resultType) => {
        val resultRune = output.addRune()
        NameTR(range, resultRune, name, resultType)
        resultRune
      }
      case OwnershipAT(range, ownership) => {
        val resultRune = output.addRune()
        OwnershipTR(range, resultRune, ownership)
        resultRune
      }
      case PermissionAT(range, permission) => {
        val resultRune = output.addRune()
        PermissionTR(range, resultRune, permission)
        resultRune
      }
      case InterpretedAT(range, ownership, permission, inner) => {
        val resultRune = output.addRune()
        InterpretedTR(range, resultRune, ownership, permission, translateTemplex(output, inner))
        resultRune
      }
      case AbsoluteNameAT(range, name, resultType) => {
        val resultRune = output.addRune()
        AbsoluteNameTR(range, resultRune, name, resultType)
        resultRune
      }
      case CallAT(range, template, args, resultType) => {
        val resultRune = output.addRune()
        CallTR(range, resultRune, translateTemplex(output, template), args.map(translateTemplex(output, _)), resultType)
        resultRune
      }
      case MutabilityAT(range, m) => {
        val resultRune = output.addRune()
        MutabilityTR(range, resultRune, m)
        resultRune
      }
      case VariabilityAT(range, m) => {
        val resultRune = output.addRune()
        VariabilityTR(range, resultRune, m)
        resultRune
      }
      case ManualSequenceAT(range, m, resultType) => {
        val resultRune = output.addRune()
        ManualSequenceTR(range, resultRune, m.map(translateTemplex(output, _)), resultType)
        resultRune
      }
      case RepeaterSequenceAT(range, mutability, variability, size, element, resultType) => {
        val resultRune = output.addRune()
        RepeaterSequenceTR(range, resultRune, translateTemplex(output, mutability), translateTemplex(output, variability), translateTemplex(output, size), translateTemplex(output, element), resultType)
        resultRune
      }
      //      case PackAT(range, members, resultType) => {
      val resultRune = output.addRune()
      //      PackTR(range, members.map(translateTemplex(output, _)), resultType)
        resultRune
      //}
      case IntAT(range, value) => {
        val resultRune = output.addRune()
        IntTR(range, resultRune, value)
        resultRune
      }
      case StringAT(range, value) => {
        val resultRune = output.addRune()
        StringTR(range, resultRune, value)
        resultRune
      }
      case CoordListAT(range, elements) => {
        val resultRune = output.addRune()
        CoordListTR(range, resultRune, elements.map(translateTemplex(output, _)))
        resultRune
      }
      case _ => vimpl(templexA.toString)
    }
  }
}
