package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{IRulexSR, RangeS}
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

class InferTemplar(
    opts: TemplarOptions,
    profiler: IProfiler,
    delegate: IInfererDelegate[IEnvironment, Temputs]) {
  private def solve(
    env: IEnvironment,
    state: Temputs,
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneS, ITemplataType],
    localRunes: Set[IRuneS],
    invocationRange: RangeS,
    directInputs: Map[IRuneS, ITemplata],
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
    typeByRune: Map[IRuneS, ITemplataType],
    localRunes: Set[IRuneS],
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
    identifyingRunes: Vector[IRuneS],
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneS, ITemplataType],
    localRunes: Set[IRuneS],
    patterns1: Vector[AtomAP],
    maybeRetRune: Option[IRuneS],
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
    identifyingRunes: Vector[IRuneS],
    rules: Vector[IRulexAR],
    typeByRune: Map[IRuneS, ITemplataType],
    localRunes: Set[IRuneS],
    patterns1: Vector[AtomAP],
    maybeRetRune: Option[IRuneS],
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
}
