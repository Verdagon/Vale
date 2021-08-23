//package net.verdagon.vale.scout.templatepredictor
//
//import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
//import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils}
//import net.verdagon.vale.scout.predictor.{Conclusions, ConclusionsBox}
//import net.verdagon.vale.scout.rules._
//import net.verdagon.vale.{vfail, vimpl}
//
//import scala.collection.immutable.List
//
//// Given enough user specified template params and param inputs, we should be able to
//// infer everything.
//// This class's purpose is to take those things, and see if it can figure out as many
//// inferences as possible.
//
//object PredictorEvaluator {
//
//  private[scout] def getAllRunes(
//    userSpecifiedIdentifyingRunes: Vector[IRuneS],
//    rules: Vector[IRulexSR],
//    patterns1: Vector[AtomSP],
//    maybeRetRune: Option[IRuneS]
//  ): Set[IRuneS] = {
//    (
//      userSpecifiedIdentifyingRunes ++
//        patterns1.flatMap(PatternSUtils.getDistinctOrderedRunesForPattern) ++
//        RuleSUtils.getDistinctOrderedRunesForRulexes(rules) ++
//        maybeRetRune.toVector
//      ).toSet
//  }
//
//  private[scout] def solve(
//    // See MKKRFA
//    knowableRunesFromAbove: Set[IRuneS],
//    rules: Vector[IRulexSR],
//    paramAtoms: Vector[AtomSP],
//  ): Conclusions = {
//    val conclusionsBox = ConclusionsBox(Conclusions(knowableRunesFromAbove, Map()))
//    solveUntilSettled(rules, conclusionsBox)
//    conclusionsBox.conclusions
//  }
//
//  private def solveUntilSettled(
//    rules: Vector[IRulexSR],
//    conclusions: ConclusionsBox,
//  ): Unit = {
//    val conclusionsBefore = conclusions.conclusions
//
//    val _ = evaluateRules(conclusions, rules)
//
//    if (conclusions.conclusions != conclusionsBefore) {
//      // Things have not settled, we made some sort of progress in this last iteration.
//      // Keep going.
//      solveUntilSettled(rules, conclusions)
//    } else {
//      // No need to do one last match, because we just did an entire iteration where nothing changed.
//
//    }
//  }
//
//  private def evaluateRule(conclusions: ConclusionsBox, rule: IRulexSR): Boolean = {
//    rule match {
//      case r @ EqualsSR(_,_, _) => evaluateEqualsRule(conclusions, r)
//      case r @ IsaSR(_,_, _) => evaluateIsaRule(conclusions, r)
//      case r @ OrSR(_,_) => evaluateOrRule(conclusions, r)
//      case r @ ComponentsSR(_,_, _) => evaluateComponentsRule(conclusions, r)
//      case r @ TypedSR(_,_, _) => evaluateTypedRule(conclusions, r)
//      case r @ BuiltinCallSR(_,_, _) => evaluateRuleCall(conclusions, r)
//      case IntSR(_, _) => true
//      case StringSR(_, _) => true
//      case BoolSR(_, _) => true
//      case MutabilitySR(_, _) => true
//      case PermissionSR(_, _) => true
//      case LocationSR(_, _) => true
//      case OwnershipSR(_, _) => true
//      case VariabilitySR(_, _) => true
//      case NameSR(_, _) => true
//      case AbsoluteNameSR(_, _) => true
//      case BorrowSR(_, inner) => evaluateRule(conclusions, inner)
//      case RuneSR(_, rune) => {
//        conclusions.knowableValueRunes.contains(rune)
//      }
//      case InterpretedSR(_, _, _, kindRule) => evaluateRule(conclusions, kindRule)
//      case CallSR(_, templateRule, paramRules) => {
//        val templateKnown = evaluateRule(conclusions, templateRule)
//        val argsKnown = evaluateRulees(conclusions, paramRules)
//        templateKnown && argsKnown.forall(_ == true)
//      }
//      case PrototypeSR(_, _, _, _) => {
//        vfail("Unimplemented")
//      }
//      case PackSR(_, memberTemplexes) => {
//        val membersKnown =
//          evaluateRulees(conclusions, memberTemplexes)
//        membersKnown.forall(_ == true)
//      }
//      case RepeaterSequenceSR(_, mutabilityTemplex, variabilityTemplex, sizeTemplex, elementTemplex) => {
//        val mutabilityKnown =
//          evaluateRule(conclusions, mutabilityTemplex)
//        val variabilityKnown =
//          evaluateRule(conclusions, variabilityTemplex)
//        val sizeKnown =
//          evaluateRule(conclusions, sizeTemplex)
//        val elementKnown =
//          evaluateRule(conclusions, elementTemplex)
//        mutabilityKnown && variabilityKnown && sizeKnown && elementKnown
//      }
//      case ManualSequenceSR(_, elementsTemplexes) => {
//        val membersKnown =
//          evaluateRulees(conclusions, elementsTemplexes)
//        membersKnown.forall(_ == true)
//      }
//    }
//  }
//
////  private def evaluatePackRule(conclusions: ConclusionsBox, rule: PackSR): Boolean = {
////    val PackSR(elements) = rule
////    evaluateRules(conclusions, elements).forall(_ == true)
////  }
//
//  private def evaluateRules(
//    conclusions: ConclusionsBox,
//    rules: Vector[IRulexSR],
//  ): Vector[Boolean] = {
//    rules.map(evaluateRule(conclusions, _))
//  }
//
//  private def evaluateRuleCall(
//    conclusions: ConclusionsBox,
//    ruleCall: BuiltinCallSR,
//  ): Boolean = {
//    val BuiltinCallSR(range, name, argumentRules) = ruleCall
//
//    name match {
//      case "toRef" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfConcrete" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfStruct" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfInterface" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
////      case "resolveExactSignature" => {
////        val Vector(nameRule, argsRule) = argumentRules
////        val evaluateNameSuccess = evaluateRule(conclusions, nameRule)
////        val evaluateArgsSuccess = evaluateRule(conclusions, argsRule)
////        evaluateNameSuccess && evaluateArgsSuccess
////      }
//      case _ => throw CompileErrorExceptionS(RangedInternalErrorS(range, "Unknown function \"" + name + "\"!"))
//    }
//  }
//  private def evaluateRulees(
//    conclusions: ConclusionsBox,
//    ruleTemplexes: Vector[IRulexSR],
//  ): Vector[Boolean] = {
//    val knowns =
//      ruleTemplexes.map({
//        case (ruleTemplex) => {
//          val result = evaluateRule(conclusions, ruleTemplex)
//          result
//        }
//      })
//    knowns
//  }
//
//  private def evaluateTypedRule(
//    conclusions: ConclusionsBox,
//    rule: TypedSR):
//  Boolean = {
//    val TypedSR(_, rune, tyype) = rule
//    conclusions.markRuneTypeKnown(rune, tyype)
//    conclusions.knowableValueRunes.contains(rune)
//  }
//
//  private def evaluateEqualsRule(
//    conclusions: ConclusionsBox,
//    rule: EqualsSR,
//  ): Boolean = {
//    val EqualsSR(_, leftRule, rightRule) = rule
//
//    val leftKnown =
//      evaluateRule(conclusions, leftRule)
//    val rightKnown =
//      evaluateRule(conclusions, rightRule)
//    if (!leftKnown && !rightKnown) {
//      false
//    } else {
//      PredictorMatcher.matchAgainstRulexSR(conclusions, leftRule)
//      PredictorMatcher.matchAgainstRulexSR(conclusions, rightRule)
//      true
//    }
//  }
//
//  private def evaluateIsaRule(
//    conclusions: ConclusionsBox,
//    rule: IsaSR,
//  ): Boolean = {
//    val IsaSR(_, leftRule, rightRule) = rule
//
//    val leftKnown =
//      evaluateRule(conclusions, leftRule)
//    val rightKnown =
//      evaluateRule(conclusions, rightRule)
//
//    // Knowing the right rule doesn't really help us with anything, unfortunately...
//    val _ = rightKnown
//
//    // We return the left thing for the rule, so if we know the left thing, we know the result of the rule.
//    leftKnown
//  }
//
//  private def evaluateOrRule(
//    conclusions: ConclusionsBox,
//    rule: OrSR
//  ): Boolean = {
//    val possibilitiesKnowns =
//      evaluateRules(conclusions, rule.alternatives)
//    // This is a conservative guess. We might be able to return true if only one is known.
//    possibilitiesKnowns.forall(_ == true)
//  }
//
//  private def evaluateComponentsRule(
//    conclusions: ConclusionsBox,
//    rule: ComponentsSR,
//  ): Boolean = {
//    val ComponentsSR(_, typedRule, componentsRules) = rule
//
//    val runeKnown =
//      evaluateRule(conclusions, typedRule)
//
//    val componentsKnown =
//      evaluateRules(conclusions, componentsRules)
//    val allComponentsKnown = componentsKnown.forall(_ == true)
//
//    runeKnown || allComponentsKnown
//  }
//}
