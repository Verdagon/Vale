package net.verdagon.vale.scout.patterns

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, _}
import net.verdagon.vale.solver.TentativeRune
import net.verdagon.vale.{vassert, vassertSome, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.List

case class RuleStateBox(var rate: IRuleState) {
  override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  def newImplicitRune(): IRuneS = {
    val (newRate, rune) = rate.newImplicitRune()
    rate = newRate
    rune
  }
}

sealed trait IRuleState {
  def newImplicitRune(): (IRuleState, IRuneS)
}

// Sometimes referred to as a "rate"
case class RuleState(
    containerName: INameS,
    nextImplicitRune: Int) extends IRuleState {
  override def hashCode(): Int = vcurious()
  def newImplicitRune(): (RuleState, IRuneS) = {
    (RuleState(containerName, nextImplicitRune + 1),
      ImplicitRuneS(containerName, nextImplicitRune))
  }
}
case class LetRuleState(
    envFullName: INameS,
    letCodeLocation: CodeLocationS,
    nextImplicitRune: Int) extends IRuleState {
  override def hashCode(): Int = vcurious()
  def newImplicitRune(): (LetRuleState, IRuneS) = {
    (
      LetRuleState(envFullName, letCodeLocation, nextImplicitRune + 1),
      LetImplicitRuneS(letCodeLocation, nextImplicitRune))
  }
}

object PatternScout {
  def getParameterCaptures(pattern: AtomSP): Vector[VariableDeclaration] = {
    val AtomSP(_, maybeCapture, _, _, maybeDestructure) = pattern
  Vector.empty ++
      maybeCapture.toVector.flatMap(getCaptureCaptures) ++
        maybeDestructure.toVector.flatten.flatMap(getParameterCaptures)
  }
  private def getCaptureCaptures(capture: CaptureS): Vector[VariableDeclaration] = {
    Vector(VariableDeclaration(capture.name))
  }

  // Returns:
  // - New rules
  // - Scouted patterns
  private[scout] def scoutPatterns(
      stackFrame: StackFrame,
      rulesS: RuleStateBox,
      knowableRunesFromAbove: Set[IRuneS],
      ruleBuilder: ScoutRuleBuilder,
      params: Vector[PatternPP]):
  Vector[AtomSP] = {
    params.map(PatternScout.translatePattern(stackFrame, rulesS, knowableRunesFromAbove, ruleBuilder, _))
  }

  // Returns:
  // - Rules, which are likely just TypedSR
  // - The translated patterns
  private[scout] def translatePattern(
    stackFrame: StackFrame,
    ruleState: RuleStateBox,
    knowableRunesFromAbove: Set[IRuneS],
    ruleBuilder: ScoutRuleBuilder,
    patternPP: PatternPP):
  AtomSP = {
    val PatternPP(range,_,maybeCaptureP, maybeTypeP, maybeDestructureP, maybeVirtualityP) = patternPP

    val maybeVirtualityS =
      maybeVirtualityP match {
        case None => None
        case Some(AbstractP) => Some(AbstractSP)
        case Some(OverrideP(range, typeP)) => {
          typeP match {
            case InterpretedPT(range, _, _, _) => {
              throw CompileErrorExceptionS(CantOverrideOwnershipped(Scout.evalRange(stackFrame.file, range)))
            }
            case _ =>
          }

          val tentativeRune =
            translateMaybeTypeIntoRune(
              stackFrame.parentEnv,
              ruleState,
              Scout.evalRange(stackFrame.file, range),
              knowableRunesFromAbove,
              ruleBuilder,
              Some(typeP),
              KindTypePR)

          // All this nonsense is because OverrideSP expects an IRuneS.
          // OverrideSP can't have a canonical rune because we don't know any canonical runes
          // yet because we're still assembling the rules.
          val runeS = ruleState.newImplicitRune()
          ruleBuilder.builder.noteRunesEqual(
            tentativeRune,
            ruleBuilder.addRune(Scout.evalRange(stackFrame.file, range), knowableRunesFromAbove, runeS))
          Some(OverrideSP(Scout.evalRange(stackFrame.file, range), runeS))
        }
      }

    val coordTentativeRune =
      translateMaybeTypeIntoRune(
        stackFrame.parentEnv, ruleState, Scout.evalRange(stackFrame.file, range), knowableRunesFromAbove, ruleBuilder, maybeTypeP, CoordTypePR)
    // All this nonsense is because AtomSP expects an IRuneS.
    // AtomSP can't have a canonical rune because we don't know any canonical runes
    // yet because we're still assembling the rules.
    val coordRuneS = ruleState.newImplicitRune()
    ruleBuilder.builder.noteRunesEqual(
      coordTentativeRune,
      ruleBuilder.addRune(Scout.evalRange(stackFrame.file, range), knowableRunesFromAbove, coordRuneS))

    val maybePatternsS =
      maybeDestructureP match {
        case None => None
        case Some(DestructureP(_, destructureP)) => {
          Some(destructureP.map(translatePattern(stackFrame, ruleState, knowableRunesFromAbove, ruleBuilder, _)))
        }
      }

    val captureS =
      maybeCaptureP match {
        case None => {
//          val codeLocation = Scout.evalPos(stackFrame.file, patternPP.range.begin)
          None
        }
        case Some(CaptureP(_,LocalNameP(NameP(_, name)))) => {
          if (name == "set" || name == "mut") {
            throw CompileErrorExceptionS(CantUseThatLocalName(Scout.evalRange(stackFrame.file, range), name))
          }
          Some(CaptureS(CodeVarNameS(name)))
        }
        case Some(CaptureP(_,ConstructingMemberNameP(NameP(_, name)))) => {
          Some(CaptureS(ConstructingMemberNameS(name)))
        }
      }

    AtomSP(Scout.evalRange(stackFrame.file, range), captureS, maybeVirtualityS, coordRuneS, maybePatternsS)
  }

  def translateMaybeTypeIntoRune(
      env: IEnvironment,
      rulesS: RuleStateBox,
      range: RangeS,
      knowableRunesFromAbove: Set[IRuneS],
      ruleBuilder: ScoutRuleBuilder,
      maybeTypeP: Option[ITemplexPT],
      runeType: ITypePR,
      // Determines whether the rune is on the left or the right in the Equals rule, which
      // can (unfortunately) affect the order in which the generics engine evaluates things.
      // This is a temporary solution, see DCRC, option A.
      runeOnLeft: Boolean = true):
  TentativeRune = {
    maybeTypeP match {
      case None => {
        val rune = rulesS.newImplicitRune()
        ruleBuilder.translateRule(knowableRunesFromAbove, TypedSR(range, rune, RuleScout.translateType(runeType)))
      }
      case Some(NameOrRunePT(NameP(_, nameOrRune))) if env.allUserDeclaredRunes().contains(CodeRuneS(nameOrRune)) => {
        val rune = CodeRuneS(nameOrRune)
        ruleBuilder.translateRule(knowableRunesFromAbove, TypedSR(range, rune, RuleScout.translateType(runeType)))
      }
      case Some(nonRuneTemplexP) => {
        val (newRulesFromInner, templexS, maybeRune) =
          translatePatternTemplex(env, rulesS, nonRuneTemplexP)
        newRulesFromInner.foreach(ruleBuilder.translateRule(knowableRunesFromAbove, _))

        maybeRune match {
          case Some(rune) => ruleBuilder.addRune(range, knowableRunesFromAbove, rune)
          case None => {
            val rune = rulesS.newImplicitRune()
            if (runeOnLeft) {
              ruleBuilder.translateRule(
                knowableRunesFromAbove,
                EqualsSR(templexS.range, TypedSR(range, rune, RuleScout.translateType(runeType)), templexS))
            } else {
              ruleBuilder.translateRule(
                knowableRunesFromAbove,
                EqualsSR(templexS.range, templexS, TypedSR(range, rune, RuleScout.translateType(runeType))))
            }
          }
        }
      }
    }
  }
  def translateMaybeTypeIntoMaybeRune(
    env: IEnvironment,
    rulesS: RuleStateBox,
    range: RangeS,
    knowableRunesFromAbove: Set[IRuneS],
    ruleBuilder: ScoutRuleBuilder,
    maybeTypeP: Option[ITemplexPT],
    runeType: ITypePR,
    // Determines whether the rune is on the left or the right in the Equals rule, which
    // can (unfortunately) affect the order in which the generics engine evaluates things.
    // This is a temporary solution, see DCRC, option A.
    runeOnLeft: Boolean = true):
  Option[IRuneS] = {
    if (maybeTypeP.isEmpty) {
      None
    } else {
      val tentativeRune =
        translateMaybeTypeIntoRune(
          env, rulesS, range, knowableRunesFromAbove, ruleBuilder, maybeTypeP, runeType, runeOnLeft)
      val runeS = ruleBuilder.nameTentativeRune(range, knowableRunesFromAbove, rulesS.newImplicitRune(), tentativeRune)
      Some(runeS)
    }
  }

//  private def translatePatternTemplexes(rulesS: WorkingRulesAndRunes, templexesP: Vector[ITemplexPT]):
//  (Vector[IRulexSR], Vector[ITemplexS]) = {
//    templexesP match {
//      case Nil => (rulesS, Vector())
//      case headTemplexP :: tailTemplexesP => {
//        val (rulesS, headTemplexS) = translatePatternTemplex(rulesS, headTemplexP)
//        val (rulesS, tailTemplexesS) = translatePatternTemplexes(rulesS, tailTemplexesP)
//        (rulesS, headTemplexS :: tailTemplexesS)
//      }
//    }
//  }

  private def translatePatternTemplexes(
    env: IEnvironment,
    rulesS: RuleStateBox,
    templexesP: Vector[ITemplexPT]):
  (Vector[IRulexSR], Vector[IRulexSR]) = {
    val results = templexesP.map(translatePatternTemplex(env, rulesS, _))
    (results.map(_._1).flatten, results.map(_._2))
  }

  // Returns:
  // - Any new rules we need to add
  // - A templex that represents the result
  // - If any, the rune associated with this exact result.
  def translatePatternTemplex(
      env: IEnvironment,
      rulesS: RuleStateBox,
      templexP: ITemplexPT):
  (Vector[IRulexSR], IRulexSR, Option[IRuneS]) = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)

    templexP match {
      case AnonymousRunePT(range) => {
        val rune = rulesS.newImplicitRune()
        (Vector.empty, RuneSR(evalRange(range), rune), Some(rune))
      }
      case IntPT(range,value) => (Vector.empty, IntSR(evalRange(range), value), None)
      case BoolPT(range,value) => (Vector.empty, BoolSR(evalRange(range), value), None)
      case NameOrRunePT(NameP(range, nameOrRune)) => {
        if (env.allUserDeclaredRunes().contains(CodeRuneS(nameOrRune))) {
          (Vector.empty, RuneSR(evalRange(range), CodeRuneS(nameOrRune)), Some(CodeRuneS(nameOrRune)))
        } else {
          (Vector.empty, NameSR(Scout.evalRange(env.file, range), CodeTypeNameS(nameOrRune)), None)
        }
      }
      case MutabilityPT(range, mutability) => (Vector.empty, MutabilitySR(evalRange(range), mutability), None)
      case VariabilityPT(range, variability) => (Vector.empty, VariabilitySR(evalRange(range), variability), None)
      case InterpretedPT(range,ownership,permission, innerP) => {
        val (newRules, innerS, _) =
          translatePatternTemplex(env, rulesS, innerP)
        (newRules, InterpretedSR(evalRange(range), ownership, permission, innerS), None)
      }
      case CallPT(range,maybeTemplateP, argsMaybeTemplexesP) => {
        val (newRulesFromTemplate, maybeTemplateS, _) = translatePatternTemplex(env, rulesS, maybeTemplateP)
        val (newRulesFromArgs, argsMaybeTemplexesS) = translatePatternTemplexes(env, rulesS, argsMaybeTemplexesP)
        (newRulesFromTemplate ++ newRulesFromArgs, CallSR(evalRange(range), maybeTemplateS, argsMaybeTemplexesS), None)
      }
      case RepeaterSequencePT(range, mutabilityP, variabilityP, sizeP, elementP) => {
        val (newRulesFromMutability, mutabilityS, _) = translatePatternTemplex(env, rulesS, mutabilityP)
        val (newRulesFromVariability, variabilityS, _) = translatePatternTemplex(env, rulesS, variabilityP)
        val (newRulesFromSize, sizeS, _) = translatePatternTemplex(env, rulesS, sizeP)
        val (newRulesFromElement, elementS, _) = translatePatternTemplex(env, rulesS, elementP)
        (newRulesFromMutability ++ newRulesFromVariability ++ newRulesFromSize ++ newRulesFromElement, RepeaterSequenceSR(evalRange(range), mutabilityS, variabilityS, sizeS, elementS), None)
      }
      case ManualSequencePT(range,maybeMembersP) => {
        val (newRules, maybeMembersS) = translatePatternTemplexes(env, rulesS, maybeMembersP)
        (newRules, ManualSequenceSR(evalRange(range), maybeMembersS), None)
      }
//      case FunctionPT(mutableP, paramsP, retP) => {
//        val (mutableS, _) = translatePatternMaybeTemplex(declaredRunes, rulesS, mutableP, None)
//        val paramsS = translatePatternTemplexes(declaredRunes, rulesS, paramsP)
//        val (retS, _) = translatePatternTemplex(env, rulesS, retP)

//        vfail("impl!")
//        CallST(
//          NameST("IFunction"),
//          Vector(
//            mutableS.getOrElse(MutableP),
//            paramsS,
//            retS))

//        (rulesS, FunctionST(mutableS, PackST(paramsS), retS), None)
//      }
      case x => vwat(x.toString)
    }
  }
}
