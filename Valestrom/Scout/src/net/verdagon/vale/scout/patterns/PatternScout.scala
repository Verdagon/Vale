package net.verdagon.vale.scout.patterns

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, _}
import net.verdagon.vale.templar.types.{CoordTemplataType, ITemplataType, KindTemplataType}
import net.verdagon.vale.{vassert, vassertSome, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
      lidb: LocationInDenizenBuilder,
      ruleBuilder: ArrayBuffer[IRulexSR],
      runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
      params: Vector[PatternPP]):
  Vector[AtomSP] = {
    params.map(
      PatternScout.translatePattern(
        stackFrame, lidb, ruleBuilder, runeToExplicitType, _))
  }

  // Returns:
  // - Rules, which are likely just TypedSR
  // - The translated patterns
  private[scout] def translatePattern(
    stackFrame: StackFrame,
    lidb: LocationInDenizenBuilder,
    ruleBuilder: ArrayBuffer[IRulexSR],
    runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
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

          val runeS =
            translateMaybeTypeIntoRune(
              stackFrame.parentEnv,
              lidb.child(),
              Scout.evalRange(stackFrame.file, range),
              ruleBuilder,
              runeToExplicitType,
              Some(typeP),
              true)
          Some(OverrideSP(Scout.evalRange(stackFrame.file, range), runeS))
        }
      }

    val coordRuneS =
      translateMaybeTypeIntoRune(
        stackFrame.parentEnv,
        lidb.child(),
        Scout.evalRange(stackFrame.file, range),
        ruleBuilder,
        runeToExplicitType,
        maybeTypeP,
        false)

    val maybePatternsS =
      maybeDestructureP match {
        case None => None
        case Some(DestructureP(_, destructureP)) => {
          Some(
            destructureP.map(
              translatePattern(
                stackFrame, lidb.child(), ruleBuilder, runeToExplicitType, _)))
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
      lidb: LocationInDenizenBuilder,
      range: RangeS,
      ruleBuilder: ArrayBuffer[IRulexSR],
      runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
      maybeTypeP: Option[ITemplexPT],
      askingForKind: Boolean,
      // Determines whether the rune is on the left or the right in the Equals rule, which
      // can (unfortunately) affect the order in which the generics engine evaluates things.
      // This is a temporary solution, see DCRC, option A.
      runeOnLeft: Boolean = true):
  RuneUsage = {
    maybeTypeP match {
      case None => {
        val resultRuneS = RuneUsage(range, ImplicitRuneS(lidb.child().consume()))
        runeToExplicitType.put(resultRuneS.rune, if (askingForKind) KindTemplataType else CoordTemplataType)
        resultRuneS
      }
      case Some(NameOrRunePT(NameP(range, nameOrRune))) if env.allDeclaredRunes().contains(CodeRuneS(nameOrRune)) => {
        val resultRuneS = RuneUsage(Scout.evalRange(env.file, range), CodeRuneS(nameOrRune))
        runeToExplicitType.put(resultRuneS.rune, if (askingForKind) KindTemplataType else CoordTemplataType)
//        ruleBuilder += ValueLeafSR(range, resultRuneS, EnvRuneLookupSR(CodeRuneS(nameOrRune)))
//        resultRuneS
        resultRuneS
      }
      case Some(nonRuneTemplexP) => {
        TemplexScout.translateTemplex(env, lidb.child(), ruleBuilder, nonRuneTemplexP, askingForKind)
      }
    }
  }
  def translateMaybeTypeIntoMaybeRune(
    env: IEnvironment,
    lidb: LocationInDenizenBuilder,
    range: RangeS,
    ruleBuilder: ArrayBuffer[IRulexSR],
    runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
    maybeTypeP: Option[ITemplexPT],
    askingForKind: Boolean,
    // Determines whether the rune is on the left or the right in the Equals rule, which
    // can (unfortunately) affect the order in which the generics engine evaluates things.
    // This is a temporary solution, see DCRC, option A.
    runeOnLeft: Boolean = true):
  Option[RuneUsage] = {
    if (maybeTypeP.isEmpty) {
      None
    } else {
      Some(
        translateMaybeTypeIntoRune(
          env, lidb.child(), range, ruleBuilder, runeToExplicitType, maybeTypeP, askingForKind, runeOnLeft))
    }
  }
}
