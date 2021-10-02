package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser.{AnonymousRunePT, BoolPT, BorrowPT, CallPT, ConstraintP, FunctionPT, ITemplexPT, InlinePT, IntPT, InterpretedPT, LocationPT, ManualSequencePT, MutabilityPT, MutableP, NameOrRunePT, NameP, OwnershipPT, PackPT, PermissionPT, PrototypePT, Range, RepeaterSequencePT, StringPT, VariabilityPT}
import net.verdagon.vale.scout.{CodeRuneS, CodeTypeNameS, IEnvironment, INameS, IRuneS, ImplicitRuneS, LocationInDenizenBuilder, RangeS, Scout}

import scala.collection.mutable.ArrayBuffer

object TemplexScout {
  def addLiteralRule(
    lidb: LocationInDenizenBuilder,
    ruleBuilder: ArrayBuffer[IRulexSR],
    rangeS: RangeS,
    valueSR: ILiteralSL):
  RuneUsage = {
    val runeS = RuneUsage(rangeS, ImplicitRuneS(lidb.child().consume()))
    ruleBuilder += LiteralSR(rangeS, runeS, valueSR)
    runeS
  }

  def addLookupRule(
    lidb: LocationInDenizenBuilder,
    ruleBuilder: ArrayBuffer[IRulexSR],
    rangeS: RangeS,
    nameSN: INameS,
    askingForKind: Boolean):
  RuneUsage = {
    val runeS = RuneUsage(rangeS, ImplicitRuneS(lidb.child().consume()))
    ruleBuilder +=
      (if (askingForKind) {
        KindLookupSR(rangeS, runeS, nameSN)
      } else {
        LookupSR(rangeS, runeS, nameSN)
      })
    runeS
  }

  def translateValueTemplex(templex: ITemplexPT): Option[ILiteralSL] = {
    templex match {
      case IntPT(_, value) => Some(IntLiteralSL(value))
      case BoolPT(_, value) => Some(BoolLiteralSL(value))
      case MutabilityPT(_, mutability) => Some(MutabilityLiteralSL(mutability))
      case VariabilityPT(_, variability) => Some(VariabilityLiteralSL(variability))
      case PermissionPT(_, permission) => Some(PermissionLiteralSL(permission))
      case StringPT(_, value) => Some(StringLiteralSL(value))
      case LocationPT(_, location) => Some(LocationLiteralSL(location))
      case OwnershipPT(_, ownership) => Some(OwnershipLiteralSL(ownership))
      case _ => None
    }
  }

  def translateTemplex(
    env: IEnvironment,
    lidb: LocationInDenizenBuilder,
    ruleBuilder: ArrayBuffer[IRulexSR],
    templex: ITemplexPT,
    // True if we want a lookup to be for a kind instead of a coord.
    askingForKind: Boolean):
  RuneUsage = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)

    translateValueTemplex(templex) match {
      case Some(x) => addLiteralRule(lidb.child(), ruleBuilder, evalRange(templex.range), x)
      case None => {
        templex match {
          case AnonymousRunePT(range) => RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
          case NameOrRunePT(NameP(range, nameOrRune)) => {
            val isRuneFromEnv = env.allDeclaredRunes().contains(CodeRuneS(nameOrRune))
            if (isRuneFromEnv) {
              RuneUsage(evalRange(range), CodeRuneS(nameOrRune))
            } else {
              val valueSR = CodeTypeNameS(nameOrRune)
              addLookupRule(lidb.child(), ruleBuilder, evalRange(range), valueSR, askingForKind)
            }
          }
          case InterpretedPT(range, ownership, permission, innerP) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            val innerRuneS = translateTemplex(env, lidb.child(), ruleBuilder, innerP, false)
            ruleBuilder += AugmentSR(evalRange(range), resultRuneS, Vector(OwnershipLiteralSL(ownership), PermissionLiteralSL(permission)), innerRuneS)
            resultRuneS
          }
          case BorrowPT(range, innerP) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            val innerRuneS = translateTemplex(env, lidb.child(), ruleBuilder, innerP, false)
            ruleBuilder += AugmentSR(evalRange(range), resultRuneS, Vector(OwnershipLiteralSL(ConstraintP)), innerRuneS)
            resultRuneS
          }
          case CallPT(range, template, args) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            ruleBuilder +=
              CallSR(
                evalRange(range),
                resultRuneS,
                askingForKind,
                translateTemplex(env, lidb.child(), ruleBuilder, template, false),
                args.map(translateTemplex(env, lidb.child(), ruleBuilder, _, false)).toArray)
            resultRuneS
          }
          case FunctionPT(range, mutability, paramsPack, returnType) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            val templateNameRuneS = addLookupRule(lidb.child(), ruleBuilder, evalRange(range), CodeTypeNameS("IFunction"), false)
            val mutabilityRuneS =
              mutability match {
                case None => addLiteralRule(lidb.child(), ruleBuilder, evalRange(range), MutabilityLiteralSL(MutableP))
                case Some(m) => translateTemplex(env, lidb.child(), ruleBuilder, m, false)
              }
            ruleBuilder +=
              CallSR(
                evalRange(range),
                resultRuneS,
                askingForKind,
                templateNameRuneS,
                Array(
                  mutabilityRuneS,
                  translateTemplex(env, lidb.child(), ruleBuilder, paramsPack, false),
                  translateTemplex(env, lidb.child(), ruleBuilder, returnType, false)))
            resultRuneS
          }
          case PrototypePT(range, NameP(_, name), parameters, returnType) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            ruleBuilder +=
              PrototypeSR(
                evalRange(range),
                resultRuneS,
                name,
                parameters.map(translateTemplex(env, lidb.child(), ruleBuilder, _, false)).toArray,
                translateTemplex(env, lidb.child(), ruleBuilder, returnType, false))
            resultRuneS
          }
          case PackPT(range, members) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            ruleBuilder +=
              PackSR(
                evalRange(range),
                resultRuneS,
                members.map(translateTemplex(env, lidb.child(), ruleBuilder, _, false)).toArray)
            resultRuneS
          }
          case RepeaterSequencePT(range, mutability, variability, size, element) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            ruleBuilder +=
              RepeaterSequenceSR(
                evalRange(range),
                resultRuneS,
                translateTemplex(env, lidb.child(), ruleBuilder, mutability, false),
                translateTemplex(env, lidb.child(), ruleBuilder, variability, false),
                translateTemplex(env, lidb.child(), ruleBuilder, size, false),
                translateTemplex(env, lidb.child(), ruleBuilder, element, false))
            resultRuneS
          }
          case ManualSequencePT(range, elements) => {
            val resultRuneS = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            ManualSequenceSR(
              evalRange(range),
              resultRuneS,
              elements.map(translateTemplex(env, lidb.child(), ruleBuilder, _, false)).toArray)
            resultRuneS
          }
        }
      }
    }
  }
}
