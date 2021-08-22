package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.RuleStateBox
import net.verdagon.vale.scout.{IEnvironment, Environment => _, FunctionEnvironment => _, _}
import net.verdagon.vale.{vassert, vassertSome}

object RuleScout {
  // Returns:
  // - new rules produced on the side while translating the given rules
  // - the translated versions of the given rules
  def translateRulexes(
    env: IEnvironment,
    ruleState: RuleStateBox,
    userDeclaredRunes: Set[IRuneS],
    rulesP: Vector[IRulexPR]):
  Vector[IRulexSR] = {
    rulesP.map(translateRulex(env, ruleState, userDeclaredRunes, _))
  }
  def translateRulex(
    env: IEnvironment,
    ruleState: RuleStateBox,
    userDeclaredRunes: Set[IRuneS],
    rulex: IRulexPR):
  IRulexSR = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)

    rulex match {
//      case PackPR(elements) => {
//        PackSR(translateRulexes(env, ruleState, userDeclaredRunes, elements))
//      }
      case EqualsPR(range, leftP, rightP) => {
        EqualsSR(evalRange(range), translateRulex(env, ruleState, userDeclaredRunes, leftP), translateRulex(env, ruleState, userDeclaredRunes, rightP))
      }
      case OrPR(range, possibilitiesP) => {
        OrSR(evalRange(range), translateRulexes(env, ruleState, userDeclaredRunes, possibilitiesP))
      }
      case ComponentsPR(range, TypedPR(typeRange, None, tyype), componentsP) => {
        val rune = ruleState.newImplicitRune()
        ComponentsSR(
          Scout.evalRange(env.file, range),
          TypedSR(evalRange(typeRange), rune, translateType(tyype)),
          translateRulexes(env, ruleState, userDeclaredRunes, componentsP))
      }
      case ComponentsPR(range, TypedPR(typeRange, Some(NameP(_, rune)), tyype), componentsP) => {
        ComponentsSR(
          Scout.evalRange(env.file, range),
          TypedSR(
            Scout.evalRange(env.file, typeRange),
            CodeRuneS(rune),
            translateType(tyype)),
          translateRulexes(env, ruleState, userDeclaredRunes, componentsP))
      }
      case TypedPR(range, None, tyype) => {
        val rune = ruleState.newImplicitRune()
        TypedSR(Scout.evalRange(env.file, range), rune, translateType(tyype))
      }
      case TypedPR(range, Some(NameP(_, runeName)), tyype) => {
        vassert(userDeclaredRunes.contains(CodeRuneS(runeName)))
        TypedSR(evalRange(range), CodeRuneS(runeName), translateType(tyype))
      }
      case TemplexPR(templex) => translateTemplex(env, ruleState, userDeclaredRunes, templex)
      case BuiltinCallPR(range, NameP(_, name), args) => BuiltinCallSR(Scout.evalRange(env.file, range), name, args.map(translateRulex(env, ruleState, userDeclaredRunes, _)))
    }
  }
  def translateType(tyype: ITypePR): ITypeSR = {
    tyype match {
      case PrototypeTypePR => PrototypeTypeSR
      case IntTypePR => IntTypeSR
      case BoolTypePR => BoolTypeSR
      case OwnershipTypePR => OwnershipTypeSR
      case MutabilityTypePR => MutabilityTypeSR
      case PermissionTypePR => PermissionTypeSR
      case LocationTypePR => LocationTypeSR
      case CoordTypePR => CoordTypeSR
      case KindTypePR => KindTypeSR
//      case StructTypePR => KindTypeSR
//      case SequenceTypePR => KindTypeSR
//      case ArrayTypePR => KindTypeSR
//      case CallableTypePR => KindTypeSR
//      case InterfaceTypePR => KindTypeSR
    }
  }

  def translateTemplex(
    env: IEnvironment,
    ruleState: RuleStateBox,
    userDeclaredRunes: Set[IRuneS],
    templex: ITemplexPT):
  IRulexSR = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)
    templex match {
      case StringPT(range, value) => StringSR(evalRange(range), value)
      case IntPT(range, value) => IntSR(evalRange(range), value)
      case MutabilityPT(range, mutability) => MutabilitySR(evalRange(range), mutability)
      case PermissionPT(range, permission) => PermissionSR(evalRange(range), permission)
      case LocationPT(range, location) => LocationSR(evalRange(range), location)
      case OwnershipPT(range, ownership) => OwnershipSR(evalRange(range), ownership)
      case VariabilityPT(range, variability) => VariabilitySR(evalRange(range), variability)
      case BoolPT(range, value) => BoolSR(evalRange(range), value)
      case NameOrRunePT(NameP(range, name)) => {
        if (userDeclaredRunes.contains(CodeRuneS(name))) {
          RuneSR(evalRange(range), CodeRuneS(name))
        } else {
          NameSR(Scout.evalRange(env.file, range), CodeTypeNameS(name))
        }
      }
      case AnonymousRunePT(range) => {
        val rune = ruleState.newImplicitRune()
        RuneSR(evalRange(range), rune)
      }
      case CallPT(range, template, args) => {
        CallSR(evalRange(range), translateTemplex(env, ruleState, userDeclaredRunes, template), args.map(translateTemplex(env, ruleState, userDeclaredRunes, _)))
      }
      case FunctionPT(range, mutability, paramsPack, returnType) => {
        CallSR(
          evalRange(range),
          NameSR(Scout.evalRange(env.file, range), CodeTypeNameS("IFunction")),
          Vector(
            mutability match { case None => MutabilitySR(evalRange(range), MutableP) case Some(m) => translateTemplex(env, ruleState, userDeclaredRunes, m) },
            translateTemplex(env, ruleState, userDeclaredRunes, paramsPack),
            translateTemplex(env, ruleState, userDeclaredRunes, returnType)))
      }
      case PrototypePT(range, NameP(_, name), parameters, returnType) => PrototypeSR(evalRange(range), name, parameters.map(translateTemplex(env, ruleState, userDeclaredRunes, _)), translateTemplex(env, ruleState, userDeclaredRunes, returnType))
      case PackPT(range, members) => PackSR(evalRange(range), members.map(translateTemplex(env, ruleState, userDeclaredRunes, _)))
      case BorrowPT(range, inner) => BorrowSR(evalRange(range), translateTemplex(env, ruleState, userDeclaredRunes, inner))
      case RepeaterSequencePT(range, mutability, variability, size, element) => {
        RepeaterSequenceSR(
          evalRange(range),
          translateTemplex(env, ruleState, userDeclaredRunes, mutability),
          translateTemplex(env, ruleState, userDeclaredRunes, variability),
          translateTemplex(env, ruleState, userDeclaredRunes, size),
          translateTemplex(env, ruleState, userDeclaredRunes, element))
      }
      case ManualSequencePT(range, elements) => ManualSequenceSR(evalRange(range), elements.map(translateTemplex(env, ruleState, userDeclaredRunes, _)))
    }
  }
}
