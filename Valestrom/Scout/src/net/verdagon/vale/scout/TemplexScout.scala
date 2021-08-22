package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules.IRulexSR

object TemplexScout {
  def translateMaybeTemplex(env: IEnvironment, maybeTemplexP: Option[ITemplexPT]): Option[IRulexSR] = {
    maybeTemplexP match {
      case None => None
      case Some(t) => Some(translateTemplex(env, t))
    }
  }

  def translateTemplex(env: IEnvironment, templexP: ITemplexPT): IRulexSR = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)

    templexP match {
      case NameOrRunePT(NameP(range, nameOrRune)) => {
        if (env.allUserDeclaredRunes().contains(CodeRuneS(nameOrRune))) {
          RuneSR(evalRange(range), CodeRuneS(nameOrRune))
        } else {
          NameSR(Scout.evalRange(env.file, range), CodeTypeNameS(nameOrRune))
        }
      }
      case InlinePT(range, inner) => {
        // Ignore the inl, not supported yet.
        translateTemplex(env, inner)
      }
      case ManualSequencePT(range,members) => ManualSequenceSR(evalRange(range), members.map(translateTemplex(env, _)))
      case IntPT(range,num) => IntSR(evalRange(range), num)
      case MutabilityPT(range,mutability) => MutabilitySR(evalRange(range), mutability)
      case VariabilityPT(range,variability) => VariabilitySR(evalRange(range), variability)
      case CallPT(range,template, args) => CallSR(evalRange(range), translateTemplex(env, template), args.map(arg => translateTemplex(env, arg)))
//      case NullablePT(range,inner) => NullableSR(evalRange(range), translateTemplex(env, inner))
      case InterpretedPT(range,ownership,permission,inner) => InterpretedSR(evalRange(range), ownership, permission, translateTemplex(env, inner))
      case RepeaterSequencePT(range, mutability, variability, size, element) => {
        RepeaterSequenceSR(
          evalRange(range),
          translateTemplex(env, mutability),
          translateTemplex(env, variability),
          translateTemplex(env, size),
          translateTemplex(env, element))
      }
    }
  }
}
