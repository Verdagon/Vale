package net.verdagon.vale.scout

import net.verdagon.vale.{FileCoordinateMap, vimpl}
import net.verdagon.vale.SourceCodeUtils.{humanizePos, lineContaining, nextThingAndRestOfLine}
import net.verdagon.vale.scout.rules.{CoordComponentsSR, IRulexSR, IsInterfaceSR, IsStructSR, KindComponentsSR, OneOfSR}
import net.verdagon.vale.templar.types._

object ScoutErrorHumanizer {
  def humanize(
    codeMap: FileCoordinateMap[String],
    err: ICompileErrorS):
  String = {
    val errorStrBody =
      (err match {
        case RangedInternalErrorS(range, message) => " " + message
        case CouldntFindVarToMutateS(range, name) => s": No variable named ${name}. Try declaring it above, like `${name} = 42;`\n"
        case CantOwnershipInterfaceInImpl(range) => s": Can only impl a plain interface, remove symbol."
        case CantOwnershipStructInImpl(range) => s": Only a plain struct/interface can be in an impl, remove symbol."
        case CantOverrideOwnershipped(range) => s": Can only impl a plain interface, remove symbol."
        case VariableNameAlreadyExists(range, name) => s": Local named " + humanizeName(name) + " already exists!\n(If you meant to modify the variable, use the `set` keyword beforehand.)"
        case InterfaceMethodNeedsSelf(range) => s": Interface's method needs a virtual param of interface's type!"
        case LightFunctionMustHaveParamTypes(range, paramIndex) => s": Function parameter must have a type!"
        case ForgotSetKeywordError(range) => s": Changing a struct's member must start with the `set` keyword."
        case CantUseThatLocalName(range, name) => s": Can't use the name ${name} for a local."
        case ExternHasBody(range) => s": Extern function can't have a body too."
        case CantInitializeIndividualElementsOfRuntimeSizedArray(range) => s": Can't initialize individual elements of a runtime-sized array."
        case InitializingRuntimeSizedArrayRequiresSizeAndCallable(range) => s": Initializing a runtime-sized array requires two arguments: a size, and a function that will populate the elements."
        case InitializingStaticSizedArrayRequiresSizeAndCallable(range) => s": Initializing a statically-sized array requires one argument: a function that will populate the elements."
        case InitializingStaticSizedArrayFromCallableNeedsSizeTemplex(range) => s": Initializing a statically-sized array requires a size in-between the square brackets."
      })

    val posStr = humanizePos(codeMap, err.range.begin)
    val nextStuff = lineContaining(codeMap, err.range.begin)
    val errorId = "S"
    f"${posStr} error ${errorId}: ${errorStrBody}\n${nextStuff}\n"
  }

  def humanizeName(name: INameS): String = {
    name match {
//      case UnnamedLocalNameS(codeLocation) => "(unnamed)"
      case ClosureParamNameS() => "(closure)"
      case CodeTypeNameS(n) => n
      case MagicParamNameS(codeLocation) => "(magic)"
      case CodeVarNameS(name) => name
      case RuneNameS(rune) => humanizeRune(rune)
      case ConstructingMemberNameS(name) => "member " + name
      case FunctionNameS(name, codeLocation) => name
    }
  }

  def humanizeRune(rune: IRuneS): String = {
    rune match {
      case ImplicitRuneS(lid) => "_" + lid.path.mkString("")
      case CodeRuneS(name) => name
      case SenderRuneS(paramRune) => "(arg for " + humanizeRune(paramRune) + ")"
      case other => vimpl(other)
    }
  }

  def humanizeTemplataType(tyype: ITemplataType): String = {
    tyype match {
      case KindTemplataType => "kind"
      case CoordTemplataType => "type"
      case FunctionTemplataType => "func"
      case IntegerTemplataType => "int"
      case BooleanTemplataType => "bool"
      case MutabilityTemplataType => "mut"
      case PrototypeTemplataType => "prot"
      case StringTemplataType => "str"
      case PermissionTemplataType => "perm"
      case LocationTemplataType => "loc"
      case OwnershipTemplataType => "own"
      case VariabilityTemplataType => "var"
      case PackTemplataType(elementType) => "pack<" + humanizeTemplataType(elementType) + ">"
      case TemplateTemplataType(params, ret) => humanizeTemplataType(ret) + "<" + params.map(humanizeTemplataType).mkString(",") + ">"
    }
  }

  def humanizeRule(rule: IRulexSR): String = {
    rule match {
      case KindComponentsSR(range, kindRune, mutabilityRune) => {
        humanizeRune(kindRune.rune) + " Kind(" + humanizeRune(mutabilityRune.rune) + ")"
      }
      case CoordComponentsSR(range, resultRune, ownershipRune, permissionRune, kindRune) => {
        humanizeRune(resultRune.rune) + " Ref(" + humanizeRune(ownershipRune.rune) + ", " + humanizeRune(permissionRune.rune) + ", " + humanizeRune(kindRune.rune) + ")"
      }
      case OneOfSR(range, resultRune, literals) => {
        humanizeRune(resultRune.rune) + " = " + literals.map(_.toString).mkString(" | ")
      }
      case IsInterfaceSR(range, resultRune) => "isInterface(" + humanizeRune(resultRune.rune) + ")"
      case IsStructSR(range, resultRune) => "isStruct(" + humanizeRune(resultRune.rune) + ")"
      case other => vimpl(other)
    }
  }
}
