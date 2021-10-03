package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment, Environment => _, FunctionEnvironment => _, _}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RuleScout {
  // Returns:
  // - new rules produced on the side while translating the given rules
  // - the translated versions of the given rules
  def translateRulexes(
    env: IEnvironment,
    lidb: LocationInDenizenBuilder,
    builder: ArrayBuffer[IRulexSR],
    runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
    rulesP: Vector[IRulexPR]):
  Vector[RuneUsage] = {
    rulesP.map(translateRulex(env, lidb.child(), builder, runeToExplicitType, _))
  }

  def translateRulex(
    env: IEnvironment,
    lidb: LocationInDenizenBuilder,
    builder: ArrayBuffer[IRulexSR],
    runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
    rulex: IRulexPR):
  RuneUsage = {
    val evalRange = (range: Range) => Scout.evalRange(env.file, range)

    rulex match {
      //      case PackPR(elements) => {
      //        PackSR(translateRulexes(env, lidb.child(), elements))
      //      }
      case EqualsPR(range, leftP, rightP) => {
        val rune = ImplicitRuneS(lidb.child().consume())
        builder +=
          EqualsSR(
            evalRange(range),
            translateRulex(env, lidb.child(), builder, runeToExplicitType, leftP),
            translateRulex(env, lidb.child(), builder, runeToExplicitType, rightP))
        RuneUsage(evalRange(range), rune)
      }
      case OrPR(range, possibilitiesP) => {
        val rune = RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))

        val values =
          possibilitiesP
            .map({
              case TemplexPR(templex) => {
                TemplexScout.translateValueTemplex(templex) match {
                  case None => vfail("Or rules can only contain values for their possibilities.")
                  case Some(x) => x
                }
              }
              case _ => vfail("Or rules can only contain values for their possibilities.")
            })

        builder += OneOfSR(evalRange(range), rune, values.toArray)
        rune
      }
      case ComponentsPR(range, TypedPR(typeRange, maybeRune, tyype), componentsP) => {
        val rune =
          maybeRune match {
            case None => RuneUsage(evalRange(range), ImplicitRuneS(lidb.child().consume()))
            case Some(r) => RuneUsage(evalRange(r.range), CodeRuneS(r.str))
          }
        runeToExplicitType.put(rune.rune, translateType(tyype))
        tyype match {
          case CoordTypePR => {
            if (componentsP.size != 3) {
              vfail("Ref rule should have three components! Found: " + componentsP.size)
            }
            val Vector(ownershipRuneS, permissionRuneS, kindRuneS) =
              translateRulexes(env, lidb.child(), builder, runeToExplicitType, componentsP)
            builder +=
              CoordComponentsSR(
                Scout.evalRange(env.file, range),
                rune,
                ownershipRuneS,
                permissionRuneS,
                kindRuneS)
          }
          case KindTypePR => {
            if (componentsP.size != 3) {
              vfail("Kind rule should have one component! Found: " + componentsP.size)
            }
            val Vector(mutabilityRuneS) =
              translateRulexes(env, lidb.child(), builder, runeToExplicitType, componentsP)
            builder +=
              KindComponentsSR(
                Scout.evalRange(env.file, range),
                rune,
                mutabilityRuneS)
          }
          case PrototypeTypePR => {
            if (componentsP.size != 3) {
              vfail("Ref rule should have three components! Found: " + componentsP.size)
            }
            val Vector(nameRuneS, paramListRuneS, returnRuneS) =
              translateRulexes(env, lidb.child(), builder, runeToExplicitType, componentsP)
            builder +=
              PrototypeComponentsSR(
                Scout.evalRange(env.file, range),
                rune,
                nameRuneS,
                paramListRuneS,
                returnRuneS)
          }
          case _ => {
            vfail("Invalid type for compnents rule: " + tyype)
          }
        }
        rune
      }
      case TypedPR(range, None, tyype) => {
        val rune = ImplicitRuneS(lidb.child().consume())
        runeToExplicitType.put(rune, translateType(tyype))
        RuneUsage(evalRange(range), rune)
      }
      case TypedPR(range, Some(NameP(_, runeName)), tyype) => {
        val rune = CodeRuneS(runeName)
        runeToExplicitType.put(rune, translateType(tyype))
        RuneUsage(evalRange(range), rune)
      }
      case TemplexPR(templex) => TemplexScout.translateTemplex(env, lidb.child(), builder, templex)
      case BuiltinCallPR(range, NameP(_, "isInterface"), args) => {
        vassert(args.length == 1)
        val argRune = translateRulex(env, lidb.child(), builder, runeToExplicitType, args.head)

        val resultRune = ImplicitRuneS(lidb.child().consume())
        builder += IsInterfaceSR(evalRange(range), argRune)
        runeToExplicitType.put(resultRune, KindTemplataType)
        runeToExplicitType.put(argRune.rune, KindTemplataType)

        RuneUsage(evalRange(range), resultRune)
      }
    }
  }

  def translateType(tyype: ITypePR): ITemplataType = {
    tyype match {
      case PrototypeTypePR => PrototypeTemplataType
      case IntTypePR => IntegerTemplataType
      case BoolTypePR => BooleanTemplataType
      case OwnershipTypePR => OwnershipTemplataType
      case MutabilityTypePR => MutabilityTemplataType
      case PermissionTypePR => PermissionTemplataType
      case LocationTypePR => LocationTemplataType
      case CoordTypePR => CoordTemplataType
      case KindTypePR => KindTemplataType
      //      case StructTypePR => KindTypeSR
      //      case SequenceTypePR => KindTypeSR
      //      case ArrayTypePR => KindTypeSR
      //      case CallableTypePR => KindTypeSR
      //      case InterfaceTypePR => KindTypeSR
    }
  }

  def collectAllRunesNonDistinct(
    destination: mutable.ArrayBuffer[IRuneS],
    runeToExplicitType: mutable.HashMap[IRuneS, ITemplataType],
    rulex: IRulexPR):
  Unit = {
    rulex match {
      case EqualsPR(_, leftP, rightP) => {
        collectAllRunesNonDistinct(destination, runeToExplicitType, leftP)
        collectAllRunesNonDistinct(destination, runeToExplicitType, rightP)
      }
      case OrPR(_, possibilitiesP) =>
      case ComponentsPR(_, TypedPR(typeRange, maybeRune, tyype), componentsP) => {
          maybeRune match {
            case None =>
            case Some(NameP(_, runeName)) => {
              val rune = CodeRuneS(runeName)
              destination += rune
              runeToExplicitType.put(rune, translateType(tyype))
              componentsP.foreach(collectAllRunesNonDistinct(destination, runeToExplicitType, _))
            }
          }
      }
      case TypedPR(_, None, tyype) =>
      case TypedPR(_, Some(NameP(_, runeName)), tyype) => {
        val rune = CodeRuneS(runeName)
        destination += rune
        runeToExplicitType.put(rune, translateType(tyype))
      }
      case TemplexPR(innerPR) => // Do nothing, we can't declare runes inside templexes
    }
  }
}
