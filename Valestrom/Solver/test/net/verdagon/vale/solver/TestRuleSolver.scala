package net.verdagon.vale.solver

import net.verdagon.vale.{Collector, Err, Ok, Result, vassert, vassertOne, vassertSome, vfail, vimpl, vwat}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Map

object TestRuleSolver extends ISolveRule[IRule, Long, Unit, Unit, String, String] {

  def instantiateAncestorTemplate(descendants: Vector[String], ancestorTemplate: String): String = {
    // IRL, we may want to doublecheck that all descendants *can* instantiate as the ancestor template.
    val descendant = descendants.head
    (descendant, ancestorTemplate) match {
      case (x, y) if x == y => x
      case (x, y) if !x.contains(":") => y
      case ("Flamethrower:int", "IWeapon") => "IWeapon:int"
      case ("Rockets:int", "IWeapon") => "IWeapon:int"
      case other => vimpl(other)
    }
  }

  def getAncestors(descendant: String): Vector[String] = {
    getTemplate(descendant) match {
      case "Firefly" => Vector("Firefly", "ISpaceship")
      case "Serenity" => Vector("Serenity", "ISpaceship")
      case "ISpaceship" => Vector("ISpaceship")
      case "Flamethrower" => Vector("Flamethrower", "IWeapon")
      case "Rockets" => Vector("Rockets", "IWeapon")
      case "IWeapon" => Vector("IWeapon")
      case "int" => Vector("int")
      case other => vimpl(other)
    }
  }

  // Turns eg Flamethrower:int into Flamethrower. Firefly just stays Firefly.
  def getTemplate(tyype: String): String = {
    if (tyype.contains(":")) tyype.split(":")(0) else tyype
  }

  override def complexSolve(solverState: ISolverStateForRule[IRule, Long, String]):
  Result[(Array[Int], Map[Long, String]), String] = {
    val unsolvedRules = solverState.getUnsolvedRules()
    val receiverRunes = unsolvedRules.collect({ case Receive(receiverRune, _) => receiverRune })

    val newConclusions =
      receiverRunes.flatMap(receiver => {
        val receiveRules = unsolvedRules.collect({ case z @ Receive(r, _) if r == receiver => z })
        val callRules = unsolvedRules.collect({ case z @ Call(r, _, _) if r == receiver => z })
        val senderConclusions = receiveRules.map(_.senderRune).flatMap(solverState.getConclusion)
        val callTemplates = callRules.map(_.nameRune).flatMap(solverState.getConclusion)
        vassert(callTemplates.distinct.size <= 1)
        // If true, there are some senders/constraints we don't know yet, so lets be
        // careful to not assume between any possibilities below.
        val anyUnknownConstraints =
          (senderConclusions.size != receiveRules.size || callRules.size != callTemplates.size)
        solveReceives(senderConclusions, callTemplates, anyUnknownConstraints) match {
          case None => List()
          case Some(receiverInstantiation) => List(receiver -> receiverInstantiation)
        }
      }).toMap

    Ok((Array(), newConclusions))
  }

  override def solve(
    state: Unit,
    env: Unit,
    ruleIndex: Int,
    rule: IRule,
    solverState: ISolverStateForRule[IRule, Long, String]):
  Result[Map[Long, String], String] = {
    rule match {
      case Equals(leftRune, rightRune) => {
        solverState.getConclusion(leftRune) match {
          case Some(left) => Ok(Map(rightRune -> left))
          case None => Ok(Map(leftRune -> vassertSome(solverState.getConclusion(rightRune))))
        }
      }
      case Lookup(rune, name) => {
        val value = name
        Ok(Map(rune -> value))
      }
      case Literal(rune, literal) => {
        Ok(Map(rune -> literal))
      }
      case OneOf(rune, literals) => {
        val literal = solverState.getConclusion(rune).get
        if (!literals.contains(literal)) {
          return Err("conflict!")
        }
        Ok(Map())
      }
      case CoordComponents(coordRune, ownershipRune, kindRune) => {
        solverState.getConclusion(coordRune) match {
          case Some(combined) => {
            val Array(ownership, kind) = combined.split("/")
            Ok(Map(ownershipRune -> ownership, kindRune -> kind))
          }
          case None => {
            (solverState.getConclusion(ownershipRune), solverState.getConclusion(kindRune)) match {
              case (Some(ownership), Some(kind)) => {
                Ok(Map(coordRune -> (ownership + "/" + kind)))
              }
              case _ => vfail()
            }
          }
        }
      }
      case Pack(resultRune, memberRunes) => {
        solverState.getConclusion(resultRune) match {
          case Some(result) => {
            val parts = result.split(",")
            Ok(memberRunes.zip(parts).toMap)
          }
          case None => {
            val result = memberRunes.map(solverState.getConclusion).map(_.get).mkString(",")
            Ok(Map(resultRune -> result))
          }
        }
      }
      case Call(resultRune, nameRune, argRune) => {
        val maybeResult = solverState.getConclusion(resultRune)
        val maybeName = solverState.getConclusion(nameRune)
        val maybeArg = solverState.getConclusion(argRune)
        (maybeResult, maybeName, maybeArg) match {
          case (Some(result), Some(templateName), _) => {
            val prefix = templateName + ":"
            vassert(result.startsWith(prefix))
            Ok(Map(argRune -> result.slice(prefix.length, result.length)))
          }
          case (_, Some(templateName), Some(arg)) => Ok(Map(resultRune -> (templateName + ":" + arg)))
          case other => vwat(other)
        }
      }
      case Receive(receiverRune, senderRune) => {
        val receiver = vassertSome(solverState.getConclusion(receiverRune))
        if (receiver == "ISpaceship" || receiver == "IWeapon:int") {
          val ruleIndex =
            solverState.addRule(Implements(senderRune, receiverRune), Array(senderRune, receiverRune))
          solverState.addPuzzle(ruleIndex, Array(senderRune, receiverRune))
          Ok(Map())
        } else {
          // Not receiving into an interface, so sender must be the same
          Ok(Map(senderRune -> receiver))
        }
      }
      case Implements(subRune, superRune) => {
        val sub = vassertSome(solverState.getConclusion(subRune))
        val suuper = vassertSome(solverState.getConclusion(superRune))
        (sub, suuper) match {
          case (x, y) if x == y => Ok(Map())
          case ("Firefly", "ISpaceship") => Ok(Map())
          case ("Serenity", "ISpaceship") => Ok(Map())
          case ("Flamethrower:int", "IWeapon:int") => Ok(Map())
          case other => vimpl(other)
        }
      }
    }
  }

  private def solveReceives(
    senders: Vector[String],
    callTemplates: Vector[String],
    anyUnknownConstraints: Boolean) = {
    val senderTemplates = senders.map(getTemplate)
    // Theoretically possible, not gonna handle it for this test
    vassert(callTemplates.toSet.size <= 1)

    // For example [Flamethrower, Rockets] becomes [[Flamethrower, IWeapon, ISystem], [Rockets, IWeapon, ISystem]]
    val senderAncestorLists = senderTemplates.map(getAncestors)
    // Calculates the intersection of them all, eg [IWeapon, ISystem]
    val commonAncestors = senderAncestorLists.reduce(_.intersect(_)).toSet
    // Filter by any call templates. eg if there's a X = ISystem:Y call, then we're now [ISystem]
    val commonAncestorsCallConstrained =
      if (callTemplates.isEmpty) commonAncestors else commonAncestors.intersect(callTemplates.toSet)
    // If there are multiple, like [IWeapon, ISystem], get rid of any that are parents of others, now [IWeapon].
    val commonAncestorsNarrowed = narrow(commonAncestorsCallConstrained, anyUnknownConstraints)
    if (commonAncestorsNarrowed.isEmpty) {
      None
    } else {
      val ancestorTemplate = commonAncestorsNarrowed.head
      val ancestorInstantiation = instantiateAncestorTemplate(senders, ancestorTemplate)
      Some(ancestorInstantiation)
    }
  }

  def narrow(
    ancestorTemplateToInstantiationUnnarrowed: Set[String],
    anyUnknownConstraints: Boolean):
  Set[String] = {
    val ancestorTemplateToInstantiation =
      if (ancestorTemplateToInstantiationUnnarrowed.size > 1) {
        if (anyUnknownConstraints) {
          // Theres some unknown constraints (calls, receives, isa, etc)
          // so we can't yet conclude what the narrowest one is.
          vfail()
        } else {
          // Then choose the narrowest one.
          // For our particular test data sets, this shortcut should work.
          ancestorTemplateToInstantiationUnnarrowed - "ISpaceship" - "IWeapon"
        }
      } else {
        ancestorTemplateToInstantiationUnnarrowed
      }
    vassert(ancestorTemplateToInstantiation.size <= 1)
    ancestorTemplateToInstantiation
  }

}
