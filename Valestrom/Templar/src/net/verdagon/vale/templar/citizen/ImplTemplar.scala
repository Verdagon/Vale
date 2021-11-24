package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.{ImplA, ImplImpreciseNameS}
import net.verdagon.vale.scout.rules.Equivalencies
import net.verdagon.vale.solver.SolverErrorHumanizer
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.names.NameTranslator
import net.verdagon.vale.{Err, IProfiler, Ok, RangeS, vassertSome, vfail, vimpl, vwat}

import scala.collection.immutable.List

trait IAncestorHelperDelegate {
  def getInterfaceRef(
    temputs: Temputs,
    callRange: RangeS,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: Vector[ITemplata]):
  InterfaceTT
}

class AncestorHelper(
    opts: TemplarOptions,
    profiler: IProfiler,
    inferTemplar: InferTemplar,
    delegate: IAncestorHelperDelegate) {

  private def getMaybeImplementedInterface(
    temputs: Temputs,
    childCitizenRef: CitizenRefT,
    implTemplata: ImplTemplata):
  (Option[(InterfaceTT, ImplTemplata)]) = {
    val ImplTemplata(env, impl) = implTemplata
    val ImplA(range, name, impreciseName, identifyingRunes, rules, runeToType, structKindRune, interfaceKindRune) = impl

    val result =
      profiler.childFrame("getMaybeImplementedInterface", () => {
        inferTemplar.solveComplete(
          env,
          temputs,
          rules,
          runeToType,
          RangeS.internal(-1875),
          Vector(InitialKnown(structKindRune, KindTemplata(childCitizenRef))),
          Vector())
      })

    result match {
      case Err(e) => {
        val _ = e
        (None)
      }
      case Ok(inferences) => {
        inferences(interfaceKindRune.rune) match {
          case KindTemplata(interfaceTT @ InterfaceTT(_)) => {
            (Some((interfaceTT, implTemplata)))
          }
          case KindTemplata(sr @ StructTT(_)) => {
            throw CompileErrorExceptionT(CantImplStruct(range, sr))
          }
          case it @ InterfaceTemplata(_, _) => {
            val interfaceTT =
              delegate.getInterfaceRef(temputs, RangeS.internal(-1875), it, Vector.empty)
            (Some((interfaceTT, implTemplata)))
          }
        }
      }
    }
  }

  def getMatchingImpls(
    temputs: Temputs,
    childCitizenRef: CitizenRefT):
  (Vector[ImplTemplata]) = {

    // See INSHN, the imprecise name for an impl is the wrapped imprecise name of its struct template.
    val needleImplName =
      TemplatasStore.getImpreciseName(childCitizenRef.fullName.last) match {
        case None => return Vector.empty
        case Some(x) => ImplImpreciseNameS(x)
      }
    val citizenEnv =
      childCitizenRef match {
        case sr @ StructTT(_) => temputs.getEnvForKind(sr)
        case ir @ InterfaceTT(_) => temputs.getEnvForKind(ir)
      }
    citizenEnv.lookupWithImpreciseName(profiler, needleImplName, Set(TemplataLookupContext, ExpressionLookupContext), false)
      .map({
        case it @ ImplTemplata(_, _) => it
        //        case ExternImplTemplata(structTT, interfaceTT) => if (structTT == childCitizenRef) Vector(interfaceTT) else Vector.empty
        case other => vwat(other.toString)
      })
      .toVector
  }

  def getParentInterfaces(
    temputs: Temputs,
    childCitizenRef: CitizenRefT):
  (Vector[(InterfaceTT, ImplTemplata)]) = {
    getMatchingImpls(temputs, childCitizenRef).flatMap({
      case it @ ImplTemplata(_, _) => getMaybeImplementedInterface(temputs, childCitizenRef, it)
      case other => vwat(other.toString)
    })
  }

  def getAncestorInterfaces(
    temputs: Temputs,
    descendantCitizenRef: CitizenRefT):
  (Map[InterfaceTT, ImplTemplata]) = {
    profiler.childFrame("getAncestorInterfaces", () => {
      val ancestorInterfacesWithDistance =
        getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
      (ancestorInterfacesWithDistance.mapValues(_._1))
    })
  }

  def isAncestor(
    temputs: Temputs,
    descendantCitizenRef: CitizenRefT,
    ancestorInterfaceRef: InterfaceTT):
  (Boolean) = {
    profiler.childFrame("isAncestor", () => {
      val ancestorInterfacesWithDistance =
        getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
      (ancestorInterfacesWithDistance.contains(ancestorInterfaceRef))
    })
  }

  def getAncestorInterfaceDistance(
    temputs: Temputs,
    descendantCitizenRef: CitizenRefT,
    ancestorInterfaceRef: InterfaceTT):
  (Option[(ImplTemplata, Int)]) = {
    profiler.childFrame("getAncestorInterfaceDistance", () => {
      val ancestorInterfacesWithDistance =
        getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
      (ancestorInterfacesWithDistance.get(ancestorInterfaceRef))
    })
  }

  // Doesn't include self
  def getAncestorInterfacesWithDistance(
    temputs: Temputs,
    descendantCitizenRef: CitizenRefT):
  (Map[InterfaceTT, (ImplTemplata, Int)]) = {
    val parentInterfacesAndImpls =
      getParentInterfaces(temputs, descendantCitizenRef)

    // Make a map that contains all the parent interfaces, with distance 1
    val foundSoFar =
      parentInterfacesAndImpls.map({ case (interfaceRef, impl) => (interfaceRef, (impl, 1)) }).toMap

    getAncestorInterfacesInner(
      temputs,
      foundSoFar,
      1,
      parentInterfacesAndImpls.toMap)
  }

  private def getAncestorInterfacesInner(
    temputs: Temputs,
    // This is so we can know what we've already searched.
    nearestDistanceByInterfaceRef: Map[InterfaceTT, (ImplTemplata, Int)],
    // All the interfaces that are at most this distance away are inside foundSoFar.
    currentDistance: Int,
    // These are the interfaces that are *exactly* currentDistance away.
    // We will do our searching from here.
    interfacesAtCurrentDistance: Map[InterfaceTT, ImplTemplata]):
  (Map[InterfaceTT, (ImplTemplata, Int)]) = {
    val interfacesAtNextDistance =
      interfacesAtCurrentDistance.foldLeft((Map[InterfaceTT, ImplTemplata]()))({
        case ((previousAncestorInterfaceRefs), (parentInterfaceRef, parentImpl)) => {
          val parentAncestorInterfaceRefs =
            getParentInterfaces(temputs, parentInterfaceRef)
          (previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
        }
      })
    val nextDistance = currentDistance + 1

    // Discard the ones that have already been found; they're actually at
    // a closer distance.
    val newlyFoundInterfaces =
      interfacesAtNextDistance.keySet
        .diff(nearestDistanceByInterfaceRef.keySet)
        .toVector
        .map(key => (key -> interfacesAtNextDistance(key)))
        .toMap

    if (newlyFoundInterfaces.isEmpty) {
      (nearestDistanceByInterfaceRef)
    } else {
      // Combine the previously found ones with the newly found ones.
      val newNearestDistanceByInterfaceRef =
        nearestDistanceByInterfaceRef ++
          newlyFoundInterfaces.mapValues((_, nextDistance)).toMap

      getAncestorInterfacesInner(
        temputs,
        newNearestDistanceByInterfaceRef,
        nextDistance,
        newlyFoundInterfaces)
    }
  }
}
