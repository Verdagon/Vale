package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.ImplA
import net.verdagon.vale.scout.RangeS
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.{Err, IProfiler, Ok, vassertSome, vfail, vimpl, vwat}

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
  (Option[InterfaceTT]) = {
    val ImplTemplata(env, impl) = implTemplata
    val ImplA(range, codeLocation, rules, runeToType, structKindRune, interfaceKindRune) = impl

    val result =
      profiler.childFrame("getMaybeImplementedInterface", () => {
        inferTemplar.solveComplete(
          env,
          temputs,
          rules,
          runeToType,
          RangeS.internal(-1875),
          Map(structKindRune.rune -> KindTemplata(childCitizenRef)))
      })

    result match {
      case Err(e) => {
        val _ = e
        (None)
      }
      case Ok(inferences) => {
        inferences(interfaceKindRune.rune) match {
          case KindTemplata(interfaceTT @ InterfaceTT(_)) => {
            (Some(interfaceTT))
          }
          case KindTemplata(sr @ StructTT(_)) => {
            throw CompileErrorExceptionT(CantImplStruct(range, sr))
          }
          case it @ InterfaceTemplata(_, _) => {
            val interfaceTT =
              delegate.getInterfaceRef(temputs, vimpl(), it, Vector.empty)
            (Some(interfaceTT))
          }
        }
      }
    }
  }

  def getParentInterfaces(
    temputs: Temputs,
    childCitizenRef: CitizenRefT):
  (Vector[InterfaceTT]) = {
    val needleImplName =
      NameTranslator.getImplNameForName(opts.useOptimization, childCitizenRef) match {
        case None => return Vector.empty
        case Some(x) => x
      }

    val citizenEnv =
      childCitizenRef match {
        case sr @ StructTT(_) => temputs.getEnvForStructRef(sr)
        case ir @ InterfaceTT(_) => temputs.getEnvForInterfaceRef(ir)
      }
    citizenEnv.lookupWithImpreciseName(profiler, needleImplName, Set(TemplataLookupContext, ExpressionLookupContext), false)
      .flatMap({
        case it @ ImplTemplata(_, _) => getMaybeImplementedInterface(temputs, childCitizenRef, it).toVector
        case ExternImplTemplata(structTT, interfaceTT) => if (structTT == childCitizenRef) Vector(interfaceTT) else Vector.empty
        case other => vwat(other.toString)
      })
      .toVector
  }

  def getAncestorInterfaces(
    temputs: Temputs,
    descendantCitizenRef: CitizenRefT):
  (Set[InterfaceTT]) = {
    profiler.childFrame("getAncestorInterfaces", () => {
      val ancestorInterfacesWithDistance =
        getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
      (ancestorInterfacesWithDistance.keySet)
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
  (Option[Int]) = {
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
  (Map[InterfaceTT, Int]) = {
    val parentInterfaceRefs =
      getParentInterfaces(temputs, descendantCitizenRef)

    // Make a map that contains all the parent interfaces, with distance 1
    val foundSoFar = parentInterfaceRefs.map((_, 1)).toMap

    getAncestorInterfacesInner(
      temputs,
      foundSoFar,
      1,
      parentInterfaceRefs.toSet)
  }

  private def getAncestorInterfacesInner(
    temputs: Temputs,
    // This is so we can know what we've already searched.
    nearestDistanceByInterfaceRef: Map[InterfaceTT, Int],
    // All the interfaces that are at most this distance away are inside foundSoFar.
    currentDistance: Int,
    // These are the interfaces that are *exactly* currentDistance away.
    // We will do our searching from here.
    interfacesAtCurrentDistance: Set[InterfaceTT]):
  (Map[InterfaceTT, Int]) = {
    val interfacesAtNextDistance =
      interfacesAtCurrentDistance.foldLeft((Set[InterfaceTT]()))({
        case ((previousAncestorInterfaceRefs), parentInterfaceRef) => {
          val parentAncestorInterfaceRefs =
            getParentInterfaces(temputs, parentInterfaceRef)
          (previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
        }
      })
    val nextDistance = currentDistance + 1

    // Discard the ones that have already been found; they're actually at
    // a closer distance.
    val newlyFoundInterfaces =
      interfacesAtNextDistance.diff(nearestDistanceByInterfaceRef.keySet)

    if (newlyFoundInterfaces.isEmpty) {
      (nearestDistanceByInterfaceRef)
    } else {
      // Combine the previously found ones with the newly found ones.
      val newNearestDistanceByInterfaceRef =
        nearestDistanceByInterfaceRef ++
          newlyFoundInterfaces.map((_, nextDistance)).toMap

      getAncestorInterfacesInner(
        temputs,
        newNearestDistanceByInterfaceRef,
        nextDistance,
        newlyFoundInterfaces)
    }
  }
}
