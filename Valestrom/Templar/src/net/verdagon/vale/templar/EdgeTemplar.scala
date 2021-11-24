package net.verdagon.vale.templar

//import net.verdagon.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
import net.verdagon.vale.astronomer.VirtualFreeImpreciseNameS
import net.verdagon.vale.scout.{CodeNameS, CodeVarNameS, GlobalFunctionFamilyNameS, IImpreciseNameS, INameS}
import net.verdagon.vale.templar.ast.{FunctionT, ImplT, InterfaceEdgeBlueprint, OverrideT, PrototypeT}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.names.{FreeNameT, FunctionNameT, VirtualFreeNameT}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail, vimpl, vwat}

object EdgeTemplar {
  sealed trait IMethod
  case class NeededOverride(
    name: IImpreciseNameS,
    paramFilters: Vector[ParamFilter]
  ) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
  case class FoundFunction(prototype: PrototypeT) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

  case class PartialEdgeT(
    struct: StructTT,
    interface: InterfaceTT,
    methods: Vector[IMethod]) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

  def assemblePartialEdges(temputs: Temputs): Vector[PartialEdgeT] = {
    val interfaceEdgeBlueprints = makeInterfaceEdgeBlueprints(temputs)

    val overrideFunctionsAndIndicesByStructAndInterface = doBlah(temputs, interfaceEdgeBlueprints)

    val partialEdges2 =
      overrideFunctionsAndIndicesByStructAndInterface
        .map({ case ((struct, superInterface), overrideFunctionsAndIndices) =>
          val blueprint = interfaceEdgeBlueprints.find(_.interface == superInterface).get
          val overrideFunctionsByIndex =
            overrideFunctionsAndIndices.groupBy(_._2).mapValues(_.map(_._1).toVector)
          // overrideIndex is the index in the itable
          val methods =
            blueprint.superFamilyRootBanners.zipWithIndex.map({ case (superFunction, overrideIndex) =>
              overrideFunctionsByIndex.get(overrideIndex) match {
                case None => {
                  val overrideParamFilters =
                    superFunction.paramTypes.zipWithIndex.map({
                      case (CoordT(ownership, permission, _), index) if index == superFunction.getVirtualIndex.get => {
                        ParamFilter(CoordT(ownership, permission, struct), Some(OverrideT(superInterface)))
                      }
                      case (tyype, _) => ParamFilter(tyype, None)
                    })
                  superFunction.fullName.last match {
                    case FunctionNameT(humanName, _, _) => NeededOverride(CodeNameS(humanName), overrideParamFilters)
                    case VirtualFreeNameT(_, _) => NeededOverride(VirtualFreeImpreciseNameS(), overrideParamFilters)
//                    case DropNameT(_, _) => NeededOverride(CodeVarNameS(CallTemplar.VIRTUAL_DROP_FUNCTION_NAME), overrideParamFilters)
                    case other => vwat(other)
                  }
                }
                case Some(Vector()) => vfail("wot")
                case Some(Vector(onlyOverride)) => FoundFunction(onlyOverride.header.toPrototype)
                case Some(multipleOverrides) => {
                  vfail("Multiple overrides for struct " + struct + " for interface " + superInterface + ": " + multipleOverrides.map(_.header.toSignature).mkString(", "))
                }
              }
            })
          PartialEdgeT(struct, superInterface, methods)
        })

    partialEdges2.toVector
  }


  def doBlah(
    temputs: Temputs,
    interfaceEdgeBlueprints: Vector[InterfaceEdgeBlueprint]
  ): Map[(StructTT, InterfaceTT), Vector[(FunctionT, Int)]] = {
    // This makes an empty entry for all impls. Most of these will be overwritten below.
    // Without this, if an impl had no functions in it, it wouldn't be included in the result.
    temputs.getAllImpls().map({ case ImplT(struct, interface) => (struct, interface) -> Vector() }).toMap ++
    (temputs.getAllFunctions().toVector.flatMap({ case overrideFunction =>
      overrideFunction.header.getOverride match {
        case None => Vector.empty
        case Some((struct, superInterface)) => {
          // Make sure that the struct actually overrides that function
          if (!temputs.getAllImpls().exists(impl => impl.struct == struct && impl.interface == superInterface)) {
            vfail("Struct " + struct + " doesn't extend " + superInterface + "!")
          }

          val virtualIndex = overrideFunction.header.getVirtualIndex.get
          vassert(virtualIndex >= 0)
          val overrideFunctionParamTypes =
            overrideFunction.header.params.map(_.tyype)
          val needleSuperFunctionParamTypes =
            overrideFunctionParamTypes.zipWithIndex.map({ case (paramType, index) =>
              if (index != virtualIndex) {
                paramType
              } else {
                paramType.copy(kind = superInterface)
              }
            })

          // Make sure that there's an abstract function in that edge that this
          // overrides
          val edgeBlueprint =
          interfaceEdgeBlueprints.find(_.interface == superInterface) match {
            case None => {
              // every interface should have a blueprint...
              vfail("wot")
            }
            case Some(ieb) => ieb
          }
          val matchesAndIndices =
            edgeBlueprint.superFamilyRootBanners.zipWithIndex
              .filter({ case (possibleSuperFunction, index) =>
                val namesMatch =
                  (possibleSuperFunction.fullName.last, overrideFunction.header.fullName.last) match {
                    case (FunctionNameT(possibleSuperFunctionHumanName, _, _), FunctionNameT(overrideFunctionHumanName, _, _)) => {
                      possibleSuperFunctionHumanName == overrideFunctionHumanName
                    }
                    case (FunctionNameT(_, _, _), _) => false
                    case (_, FunctionNameT(_, _, _)) => false
                    case (VirtualFreeNameT(_, _), VirtualFreeNameT(_, _)) => true
                    case (VirtualFreeNameT(_, _), _) => false
                    case (_, VirtualFreeNameT(_, _)) => false
                    //                    case (ImmInterfaceDestructorNameT(_, _), ImmInterfaceDestructorNameT(_, _)) => true
//                    case (DropNameT(_, possibleSuperFunctionCoord), FunctionNameT(humanName, _, parameters)) => {
//                      humanName == CallTemplar.DROP_FUNCTION_NAME && parameters.size == 1 && possibleSuperFunctionCoord == parameters.head
//                    }
                    case other => vimpl(other)
//                    case _ => false
                  }
                namesMatch && possibleSuperFunction.paramTypes == needleSuperFunctionParamTypes
              })
          matchesAndIndices match {
            case Vector() => {
              vfail("Function " + overrideFunction.header.toSignature + " doesn't override anything in " + superInterface)
            }
            case Vector((_, indexInEdge)) => {
              Vector((struct, superInterface) -> (overrideFunction, indexInEdge))
            }
            case _ => {
              vfail("Function " + overrideFunction.header.toSignature + " overrides multiple things in " + superInterface + ": " + matchesAndIndices.map(_._1.toSignature).mkString(", "))
            }
          }
        }
      }
    })
      .groupBy(_._1)
      .mapValues(_.map(_._2)))
  }

  def makeInterfaceEdgeBlueprints(temputs: Temputs): Vector[InterfaceEdgeBlueprint] = {
    val abstractFunctionHeadersByInterfaceWithoutEmpties =
      temputs.getAllFunctions().flatMap({ case function =>
        function.header.getAbstractInterface match {
          case None => Vector.empty
          case Some(abstractInterface) => Vector(abstractInterface -> function)
        }
      })
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .map({ case (interfaceTT, functions) =>
          // Sort so that the interface's internal methods are first and in the same order
          // they were declared in. It feels right, and vivem also depends on it
          // when it calls array generators/consumers' first method.
          val interfaceDef = temputs.getAllInterfaces().find(_.getRef == interfaceTT).get
          // Make sure `functions` has everything that the interface def wanted.
          vassert((interfaceDef.internalMethods.toSet -- functions.map(_.header).toSet).isEmpty)
          // Move all the internal methods to the front.
          val orderedMethods =
            interfaceDef.internalMethods ++
              functions.map(_.header).filter(!interfaceDef.internalMethods.contains(_))
          (interfaceTT -> orderedMethods)
        })
    // Some interfaces would be empty and they wouldn't be in
    // abstractFunctionsByInterfaceWithoutEmpties, so we add them here.
    val abstractFunctionHeadersByInterface =
    abstractFunctionHeadersByInterfaceWithoutEmpties ++
      temputs.getAllInterfaces().map({ case i =>
        (i.getRef -> abstractFunctionHeadersByInterfaceWithoutEmpties.getOrElse(i.getRef, Set()))
      })

    val interfaceEdgeBlueprints =
      abstractFunctionHeadersByInterface
        .map({ case (interfaceTT, functionHeaders2) =>
          ast.InterfaceEdgeBlueprint(
            interfaceTT,
            // This is where they're given order and get an implied index
            functionHeaders2.map(_.toBanner).toVector)
        })
    interfaceEdgeBlueprints.toVector
  }
}
