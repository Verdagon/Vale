package dev.vale.typing

//import dev.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
//import dev.vale.astronomer.VirtualFreeImpreciseNameS
import dev.vale.{Err, Interner, Keywords, Ok, RangeS, vassert, vassertSome, vcurious, vimpl}
import dev.vale.postparsing.IImpreciseNameS
import dev.vale.typing.ast.{InterfaceEdgeBlueprint, PrototypeT}
import dev.vale.typing.env.{IEnvironment, TemplatasStore}
import dev.vale.typing.types._
import dev.vale.postparsing.GlobalFunctionFamilyNameS
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.names.{FullNameT, ICitizenTemplateNameT, IInterfaceTemplateNameT, InterfaceTemplateNameT, StructTemplateNameT}
import dev.vale.typing.types._

sealed trait IMethod
case class NeededOverride(
  name: IImpreciseNameS,
  paramFilters: Vector[CoordT]
) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }
case class FoundFunction(prototype: PrototypeT) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

case class PartialEdgeT(
  struct: StructTT,
  interface: InterfaceTT,
  methods: Vector[IMethod]) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

class EdgeCompiler(
    interner: Interner,
    keywords: Keywords,
    overloadCompiler: OverloadResolver,
    implCompiler: ImplCompiler) {
  def compileITables(coutputs: CompilerOutputs):
  (
    Vector[InterfaceEdgeBlueprint],
    Map[
      FullNameT[IInterfaceTemplateNameT],
      Map[
        FullNameT[ICitizenTemplateNameT],
        Vector[PrototypeT]]]) = {
    val interfaceEdgeBlueprints =
      makeInterfaceEdgeBlueprints(coutputs)

    val itables =
      interfaceEdgeBlueprints.map(interfaceEdgeBlueprint => {
        val interfaceTemplateFullName = interfaceEdgeBlueprint.interface
        val interfaceDefinition = coutputs.lookupInterface(interfaceTemplateFullName)
        val interfacePlaceholderedCitizen = interfaceDefinition.placeholderedInterface
        val overridingImpls = coutputs.getChildImplsForSuperInterfaceTemplate(interfaceTemplateFullName)
        val overridingCitizenToFoundFunction =
          overridingImpls.map(overridingImpl => {
            val overridingCitizen = overridingImpl.subCitizenTemplateName

            val foundFunctions =
              interfaceEdgeBlueprint.superFamilyRootHeaders.map(abstractFunctionHeader => {
                val abstractFunctionTemplateFullName =
                  TemplataCompiler.getFunctionTemplate(abstractFunctionHeader.fullName)
//                val abstractFunctionSignature = abstractFunctionBanner.toSignature
                val abstractFunctionParamTypes = abstractFunctionHeader.paramTypes
                val abstractIndex = abstractFunctionHeader.params.indexWhere(_.virtuality.nonEmpty)
                vassert(abstractIndex >= 0)
                val abstractParamType = abstractFunctionParamTypes(abstractIndex)
                val abstractParamCitizen = abstractParamType.kind.expectInterface()
                val abstractFunctionOuterEnv =
                  coutputs.getOuterEnvForFunction(abstractFunctionTemplateFullName)
                val abstractFunctionInnerEnv =
                  coutputs.getInnerEnvForFunction(abstractFunctionTemplateFullName)

                // We don't use overridingCitizenDefinition.placeholderedName because that's placeholdered
                // according to itself, not placeholdered according to the interface that it overrode.
                // For example, if Firefly<X, Y> impl IShip<Y, X>, we want StructT(Firefly, (IShip:$_1, IShip:$_0))
                // not StructT(Firefly, (Firefly:$_0, Firefly:$_1)).
                // val overridingCitizenDefinition = coutputs.lookupCitizen(overridingCitizen)
                // So instead, we use overridingImpl.subCitizenFromPlaceholderedParentInterface.
                val overridingParamKind =
                    implCompiler.getImplDescendantGivenParent(coutputs, abstractFunctionHeader.maybeOriginFunction.map(_.range).toList, abstractFunctionOuterEnv, overridingImpl.templata, abstractParamCitizen, true, true) match {
                      case Ok(c) => c
                      case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(List(overridingImpl.templata.impl.range), e))
                    }
                val overridingParamCoord = abstractParamType.copy(kind = overridingParamKind)
                val overrideFunctionParamTypes =
                  abstractFunctionParamTypes.updated(abstractIndex, overridingParamCoord)

                val range = abstractFunctionHeader.maybeOriginFunction.map(_.range).getOrElse(RangeS.internal(interner, -2976395))

                val impreciseName =
                  vassertSome(
                    TemplatasStore.getImpreciseName(
                      interner, abstractFunctionHeader.fullName.last))

                // We need the abstract function's env because it contains knowledge of the existence
                // of certain things like concept functions, see NFIEFRO.

                val foundFunction =
                  resolveOverride(
                    coutputs, List(range), abstractFunctionInnerEnv, interfaceTemplateFullName, overridingCitizen, impreciseName, overrideFunctionParamTypes)

                foundFunction
              })
            overridingCitizen -> foundFunctions
          }).toMap
        interfaceTemplateFullName -> overridingCitizenToFoundFunction
      }).toMap
    (interfaceEdgeBlueprints, itables)
  }

  private def resolveOverride(
      coutputs: CompilerOutputs,
      range: List[RangeS],
      abstractFunctionEnv: IEnvironment,
      interface: FullNameT[IInterfaceTemplateNameT],
      overridingCitizen: FullNameT[ICitizenTemplateNameT],
      impreciseName: IImpreciseNameS,
      paramTypes: Vector[CoordT]):
  PrototypeT = {
    overloadCompiler.findFunction(
      // It's like the abstract function is the one calling the override.
      // This is important so the override can see existing concept functions, see NAFEWRO.
      abstractFunctionEnv,
      coutputs,
      range,
      impreciseName,
      Vector.empty,
      Array.empty,
      paramTypes,
      Vector(
        coutputs.getOuterEnvForType(interface),
        coutputs.getOuterEnvForType(overridingCitizen)),
      true,
      true) match {
      case Err(e) => throw CompileErrorExceptionT(CouldntFindOverrideT(range, e))
      case Ok(x) => x.prototype
    }
  }

  private def makeInterfaceEdgeBlueprints(coutputs: CompilerOutputs): Vector[InterfaceEdgeBlueprint] = {
    val abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties =
      coutputs.getAllFunctions().flatMap({ case function =>
        function.header.getAbstractInterface match {
          case None => Vector.empty
          case Some(abstractInterface) => Vector(abstractInterface -> function)
        }
      })
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .map({ case (interfaceTT, functions) =>
          val interfaceTemplateFullName =
            TemplataCompiler.getInterfaceTemplate(interfaceTT.fullName)
          // Sort so that the interface's internal methods are first and in the same order
          // they were declared in. It feels right, and vivem also depends on it
          // when it calls array generators/consumers' first method.
          val interfaceDef = coutputs.getAllInterfaces().find(_.templateName == interfaceTemplateFullName).get
          // Make sure `functions` has everything that the interface def wanted.
          vassert((interfaceDef.internalMethods.toSet -- functions.map(_.header).toSet).isEmpty)
          // Move all the internal methods to the front.
          val orderedMethods =
            interfaceDef.internalMethods ++
              functions.map(_.header).filter(x => {
                !interfaceDef.internalMethods.contains(x)//exists(y => y.toSignature == x.toSignature)
              })
          (interfaceTemplateFullName -> orderedMethods)
        })
    // Some interfaces would be empty and they wouldn't be in
    // abstractFunctionsByInterfaceWithoutEmpties, so we add them here.
    val abstractFunctionHeadersByInterfaceTemplateFullName =
      abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties ++
        coutputs.getAllInterfaces().map({ case i =>
          (i.templateName -> abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties.getOrElse(i.templateName, Set()))
        })

    val interfaceEdgeBlueprints =
      abstractFunctionHeadersByInterfaceTemplateFullName
        .map({ case (interfaceTemplateFullName, functionHeaders2) =>
          InterfaceEdgeBlueprint(
            interfaceTemplateFullName,
            // This is where they're given order and get an implied index
            functionHeaders2.toVector)
        })
    interfaceEdgeBlueprints.toVector
  }
}
