package dev.vale.typing

//import dev.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
//import dev.vale.astronomer.VirtualFreeImpreciseNameS
import dev.vale.{Err, Interner, Ok, RangeS, vassert, vassertSome, vcurious, vimpl}
import dev.vale.postparsing.IImpreciseNameS
import dev.vale.typing.ast.{InterfaceEdgeBlueprint, PrototypeT}
import dev.vale.typing.env.{IEnvironment, TemplatasStore}
import dev.vale.typing.types._
import dev.vale.postparsing.GlobalFunctionFamilyNameS
import dev.vale.typing.ast._
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
    overloadCompiler: OverloadResolver) {
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
              interfaceEdgeBlueprint.superFamilyRootBanners.map(abstractFunctionBanner => {
                val abstractFunctionTemplateFullName =
                  TemplataCompiler.getFunctionTemplate(abstractFunctionBanner.fullName)
                val abstractFunctionEnv =
                  coutputs.getEnvForTemplate(abstractFunctionTemplateFullName)
                val abstractFunctionSignature = abstractFunctionBanner.toSignature
                val abstractFunctionParamTypes = abstractFunctionSignature.paramTypes
                val abstractIndex = abstractFunctionBanner.params.indexWhere(_.virtuality.nonEmpty)
                vassert(abstractIndex >= 0)
                val abstractParamType = abstractFunctionParamTypes(abstractIndex)
                val abstractParamCitizen = abstractParamType.kind.expectCitizen()
                // We don't use overridingCitizenDefinition.placeholderedName because that's placeholdered
                // according to itself, not placeholdered according to the interface that it overrode.
                // For example, if Firefly<X, Y> impl IShip<Y, X>, we want StructT(Firefly, (IShip:$_1, IShip:$_0))
                // not StructT(Firefly, (Firefly:$_0, Firefly:$_1)).
                // val overridingCitizenDefinition = coutputs.lookupCitizen(overridingCitizen)
                // So instead, we use overridingImpl.subCitizenFromPlaceholderedParentInterface.
                val overridingParamType =
                    abstractParamType
                      .copy(kind = overridingImpl.subCitizenFromPlaceholderedParentInterface)
                val overrideFunctionParamTypes =
                  abstractFunctionParamTypes.updated(abstractIndex, overridingParamType)

                val range = abstractFunctionBanner.originFunction.map(_.range).getOrElse(RangeS.internal(interner, -2976395))

                val impreciseName =
                  vassertSome(
                    TemplatasStore.getImpreciseName(
                      interner, abstractFunctionSignature.fullName.last))

                val foundFunction =
                  resolveOverride(
                    coutputs, range, abstractFunctionEnv, interfaceTemplateFullName, overridingCitizen, impreciseName, overrideFunctionParamTypes)

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
      range: RangeS,
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
        coutputs.getEnvForTemplate(interface),
        coutputs.getEnvForTemplate(overridingCitizen)),
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
          vassert((interfaceDef.internalMethods.map(_.toSignature).toSet -- functions.map(_.header.toSignature).toSet).isEmpty)
          // Move all the internal methods to the front.
          val orderedMethods =
            interfaceDef.internalMethods ++
              functions.map(_.header).filter(x => {
                !interfaceDef.internalMethods.exists(y => y.toSignature == x.toSignature)
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
            functionHeaders2.map(_.toBanner).toVector)
        })
    interfaceEdgeBlueprints.toVector
  }
}
