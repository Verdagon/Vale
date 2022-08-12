package dev.vale.typing

//import dev.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
//import dev.vale.astronomer.VirtualFreeImpreciseNameS
import dev.vale.{Err, Interner, Keywords, Ok, RangeS, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing.{GlobalFunctionFamilyNameS, IImpreciseNameS, RuneNameS}
import dev.vale.typing.ast.{InterfaceEdgeBlueprint, PrototypeT}
import dev.vale.typing.env.{IEnvironment, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess}
import dev.vale.typing.names.{FullNameT, ICitizenTemplateNameT, IInterfaceTemplateNameT, InterfaceTemplateNameT, StructTemplateNameT}
import dev.vale.typing.templata.{FunctionTemplata, KindTemplata}
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
    functionCompiler: FunctionCompiler,
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
//        val interfacePlaceholderedCitizen = interfaceDefinition.placeholderedInterface
        val overridingImpls = coutputs.getChildImplsForSuperInterfaceTemplate(interfaceTemplateFullName)
        val overridingCitizenToFoundFunction =
          overridingImpls.map(overridingImpl => {
            val overridingCitizen = overridingImpl.subCitizenTemplateName
            val superInterfaceWithSubCitizenPlaceholders = overridingImpl.parentInterfaceFromPlaceholderedSubCitizen
//            val substituter = TemplataCompiler.getPlaceholderSubstituter(interner, superInterfaceWithSubCitizenPlaceholders.fullName)

            // Given:
            //   impl<T> IObserver<T, int> for MyThing<T>
            // Start by compiling the impl, supplying any placeholders.
            //   MyThing<$0> implements IObserver<$0, int>
            // Now, take the abstract function:
            //   abstract func myFunc<X, Y>(self IObserver<X, Y>, bork Y);
            // and try to resolve an overload with the placeholdered struct and the rest of the param types.
            // IOW, resolve: myFunc(MyThing<$0>, int)
            // It should find the override func:
            //   func myFunc<T>(self MyThing<T>, bork int)
            // (and also conclude that its T = $0)
            // We also know the parent interface (IObserver<$0, int>).
            // Now we can add that to the impl's edge.


            // We have interface:
            //   interface IObserver<X, Y>
            // And struct:
            //   struct MyThing<T>
            // and impl:
            //   MyThing<impl$0> implements IObserver<impl$0, int>
            // we have transformers now:
            //   impl's call for sub citizen:
            //     MyThing$0 -> impl$0
            //   impl's call for super interface:
            //     IObserver$0 -> impl$0
            //     IObserver$1 -> int
            //
            // We have function:
            //   abstract func myFunc<X, Y>(self IObserver<X, Y>, bork Y);
            // And its transformers:
            //   its name:
            //     myFunc<myFunc$0, myFunc$1>
            //   myFunc's call for interface:
            //     IObserver$0 -> myFunc$0
            //     IObserver$1 -> myFunc$1
            //
            // We need:
            //   myFunc(MyThing<???>, int>
            //
            // need to substitute myFunc$1 -> int
            //
            // we need myFunc$1 -> IObserver$1, then do IObserver$1 -> int
            //
            // could we run the function's solver again?
            // perhaps not supply the other args, just this one?
            // supposedly thats what the templar is doing anyway...
            // no, children have overloaded funcs with same name.

            val foundFunctions =
              interfaceEdgeBlueprint.superFamilyRootHeaders.map(abstractFunctionHeader => {
                val abstractFunctionTemplateFullName =
                  TemplataCompiler.getFunctionTemplate(abstractFunctionHeader.fullName)
//                val abstractFunctionSignature = abstractFunctionBanner.toSignature
//                val abstractFunctionParamUnsubstitutedTypes = abstractFunctionHeader.paramTypes
//                val abstractIndex = abstractFunctionHeader.params.indexWhere(_.virtuality.nonEmpty)
//                vassert(abstractIndex >= 0)
//                val abstractParamUnsubstitutedType = abstractFunctionParamUnsubstitutedTypes(abstractIndex)
//                val abstractParamUnsubstitutedCitizen = abstractParamUnsubstitutedType.kind.expectInterface()
//                val otherSubstituter = TemplataCompiler.getPlaceholderSubstituter(interner, abstractParamCitizen.fullName)
//                val abstractFunctionOuterEnv =
//                  coutputs.getOuterEnvForFunction(abstractFunctionTemplateFullName)
//                val abstractFunctionInnerEnv =
//                  coutputs.getInnerEnvForFunction(abstractFunctionTemplateFullName)
//                val subCitizenEnv = coutputs.getOuterEnvForType(overridingCitizen)

//                val citizenDef = coutputs.lookupCitizen(overridingCitizen)
//                val placeholderedCitizen = citizenDef.placeholderedCitizen

//                val abstractParamType =
//                  abstractParamUnsubstitutedType.copy(kind = superInterfaceWithSubCitizenPlaceholders)

                // run the abstract function's rules with the placeholdered struct?

                val range = abstractFunctionHeader.maybeOriginFunctionTemplata.map(_.function.range).getOrElse(RangeS.internal(interner, -2976395))

                val originFunctionTemplata = vassertSome(abstractFunctionHeader.maybeOriginFunctionTemplata)

                val (abstractFuncPrototype, abstractFuncEnv) =
                  functionCompiler.evaluateGenericLightFunctionParentForPrototype(
                    coutputs,
                    List(range),
                    overridingImpl.implOuterEnv,
                    originFunctionTemplata,
                    overridingImpl.templata) match {
                    case EvaluateFunctionFailure(x) => {
                      throw CompileErrorExceptionT(CouldntEvaluateFunction(List(range), x))
                    }
                    case EvaluateFunctionSuccess(x) => x
                  }

                val superFunctionParamTypes = abstractFuncPrototype.paramTypes

                val abstractFunctionParamTypes = abstractFunctionHeader.paramTypes
                val abstractIndex = abstractFunctionHeader.params.indexWhere(_.virtuality.nonEmpty)
                vassert(abstractIndex >= 0)
                val abstractParamType = abstractFunctionParamTypes(abstractIndex)

                val overridingParamKind =
                  abstractFuncEnv.lookupNearestWithImpreciseName(
                      interner.intern(RuneNameS(overridingImpl.templata.impl.subCitizenRune.rune)),
                      Set(TemplataLookupContext)) match {
                    case None => vwat()
                    case Some(KindTemplata(kind)) => kind
                    case Some(other) => vfail(other)
                  }

                val overridingParamCoord = abstractParamType.copy(kind = overridingParamKind)
                val overrideFunctionParamTypes =
                  superFunctionParamTypes.updated(abstractIndex, overridingParamCoord)

                val impreciseName =
                  vassertSome(
                    TemplatasStore.getImpreciseName(
                      interner, abstractFunctionHeader.fullName.last))


                // // We don't use overridingCitizenDefinition.placeholderedName because that's placeholdered
                // // according to itself, not placeholdered according to the interface that it overrode.
                // // For example, if Firefly<X, Y> impl IShip<Y, X>, we want StructT(Firefly, (IShip:$_1, IShip:$_0))
                // // not StructT(Firefly, (Firefly:$_0, Firefly:$_1)).
                // // val overridingCitizenDefinition = coutputs.lookupCitizen(overridingCitizen)
                // // So instead, we use overridingImpl.subCitizenFromPlaceholderedParentInterface.
                // val overridingParamKind =
                //     implCompiler.getImplDescendantGivenParent(coutputs, abstractFunctionHeader.maybeOriginFunction.map(_.range).toList, abstractFunctionOuterEnv, overridingImpl.templata, abstractParamCitizen, true, false) match {
                //       case Ok(c) => c
                //       case Err(e) => throw CompileErrorExceptionT(CouldntEvaluatImpl(List(overridingImpl.templata.impl.range), e))
                //     }

                // We need the abstract function's env because it contains knowledge of the existence
                // of certain things like concept functions, see NFIEFRO.
                val foundFunction =
                  resolveOverride(
                    coutputs, List(range), abstractFuncEnv, interfaceTemplateFullName, overridingCitizen, impreciseName, overrideFunctionParamTypes)

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
      abstractFuncEnv: IEnvironment,
      interface: FullNameT[IInterfaceTemplateNameT],
      overridingCitizen: FullNameT[ICitizenTemplateNameT],
      impreciseName: IImpreciseNameS,
      paramTypes: Vector[CoordT]):
  PrototypeT = {
    overloadCompiler.findFunction(
      // It's like the abstract function is the one calling the override.
      // This is important so the override can see existing concept functions, see NAFEWRO.
      abstractFuncEnv,
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
    val x1 =
      coutputs.getAllFunctions().flatMap({ case function =>
        function.header.getAbstractInterface match {
          case None => Vector.empty
          case Some(abstractInterface) => {
            val abstractInterfaceTemplate =
              TemplataCompiler.getInterfaceTemplate(abstractInterface.fullName)
            Vector(abstractInterfaceTemplate -> function)
          }
        }
      })
    val x2 = x1.groupBy(_._1)
    val x3 = x2.mapValues(_.map(_._2))
    val x4 =
      x3.map({ case (interfaceTemplateFullName, functions) =>
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
    val abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties = x4
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
