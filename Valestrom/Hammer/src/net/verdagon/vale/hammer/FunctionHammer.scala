package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl, vwat, metal => m}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{Export2, Extern2, FunctionHeaderT, IFunctionAttribute2, PrototypeT, Pure2, UserFunction2}

object FunctionHammer {

  def translateFunctions(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    functions2: List[FunctionT]):
  (List[FunctionRefH]) = {
    functions2.foldLeft((List[FunctionRefH]()))({
      case ((previousFunctionsH), function2) => {
        val (functionH) = translateFunction(hinputs, hamuts, function2)
        (functionH :: previousFunctionsH)
      }
    })
  }

  def translateFunction(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    function2: FunctionT):
  (FunctionRefH) = {
//    opts.debugOut("Translating function " + function2.header.fullName)
    hamuts.functionRefs.get(function2.header.toPrototype) match {
      case Some(functionRefH) => functionRefH
      case None => {
        val FunctionT(
            header @ FunctionHeaderT(humanName, attrs2, params2, returnType2, _),
            body) = function2;

        val (prototypeH) = translatePrototype(hinputs, hamuts, header.toPrototype);
        val temporaryFunctionRefH = FunctionRefH(prototypeH);
        hamuts.forwardDeclareFunction(header.toPrototype, temporaryFunctionRefH)

        val locals =
          LocalsBox(
            Locals(
              Map[FullNameT[IVarNameT], VariableIdH](),
              Set[VariableIdH](),
              Map[VariableIdH,Local](),
              1));
        val (bodyH, List()) =
          ExpressionHammer.translate(hinputs, hamuts, header, locals, body)
        vassert(locals.unstackifiedVars.size == locals.locals.size)
        val resultCoord = bodyH.resultType
        if (resultCoord.kind != NeverH() && resultCoord != prototypeH.returnType) {
          vfail(
            "Result of body's instructions didnt match return type!\n" +
            "Return type:   " + prototypeH.returnType + "\n" +
            "Body's result: " + resultCoord)
        }

        val maybeExportNsCoord = function2.header.attributes.collectFirst({ case Export2(packageCoord) => packageCoord })
        val isAbstract = header.getAbstractInterface.nonEmpty
        val isExtern = header.attributes.exists({ case Extern2(packageCoord) => true case _ => false })
        val attrsH = translateFunctionAttributes(attrs2.filter(a => !a.isInstanceOf[Extern2] && !a.isInstanceOf[Export2]))
        val functionH = FunctionH(prototypeH, maybeExportNsCoord.nonEmpty, isAbstract, isExtern, attrsH, bodyH);
        hamuts.addFunction(header.toPrototype, functionH)

        maybeExportNsCoord match {
          case None =>
          case Some(exportPackageCoord) => {
            val exportedName =
              humanName.last match {
                case FunctionNameT(humanName, _, _) => humanName
                case _ => vfail("Can't export something that doesn't have a human readable name!")
              }
            hamuts.addFunctionExport(prototypeH, exportPackageCoord, exportedName)
          }
        }

        (temporaryFunctionRefH)
      }
    }
  }

  def translateFunctionAttributes(attributes: List[IFunctionAttribute2]) = {
    attributes.map({
      case UserFunction2 => UserFunctionH
      case Pure2 => PureH
      case Extern2(_) => vwat() // Should have been filtered out, hammer cares about extern directly
      case Export2(_) => vwat() // Should have been filtered out, hammer cares about export directly
      case x => vimpl(x.toString)
    })
  }

  def translatePrototypes(
      hinputs: Hinputs, hamuts: HamutsBox,
      prototypes2: List[PrototypeT]):
  (List[PrototypeH]) = {
    prototypes2 match {
      case Nil => Nil
      case headPrototype2 :: tailPrototypes2 => {
        val (headPrototypeH) = translatePrototype(hinputs, hamuts, headPrototype2)
        val (tailPrototypesH) = translatePrototypes(hinputs, hamuts, tailPrototypes2)
        (headPrototypeH :: tailPrototypesH)
      }
    }
  }

  def translatePrototype(
      hinputs: Hinputs, hamuts: HamutsBox,
      prototype2: PrototypeT):
  (PrototypeH) = {
    val PrototypeT(fullName2, returnType2) = prototype2;
    val (paramsTypesH) = TypeHammer.translateReferences(hinputs, hamuts, prototype2.paramTypes)
    val (returnTypeH) = TypeHammer.translateReference(hinputs, hamuts, returnType2)
    val (fullNameH) = NameHammer.translateFullName(hinputs, hamuts, fullName2)
    val prototypeH = PrototypeH(fullNameH, paramsTypesH, returnTypeH)
    (prototypeH)
  }

  def translateFunctionRef(
      hinputs: Hinputs,
      hamuts: HamutsBox,
    currentFunctionHeader: FunctionHeaderT,
      prototype2: PrototypeT):
  (FunctionRefH) = {
    val (prototypeH) = translatePrototype(hinputs, hamuts, prototype2);
    val functionRefH = FunctionRefH(prototypeH);
    (functionRefH)
  }
}
