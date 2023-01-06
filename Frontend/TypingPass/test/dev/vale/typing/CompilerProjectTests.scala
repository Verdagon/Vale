package dev.vale.typing

import dev.vale.postparsing.{DefaultRegionRuneS, RegionTemplataType}
import dev.vale.{CodeLocationS, FileCoordinate, PackageCoordinate, RangeS, StrI, Tests, vassert, vassertSome, vimpl}
import dev.vale.typing.ast.SignatureT
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, DenizenDefaultRegionNameT, FunctionNameT, FunctionTemplateNameT, IdT, LambdaCallFunctionNameT, LambdaCallFunctionTemplateNameT, LambdaCitizenNameT, LambdaCitizenTemplateNameT, KindPlaceholderNameT, KindPlaceholderTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.{CoordTemplata, PlaceholderTemplata}
import dev.vale.typing.types._
import dev.vale.typing.types._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class CompilerProjectTests extends FunSuite with Matchers {

  test("Function has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """exported func main() { }""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val interner = compile.interner

    val packageCoord = interner.intern(PackageCoordinate(interner.intern(StrI("test")),Vector()))
    val mainLoc = CodeLocationS(interner.intern(FileCoordinate(packageCoord, "test.vale")), 0)
    val mainTemplateName = interner.intern(FunctionTemplateNameT(interner.intern(StrI("main")), mainLoc))
    val mainName = interner.intern(FunctionNameT(mainTemplateName, Vector(), Vector()))
    val fullName = IdT(packageCoord, Vector(), mainName)
    vassertSome(coutputs.functions.headOption).header.id shouldEqual fullName
  }

  test("Lambda has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """exported func main() { {}() }""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val interner = compile.interner

    val packageCoord = interner.intern(PackageCoordinate(interner.intern(StrI("test")),Vector()))
    val tzCodeLoc = CodeLocationS.testZero(interner)

    val mainLoc = CodeLocationS(interner.intern(FileCoordinate(packageCoord, "test.vale")), 0)
    val mainTemplateName = interner.intern(FunctionTemplateNameT(interner.intern(StrI("main")), mainLoc))
    val mainName = interner.intern(FunctionNameT(mainTemplateName, Vector(), Vector()))
    val mainTemplateFullName = IdT(packageCoord, Vector(), mainTemplateName)
    val mainFullName = IdT(packageCoord, Vector(), mainName)
//    val region = mainTemplateFullName.addStep(interner.intern(DenizenDefaultRegionNameT()))
    val regionName = mainTemplateFullName.addStep(interner.intern(KindPlaceholderNameT(interner.intern(KindPlaceholderTemplateNameT(0, DefaultRegionRuneS())))))
    val region = PlaceholderTemplata(regionName, RegionTemplataType())
    vimpl() // fulln to id

    val lambdaLoc = CodeLocationS(interner.intern(FileCoordinate(packageCoord, "test.vale")), 23)
    val lambdaCitizenTemplateName = interner.intern(LambdaCitizenTemplateNameT(lambdaLoc))
    val lambdaCitizenName = interner.intern(LambdaCitizenNameT(lambdaCitizenTemplateName))
    val lambdaFuncTemplateName = interner.intern(LambdaCallFunctionTemplateNameT(lambdaLoc, Vector(CoordT(ShareT,region,interner.intern(StructTT(IdT(packageCoord, Vector(mainName), lambdaCitizenName)))))))
    val lambdaCitizenFullName = IdT(packageCoord, Vector(mainName), lambdaCitizenName)
    val lambdaStruct = interner.intern(StructTT(lambdaCitizenFullName))
    val lambdaShareCoord = CoordT(ShareT, region, lambdaStruct)
    val lambdaFuncName = interner.intern(LambdaCallFunctionNameT(lambdaFuncTemplateName, Vector(), Vector(lambdaShareCoord)))
    val lambdaFuncFullName =
      IdT(packageCoord, Vector(mainName, lambdaCitizenTemplateName), lambdaFuncName)

    val lamFunc = coutputs.lookupLambdaIn("main")
    lamFunc.header.id shouldEqual lambdaFuncFullName
  }

  test("Struct has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """
          |
          |exported struct MyStruct { a int; }
          |""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()

    val struct = coutputs.lookupStruct("MyStruct")
    struct.templateName match {
      case IdT(x,Vector(),StructTemplateNameT(StrI("MyStruct"))) => {
        vassert(x.isTest)
      }
    }
  }
}
