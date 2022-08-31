package dev.vale.typing

import dev.vale.{CodeLocationS, FileCoordinate, PackageCoordinate, RangeS, StrI, Tests, vassert, vassertSome, vimpl}
import dev.vale.typing.ast.SignatureT
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, LambdaCitizenNameT, LambdaCitizenTemplateNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.CoordTemplata
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
    val fullName = FullNameT(packageCoord, Vector(), mainName)
    vassertSome(coutputs.functions.headOption).header.fullName shouldEqual fullName
  }

  test("Lambda has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """exported func main() { {}() }""".stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val interner = compile.interner

    val packageCoord = interner.intern(PackageCoordinate(interner.intern(StrI("test")),Vector()))
    val mainLoc = CodeLocationS(interner.intern(FileCoordinate(packageCoord, "test.vale")), 0)
    val mainTemplateName = interner.intern(FunctionTemplateNameT(interner.intern(StrI("main")), mainLoc))
    val mainName = interner.intern(FunctionNameT(mainTemplateName, Vector(), Vector()))

    val lambdaLoc = CodeLocationS(interner.intern(FileCoordinate(packageCoord, "test.vale")), 23)
    val lambdaCitizenTemplateName = interner.intern(LambdaCitizenTemplateNameT(lambdaLoc))
    val lambdaCitizenName = interner.intern(LambdaCitizenNameT(lambdaCitizenTemplateName))
    val lambdaFuncTemplateName = interner.intern(FunctionTemplateNameT(interner.intern(StrI("__call")),lambdaLoc))
    val lambdaCitizenFullName = FullNameT(packageCoord, Vector(mainName), lambdaCitizenName)
    val lambdaStruct = interner.intern(StructTT(lambdaCitizenFullName))
    val lambdaShareCoord = CoordT(ShareT, lambdaStruct)
    val lambdaFuncName = interner.intern(FunctionNameT(lambdaFuncTemplateName, Vector(), Vector(lambdaShareCoord)))
    val lambdaFuncFullName = FullNameT(packageCoord, Vector(mainName, lambdaCitizenTemplateName), lambdaFuncName)

    val lamFunc = coutputs.lookupFunction("__call")
    lamFunc.header.fullName shouldEqual lambdaFuncFullName
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
      case FullNameT(x,Vector(),StructTemplateNameT(StrI("MyStruct"))) => {
        vassert(x.isTest)
      }
    }
  }
}
