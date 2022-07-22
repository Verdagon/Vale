package dev.vale.typing

import dev.vale.{PackageCoordinate, RangeS, StrI, Tests, vassert, vassertSome, vimpl}
import dev.vale.typing.ast.SignatureT
import dev.vale.typing.names.{CitizenNameT, CitizenTemplateNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, LambdaCitizenNameT, StructNameT, StructTemplateNameT}
import dev.vale.typing.templata.CoordTemplata
import dev.vale.typing.types._
import dev.vale.typing.types._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class CompilerProjectTests extends FunSuite with Matchers {

  test("Function has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """
          |import v.builtins.tup.*;
          |exported func main() { }
          """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()
    val interner = compile.interner

    val tz = RangeS.testZero(interner)
    val fullName = FullNameT(PackageCoordinate.TEST_TLD(interner, compile.keywords), Vector(), interner.intern(FunctionNameT(interner.intern(FunctionTemplateNameT(interner.intern(StrI("main")), tz.begin)), Vector(), Vector())))
    vassertSome(coutputs.lookupFunction(SignatureT(fullName)))
  }

  test("Lambda has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """
          |import v.builtins.tup.*;
          |exported func main() { {}() }
          """.stripMargin)
    val coutputs = compile.expectCompilerOutputs()

//    val fullName = FullName2(PackageCoordinate.TEST_TLD, Vector(), FunctionName2("lamb", Vector(), Vector()))

    vimpl()
//    val lamFunc = coutputs.lookupFunction("__call")
//    lamFunc.header.fullName match {
//      case FullNameT(
//        x,
//        Vector(FunctionNameT(FunctionTemplateNameT(StrI("main"), _),Vector(),Vector()), LambdaCitizenNameT(_)),
//        FunctionNameT(FunctionTemplateNameT(StrI("__call"), _),Vector(),Vector(CoordT(ShareT,_)))) =>
//        vassert(x.isTest)
//    }
  }

  test("Struct has correct name") {
    val compile =
      CompilerTestCompilation.test(
        """
          |import v.builtins.tup.*;
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
