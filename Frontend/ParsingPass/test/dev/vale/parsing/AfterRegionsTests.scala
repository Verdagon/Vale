package dev.vale.parsing

import dev.vale.lexing.{BadExpressionEnd, BadStartOfStatementError, ForgotSetKeyword}
import dev.vale.parsing.ast._
import dev.vale.{Collector, StrI}
import org.scalatest.funsuite._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AfterRegionsTests extends AnyFunSuite with Collector with TestParseUtils {

  test("Forgetting set when changing") {
    val error =
      compileStatement(
        """ship.x = 4;""".stripMargin).expectErr()
    error match {
      case ForgotSetKeyword(_) =>
    }
  }

  test("Report leaving out semicolon or ending body after expression, for paren") {
    compileBlockContents(
      """
        |  a = 3;
        |  set x = 7 )
        """.stripMargin).expectErr() match {
      case BadExpressionEnd(_) =>
    }
  }

}
