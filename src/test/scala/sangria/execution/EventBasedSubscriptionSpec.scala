package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class EventBasedSubscriptionSpec extends WordSpec with Matchers with AwaitSupport {
  "Foo" should {
    "correctly threads arguments" in {
      var resolvedArgs: Map[String, Any] = Map.empty

      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("b", OptionType(StringType),
          arguments = Argument("numArg", OptionInputType(IntType)) :: Argument("stringArg", OptionInputType(StringType)) :: Nil,
          resolve = ctx ⇒ {resolvedArgs = ctx.args.raw; None}))))

      val Success(doc) = QueryParser.parse("""
        query Example {
          b(numArg: 123, stringArg: "foo")
        }
      """)

      Executor(schema).execute(doc).await

      resolvedArgs should be (Map("numArg" → Some(123), "stringArg" → Some("foo")))
    }

  }
}
