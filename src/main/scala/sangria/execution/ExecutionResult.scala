package sangria.execution

import scala.language.higherKinds

import sangria.schema.Schema

import scala.concurrent.Future

trait ExecutionResult[T] {
  type Result[V]
}

object ExecutionResult {
  implicit def default[Ctx, Root] = new ExecutionResult[Schema[Ctx, Root]] {
    type Result[V] = Future[V]
  }
}