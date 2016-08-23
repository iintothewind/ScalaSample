package fp.design

import java.util.concurrent.{Future, ExecutorService}

package object combinator {
  type Par[A] = ExecutorService => Future[A]

}
