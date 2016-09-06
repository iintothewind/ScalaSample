package fp.design

import java.util.concurrent.ExecutorService

package object combinator {
  type Par[A] = ExecutorService => Future[A]

}
