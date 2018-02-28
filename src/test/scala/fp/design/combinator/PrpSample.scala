package fp.design.combinator

import scala.util.{Failure, Success, Try}



class PrpSample {
  sealed case class Prp(succNum: Int = 0, run: Try[Boolean]) {
    def check: Either[String, Int] = run match {
      case Success(true) => Right(succNum + 1)
      case Success(false) => Left("test executed but failed")
      case Failure(t) => Left(t.getMessage)
    }

    def and(p: Prp): Prp = Prp(succNum + p.succNum + 2, run.transform(_ => p.run, _ => p.run))
  }

}
