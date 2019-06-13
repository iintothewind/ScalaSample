package db

import org.scalatest._
import scalikejdbc._
import scalikejdbc.config.{DBs, DBsWithEnv}

class ScalikeJdbcSample
  extends Matchers
    with FlatSpecLike
    with BeforeAndAfterEachTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach {
  implicit val session: AutoSession = AutoSession

  override def beforeAll(): Unit = {
    DBs.loadGlobalSettings()
    DBsWithEnv("scalikejdbc.env.dev").setupAll()
    SQL(
      """
    create table members (
      id serial not null primary key,
      name varchar(64),
      created_at timestamp not null
    )
    """).execute.apply()
  }

  override def afterAll(): Unit = {
    SQL("drop table members").execute().apply()
    DBs.closeAll()
  }

  trait Builder {
    Seq("Alice", "Bob", "Chris") foreach { name =>
      SQL("insert into members (name, created_at) values (?, current_timestamp)").bind(name).update.apply()
    }
  }

  "Sqlike" should "be able to select with parameters" in new Builder {
    val table = "members"
    val name = "Bob"
    val m = SQL("select * from members where name = ? ").bind(name).map(_.toMap()).single().apply().getOrElse(Map.empty)
    println(m)
  }

}
