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
    sql"""
    create table members (
      id serial not null primary key,
      name varchar(64),
      created_at timestamp not null
    )
    """.execute.apply()
  }

  override def afterAll(): Unit = {
    sql"drop table members".execute().apply()
    DBs.closeAll()
  }

  trait Builder {
    Seq("Alice", "Bob", "Chris") foreach { name =>
      sql"insert into members (name, created_at) values ($name, current_timestamp)".update.apply()
    }
  }

  "Sqlike" should "be able to select with parameters" in new Builder {
    val table = sqls"members"
    val name = "Bob"
    val m = sql"select * from $table where name = $name ".map(_.toMap()).single().apply().getOrElse(Map.empty)
    println(m)
  }

}
