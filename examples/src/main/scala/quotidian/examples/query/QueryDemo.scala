package quotidian.examples.query

object QueryDemo extends App:
  println("Hello, world!")

  val user =
    User
      .select(_.name, _.age)
      // .join(Post)
      .run

  println(user.name)
  println(user.age)
  // println(cool.posts.author)

type UserFields = {
  def name: String
  def age: Int
}
