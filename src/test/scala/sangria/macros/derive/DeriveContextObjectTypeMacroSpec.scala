package sangria.macros.derive

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.util.FutureResultSupport
import spray.json._

import scala.concurrent.Future
import scala.collection.mutable.{Map => MutMap}

object DeriveContextObjectTypeMacroSpecDomain {
  case class User(id: String, name: String)
  object User extends DefaultJsonProtocol {
    implicit val JsonFormat = jsonFormat2(User.apply)
  }
  case class Tweet(author: User, text: String, time: Long)
  object Tweet extends DefaultJsonProtocol {
    implicit val JsonFormat = jsonFormat3(Tweet.apply)
  }

  trait QueryApi {
    @GraphQLField
    @GraphQLDescription("myself")
    def me: Future[User]
    @GraphQLField
    @GraphQLName("tweetsByAuthor")
    def tweets(authorId: String): Future[Seq[Tweet]]
    def ignored(notUsed: String): Future[Unit]
  }

  @GraphQLName("MyMutationsType")
  @GraphQLDescription("My mutation type!")
  trait MutationApi {
    def tweet(text: String): Future[Tweet]
    def ignored(notUsed: String): Future[Unit]
  }

  trait Api extends QueryApi with MutationApi
  case class ApiContext(
      user: User = User("0", "Lee"),
      tweetsMap: MutMap[User, Seq[Tweet]] = MutMap.empty) extends Api {
    override def me = Future.successful(user)
    override def tweets(authorId: String) =
      Future.successful(tweetsMap.getOrElse(user, Nil))
    override def tweet(text: String) =
      Future.successful(Tweet(user, text, 123456789L))
    override def ignored(notUsed: String) = ???
  }
}

class DeriveContextObjectTypeMacroSpec extends WordSpec with Matchers with FutureResultSupport {
  import DeriveMacroTestModel.{CachedTag, AuthorizedTag}
  import DeriveContextObjectTypeMacroSpecDomain._

  implicit val UserType = deriveObjectType[Unit, User]()
  implicit val TweetType = deriveObjectType[Unit, Tweet]()

  val MutationApiMethods = scala.collection.immutable.Set("tweet")

  "ContextObjectType derivation" should {
    "use class name and have no description by default" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](identity)

      tpe.name should be ("QueryApi")
      tpe.description should be (None)
    }

    "allow to change name and description with config" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "allow to change name and description with annotations" in {
      val tpe = deriveContextObjectType[Api, MutationApi, Unit](
        identity,
        IncludeMethods("tweet"))

      tpe.name should be ("MyMutationsType")
      tpe.description should be (Some("My mutation type!"))
    }

    "prioritize config over annotation for name and description" in {
      val tpe = deriveContextObjectType[Api, MutationApi, Unit](
        identity,
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"),
        IncludeMethods("tweet"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "expose trait fields" in {
      val tpe = deriveContextObjectType[Api, MutationApi, Unit](
        identity,
        IncludeMethods("tweet"))
      val fields = tpe.fields

      fields should have size 1

      fields(0).name should be ("tweet")
      fields(0).fieldType should be (TweetType)
    }

    "expose trait fields named via annotation" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](identity)
      val fields = tpe.fields

      fields should have size 2

      fields(0).name should be ("me")
      fields(0).fieldType should be (UserType)

      fields(1).name should be ("tweetsByAuthor")
      fields(1).fieldType should be (ListType(TweetType))
    }

    "validate known field names" in {
      """deriveContextObjectType[Api, QueryApi, Unit](identity, IncludeFields("me", "list1"))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, ExcludeFields("me1"))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, DocumentField("me1", "foo"))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, RenameField("me1", "foo"))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, FieldTags("me1", CachedTag))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, DeprecateField("me1", "test"))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, FieldComplexity("me1", (_, _, _) ⇒ 1.0))""" shouldNot compile
      """deriveContextObjectType[Api, QueryApi, Unit](identity, ExcludeFields("me", "tweetsByAuthor"))""" shouldNot compile
    }

    "respect whitelist and blacklist provided via config" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        IncludeFields("me", "tweets"),
        ExcludeFields("tweets"))

      tpe.fields should have size 1

      tpe.fields(0).name should be ("me")
    }

    "respect blacklist provided via annotations" in {
      trait QueryWithExcluded extends QueryApi {
        @GraphQLExclude
        override def me: Future[User]
      }
      val tpe = deriveContextObjectType[Api, QueryWithExcluded, Unit](
        _.asInstanceOf[QueryWithExcluded])

      tpe.fields should have size 1
      tpe.fields(0).name should be ("tweetsByAuthor")
    }

    "allow to add new fields" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        IncludeFields("me"),
        AddFields(
          Field("foo", ListType(StringType), resolve = _ => Seq("a", "b")),
          Field("bar", BooleanType, resolve = _ ⇒ true)))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("me")
      tpe.fields(0).fieldType should be (UserType)

      tpe.fields(1).name should be ("foo")
      tpe.fields(1).fieldType should be (ListType(StringType))

      tpe.fields(2).name should be ("bar")
      tpe.fields(2).fieldType should be (BooleanType)
    }

    "allow to override fields" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        ReplaceField("me", Field("me", ListType(StringType), resolve = _ => Seq("a", "b"))),
        ReplaceField("tweets", Field("bar", BooleanType, resolve = _ ⇒ true)))

      tpe.fields should have size 2

      tpe.fields(0).name should be ("me")
      tpe.fields(0).fieldType should be (ListType(StringType))

      tpe.fields(1).name should be ("bar")
      tpe.fields(1).fieldType should be (BooleanType)
    }

    "allow to set field complexity with config" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        FieldComplexity("me", (_, _, child) ⇒ child * 123.0))

      tpe.fields(0).complexity.get(ApiContext(), Args.empty, 2D) should be (246.0)
    }

    "allow to set name, description, deprecationReason and fieldTags with config" in {
      val tpe = deriveContextObjectType[Api, QueryApi, Unit](
        identity,
        DocumentField("me", "the logged in user", deprecationReason = Some("foo")),
        RenameField("me", "loggedInUser"),
        RenameField("tweets", "status"),
        DocumentField("tweets", "my status"),
        AddFields(
          Field("blog", ListType(StringType), resolve = _ => ???)),
        DeprecateField("tweets", "bar"),
        FieldTags("me", CachedTag, AuthorizedTag))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("loggedInUser")
      tpe.fields(0).description should be (Some("the logged in user"))
      tpe.fields(0).deprecationReason should be (Some("foo"))
      tpe.fields(0).tags should be (List(CachedTag, AuthorizedTag))
      tpe.fields(0).fieldType should be (UserType)

      tpe.fields(1).name should be ("status")
      tpe.fields(1).description should be (Some("my status"))
      tpe.fields(1).deprecationReason should be (Some("bar"))
      tpe.fields(1).tags should be (Nil)
      tpe.fields(1).fieldType should be (ListType(TweetType))

      tpe.fields(2).name should be ("blog")
      tpe.fields(2).description should be (None)
      tpe.fields(2).deprecationReason should be (None)
      tpe.fields(2).tags should be (Nil)
      tpe.fields(2).fieldType should be (ListType(StringType))
    }

    "allow to set name, description, deprecationReason and fieldTags with annotations" in {
      trait QueryApiWithDeprecations extends QueryApi {
        @GraphQLField
        @GraphQLName("loggedInUser")
        @GraphQLDescription("the logged in user")
        @GraphQLDeprecated("foo")
        @GraphQLFieldTags(CachedTag, AuthorizedTag)
        override def me: Future[User]
      }
      trait ApiWithDeprecation extends QueryApiWithDeprecations
      val tpe = deriveContextObjectType[ApiWithDeprecation, QueryApiWithDeprecations, Unit](identity)

      tpe.fields should have size 2

      tpe.fields(0).name should be ("tweetsByAuthor")
      tpe.fields(0).description should be (None)
      tpe.fields(0).deprecationReason should be (None)
      tpe.fields(0).tags should be (Nil)
      tpe.fields(0).fieldType should be (ListType(TweetType))

      tpe.fields(1).name should be ("loggedInUser")
      tpe.fields(1).description should be (Some("the logged in user"))
      tpe.fields(1).deprecationReason should be (Some("foo"))
      tpe.fields(1).tags should be (List(CachedTag, AuthorizedTag))
      tpe.fields(1).fieldType should be (UserType)
    }

    /*FIXME
    "prioritize field config name, description, deprecationReason and merge fieldTags" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated]()

      tpe.fields should have size 2

      tpe.fields(0).name should be ("id")
      tpe.fields(0).description should be (Some("new descr"))
      tpe.fields(0).deprecationReason should be (Some("new depr"))
      tpe.fields(0).tags should be (List(FooTag))
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("fooBar")
      tpe.fields(1).description should be (None)
      tpe.fields(1).deprecationReason should be (None)
      tpe.fields(1).tags should be (List(FooTag, CachedTag, AuthorizedTag))
      tpe.fields(1).fieldType should be (ListType(StringType))
    }
    */

    "support vals" in {
      trait QueryApiWithVal {
        @GraphQLField
        @GraphQLName("foo")
        @GraphQLDescription("test field")
        val bar: Option[List[Int]] = Some(List(1, 2, 3))
      }
      trait ApiWithVal extends QueryApiWithVal
      val tpe = deriveContextObjectType[ApiWithVal, QueryApiWithVal, Unit](identity)

      tpe.fields should have size 1

      tpe.fields(0).name should be ("foo")
      tpe.fields(0).description should be (Some("test field"))
      tpe.fields(0).deprecationReason should be (None)
      tpe.fields(0).fieldType should be (OptionType(ListType(IntType)))
    }

    /*
    "support companion objects for `Enumeration`s" in {
      val enum = test.AnotherEnum.valNameType

      enum.values.map(_.name) should (
        have(size(3)) and
        contain("FOO") and
        contain("BAR") and
        contain("BAZ")
      )
    }

    "be able to find other types via implicit GraphQL types" in {
      implicit val FruitType = deriveEnumType[FruitAnnotated]()
      implicit val ColorType = deriveEnumType[ColorAnnotated.Value]()

      implicit val CommentType = deriveObjectType[Unit, Comment](
        DocumentField("author", "The comment author"),
        DocumentField("text", "Comment text"))

      val ArticleType = deriveObjectType[Unit, Article](
        RenameField("tags", "myTags"))

      val testArticle = Article("My First Article", Some("foo bar"), None,
        Some(Vector(Some(Comment("bob", None)), None, Some(Comment("jane", Some("yay!"))))))

      val query =
        graphql"""
          {
            title
            text
            myTags
            fruit
            comments {
              author
              text
              color
            }
          }
        """

      val schema = Schema(ArticleType)

      Executor.execute(schema, query, root = testArticle).await should be (Map(
        "data" → Map(
          "title" → "My First Article",
          "text" → "foo bar",
          "myTags" → null,
          "fruit" → "JustApple",
          "comments" → List(
            Map("author" → "bob", "text" → null, "color" → "NormalRed"),
            null,
            Map("author" → "jane", "text" → "yay!", "color" → "NormalRed")))))

      import sangria.marshalling.queryAst._
      import sangria.parser.DeliveryScheme.Throw

      val intro = IntrospectionParser.parse(Executor.execute(schema, sangria.introspection.introspectionQuery, root = testArticle).await)

      intro.queryType.name should be ("Article")

      val Some(articleIntro: IntrospectionObjectType) = intro.types.find(_.name == "Article")
      val Some(commentIntro: IntrospectionObjectType) = intro.types.find(_.name == "Comment")

      commentIntro.fields should have size 3

      commentIntro.fields(0).name should be ("author")
      commentIntro.fields(0).description should be (Some("The comment author"))

      commentIntro.fields(1).name should be ("text")
      commentIntro.fields(1).description should be (Some("Comment text"))

      commentIntro.fields(2).name should be ("color")
      commentIntro.fields(2).description should be (None)

      articleIntro.fields(3).name should be ("comments")
      articleIntro.fields(3).tpe should be (IntrospectionListTypeRef(IntrospectionNamedTypeRef(TypeKind.Object, "Comment")))

      articleIntro.fields(4).name should be ("fruit")
      articleIntro.fields(4).tpe should be (IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "MyFruit")))
    }

    "be able handle recursive types with field overrides" in {
      case class A(id: Int, b: B)
      case class B(name: String, a: A, b: B)

      implicit lazy val AType = deriveObjectType[Unit, A](
        ReplaceField("b", Field("b", BType, resolve = _.value.b)))

      implicit lazy val BType: ObjectType[Unit, B] = deriveObjectType(
        ReplaceField("a", Field("a", AType, resolve = _.value.a)),
        ReplaceField("b", Field("b", BType, resolve = _.value.b)))

      val schema = Schema(AType)

      val query =
        graphql"{id, b {name, a {id}, b {name}} }"

      Executor.execute(schema, query, root = A(1, B("foo", A(2, null), B("bar", null, null)))).await should be (Map(
        "data" → Map("id" → 1, "b" → Map("name" → "foo", "a" → Map("id" → 2), "b" → Map("name" → "bar")))))
    }

    "use companion object to resolve derived types" in {
      import test._

      val schema = Schema(CompanionA.graphqlType)

      val query = graphql"{b {myC {e, e1}}}"

      Executor.execute(schema, query, root = CompanionA(CompanionB(CompanionC(CompanionEnum1, AnotherEnum.FOO)))).await should be (Map(
        "data" → Map("b" → Map("myC" → Map("e" → "first", "e1" → "FOO")))))
    }

    "support `Future`, `Try`, `Defer` and `Action` return types" in {
      case class MyTest(deferVal: TestDefer) {
        @GraphQLField
        def futureVal: Future[List[Int]] = Future.successful(List(11, 22))

        @GraphQLField
        def tryVal: Try[Option[List[Int]]] = Success(Some(List(33, 44)))

        @GraphQLField
        def actionVal = DeferredFutureValue(Future.successful(TestDefer(1)))
      }

      val tpe = deriveObjectType[Unit, MyTest]()

      tpe.fields.sortBy(_.name).map(f ⇒ f.name → f.fieldType) should be (List(
        "actionVal" → OptionType(ListType(IntType)),
        "deferVal" → OptionType(ListType(IntType)),
        "futureVal" → ListType(IntType),
        "tryVal" → OptionType(ListType(IntType))
      ))
    }

    "derive methods with arguments via annotations" in {
      object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val PetFormat = jsonFormat2(Pet.apply)
      }

      import MyJsonProtocol._
      import sangria.marshalling.sprayJson._

      implicit val PetType = deriveInputObjectType[Pet]()
      implicit val colorType = deriveEnumType[Color.Value]()

      case class Ctx(num: Int, fooBar: FooBar)

      class FooBar {
        @GraphQLField
        @GraphQLName("foo")
        def hello(
          @GraphQLDefault(123)
          id: Int,
          songs: Seq[String]
        )(
          ctx: Context[Ctx, Unit],

          @GraphQLDefault(Pet("xxx", Some(322)))
          pet: Pet,

          @GraphQLName("aaa")
          @GraphQLDescription("bbbb")
          @GraphQLDefault(ScalaInput.scalaInput(List(Color.Red)))
          colors: Seq[Color.Value]
        ) =
          s"id = $id, songs = ${songs mkString ","}, cc = ${colors mkString ","}, pet = $pet, ctx = ${ctx.ctx.num}"

        @GraphQLField
        def opt(str: Option[String], color: Option[Color.Value])(pet: Option[Pet]) =
          s"str = $str, color = $color, pet = $pet"
      }

      val tpe = deriveContextObjectType[Ctx, FooBar, Unit](_.fooBar)

      val schema = Schema(tpe)

      val query =
        graphql"""
          {
            foo(songs: ["a", "b"])
            foo1: foo(songs: ["a", "b"], pet: {name: "mypet", size: 156})
            opt
            opt1: opt(str: "test", color: Red, pet: {name: "anotherPet", size: 321})
          }
        """

      Executor.execute(schema, query, Ctx(987, new FooBar)).await should be (
        JsObject("data" → JsObject(
          "foo" → JsString("id = 123, songs = a,b, cc = Red, pet = Pet(xxx,Some(322)), ctx = 987"),
          "foo1" → JsString("id = 123, songs = a,b, cc = Red, pet = Pet(mypet,Some(156)), ctx = 987"),
          "opt" → JsString("str = None, color = None, pet = None"),
          "opt1" → JsString("str = Some(test), color = Some(Red), pet = Some(Pet(anotherPet,Some(321)))"))))

      import sangria.parser.DeliveryScheme.Throw

      val intro = IntrospectionParser.parse(Executor.execute(schema, introspectionQuery, Ctx(987, new FooBar)).await)
      val introType = intro.types.find(_.name == "FooBar").get.asInstanceOf[IntrospectionObjectType]

      introType.fields should have size 2

      val Some(helloField) = introType.fields.find(_.name == "foo")

      helloField.args should have size 4

      helloField.args should be (List(
        IntrospectionInputValue("id", None, IntrospectionNamedTypeRef(TypeKind.Scalar, "Int"), Some("123")),
        IntrospectionInputValue("songs", None,
          IntrospectionNonNullTypeRef(IntrospectionListTypeRef(IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Scalar, "String")))),None),
        IntrospectionInputValue("pet", None, IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"), Some("""{name:"xxx",size:322}""")),
        IntrospectionInputValue("aaa", Some("bbbb"), IntrospectionListTypeRef(IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "Color"))), Some("[Red]"))))

      val Some(optField) = introType.fields.find(_.name == "opt")

      optField.args should have size 3

      optField.args should be (List(
        IntrospectionInputValue("str", None, IntrospectionNamedTypeRef(TypeKind.Scalar, "String"), None),
        IntrospectionInputValue("color", None, IntrospectionNamedTypeRef(TypeKind.Enum, "Color"), None),
        IntrospectionInputValue("pet", None, IntrospectionNamedTypeRef(TypeKind.InputObject, "Pet"), None)))
    }

    "not set a default value to `null`" in {
      class Query {
        @GraphQLField def foo(a: Option[String] = None) = "" + a
      }

      import sangria.marshalling.sprayJson._
      import sangria.parser.DeliveryScheme.Throw

      val QueryType = deriveObjectType[Unit, Query]()

      val schema = Schema(QueryType)

      val intro = IntrospectionParser.parse(Executor.execute(schema, sangria.introspection.introspectionQuery, root = new Query).await)

      intro.typesByName("Query").asInstanceOf[IntrospectionObjectType].fieldsByName("foo").argsByName("a").defaultValue should be (None)
    }
    */
  }
}
