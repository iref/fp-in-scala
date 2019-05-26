package fpinscala.ch09

trait Json

object Json {
    case object JNull extends Json
    case class JNumber(get: Double) extends Json
    case class JString(get: String) extends Json
    case class JBool(get: Boolean) extends Json
    case class JArray(get: IndexedSeq[Json]) extends Json
    case class JObject(get: Map[String, Json]) extends Json

   /* def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Json] = {
        import P._
        val spaces = char(' ').many.slice
        val letter = regex("\\s+".r).map(JString)
        val digit = regex("[0-9]+".r)
        val boolean = "true" | "false"
        val jNull = string("null")
        val tokens = letter | digit | boolean | jNull
        val jarray = "["

        val parser =
    }
    */
}
