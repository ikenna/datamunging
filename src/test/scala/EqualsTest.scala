import org.scalatest.FunSuite

class EqualsTest extends FunSuite{

  test("Equality"){

    val p1 = new Person("henry", 23)
    val p2 = new Person("henry", 23)


    assert(p1 === p2)
  }

}



class Person(val name:String, val age:Int) {

  override def equals(other: Any):Boolean = {
    other match {
      case that: Person => (that canEqual this ) && name == that.name && age == that.age
      case _ => false
    }
  }

   def canEqual(other: Any):Boolean = {
     other.isInstanceOf[Person]
  }

  override def hashCode:Int = {
    41 * (41 * (41 + age) + name.hashCode)
  }
}