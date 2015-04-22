package coloringbots

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 21.04.15
 * Time: 15:32
 * Since: 
 *
 */
trait NameProviderComponent {
  val nameProvider:NameProvider

  trait NameProvider {
    def getName:String
  }

}

trait SayHelloComponent {
  val sayHelloService:SayHelloService

  trait SayHelloService {
    def sayHello:Unit
  }

}

trait NameProviderComponentImpl extends NameProviderComponent {

  class NameProviderImpl extends NameProvider {
    def getName:String = "World"
  }

}

trait SayHelloComponentImpl extends SayHelloComponent {
  this: SayHelloComponentImpl with NameProviderComponent =>

  class SayHelloServiceImpl extends SayHelloService {
    def sayHello:Unit = println("Hello, "+nameProvider.getName+"!")
  }

}

object ComponentRegistry
  extends SayHelloComponentImpl
  with NameProviderComponentImpl
{
  val nameProvider = new NameProviderImpl
  val sayHelloService = new SayHelloServiceImpl
}


object MyApplication {
  def main(args : Array[String]) : Unit = {
    ComponentRegistry.sayHelloService.sayHello
  }
}