package clicker.game
import play.api.libs.json.{JsValue, Json}
import akka.actor.Actor
import clicker.{BuyEquipment, Click, GameState, Update}

class GameActor(username: String, configuration: String) extends Actor {

  var allEquipment:List[(String,(String,Double),(String,Double),(String,Double),(String,Double),(String,Int))]=List()
  var jsonValue: JsValue = Json.parse(configuration)
  val cur: String = (jsonValue \ "currency").as[String]
  val equ:List[JsValue] =(jsonValue \ "equipment").as[List[JsValue]]
  for (e<-equ){
    var dic:(String,(String,Double),(String,Double),(String,Double),(String,Double),(String,Int)) =(
      (e \ "id").as[String] ,
        ("incomePerClick", (e \ "incomePerClick").as[Double]),
        ("incomePerSecond",(e \ "incomePerSecond").as[Double]),
        ("initialCost",(e \ "initialCost").as[Double]),
        ("priceExponent", (e \ "priceExponent").as[Double]),
          ("count",0)
    )
    allEquipment=allEquipment:+dic
  }


  var currency = 0.0
  var timebefore = System.nanoTime()/1000000000
  println(allEquipment)
  var incomePerClick=1.0
  var incomePerSecond=0.0
  var first:Boolean =true
  println(incomePerSecond)
  var count=0


  for(a<-allEquipment){
      println(a._1)
  }

//  var changeInItem:((String,String),(String,Double),(String,Double))=(("",""),("",0),("",0))
//
//
//  var Scost=10.0
//  var Mcost=1000.0
//  var Ecost=200.0
//
//  var Scount=0
//  var Mcount=0
//  var Ecount=0

  var lisId:List[String]=List()
  var newLis:List[(String,Double,Double,Double,Double,Double)] = List()
  // new lis will contains ( string, cost, count , incomeperclick, income persecond, rate  )
//  var finLis:(String,Double,Double)=()
  override def receive: Receive = {
  case Click =>
        if (first) {
          first = false
          currency = 1
        }
        else {
    currency = currency + incomePerClick
    }

  case equip: BuyEquipment =>
    if (!lisId.contains(equip.equipmentId)) {
      for (x <- allEquipment) {
        if (x._1 == equip.equipmentId) {
          if (currency >= x._4._2) {
            currency = currency - x._4._2
            val cost = x._4._2 * x._5._2
            val count = 1.0
            incomePerClick = incomePerClick + x._2._2
            incomePerSecond = incomePerSecond + x._3._2
            //              finLis=(x._1,cost,count)
            newLis = newLis :+ (x._1, cost, count, x._2._2, x._3._2, x._5._2)
            lisId = lisId :+ equip.equipmentId
          }
        }
      }
    }
    else {
      var c = 0
      var cc=1
      for (x <- newLis) {
        if (x._1 == equip.equipmentId) {
          c = c + 1
        }
      }

      for (x <- newLis) {
        if (x._1 == equip.equipmentId) {
          if(cc==c) {
            if (currency > x._2) {
              currency=currency-x._2
              val newcost = x._2 * x._6
              val newcount = x._3 + 1.0
              incomePerSecond=incomePerSecond+x._5
              incomePerClick=incomePerClick+x._4
              //            finLis=(x._1,newcost,newcount)
              newLis = newLis :+ (x._1, newcost, newcount, x._4, x._5, x._6)
            }
          }
          else{
            cc=cc+1
          }


        }
      }
      println(newLis)
    }


//      for(a<-allEquipment){
//        if(a._1==equip.equipmentId){
//
//          if(a._1=="shovel"){
//            if(currency>=Scost){
//              currency=currency-Scost
//              incomePerClick=incomePerClick+a._2._2
//              Scost=Scost*1.1
//              Scount=Scount+1
//            }
//          }
//          if(a._1=="mine"){
//            if(currency>=Mcost){
//              currency=currency-Mcost
//              incomePerClick=incomePerClick+a._2._2
//              incomePerSecond=incomePerSecond+a._3._2
//              Mcost=Mcost*1.1
//              Mcount=Mcount+1
//            }
//          }
//          if(a._1=="excavator"){
//            if(currency>=Ecost){
//              currency=currency-Ecost
//              incomePerClick=incomePerClick+a._2._2
//              incomePerSecond=incomePerSecond+a._3._2
//              Ecost=Ecost*1.1
//              Ecount=Ecount+1
//            }
//          }
//
//        }
//      }


    case Update =>
      val timenow= System.nanoTime()/1000000000
      currency=(incomePerSecond * (timenow-timebefore))+currency
      timebefore=System.nanoTime()/1000000000

      var firstmap:List[JsValue]=List()
//      for(x<-allEquipment) {
//        if(x._1=="shovel"){
//          firstmap=firstmap:+(Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(Scount),"cost"->Json.toJson(Scost))))
//        }
//        if(x._1=="mine"){
//          firstmap=firstmap:+(Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(Mcount),"cost"->Json.toJson(Mcost))))
//        }
//        if(x._1=="excavator"){
//          firstmap=firstmap:+(Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(Ecount),"cost"->Json.toJson(Ecost))))
//        }
//
//      }

      for(x<-allEquipment){
        var line=Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(1),"cost"->Json.toJson(2)))
        for(xx<-newLis){
          if(x._1==xx._1){
            line=(Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(xx._3),"cost"->Json.toJson(xx._2))))
          }
        }
        firstmap=firstmap:+line
        if(!lisId.contains(x._1)){
          firstmap=firstmap:+(Json.toJson(Map("id"->Json.toJson(x._1),"numberOwned"->Json.toJson(0),"cost"->Json.toJson(x._4._2))))
        }
      }



      val lastmap:Map[String,JsValue]= Map(
        "username"->Json.toJson(username),
        "currency"->Json.toJson(currency),
        "equipment"->Json.toJson(firstmap)
      )
      sender() ! GameState(Json.stringify(Json.toJson(lastmap)))
  }
}
