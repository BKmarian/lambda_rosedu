val inputVector = {
  Vector(Vector(1,1,0),
         Vector(1,0,1),
         Vector(0,1,1))
}

case class Punct(x: Int, y: Int)

val rowCount = inputVector.length
val colCount = inputVector(0).length
val set = MSet.empty[Punct]

def findIsland(x:Int , y: Int):Unit = {
        inputVector.lift(x + 1).flatMap(_.lift(y)).map{case v => {
        if(set.contains(Punct(x + 1,y)) == false && v == 1) {
                set.add(Punct(x + 1,y))
                findIsland(x + 1,y)
        }
        }
        }
        inputVector.lift(x - 1).flatMap(_.lift(y)).map{case v => {
        if(set.contains(Punct(x - 1,y)) == false && v == 1) {
                set.add(Punct(x - 1,y))
                findIsland(x - 1,y)
        }
        }
        }
        inputVector.lift(x).flatMap(_.lift(y - 1)).map{v => {
        if(set.contains(Punct(x,y - 1)) == false && v == 1) {
                set.add(Punct(x,y - 1))
                findIsland(x,y - 1)
        }
        }
        }
        inputVector.lift(x).flatMap(_.lift(y + 1)).map{v => {
        if(!set.contains(Punct(x,y + 1))  && v == 1) {
                set.add(Punct(x,y + 1))
                findIsland(x ,y + 1)
        }
        }
        }

}

val fullList = (0 to (rowCount - 1)).zipWithIndex
val ff = fullList.map{case (v1,v2) => if(!set.contains(Punct(v1,v2)) && inputVector.lift(v1).flatMap(_.lift(v2)).get == 1) 1 else 0}.count(_ == 1)
println(ff)
