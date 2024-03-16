/*Angie Joya - 2322609
Emily Nuñez - 2240156*/
import  Comparador._
import scala.util.Random

val random = new Random()

def listaAlAzar(long:Int): List[Int] = {
    /*Crea una lista de long enteros
    con valores aleatorios entre 1 y long*2*/
    val v = Vector.fill(long){
        random.nextInt(long*2)+1
    }
    v.toList
}


