/*Angie Joya - 2322609
Emily Nuñez - 2240156*/
import  Comparador._
import scala.util.Random

val random = new Random()

def listaAlAzar(long:Int): List[Int] = {
    /*Crea una lista de long enteros con valores aleatorios entre 1 y long*2*/
    val v = Vector.fill(long){
        random.nextInt(long*2)+1
    }
    v.toList
}

def menorQue (a:Int, b:Int) : Boolean = a<b
def mayorQue (a:Int, b:Int) : Boolean = a>b
def smenorQue (a:String,b:String) : Boolean = a<b

val iSort_Asc = insertionSort[Int](menorQue)
val iSort_Desc = insertionSort[Int](mayorQue)
val iSort_Asc_String = insertionSort[String](smenorQue)

val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)
val qSort_Asc_String = quickSort[String](smenorQue)


longInsert = List(1, 5, 5, 6, 9, 12, 12, 15, 16, 16, 17, 19, 20, 23, 24, 26, 32, 34, 38, 39, 40, 41, 43, 49, 51, 54, 55, 56, 57, 61, 64, 65, 66, 66, 67, 71, 74, 74, 76, 78)
insert(28,longInsert,menorQue)
insert(5,List[Int](),menorQue)
insert(1,List(2,4,6,8),menorQue)
insert(9,List(1,3,5,7),menorQue)
insert(5,List(1,3,7,9),menorQue)
insert(5,List(1,3,5,7),menorQue)
insert("e", List( "a", "b","c","l","o"), smenorQue)

val l100 = listaAlAzar(100)
val l20 = listaAlAzar(20)

iSort_Asc(List(4,5,6,1,2,3))
iSort_Asc(List())
iSort_Asc(List(-4,2,7,-10,20,-28,5,14))
iSort_Asc(l100)
iSort_Desc(l20)
iSort_Asc_String(List("z","b","l","e","a","m","h","n"))

menoresQue_noMenoresQue(longInsert,24,menorQue)
menoresQue_noMenoresQue(List[Int](),5,menorQue)
menoresQue_noMenoresQue(List(7),7,menorQue)
menoresQue_noMenoresQue(List(-4,2,7,-10,20,-28,5,14),5,menorQue)
menoresQue_noMenoresQue(List("z","b","l","e","a","m","h","n"),"h",smenorQue)

qSort_Asc(List(4,5,6,1,2,3))
qSort_Asc(List())
qSort_Asc(List(-4,2,7,-10,20,-28,5,14))
qSort_Asc(l100)
qSort_Desc(l20)
qSort_Asc_String(List("z","b","l","e","a","m","h","n"))

comparar(iSort_Asc,qSort_Asc,List(4,5,6,1,2,3))
comparar(iSort_Asc,qSort_Asc,List())
comparar(iSort_Asc,qSort_Asc,List(-4,2,7,-10,20,-28,5,14))
comparar(iSort_Asc,qSort_Asc,l100)
comparar(iSort_Desc,qSort_Desc,l20)
comparar(iSort_Asc_String,qSort_Asc_String,List("z","b","l","e","a","m","h","n"))