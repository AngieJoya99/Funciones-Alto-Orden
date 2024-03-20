package object Comparador {
    type AlgoritmoOrd[T] = List[T] => (List[T], Int)
    type Comparador [T] = (T,T) => Boolean

    /*Recibe un elemento e de tipo T a insertar en una lista ordenada l de elementos
    de tipo T y devuelve , en una pareja la lista ordenada incluyendo el elemento e
    y cuantas comparaciones se hicieron para lograrlo*/
    def insert[T] (e:T, l:List[T], comp:Comparador[T]): (List[T], Int) = {

       def insertar[T](e:T, l:List[T], comp:Comparador[T],cont:Int):(List[T],Int) =
       {
        if (l.isEmpty) (e::l,cont) else if (comp(e,l.head)) (e::l,cont+1) 
        else 
        {
            val (lista,contador) = insertar(e,l.tail,comp,cont+1)
            (l.head::lista,contador)
        }
       }
       insertar(e,l,comp,0)    
    }

    /*Generadora de instancias. Recibe un comparador de elementos de tipo T
    y devuelve la lista ordenada y el numero de comparaciones realizadas en una 
    pareja usando el InsertionSort. */
    def insertionSort[T] (comp:Comparador[T]): AlgoritmoOrd[T] = 
    {
        def insertion(gl:List[T],nl:List[T],cont:Int):(List[T],Int) = 
        {
            if(gl.isEmpty) (nl,cont)
            else {
                val(lista,contador)=insert(gl.head,nl,comp)
                insertion(gl.tail,lista,contador+cont)
            }
        }
        (gl:List[T]) =>{insertion(gl,List[T](), 0)}        
    }

    def menoresQue_noMenoresQue[T]( l: List[T], v: T, comp: Comparador[T] ): (List[T], List[T], Int) = {
        if ( l.isEmpty ) (List(), List(), 0)
        else
            val (lista1, lista2, contador) = menoresQue_noMenoresQue(l.tail, v, comp)
            if ( comp(l.head, v) ) ( l.head :: lista1, lista2, contador + 1)
            else ( lista1, l.head :: lista2, contador + 1)
    }

    def quickSort[T] (comp:Comparador[T]): AlgoritmoOrd[T] = {
        def algoritmo(lista:List[T]):(List[T],Int) = {
            if(lista.isEmpty) (List(),0)
            else {
                val pivote : T = lista.head
                val (menores, mayores, contador) = menoresQue_noMenoresQue(lista.tail, pivote, comp)
                val (menoresOrdenados, contadorMenores) = algoritmo(menores)
                val (mayoresOrdenados, contadorMayores) = algoritmo(mayores)
                (menoresOrdenados ::: (pivote :: mayoresOrdenados), contador + contadorMenores + contadorMayores)
            }
        }
        algoritmo
    }

    /*Recibe dos algoritmos de ordenamiento y una lista para ordenar
    y devuelve una pareja con el numero de comparaciones hechas por a1,
    y el numero de comparaciones hechas por a2 para esa instancia de l en particular
    si los dos algoritmos dan el mismo resultado si no, devuelve (-1,-1)*/
    def comparar[T] (a1:AlgoritmoOrd[T], a2:AlgoritmoOrd[T], l:List[T]): (Int, Int) ={
        val (l1, c1) = a1(l)
        val (l2, c2) = a2(l)

        if(l1 == l2) (c1,c2) else (-1,-1)
    } 

    @main def main() = {

        def menorQue(a: Int, b: Int) = a < b  

        val (l1, l2, c) = menoresQue_noMenoresQue(List(6,5,3,2,1), 4, menorQue)

        println((l1,l2,c)) 

    }
}