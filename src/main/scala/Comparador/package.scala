/*Angie Joya - 2322609
Emily Nuñez - 2240156*/
package object Comparador{
    type AlgoritmoOrd[T] = List[T] => (List[T], Int)
    type Comparador [T] = (T,T) => Boolean

    /*Recibe un elemento e de tipo T a insertar en una lista ordenada l de elementos
    de tipo T y devuelve , en una pareja la lista ordenada incluyendo el elemento e
    y cuantas comparaciones se hicieron para lograrlo*/
    def insert[T] (e:T, l:List[T], comp:Comparador[T]): (List[T], Int) = {
        
    }

    /*Generadora de instancias. Recibe un comparador de elementos de tipo T
    y devuelve la lista ordenada y el numero de comparaciones realizadas en una 
    pareja usando el InsertionSort. */
    def insertionSort[T] (comp:Comparador[T]): AlgoritmoOrd[T] = {
        
    }

    /*Recibe una lista de elementos de tipo T y un valor v de tipo T 
    y un comparador de elementos de T, y devuelve la lista de elementos de l
    que son menores que v, la lista de los que no son menores que v
    y cuantas comparaciones se hicieron para llegar a ella*/
    def menoresQue_noMenoresQue (l:List[T], v:T, comp:Comparador[T]) : (List[T], List[T], Int) = {
        
    }

    /*Recibe una lista de elementos de tipo T y un comparador de esos elementos
    y devuelve la lista ordenada y el numero de comparaciones realizadas en una 
    pareja Usando el quickSort*/
    def quickSort[T] (comp:Comparador[T]): AlgoritmoOrd[T] = {
        
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
}

 