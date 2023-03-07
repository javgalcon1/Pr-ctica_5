                   ##PRÁCTICA 5: ESTADÍSTICA DESCRIPTIVA##

#Ejercicio 1: Crea un vector llamado ‘numArtefactos’ a partir de los siguientes valores referidos al número de artefactos por yacimiento:‘17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95’

numArtefactos <- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
typeof(numArtefactos) #son valores enteros pero aparecen como "double", por lo que debemos transformarlos a "interger"
numArtefactos_int <- as.integer(numArtefactos) #así transformamos los valores a números enteros
typeof(numArtefactos_int)
as.integer(numArtefactos)

#Ejercicio 2: Calcula la media del objeto ‘numArtefactos_int’. 
media <- mean(numArtefactos_int)
media 

#Ejercicio 3: Calcula la mediana del objeto ‘numArtefactos_int’. Define brevemente la mediana: concepto y cálculo. 
mediana <- median(numArtefactos)
mediana (#La mediana es un valor estadístico que se calcula a partir de un conjunto 
    #de datos ordenados. Corresponde al valor que ocupa la posición central en la lista ordenada. 
    #En otras palabras, es el valor que divide al conjunto de datos en dos partes iguales: 
    #la mitad inferior y la mitad superior.)

#Ejercicio 4: Calcula la moda del objeto ‘numArtefactos_int’. Explica detalladamente el procedimiento para su cálculo: empleo de funciones, operadores etc
  
(#En R, por defecto, no existe una función predefinida para calcular la moda de un conjunto de datos, 
  #pero se puede crear una función personalizada para hacerlo)
  
moda <- function(x) {
 u <- unique(x) #valores unicos del vector
 tab <- tabulate(match(x, u)) #conteo de ocurrencias y con la función 'match' averigua la posicion indexada de un valor en el vector
 u[tab == max(tab)]
} #mediante una seleccion con operadores logicos, se le indica que extraiga el subconjunto devalores unicos 
moda(numArtefactos_int)

#Ejercicio 5: Calcula el número de veces que se repite el valor correspondiente con la moda

table(numArtefactos_int) #por tanto, el 10, la moda, se repite 2 veces

#Ejercicio 6:Calcula los cuartiles del objeto ‘numArtefactos_int’

quantile(numArtefactos_int, c(0.25, 0.5, 0.75, 1))
  #Los cuartiles se expresan como porcentajes, por lo que el cuartil 1 es el percentil 25, 
  #el cuartil 2 es el percentil 50 (la mediana) y el cuartil 3 es el percentil 75.

#Ejercicio 7: Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. Interpreta el resultado.

rango_int <- IQR(numArtefactos_int)
rango_int #El rango intercuartílico es la diferencia entre el tercer y 
          #primer cuartil de un conjunto de datos

#Ejercicio 8: Calcula el rango del objeto ‘numArtefactos_int’. Almacena el rango en un vector denominado ‘rango_artefactos’.

