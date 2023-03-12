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
  
       (#En R, por defecto, no existe una función predefinida para calcular la moda de un 
        #conjunto de datos, pero se puede crear una función personalizada para hacerlo)
  
   moda <- function(x) {
   u <- unique(x) #establecemos los valores unicos del vector
   tab <- tabulate(match(x, u)) #conteo de ocurrencias y con la función 'match' averiguamos la posicion indexada de un valor en el vector
   u[tab == max(tab)]
   } #mediante una seleccion con operadores logicos, se le indica que extraiga el subconjunto de valores unicos 
   moda(numArtefactos_int)

#Ejercicio 5: Calcula el número de veces que se repite el valor correspondiente con la moda

   table(numArtefactos_int) #por tanto, el 10 (la moda), se repite 2 veces

#Ejercicio 6:Calcula los cuartiles del objeto ‘numArtefactos_int’

   quantile(numArtefactos_int, c(0.25, 0.5, 0.75, 1))
     #Los cuartiles se expresan como porcentajes, por lo que el cuartil 1 es el percentil 25, 
     #el cuartil 2 es el percentil 50 (la mediana) y el cuartil 3 es el percentil 75.

#Ejercicio 7: Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. Interpreta el resultado.

   rango_int <- IQR(numArtefactos_int)
   rango_int #El rango intercuartílico es la diferencia entre el tercer y 
             #primer cuartil de un conjunto de datos

#Ejercicio 8: Calcula el rango del objeto ‘numArtefactos_int’. Almacena el rango en un vector denominado ‘rango_artefactos’.

   range(numArtefactos_int) #esta función devuelve un vector con el valor mínimo y máximo del objeto
   rango_artefactos <- range(numArtefactos)

#Ejercicio 9: Calcula la varianza del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo. 

    #Primera forma, las más sencilla, con la función 'var', que nos devuelve la varianza del objeto
     
      var(numArtefactos_int) 

    #Segunda forma, usando la  fórmula matemática: varianza = sum((x - mean(x))^2) / (n - 1),
    #donde x es el vector de datos, mean(x) es la media de los datos en x y n es el número de elementos en x. 

       n <- length (numArtefactos_int)
       media <- (numArtefactos_int)
       varianza <- sum((numArtefactos_int - mean(numArtefactos_int))^2) / (n - 1)
       varianza
       
#Ejercicio 10: Calcula la desviación estándar del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo
 
       #Primera forma, usando la función 'sd', que toma el objeto y devuelve su desviación estándar. 
       #La desviación estándar sirve para cuantificar la variación o la dispersión de un conjunto de datos numéricos 
       #(una desviación estándar baja indica que la mayor parte de los datos de una muestra tienden a estar agrupados 
       #cerca de su media, y una alta, que los datos se extienden sobre un rango de valores más amplio).
        
        desviación <- sd(numArtefactos_int)
        desviación
        
       #Segunda forma, usando la misma fórmula matemática que en el anterior ejercicio:
       #desviacion = sqrt(sum((x - mean(x))^2) / (n - 1)). Podemos aplicar esta fórmula utilizando 
       #las funciones sum(), mean(), length(), sqrt() y el operador ^ para elevar al cuadrado.
        
        desviación_2 <- sqrt(sum((numArtefactos_int - mean(numArtefactos_int))^2) / (n - 1))
        desviación_2

#Ejercicio 11: ¿En qué se diferencia la desviación estándar de la varianza?
 
 #Si bien la desviación estándar y la varianza son ambas medidas de dispersión empleadas para describir la variabilidad de los 
 #datos en un conjunto de observaciones, existen algunas diferencias. La varianza se expresa en unidades al cuadrado, mientras que 
 #la desviación estándar se expresa en las mismas unidades que los datos originales. Por lo tanto, la desviación estándar es más fácil 
 #de interpretar y comparar. Además, la varianaza se calcula elevando al cuadrado la diferencia entre cada valor y la media, lo que 
 #da más peso a los valores extremos. De este modo, la desviación estándar es más comúnmente utilizada en la práctica porque es más fácil 
 #de interpretar y está más estandarizada que la varianza en R.
        
#Ejercicio 12: Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’

   barplot(numArtefactos_int, horiz = TRUE, names.arg = c("A", "B", "C", "D", "E", "F,", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"))      

#Ejercicio 13: Crea un vector llamado ‘vector3’ a partir de la siguiente secuencia de valores ’21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1’

   vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

#Ejercicio 14: Calcula el coeficiente de variación de los objetos: 1)‘numArtefactos_int’ y 2) ‘vector3’. Emplea 2 funciones para su cálculo. Compara e interpreta los resultados
   
   #Primera forma: usando la siguiente fórmula matemática para dividir la desviación estándar por la media 
   #y multiplicando por 100 para expresar el resultado como un porcentaje:
   
   cv <- function(numArtefactos_int, vector3) {
     (sd(numArtefactos_int, vector3) / mean(numArtefactos_int, vector3)) * 100
   }

   cv(numArtefactos_int, vector3)
   
   #Esto indica que la variabilidad relativa de los dos vectores es del %. Un coeficiente de
   #variación más alto indica una mayor variabilidad relativa entre los datos en los vectores.
   
   #Segunda forma: utilizando la función 'coefvar()' del paquete DescTools
   install.packages("DescTools")
   library("DescTools")
   CoefVar(numArtefactos_int, vector3) 

#Ejercicio 15: Genera una tabla-resumen de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc.
   
   #podemos crear una tabla resumen de un conjunto de datos utilizando la función stats_summary()
   #Esta función proporciona la opción de personalizar un resumen estadístico básico a partir de un conjunto de datos dado
   
   install.packages("dplyr")
   library("dplyr")
   stats_summary <- function(numArtefactos_int) {
     mean <- mean(numArtefactos_int)
     median <- median(numArtefactos_int)
     sd <- sd(numArtefactos_int)
     cv <- sd / mean * 100
     iqr <- IQR(numArtefactos_int)
     range <- max(numArtefactos_int) - min(numArtefactos_int)
     var <- var(numArtefactos_int)
     
     stats_table <- data.frame(valores = c("Media", "Mediana", "Desviacion", "Rango_intercuartilico", "Rango", "Coeficiente_Variacion", "Varianza"),
                              valor = c(mean, median, sd, iqr, range, cv, var))
     return(stats_table)
   }
   stats_summary(numArtefactos_int) #Se genera una tabla-resumen
   
#Ejercicio 16: Calcula el coeficiente de asimetría del objeto ‘vector3’. Interpreta su resultado. Exponga ejemplos de distribuciones de variables con asimetría positiva y negativa y simétricas. Explique cada uno de estos escenarios. 
   
   install.packages("e1071")
   library("e1071")     
   skewness(numArtefactos_int, type = 3, na.rm = FALSE)
   #El resultado, 0.41, indica que nuestro vector tiene una asimetría positiva, ya que el 
   #coeficiente de asimetría es mayor que cero. A continuación, explicamos las distintas asimetrías:
       #Distribución con asimetría positiva: La cola de la derecha es más larga que la cola de la izquierda. 
       #Un ejemplo común de una distribución con asimetría positiva es la distribución de los salarios. La mayoría 
       #de las personas ganan salarios bajos o medios, pero algunas pocas ganan salarios muy altos, lo que hace 
       #que la distribución tenga una cola larga hacia la derecha.
   
       #Distribución con asimetría negativa: La cola de la izquierda es más larga que la cola de la derecha. 
       #Un caso frecuente es la distribución de las edades de las personas jubiladas. La mayoría de los jubilados
       #tienen edades por encima de la media y solo unas pocas personas tienen edades muy bajas.
   
       #Distribución simétrica: La distribución es simétrica cuando las colas izquierda y derecha son simétricas 
       #en torno a la media. Un ejemplo común de una distribución simétrica es la distribución normal o gaussiana.
   
   #Es importante tener en cuenta que la asimetría de una distribución puede afectar la interpretación de los estadísticos
   #descriptivos y la selección de técnicas de análisis estadístico adecuadas. 
   
#Ejercicio 17: Calcula la curtosis del objeto ‘vector3’. ¿Qué tipo de curtosis se encuentra asociada al anterior objeto? Justifica tu respuesta. 
   
   install.packages("moments")
   library(moments)   
   kurtosis(vector3)
   
   
   #La curtosis mide la cantidad de datos que se concentran alrededor de la media en comparación con la cantidad de datos que se encuentran 
   #en las colas de la distribución. Si la curtosis es mayor que cero (1,95), la distribución es más "puntiaguda" que la distribución normal 
   #(distribución con curtosis igual a cero), mientras que si la curtosis es menor que cero, la distribución es más "aplana" que la distribución normal. 
   #En nuestro caso, la curtosis asociada al vector es de tipo "puntiaguda" (también conocida como leptocúrtica) ya que su valor es mayor que cero, 
   #es decir, sus valores se concentran mucho en torno a su media