#install.packages("readxl")
library(igraph)
library(readxl)

#' @title Generar Grafo
#' @description Función que genera un grafo a a partir de una matriz de adyacencia contenida
# en un archivo CSV.
#' @param  archivo_datos ruta al archivo CSV que contiene la matriz de adyacencia.
#' La matriz tiene que tener cabecera tanto horizontal como vertical.
#' @param separador caracter que separa los campos en el CSV, por defecto ,
#' @return el grafo correspondiente a la matriz de adyacencia.
#' @export
generar_grafo <-function(archivo_datos,separador=","){
  # leemos el archivo CSV
  datos_relaciones <-read.csv(archivo_datos,sep = separador)
  # quitamos la cabecera vertical y convertimos a tipo de dato matriz
  lista_nodos <- names(datos_relaciones)[-1]
  matriz_relaciones<-as.matrix(datos_relaciones)[,-1]
  # ponemos nombre a las filas y columnas de la matriz
  rownames(matriz_relaciones) <- colnames(matriz_relaciones) <- lista_nodos
  # creamos el grafo
  grafo <-graph.adjacency(matriz_relaciones,mode="undirected",diag=FALSE);
  return(grafo)
}


#' @title Generar Estadísticas
#'
#' @description Función que genera las principales características y propiedades de un grafo
# exportándolas a un archivo .pdf  y otro .txt.
#' @param  grafo  objeto tipo grafo de igraph del que queremos extraer sus características.
#' @param salida_pdf ruta al archivo .pdf donde se dibujarán los grafos.
#' @param salida_txt ruta al archivo .txt donde se escribirá el resto de información
#' @export
generar_estadisticas <-function(grafo,salida_pdf,salida_txt){
  # abrimos archivo pdf para escritura
  pdf(salida_pdf)
  # fijamos la estructura que tendra el grafo al dibujarlo
  layout_comp <-layout_components(grafo)
  # abrimos archivo txt para escritura
  salida_txt_punt <-file(salida_txt,"a")
  E(grafo)$color <- "red"
  #dibujamos el grafo sin nombres
  plot(grafo,vertex.size=2,vertex.label=NA,layout=layout_comp,main="Estructura del grafo sin nombres")
  # escribimos los grados de los vértices y pintamos el grafo poniendo
  # nombre a los vértices con mayor grado
  degrees <-sort(degree(grafo,mode="total"))
  nombres_en_grafo <- rep(c(NA),vcount(grafo))
  for(i in (length(degrees)-3):length(degrees)){
    nombres_en_grafo[which(V(grafo)$name == names(degrees[i])) ] <- names(degrees[i])
  }
  plot(grafo,vertex.size=2,vertex.label=nombres_en_grafo,layout=layout_comp,main="Grafo con nombre de los vértices con mayor grado")
  # escribimos los grados de los vértices
  write("Grados:",salida_txt_punt)
  write.table(sort(degree(grafo,mode="total")),salida_txt_punt)
  # escribimos el grado máximo
  write("Grado máximo:",salida_txt_punt)
  write(max(degrees),salida_txt_punt)
  #escribimos el grado mínimo
  write("Grado mínimo:",salida_txt_punt)
  write(min(degrees),salida_txt_punt)
  #escribimos la distribución del grado
  write.table("\n\nDistribuccion del grado:",salida_txt_punt)
  write(degree.distribution(grafo),salida_txt_punt)
  # escribimos la centralidad por intermediaccion de los vértices y pintamos el grafo poniendo
  # nombre a los vértices con mayor centralidad por intermediacción
  medida_betweenness <-sort(betweenness(grafo,normalized = T))
  write("Centralidad por intermediacción:",salida_txt_punt)
  write.table(medida_betweenness,salida_txt_punt)
  nombres_en_grafo <- rep(c(NA),vcount(grafo))
  for(i in (length(medida_betweenness)-3):length(medida_betweenness)){
    nombres_en_grafo[which(V(grafo)$name == names(medida_betweenness[i]))] <-names(medida_betweenness[i])
  }
  plot(grafo,vertex.size=2,vertex.label=nombres_en_grafo,layout=layout_comp,main="Grafo con nombre de los vértices con mayor centralidad por intermediacción")
  # escribimos la centralidad por cercanía de los vértices y pintamos el grafo poniendo
  # nombre a los vértices con mayor centralidad por cercanía
  medida_closeness <-  sort(closeness(grafo,normalized = T))
  write("Centralidad por cercanía:",salida_txt_punt)
  write.table(medida_closeness,salida_txt_punt)
  nombres_en_grafo <- rep(c(NA),vcount(grafo))
  for(i in (length(medida_closeness)-3):length(medida_closeness)){
    nombres_en_grafo[which(V(grafo)$name == names(medida_closeness[i])) ] <-names(medida_closeness[i])
  }
  plot(grafo,vertex.size=2,vertex.label=nombres_en_grafo,layout=layout_comp,main="Grafo con nombre de los vértices con mayor centralidad por cercanía")
  # escribimos la centralidad por autovalores de los vértices y pintamos el grafo poniendo
  # nombre a los vértices con mayor centralidad por autovalores
  medida_autovalores<-sort(eigen_centrality(grafo,scale = F)$vector)
  write("Centralidad por autovalores:",salida_txt_punt)
  write.table(medida_autovalores,salida_txt_punt)
  #escribimos la distancia media del grafo
  write("Distancia media:",salida_txt_punt)
  write( mean_distance(grafo),salida_txt_punt)
  # escribimos la medida de excentricidad y pintamos el grafo poniendo
  #nombre a los vértices con mayor medida de excentricidad.
  # la excentricidad de un vértice es la distancia más grande entre
  # este y el resto de vértices del grafo.
  medida_excentricidad <- sort(eccentricity(grafo,mode="all"))
  write("Excentricidad:",salida_txt_punt)
  write.table(medida_excentricidad ,salida_txt_punt)
  nombres_en_grafo <- rep(c(NA),vcount(grafo))
  for(i in (length(medida_excentricidad)-3):length(medida_excentricidad)){
    nombres_en_grafo[which(V(grafo)$name == names(medida_excentricidad[i])) ] <-names(medida_excentricidad[i])
  }
  plot(grafo,vertex.size=2,vertex.label=nombres_en_grafo,layout=layout_comp,main="Grafo con nombre de los vértices con mayor excentricidad")
  # escribimos el diámetro del grafo
  # el diámetro es el máximo de las excentricidades
  write("Diámetro:",salida_txt_punt)
  write(diameter(grafo) ,salida_txt_punt)
  # escribimos la densidad del grafo
  write("Densidad:",salida_txt_punt)
  write(edge_density(grafo) ,salida_txt_punt)
  # escribimos el coeficiente de transitividad del grafo.
  write("Transitividad:",salida_txt_punt)
  write(transitivity(grafo) ,salida_txt_punt)

  # separacion en comunidades usando el algortimo de Louvain
  cluster <- cluster_louvain(grafo)
  plot(cluster,grafo,vertex.size=2,layout=layout_comp)
  write("Separación en comunidades:",salida_txt_punt)
  write.table(sort(membership(cluster)),salida_txt_punt)
  # separacion en comunidades usando otras técnicas

  # separacion en comunidades en función de
  # la centralidad por intermediacción de los enlaces
  cluster <-cluster_edge_betweenness(grafo)
  plot(cluster,grafo,vertex.size=2,layout=layout_comp)
  # pintamos el dendograma
  dendPlot(cluster,mode="hclust",rect=2)
  write("Separacion en comunidades:",salida_txt_punt)
  write.table(sort(membership(cluster)),salida_txt_punt)

  # separación en comunidades en función de la técnica
  # de propagación de etiquetas
  cluster <-label.propagation.community(grafo)
  plot(cluster,grafo,vertex.size=2,layout=layout_comp)
  #cerramos los dos archivos
  dev.off()
  close(salida_txt_punt)
}


#' @title Crear grafo a partir de lista de enlaces.
#'
#' @description Función que genera un grafo a a partir de la lista de enlaces entre vértices.
#' @param  fichero  ruta al archivo que contiene la lista de enlaces, puede ser .txt o .csv.
#'  Cada fila del fichero contiene los dos vértices que forman cada enlace.
#' @param identificadores booleano que indica si el identificador de cada vértice es númerico
#' o no. En caso de ser númerico la función lo convierte a carácteres.
#' @param sep carácter que separa en el fichero los dos vértices de cada enlace. Por defecto será
#' uno o más espacios en blanco
#' @return  el grafo correspondiente a la lista de enlaces.
#' @export
crear_grafo_desde_tabla_enlaces <-function(fichero,identNumerico,sep=""){
  enlaces<- read.table(fichero, header=F,sep)
  enlaces <-as.matrix(enlaces)
  if(identNumerico == FALSE){
    grafo <-graph_from_edgelist(enlaces,directed =FALSE)
    return(grafo)
  }
  else{
    enlaces[,1] <-as.character(enlaces[,1])
    enlaces[,2] <-as.character(enlaces[,2])
    grafo <-graph_from_edgelist(enlaces,directed =FALSE)
    return(grafo)
  }
}

#' @title Subgrafo inducido.
#'
#' @description Función que crea una muestra de un grafo con la técnica de muestreo inducido.
#' @param  grafo  grafo que se quiere muestrear .
#' @param lonMuestra tamaño del conjunto inicial de vértices muestreados.
#' @return  subgrafo muestreado.
#' @export

crear_subgrafo_inducido <- function(grafo ,lonMuestra){
  inducido <- induced.subgraph(grafo,vids = sample(V(grafo),lonMuestra))
  return(inducido)
}



#' @title Subgrafo incidente.
#'
#' @description Función que crea una muestra de un grafo con la técnica de muestreo incidente.
#' @param  grafo  grafo que se quiere muestrear .
#' @param lonMuestra  tamaño del conjunto inicial de enlaces de la muestra.
#' @return  subgrafo muestreado.
#' @export

crear_subgrafo_incidente <- function(grafo ,lonMuestra){
  incidente <- subgraph.edges(grafo,sample(E(grafo),lonMuestra))
  return(incidente)
}

#' @title Función auxiliar.
#'
#' @description Función auxiliar utilizada para el muestreo estrella.
#' @param  x  .
#' @return  .
#' @export
duplicar <- function(x){
  return(rep(x[[1]]$name,length(x)))
}

#' @title Subgrafo estrella.
#'
#' @description Función que crea una muestra de un grafo con la técnica de muestreo estrella.
#' @param  grafo  grafo que se quiere muestrear .
#' @param lonMuestra  tamaño del conjunto inicial de vértices de la muestra.
#' @return  subgrafo muestreado.
#' @export

crear_subgrafo_estrella <- function(grafo,lonMuestra){
  # Cálculamos el conjunto de vértices inicial y sus vecinos
  vecindario <-neighborhood(grafo,order=1,nodes = sample(V(grafo),lonMuestra))
  # vecindario contendrá una lista donde cada entrada es una lista que contiene los vértices adyacentes a uno de
  # los vértices muestreados en e conjunto inicial
  # calculamos la lista de enlaces del subgrafo
  listado1 <- unlist(lapply(vecindario,duplicar))
  listado2 <-names(unlist(vecindario))
  listado <- data.frame(listado1,listado2)
  #eliminamos enlaces duplicados de la lista
  listado$listado1 <- ifelse(listado1 < listado2,as.character(listado1),as.character(listado2))
  listado$listado2 <- ifelse(listado1 < listado2,as.character(listado2),as.character(listado1))
  listado <-listado[which(duplicated(listado) == F),]
  #eliminamos bucles
  listado <-listado[which(listado$listado1 != listado$listado2),]
  # creamos lista con los vértices que tendra el grafo
  vertices <-c(listado$listado1,listado$listado2)
  vertices <- vertices[!duplicated(vertices)]
  grafo_estrella <-graph_from_data_frame(d=listado, vertices=vertices, directed=FALSE)
  return(grafo_estrella)
}


#' @title Estimador del número de enlaces con muestreo inducido.
#'
#' @description Estimador del número de enlaces de un grafo. Usa una muestra del grafo
#' generada mediante muestreo inducido y el estimador de Hortvitz-Thompson.
#' @param inducido subgrafo generado mediante muestreo inducido.
#' @param numVertices número del vértices del grafo original.
#' @return  estimación del número de enlaces.
#' @export

estimacion_num_enlaces_con_subgrafo_inducido<-function(inducido,numVertices){
  numVerticesInd <- vcount(inducido)
  numEnlacesInd <- ecount(inducido)
  return(numEnlacesInd/((numVerticesInd*(numVerticesInd-1))/(numVertices*(numVertices-1))))
}


#' @title Estimador del número de enlaces con muestreo estrella.
#'
#' @description Estimador del número de enlaces de un grafo. Usa una muestra del grafo
#' generada mediante muestreo estrella y el estimador de Hortvitz-Thompson.
#' @param  estrella subgrafo generado mediante muestreo estrella.
#' @param numVertices número del vértices del grafo original.
#' @param lonMuestra0: tamaño del conjunto inicial de vértices utilizado
#' para el muestreo estrella.
#' @return  estimación del número de enlaces.
#' @export

estimacion_num_enlaces_con_subgrafo_estrella<-function(estrella,numVertices,lonMuestra0){
  probabilidad <- as.numeric(1 -(as.bigq(chooseZ((numVertices-2),lonMuestra0)/chooseZ(numVertices,lonMuestra0))))
  numEnlacesEst <- ecount(estrella)
  return(numEnlacesEst/probabilidad)
}

#' @title Estimador del grado medio del grafo con muestreo incidente
#'
#' @description Estimador del grado medio del grafo. Usa una muestra del grafo
#' generada mediante muestreo incidente y Hortvitz-Thompson.
#' @param  incidente subgrafo generado mediante muestreo incidente.
#' @param  numEnlaces  número de enlaces del grafo original.
#' @param numVertices número del vértices del grafo original.
#' @param distribuccionesInc lista con los grados de los vértices muestreados.
#' @return  estimación del grado medio de los vértices del grafo.
#' @export

estimacion_grado_medio_con_subgrafo_incidente<-function(incidente,numEnlaces,numVertices,distribuccionesInc){
  numEnlacesInc <- ecount(incidente)
  #calculamos las probabilidades que tiene cada vertice de ser incluido
  probabilidades <-1 -(as.bigq(chooseZ((numEnlaces -distribuccionesInc),numEnlacesInc)/chooseZ(numEnlaces,numEnlacesInc)))
  return(horvitzThompson(y = distribuccionesInc, pi = as.numeric(probabilidades), N=numVertices)[2])
}

#' @title Estimador del grado medio del grafo con muestreo estrella
#'
#' @description Estimador del grado medio del grafo. Usa una muestra del grafo
#' generada mediante muestreo estrella y el estimador de Hortvitz-Thompson.
#' @param  estrella subgrafo generado mediante muestreo estrella.
#' @param numVertices número del vértices del grafo original.
#' @param distribuccionesEst lista con los grados de los vértices muestreados.
#' @param lonMuestra0 tamaño del conjunto inicial de vértices utilizado
#' para el muestreo estrella.
#' @return estimación del grado medio de los vértices del grafo.
#' @export

estimacion_grado_medio_con_subgrafo_estrella<-function(estrella,numVertices,distribuccionesEst,lonMuestra0){
  verticesEst <- V(estrella)
  tamanoVecindario <- distribuccionesEst +1
  #calculamos las probabilidades que tiene cada vertice de ser incluido
  probabilidades <-1 -(as.bigq(chooseZ((numVertices-tamanoVecindario),lonMuestra0)/chooseZ(numVertices,lonMuestra0)))
  return(horvitzThompson(y = distribuccionesEst, pi = as.numeric(probabilidades), N=numVertices)[2])
}


#' @title Estimador del número de vértices del grafo con muestreo incidente
#'
#' @description Estimador del número de vértices del grafo. Usa una muestra del grafo
#' generada mediante muestreo incidente y el estimador de  Hortvitz-Thompson.
#' @param incidente subgrafo generado mediante muestreo incidente.
#' @param numEnlaces número de enlaces del grafo original.
#' @param distribuccionesInc lista con los grados de los vértices muestreados.
#' @return estimación del número de vértices del grafo.
#' @export

estimacion_num_vertices_con_subgrafo_incidente <-function(incidente,numEnlaces,distribuccionesInc){
  numEnlacesInc <- ecount(incidente)
  numVerticesInc <- vcount(incidente)
  probabilidades <-1 -(as.bigq(chooseZ((numEnlaces -distribuccionesInc),numEnlacesInc)/chooseZ(numEnlaces,numEnlacesInc)))
  return(horvitzThompson(y =rep(1,numVerticesInc), pi = as.numeric(probabilidades))[1])
}



#' @title Estimador del número de vértices del grafo con captura-recaptura
#'
#' @description Estimador del número de vértices del grafo usando la
#' técnica de captura-recaptura.
#' @param grafo grafo del que se quiere calcular el número de vértices.
#' @param lonMuestra1 longitud del primer conjunto de vértices muestrado.
#' @param lonMuestra2 longitud del segundo conjunto de vértices muestreado.
#' @return estimación del número de vértices del grafo.
#' @export

estimacion_num_vertices_cap_recaptura <- function(grafo,lonMuestra1,lonMuestra2){
  muestra1 <- sample(V(grafo),lonMuestra1)
  muestra2 <- sample(V(grafo),lonMuestra2)
  iguales <- length(which(muestra1 %in% muestra2))
  estimacion_vertices_captura <- (lonMuestra1*lonMuestra2)/ iguales
  return(estimacion_vertices_captura)
}

#' @title Estimador del número de vértices del grafo con muestreo estrella.
#'
#' @description Estimador del número de vértices del grafo. Usa la técnica de estimación
#' del tamaño de poblaciones con muestreo estrella, que no utiliza el estimador de Hortvitz-Thompson.
#' @param grafo grafo del que se quiere calcular el número de vértices .
#' @param p0 probabilidad de inclusión de cada vértice en la muestra inicial.
#' @return estimación del número de vértices del grafo.
#' @export

estimacion_num_vertices_estrella_sin_HT <- function(grafo,p0){
  #seleccionamos la muestra inicial con muestreo bernoulli
  muestreados <- rbernoulli(vcount(grafo),p0)
  #calculamos m1, enlaces entre vertices de V0*
  enlaces_internos <- ecount(induced.subgraph(grafo,vids = V(grafo)[muestreados]))
  #creamos el subgrafo estrella etiquetado
  vecindario <-neighborhood(grafo,order=1,nodes = V(grafo)[muestreados])
  # calculamos la lista de enlaces del subgrafo
  listado1 <- unlist(lapply(vecindario,duplicar))
  listado2 <-names(unlist(vecindario))
  listado <- data.frame(listado1,listado2)
  #eliminamos enlaces duplicados de la lista
  listado$listado1 <- ifelse(listado1 < listado2,as.character(listado1),as.character(listado2))
  listado$listado2 <- ifelse(listado1 < listado2,as.character(listado2),as.character(listado1))
  listado <-listado[which(duplicated(listado) == F),]
  #eliminamos bucles de la lista
  listado <-listado[which(listado$listado1 != listado$listado2),]
  # creamos lista con los vertices que tendrá el grafo
  vertices <-c(listado$listado1,listado$listado2)
  vertices <- vertices[!duplicated(vertices)]
  grafo_estrella <-graph_from_data_frame(d=listado,vertices = vertices, directed=FALSE)
  #calculamos m2 enlaces entre elementos de V0* y V1*
  enlaces_externos <- ecount(grafo_estrella) - enlaces_internos
  probabilidad <- 2*enlaces_internos/((enlaces_externos + (2*enlaces_internos)))
  estimacion_vertices <- sum(muestreados)/probabilidad
  return(estimacion_vertices)
}






