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




