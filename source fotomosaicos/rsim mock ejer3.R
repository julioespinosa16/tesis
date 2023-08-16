library(dplyr)
# library(RCurl)
# library(colorspace)
library(png)
library(tidyr)
library(ggplot2)
library(data.table)
library(TSP)
library(lpSolve)
library(grid)

library(RsimMosaic)
library(stringr)
library(magick)
library(jpeg)



# Número de cuadros-renglón
dim1<- 25
# Número de cuadros-columna
dim2<- 25




# Número de cuadros-renglón
dim1<- 35
# Número de cuadros-columna
dim2<- 35



# Número de cuadros-renglón
# dim1<- 2
# Número de cuadros-columna
# dim2<- 6



# llama nombres de archivos y título de fotos
nombreTitulo<-'Mi México'
subC<-'try1'
nombrePng<-'mex_flaga'
# nombrePng<-'fran_flag'
habilitaEscrituras<- TRUE 
carpetaCat<- 'flores colores'


holguraDisposiciones<- dim1*dim2
variabilidadDisposiciones<-0


# subcarpeta-intento
val<-1 


funct2 <- function( arr_col ){
  if( class(arr_col) == 'array' ){
    return(2)
  }else{
    return(-1)
  }
}

# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}




# opción B: ordenado es por llenado por columna 
coordGe_c<- function(num) {
  # num<-16
  ye<- num%%dim2
  
  if(ye==0){
    ye<- dim2
  }
  equis<-(num-ye)/dim2+1
  
  coord<- c(equis, ye)
  return(coord)
}


modCol <- function( arr_col, funct, plotting   ){
  
  
  # arr_col <- img
  # funct <- funct
  # plotting <- TRUE
  
  
  # Obtain the dimension
  imgDm <- dim(arr_col)
  
  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    x = rep(1:imgDm[2], each = imgDm[1]),
    y = rep(imgDm[1]:1, imgDm[2]),
    R = as.vector(arr_col[,,1]),
    G = as.vector(arr_col[,,2]),
    B = as.vector(arr_col[,,3])
  )
  
  
  # Plot the image
  if(plotting){
    pr1 <- ggplot(data = imgRGB, aes(x = x, y = y)) + 
      geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
      labs(title = "Original Image: Colorful Bird") +
      xlab("x") +
      ylab("y") +
      plotTheme()
    print(pr1)
  }
  
  
  
  # kas óptimas:
  # Bandera mex: 4
  
  # x blanco: 
  
  # kClusters <- 4
  #kClusters <- 3
  # kClusters <- 2
  kClusters <- funct( arr_col )
  
  tot_col<-imgRGB %>%group_by(R, G,B)%>% summarise()
  kClusters<-min( kClusters, dim(tot_col)[1]  )
  
  
  
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
  # kColours <- rgb(kMeans$centers[kMeans$cluster,])
  
  kColours<-data.frame(rgb(kMeans$centers[kMeans$cluster,]),kMeans$centers[kMeans$cluster,])
  names(kColours)<-c('value', 'r', 'g','b')
  
  length(kColours)
  col_dist <- kColours %>% as_tibble() %>% group_by(value, r,g,b)%>%
    summarise( n = n()  ) %>% arrange(desc(n)) %>% ungroup()
  tot_n <- sum(col_dist$n)
  col_dist <-  col_dist %>% mutate( pct_n = n /tot_n  ) %>%as.matrix()
  
  
  if(plotting){
    
    pr2 <- ggplot(data = imgRGB, aes(x = x, y = y)) + 
      geom_point(colour = kColours) +
      labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
      xlab("x") +
      ylab("y") + 
      plotTheme()
    print(pr2)
  }
  
  
  return( col_dist  )
  
  
}


# lee dos archivos: resumen-información (sumF) y pixeles de las distintas fotos (frameModTot)
folder<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', val)
folderI<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', 2)
list.files(folderI)


folderJ<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', 3)
list.files(folderJ)



folderK<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', 4)
list.files(folderK)



#20-feb aquí valdría la penaincorporar los parámetros de las flores
frameModTot<- read.csv(paste0(folder, '/', 'finalInf v2.csv'), stringsAsFactors  = FALSE )




frameModTotF <- frameModTot


# Extraer dimensiones de pixel de todos los cuadros
largPix<- frameModTotF %>% group_by(num, col) %>% summarise(n=n()) %>% ungroup() %>%
  summarise(nf=unique(n))  %>% as.numeric()


# función de particionar matrices en submatrices
matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 

# Función de rotación
# rotate <- function(x) t(apply(x, 2, rev))


list.files(paste0(getwd(),"/", subC  ))

# Lectura archivo-imagen
x <- readPNG( paste0(getwd(),"/", subC ,"/",nombrePng,".png" )  )
x <- x[,,1:3]

# x: dimensión de pixeles
dim(x)

# Arreglo matricial
#x0 <- x[,,1] # will hold the grayscale values divided by 255
x0 <-x 


x0_mod<- x0
dim(x0_mod)



# determina qué pixeles dejar para que largo sea modular dim1; ancho sea modular dim2

# largo
analizR<- tibble( pos=1:( nrow( x0_mod )*2  ) ) %>% mutate(
  modR=pos%%dim1,  minT=  min(pos[ modR==0 & pos>=nrow( x0_mod )  ])  )%>% filter(pos==minT)
numR<-  nrow( x0 )-analizR$minT[1]

# Ancho
analizC<- tibble( pos=1:( ncol( x0_mod )*2  ) ) %>% mutate(
  modC=pos%%dim2,  minT=  min(pos[ modC==0 & pos>=ncol( x0_mod )  ])  )%>% filter(pos==minT)
numC<-ncol( x0 )- analizC$minT[1]


# Delimitación de pixeles 
if(numR>0){
  x0_modN<- x0[  -c((1:(numR/2) ), ( (dim(x0)[1]-(  (numR/2)  -1)  )  :dim(x0)[1] )  ),
                 ,
                 ,
  ]
}else{
  x0_modN<- x0
}

if(numC>0){
  x0_modN<- x0_modN[ ,
                     -c((1: (numC/2) ), ( (dim(x0)[2]-(  (numC/2) -1)  )  :dim(x0)[2] )  )
                     ,
  ]
}else{
  x0_modN<- x0_modN
}


dim(x0_modN)


# Terminar de acotar matriz
rowN<- tibble(  pos=1:(nrow(x0_modN)) ) %>% mutate(modF=  pos%%dim1==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()
colN<- tibble(  pos=1:(ncol(x0_modN)) ) %>% mutate(modF=  pos%%dim2==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()

x0_modN<- x0_modN[1:rowN,1:colN, ]
dim(x0_modN)

# Desplegar imagen en tonos grises
# x0N_img<- rotate(x0_modN)
# image(x0N_img, col  = gray((0:255)/255)) # plot in grayscale

grid.raster(x0_modN, interpolate=FALSE)




source <- frameModTotF

col_cand<-uniqueN(frameModTotF$num)
frameModTotF3D<-list()


for(sel_n in 1:col_cand){
  
  
  # sel_n <- 2
  el_r <- frameModTotF %>% filter(num==sel_n & col=='r') %>%
    select(-num, -col)%>% as.matrix()
  el_g <- frameModTotF %>% filter(num==sel_n & col=='g') %>%
    select(-num, -col)%>% as.matrix()
  el_b <- frameModTotF %>% filter(num==sel_n & col=='b') %>%
    select(-num, -col)%>% as.matrix()
  
  dim(el_r)
  dim(el_g)
  dim(el_b)
  
  d<-min(dim(el_g))
  frameArray = rep(NA, d*d*3)
  dim(frameArray) = c(d,d,3 )
  frameArray[,,1]<-el_r
  frameArray[,,2]<-el_g
  frameArray[,,3]<-el_b
  
  grid.raster(frameArray, interpolate=FALSE)
  
  frameModTotF3D[[sel_n]]<-frameArray
  
}
length(frameModTotF3D)
dim(frameModTotF3D[[1]])

modCol(frameModTotF3D[[1]], funct2,FALSE) 

dim(frameModTotF3D[[6]])
length(frameModTotF3D)
# promediosFigCol<- sapply(frameModTotF3D, function(x)modCol(x, funct2,FALSE)   )%>% as.matrix()
# 25-feb aquí guardamos coordenadas de color promedio de una dada ficha####
promediosFigCol<- tibble()
frameModTotF

for(xx in 1:length(frameModTotF3D)){
  prom<-modCol(frameModTotF3D[[xx]], funct2,FALSE)
  coord_col<-as.numeric( prom[1,c('r','g', 'b')])
  coord_col<-as_tibble(t(coord_col))
  coord_col<-tibble(num=xx,coord_col)
  
  if(xx==1){
    coord_col_fichs<-coord_col
  }else{
    coord_col_fichs<-rbind(coord_col_fichs, coord_col)
  }
}

# apply(frameModTotF3D,1, function(x)modCol(x, funct2,FALSE)   )
# promediosFigCol[[1]]

# Desplegar imagen en tonos grises
# x0N_img<- rotate(x0_modN)
# image(x0N_img, col  = gray((0:255)/255))
# plot in grayscale


dim(x0_mod)
# Convertir escala [0,1] continua a {0,1,...,254, 255} entera
x0_esc1<- x0_mod[,,1] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_esc2<- x0_mod[,,2] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_esc3<- x0_mod[,,3] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)



x0_escN1<- x0_modN[,,1] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_escN2<- x0_modN[,,2] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_escN3<- x0_modN[,,3] %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
# dim(x0_esc)


# Muestra valores dentro de la escala existentes
# valoresExistente<-unique(unlist(x0_esc))
# valoresExistente






# Extraemos las matrices disponibles en un array 3-dimensional
# matrices<-matsplitter(x0_escN,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)



# Extraemos las matrices disponibles en un array 3-dimensional
matrices1<-matsplitter(x0_escN1,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)
matrices2<-matsplitter(x0_escN2,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)
matrices3<-matsplitter(x0_escN3,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)


# 22-feb: revisar estas dimensiones


# Matrices en lista
listmatsG<-list()
listMats1<- list()
listMats2<- list()
listMats3<- list()

for(  ls in  1:dim(  matrices1)[3] ){
  
  dim(matrices1[,,ls])
  primero<-rep(NA, dim(matrices1[,,ls])[1]*dim(matrices1[,,ls])[2]*3)
  dim(primero)<-c(dim(matrices1[,,ls])[1],dim(matrices1[,,ls])[2],3)
  
  listMats1[[ls]]<-   matrices1[,,ls]
  listMats2[[ls]]<-   matrices2[,,ls]
  listMats3[[ls]]<-   matrices3[,,ls]
  
  primero[,,1]<-listMats1[[ls]]/255
  primero[,,2]<-listMats2[[ls]]/255
  primero[,,3]<-listMats3[[ls]]/255
  
  listmatsG[[ls]]<-primero
}




promediosPart1<- sapply(listMats1, function(x)mean(x)   )


# modCol( arr_col = , funct = funct2, plotting=FALSE)
# mu: valor de luminosidad promedio de todas las sumbatrices
promediosPartCol<- sapply(listmatsG, function(x)modCol(x, funct2,FALSE)   )




coord_col_real<- tibble()

for(xx in 1:(dim1*dim2) ){
  prom<-modCol(listmatsG[[xx]], funct2,FALSE)
  coord_col<-as.numeric( prom[1,c('r','g', 'b')])
  coord_col<-as_tibble(t(coord_col))
  coord_col<-tibble(num=xx,coord_col)
  
  if(xx==1){
    coord_col_real<-coord_col
  }else{
    coord_col_real<-rbind(coord_col_real, coord_col)
  }
}


# promediosPart<- sapply(listMats, function(x)mean(x)   )
geij<- coord_col_real#%>% mutate_at(-1,funs(replace(.,is.numeric(.), . ))) 






# Generar arreglo 3-dimensional de distancias (renglón x columna x figuar)
arr3 = rep(NA, dim1*dim2* uniqueN(frameModTotF$num) )
dim(arr3) = c(dim1,dim2, uniqueN(frameModTotF$num) )




matObj<- matrix(0, nrow=dim1, ncol = dim2)

# Generar arreglo 3-dimensional de distancias (renglón x columna x figuar)
matObj = rep(NA, dim1*dim2*3 ) 

dim(matObj) = c(dim1,dim2,3 )





# Recorrido por los dim1 x dim2 cuadros generados
for(  k in 1:nrow(geij
                  # [1:floor(length(geij) /2 )  ]
) ){
  # k<-1
  print(paste0('coordenada ', k))
  
  osc<- geij[k,-1]
  
  # Asginación k- coordendas i, j 
  cua<-coordGe_c(k)
  # cua<-coordGe(k)
  ele1<-geij[k,-1]
  ele2<-coord_col_fichs[,-1]
  
  euclidian <- function (a, b) sqrt ( sum ((a - b) ^ 2))
  euclidian(ele1[1,], ele2[1,] )
  
  
  
  # cuadrado de distancia (en luminosidad) de la celda a las distintas figuras
  difIj<- apply(ele2 ,1, function(x)euclidian(x,ele1)  )
  dim(arr3)
  arr3[ cua[1], cua[2],   ]<- difIj
  
  # Oscuridad objetivo
  matObj[ cua[1], cua[2],   ]<- ele1%>%as.matrix()
  
  print(max(ele1))
}


grid.raster(matObj, interpolate=FALSE)



writeJPEG(matObj, target = 'mex_fl0.jpg', quality = 0.7, bg = "white", color.space=NULL)
writeJPEG(matObj, target = 'mex_fl0.jpeg', quality = 0.7, bg = "white", color.space=NULL)

dim(readJPEG('mex_fl0.jpeg'))



composeMosaicFromImageRandomOptim("mex_fl0.jpeg", "finalfile0.jpg",
                                  pathToTileLib, useGradients = FALSE, removeTiles = TRUE,
                                  fracLibSizeThreshold = 0.7, repFracSize = 0.25, verbose = TRUE, neig=20)




# composeMosaicFromImageRandomOptim("mex_fl0.jpg", "finalfile1.jpg",
#                                   folderJ, useGradients = FALSE, removeTiles = TRUE,
#                                   fracLibSizeThreshold = 0.7, repFracSize = 0.25, verbose = TRUE, neig=20)
# 


iniciaP<- proc.time()


# composeMosaicFromImageRandomOptim("mex_fl0.jpg", "finalfile1.jpg",
#                                   folderK, useGradients = FALSE, removeTiles = TRUE,
#                                   fracLibSizeThreshold = 0.7, repFracSize = 0.25, verbose = TRUE, neig=20
#                                   )
composeMosaicFromImageRandomOptim("mex_fl0.jpg", "finalfile1.jpg",
                                  folderK, useGradients = FALSE
                                  , repFracSize = 0.25, verbose = TRUE, neig=20
                                  ,removeTiles = FALSE,
                                  fracLibSizeThreshold = 0.01 )


tEjec<-(proc.time()- iniciaP)[3]


# composeMosaicFromImageRandomOptim("twitter.jpg", "finalfile2.jpg",
#                                   folderJ, useGradients = FALSE, removeTiles = TRUE,
#                                   fracLibSizeThreshold = 0.7, repFracSize = 0.25, verbose = TRUE, neig=20)
# 


# dim(
# readJPEG("C:/Users/Julio/AppData/Local/R/win-library/4.2/RsimMosaic/extdata/2Massier/m2.jpg")
# )
# dim(
#   readJPEG("C:/Users/Julio/AppData/Local/R/win-library/4.2/RsimMosaic/extdata/2Massier/m3.jpg")
# )



grid.raster(frameModTotF3D[[1]])
writeJPEG(frameModTotF3D[[1]], target = 'fl_bl_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)

grid.raster(frameModTotF3D[[2]])
writeJPEG(frameModTotF3D[[2]], target = 'fl_ve_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)

grid.raster(frameModTotF3D[[3]])
writeJPEG(frameModTotF3D[[3]], target = 'fl_ro_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)


grid.raster(frameModTotF3D[[4]])
writeJPEG(frameModTotF3D[[4]], target = 'fl_ca_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)

grid.raster(frameModTotF3D[[5]])
writeJPEG(frameModTotF3D[[5]], target = 'fl_na_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)

grid.raster(frameModTotF3D[[6]])
writeJPEG(frameModTotF3D[[6]], target = 'fl_ros_tot.jpeg', quality = 0.7, bg = "white", color.space=NULL)



# dim(readJPEG('fl_bl_tot.jpeg'))
# dim(readJPEG('fl_ve_tot.jpeg'))
# dim(readJPEG('fl_ro_tot.jpeg'))




list.files(folderI)
list.files(folderJ)


dim(
readJPEG(
"C:/Users/Julio/Documents/th tesis/flores colores/flores coloresProp_3/fl_bl_tot.jpeg"
))
dim(
  readJPEG(
    "C:/Users/Julio/Documents/th tesis/flores colores/flores coloresProp_3/fl_ve_tot.jpeg"
  ))
dim(
  readJPEG(
    "C:/Users/Julio/Documents/th tesis/flores colores/flores coloresProp_3/fl_ro_tot.jpeg"
  ))


dim(
  readJPEG(
    "C:/Users/Julio/Documents/th tesis/flores colores/flores coloresProp_4/fl_na_tot.jpg"
  ))



dim(
  readJPEG(
    "C:/Users/Julio/AppData/Local/R/win-library/4.2/RsimMosaic/extdata/2Massier/m2.jpg"
  ))





