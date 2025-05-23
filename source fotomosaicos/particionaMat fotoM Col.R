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



# Número de cuadros-renglón
dim1<- 10
# Número de cuadros-columna
dim2<- 15




# Número de cuadros-renglón
# dim1<- 20
# Número de cuadros-columna
# dim2<- 30



# Número de cuadros-renglón
dim1<- 2
# Número de cuadros-columna
dim2<- 6



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




#20-feb aquí valdría la penaincorporar los parámetros de las flores
frameModTot<- read.csv(paste0(folder, '/', 'finalInf.csv'), stringsAsFactors  = FALSE )
# sumF<- read.csv(paste0(folder, '/', 'parametrosSelFin.csv'), stringsAsFactors = FALSE )

# Reasignar número de cuadro
# sumF<- sumF %>% mutate(FIG=row_number())

# selección final de fotos a ocupar
# frameModTotF <- frameModTot %>% filter(num %in% sumF$num) %>%
#   mutate(rn=row_number()) %>% relocate(rn)
# frameModTotF<- merge(sumF %>%select(num, FIG), frameModTotF, 
#                      by='num') %>% arrange(rn)

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
rotate <- function(x) t(apply(x, 2, rev))


list.files(paste0(getwd(),"/", subC  ))

# Lectura archivo-imagen
x <- readPNG( paste0(getwd(),"/", subC ,"/",nombrePng,".png" )  )
x <- x[,,1:3]

# x: dimensión de pixeles
dim(x)

# Arreglo matricial
#x0 <- x[,,1] # will hold the grayscale values divided by 255
x0 <-x 


# Rotación y visualización de imagen en grayscales
# x0_img<- rotate(x0)
# image(x0_img, col  = gray((0:255)/255)) 
# plot in grayscale
# image(x0_img) 



# Cuántos pixeles remover en cada dimensión (largo y ancho)
# libertadadPar<-40
# 
# 
# if(libertadadPar>0){
#   x0_mod<- x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
#               -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
#   ]
# }else{
#   x0_mod<- x0
# }
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


# dim(listmatsG[[5]])

# length(listmatsG)
# grid.raster(listmatsG[[3]], interpolate=FALSE)
# 
# listmatsG[[5]][,,1]%>% View()
# listmatsG[[5]][,,2]%>% View()
# listmatsG[[5]][,,3]%>% View()
# 
# 
# 
# mean(listmatsG[[5]][,,1])
# mean(listmatsG[[5]][,,2])
# mean(listmatsG[[5]][,,3])


# modCol( arr_col = , funct = funct2, plotting=FALSE)
# mu: valor de luminosidad promedio de todas las sumbatrices
promediosPartCol<- sapply(listmatsG, function(x)modCol(x, funct2,FALSE)   )




#modCol(listmatsG[[10]], funct2,FALSE)
#promediosPartCol
#promediosPartCol[[10]]


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




for(  y in 1:dim(arr3)[3] ){
  
  # y<-1
  tbl<- arr3[,,y ] %>% as_tibble()
  
  if(y==1){
    tblF<- tbl
  }else{
    tblF<-rbind( tblF, tbl)
  }
  # print(tbl)
  
}



# Indicar si las dimensiones son correctas y
# los valores se establecen en el rango correcto
dimCorr<- FALSE
if( sum( sapply(tblF, function(x)sum(is.na(x) & between(x, 0, 1)  )  )  ) ==0 &
    dim(arr3)[1]*dim(arr3)[2]*dim(arr3)[3]==nrow(tblF)*ncol(tblF)
    & dim(arr3)[1]*dim(arr3)[2]>dim(arr3)[3]
){
  dimCorr<- TRUE
}








if(dimCorr){
  
  
  # Vector de disponibilidades
  finalDisp<- c()
  
  # Disposicones que sean más que las justas para llenar los espaciones
  disposiciones<- floor(dim(arr3)[1]*dim(arr3)[2] * (1+holguraDisposiciones) )
  
  
  for(kk in 1: dim(arr3)[3]){
    
    # añadir componente de variabilidad (qué tanto más o menos del promedio puedo disponer)
    vector<- floor(  (disposiciones/dim(arr3)[3])  *(1- variabilidadDisposiciones) ): 
      floor(  (disposiciones/dim(arr3)[3]  )*(1+ variabilidadDisposiciones) )  
    set.seed(kk+1)
    elemAdd<-sample(vector, 1)
    
    finalDisp<- c(finalDisp,elemAdd )
  }
  
  # frame disponibilidades
  finalDispDf<- tibble(quant=finalDisp) %>% mutate(FIG=row_number()) %>% relocate(FIG)
  
  # Construcción del data frame de distancias
  distancias<- tibble()
  
  for(jj in 1: dim(arr3)[3]){
    # jj<-1
    
    
    # Matriz de la jj-ésimo figura
    mat<- arr3[,,jj]%>%as.matrix()
    if( dim(arr3)[1]==1 ){
      mat<-mat %>% t() %>%as.matrix()
    }
     
    
    for(  ll in 1:dim(mat)[1]  ){
      
      
      # ll-ésimo renglón
      renVal<- mat[ll, ]
      
      # información con costos (luminosidad)
      dist0<- tibble(FIG=jj, ROW=ll, COL=1:length(renVal), cost=renVal)
      
      if(nrow(  distancias )==0  ){
        distancias<-dist0
      }else{
        distancias<- rbind(distancias, dist0)
      }
      
    }
  }
  
  
  distancias %>% group_by(FIG) %>% summarise(n())#%>%View()
  distancias %>% group_by(ROW, COL) %>% summarise(n())#%>%View()
  distancias %>% group_by(FIG,ROW, COL) %>% summarise(n())#%>%View()
  
  
  # Escritura de df's de costos y de disponibilidades en csv's
  
  # 27-feb Reevaluar si merece la pena la existencia de estas variables####
  nombreCostos<- paste0('costoFotoMos', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
  nombreCostos<- paste0(folder, '/', nombreCostos)
  
  nombreDisp<- paste0('DispFotoMos', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
  nombreDisp<- paste0(folder, '/', nombreDisp)
  
  
  
  # write.csv(distancias,nombreCostos,row.names = FALSE )
  # write.csv(finalDispDf,nombreDisp,row.names = FALSE )
  
  
  # ordenamiento del data frame de distancias
  distanciasO<- distancias %>% mutate(rn=row_number())
  
  # Construcción de parámetros para problema lineal
  dirU<- 'min'
  coefs<- distanciasO$cost
  
  # Matriz de restricciones lineales
  matGen<- matrix(NA, nrow =(nrow(finalDispDf)  + dim1*dim2     ),ncol=length(coefs) )
  
  
  # Restricciones de disponibilidades de figuras
  for(  k in finalDispDf$FIG ){
    # k<-1
    matGen[k,which( distanciasO$FIG  ==k )]<- 1
    matGen[k,-which( distanciasO$FIG  ==k )]<-0
    
  }
  
  
  # Asignar un número, llamado primero a cada combinación ROW, COL
  asignRC<-distanciasO %>%group_by(ROW, COL) %>% summarise( primero=min(rn)  ) %>% ungroup()#%>% View()
  distanciasO<- merge(distanciasO, asignRC, by=c('ROW', 'COL')) %>% arrange(rn)
  
  
  figs<-max(finalDispDf$FIG)
  
  
  # Restricción de unicidad de piezas (figuras) por coordenada i,j
  for(  k in 1:nrow(asignRC)  ){
    
    # k<-1
    vecPrev<- rep(0, nrow(distanciasO))
    vecPrev[which(distanciasO$primero==k   )   ]<- 1
    matGen[  figs+k ,]<-vecPrev
    
  }
  
  
  # Dirección restricciones
  dirC<- rep('=',dim(matGen)[1] )
  dirC[1:figs]<- '<='
  
  
  # mano derecha, constantes restricciones
  rightH<-  rep(1,dim(matGen)[1] )
  rightH[1:figs]<- finalDispDf$quant
  
  # Construcción del problema, plasmarlo en objeto de clase lp
  problema<-lp(direction = dirU, objective.in=coefs,
               const.mat=matGen, const.dir=dirC, const.rhs=rightH)
  
  
  
  # Incorporar solución al data frame
  distanciasO<-distanciasO %>% mutate( seleccion=round(problema$solution) )
  
  # Selección final 
  seleccionF<-distanciasO %>% filter(seleccion==1) 
  seleccionF %>% group_by(ROW, COL) %>%
    summarise(n()) 
  
  # Matriz-resultado final
  # matFin<- matrix(NA, nrow=dim1*largPix, ncol = dim2*largPix)
  matFin<- rep(NA, dim1*largPix*dim2*largPix*3)
  dim(matFin)<-c(dim1*largPix,dim2*largPix, 3)
  dim(matFin)
  
  for(  mm in 1:dim(geij)[1] ){
    
    
    
    numeroFig<-seleccionF %>% filter(primero==mm) %>% select(FIG)%>% as.numeric()
    
    
    # extraer combinación renglón-columna
    # ubicar coordenadas del pixel donde inicia el fotomosaico
    pos<- asignRC %>% filter(primero==mm)
    r<-pos$ROW
    rowI<-(r -1)*largPix+1
    
    c<-pos$COL
    colI<-(c -1)*largPix+1
    
    
    # Incorporar arreglo de figuras
    # arreglo<- frameModTotF %>% filter(FIG==numeroFig) %>%
      # select(-num,
             # -FIG,
             # -rn) %>% as.matrix()
    
    matFin[rowI:(rowI+largPix-1),colI:(colI+largPix-1),]<- frameModTotF3D[[numeroFig]]
    
  }
  
  
  
  # Desplegar imagen-objetivo (mejor reproducción posible usando dim1 x dim2 pixeles)
  # a color
  # dim(matFin)
  grid.raster(matFin, interpolate=FALSE)
  
  
  
  
  
}


