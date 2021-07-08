
library(dplyr)
# library(RCurl)
# library(colorspace)
library(png)
library(tidyr)
library(ggplot2)
library(data.table)
library(TSP)
library(lpSolve)

# Número de cuadros-renglón
dim1<- 20
# Número de cuadros-columna
dim2<- 20

# llama nombres de archivos y título de fotos
nombreTitulo<-'Twitter'
subC<-'try1'
nombrePng<-'twitter'
habilitaEscrituras<- TRUE 
carpetaCat<- 'instagram'


holguraDisposiciones<- 0.3
variabilidadDisposiciones<-0.3

# subcarpeta-intento
val<-1 

# lee dos archivos: resumen-información (sumF) y pixeles de las distintas fotos (frameModTot)
folder<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', val)

frameModTot<- read.csv(paste0(folder, '/', 'finalInf.csv'), stringsAsFactors  = FALSE )
sumF<- read.csv(paste0(folder, '/', 'parametrosSelFin.csv'), stringsAsFactors = FALSE )

# Reasignar número de cuadro
sumF<- sumF %>% mutate(FIG=row_number())

# selección final de fotos a ocupar
frameModTotF <- frameModTot %>% filter(num %in% sumF$num) %>%
  mutate(rn=row_number()) %>% relocate(rn)
frameModTotF<- merge(sumF %>%select(num, FIG), frameModTotF, 
                     by='num') %>% arrange(rn)

# Extraer dimensiones de pixel de todos los cuadros
largPix<- frameModTotF %>% group_by(num) %>% summarise(n=n()) %>% ungroup() %>%
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



# Lectura archivo-imagen
x <- readPNG( paste0(getwd(),"/", subC ,"/",nombrePng,".png" )  )

# x: dimensión de pixeles
dim(x)

# Arreglo matricial
x0=x[,,1] # will hold the grayscale values divided by 255


# x0<- x0 %>% as_tibble() %>% mutate_all(funs(replace(., .>=0.9, 1))) %>% as.matrix()

# Rotación y visualización de imagen en grayscales
x0_img<- rotate(x0)
image(x0_img, col  = gray((0:255)/255)) # plot in grayscale



# Cuántos pixeles remover en cada dimensión (largo y ancho)
libertadadPar<-40


if(libertadadPar>0){
  x0_mod<- x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
              -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
  ]
}else{
  x0_mod<- x0
}




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
  ]
}else{
  x0_modN<- x0
}
if(numC>0){
  x0_modN<- x0_modN[ ,
                     -c((1: (numC/2) ), ( (dim(x0)[2]-(  (numC/2) -1)  )  :dim(x0)[2] )  )
  ]
}else{
  x0_modN<- x0_modN
}

# Terminar de acotar matriz
rowN<- tibble(  pos=1:(nrow(x0_modN)) ) %>% mutate(modF=  pos%%dim1==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()
colN<- tibble(  pos=1:(ncol(x0_modN)) ) %>% mutate(modF=  pos%%dim2==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()

x0_modN<- x0_modN[1:rowN,1:colN ]


# Desplegar imagen en tonos grises
x0N_img<- rotate(x0_modN)
image(x0N_img, col  = gray((0:255)/255)) # plot in grayscale




# Convertir escala [0,1] continua a {0,1,...,254, 255} entera
x0_esc<- x0_mod %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_escN<- x0_modN %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)



# Muestra valores dentro de la escala existentes
valoresExistente<-unique(unlist(x0_esc))
valoresExistente


# Extraemos las matrices disponibles en un array 3-dimensional
matrices<-matsplitter(x0_escN,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)


# Matrices en lista
listMats<- list()
for(  ls in  1:dim(  matrices)[3] ){
  listMats[[ls]]<-   matrices[,,ls]
}
length(listMats)


# mu: valor de luminosidad promedio de todas las sumbatrices
promediosPart<- sapply(listMats, function(x)mean(x)   )


# Ciudades a computar por cuadro
geij<- promediosPart/255


# Función: convierte un valor de k-ésima submatriz (o cuadrado) en coordenadas (i, j)
# (renglón-columna) de matriz


# opción B: ordenado es por llenado por columna 
coordGe_c<- function(num) {
  # num<-16
  ye<- num%%dim1

  if(ye==0){
    ye<- dim1
  }

  equis<-(num-ye)/dim1+1
  

  coord<- c(equis, ye)
  return(coord)
}


# Matriz objetivo a reproducir (luminosidad promedio por cuadro de la imagen original)
matObj<- matrix(NA, nrow=dim1, ncol = dim2)

# Generar arreglo 3-dimensional de distancias (renglón x columna x figuar)
arr3 = rep(NA, dim1*dim2*nrow(sumF))
dim(arr3) = c(dim1,dim2,  nrow(sumF) )


# Recorrido por los dim1 x dim2 cuadros generados
for(  k in 1:length(geij) ){
  # k<-16
  print(paste0('coordenada ', k))
  
  osc<- geij[k]
  
  # Asginación k- coordendas i, j 
  cua<-coordGe_c(k)
  
  # cuadrado de distancia (en luminosidad) de la celda a las distintas figuras
  difIj<- (osc-sumF$lumP_MinFin)^2
  arr3[ cua[1], cua[2],   ]<- difIj
  
  # Oscuridad objetivo
  matObj[ cua[1], cua[2]   ]<- osc
  
  
}


for(  y in 1:dim(arr3)[3] ){
  
  # y<-1
  tbl<- arr3[,,y ] %>% as_tibble()
  
  if(y==1){
    tblF<- tbl
  }else{
    tblF<-rbind( tblF, tbl)
  }
  
  
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



if(dimCorr   ){
  
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
  mat<- arr3[,,jj]
  
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

nombreCostos<- paste0('costoFotoMos', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
nombreCostos<- paste0(folder, '/', nombreCostos)

nombreDisp<- paste0('DispFotoMos', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
nombreDisp<- paste0(folder, '/', nombreDisp)



# write.csv(distancias,nombreCostos,row.names = FALSE )
# write.csv(finalDispDf,nombreDisp,row.names = FALSE )




distanciasO<- distancias %>% mutate(rn=row_number())
dirU<- 'min'
coefs<- distanciasO$cost


matGen<- matrix(NA, nrow =(nrow(finalDispDf)  + dim1*dim2     ),ncol=length(coefs) )
# dim(matGen)

for(  k in finalDispDf$FIG ){
  # k<-1
  matGen[k,which( distanciasO$FIG  ==k )]<- 1
  matGen[k,-which( distanciasO$FIG  ==k )]<-0
  
}



asignRC<-distanciasO %>%group_by(ROW, COL) %>% summarise( primero=min(rn)  ) %>% ungroup()#%>% View()
distanciasO<- merge(distanciasO, asignRC, by=c('ROW', 'COL')) %>% arrange(rn)


figs<-max(finalDispDf$FIG)

for(  k in 1:nrow(asignRC)  ){
  
  # k<-1
  vecPrev<- rep(0, nrow(distanciasO))
  vecPrev[which(distanciasO$primero==k   )   ]<- 1
  matGen[  figs+k ,]<-vecPrev
  
}



dirC<- rep('=',dim(matGen)[1] )
dirC[1:figs]<- '<='

rightH<-  rep(1,dim(matGen)[1] )
rightH[1:figs]<- finalDispDf$quant

problema<-lp(direction = dirU, objective.in=coefs, const.mat=matGen, const.dir=dirC, const.rhs=rightH)


distanciasO<-distanciasO %>% mutate( seleccion=round(problema$solution) )

seleccionF<-distanciasO %>% filter(seleccion==1) 
seleccionF %>% group_by(ROW, COL) %>%
  summarise(n()) #%>% View()

matFin<- matrix(NA, nrow=dim2*largPix, ncol = dim1*largPix)
# matFinIm<- matrix(NA, nrow=dim2, ncol = dim1)


for(  mm in 1:length(geij) ){
  
  
  numeroFig<-seleccionF %>% filter(primero==mm) %>% select(FIG)%>% as.numeric()
  # mm<-1
  pos<- asignRC %>% filter(primero==mm)
  r<-pos$ROW
  rowI<-(r -1)*largPix+1
  
  c<-pos$COL
  colI<-(c -1)*largPix+1
    
  arreglo<- frameModTotF %>% filter(FIG==numeroFig) %>%
    select(-num, -FIG, -rn) %>% as.matrix()
  # dim(arreglo)
  
  matFin[rowI:(rowI+largPix-1),colI:(colI+largPix-1)]<- arreglo
    
}



matFinA<- matrix(NA, nrow=dim1, ncol = dim2)





matObj_img<- rotate(matObj)
image(matObj_img, col  = gray((0:255)/255)
) # plot in grayscale




matFin_img<- rotate(matFin)
image(matFin_img, col  = gray((0:255)/255),
      xlab='eje X: final plot'
      ) # plot in grayscale

}
# matGen[1: length(finalDispDf$FIG) , ]%>% View()
# apply(matGen, 1,  function(x)sum( is.na(x)  ))
# apply(matGen, 1,  function(x)sum( is.na(x)  ))
