library(dplyr)
library(RCurl)
library(colorspace)
library(png)
library(tidyr)
library(ggplot2)
library(data.table)
library(TSP)

# Número de cuadros-renglón
dim1<- 15
# Número de cuadros-columna
dim2<- 15

# Máximo de ciudades a computar por cuadro
gamma_par<- 7

# llama nombres de archivos y título de fotos
nombreTitulo<-'Twitter'
subC<-'try1'
nombrePng<-'twitter'
reshapeDists<- TRUE
habilitaEscrituras<- TRUE 


# llama función de particionar matrices en submatrices
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


# Nuevo punto
x0=x[,,1] # will hold the grayscale values divided by 255



x0<- x0 %>% as_tibble() %>% mutate_all(funs(replace(., .>=0.9, 1))) %>% as.matrix()

# Rotación (para hacer plot en grayscales)
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
geij<-  gamma_par  - floor( gamma_par* promediosPart/255 )



# opción A: ordenado es por llenado por renglón 
# Función: convierte un valor de k-ésima submatriz (o cuadrado) en coordenadas (i, j)
coordGe<- function(num) {
  
  # num<- 28
  # num<-23
  
  # define renglón
  equis<- num%%dim2
  if(equis==0){
    equis<- dim2
  }
  
  # Define columna
  ye<-(num-equis)/dim2+1
  
  
  coord<- c(equis, ye)
  return(coord)
}

# opción B: ordenado es por llenado por columna 
# coordGe_c<- function(num) {
#   # num<-23
#   ye<- num%%dim1
#   
#   if(ye==0){
#     ye<- dim1
#   }
#   
#   equis<-(num-ye)/dim1+1
#   
#   equis
#   ye
#   
#   coord<- c(equis, ye)
#   return(coord)
# }



# Genera los cuatro extremos de las cuatro coordenadas del cuadrado x, y
coordCuadr<- function(ex, yi){
  # ex<- 10
  # yi<-1
  
  # Define número de decimales en x y y
  pow_x<-match(TRUE, round(1/dim1, 1:20) == 1/dim1)
  pow_y<-match(TRUE, round(1/dim2, 1:20) == 1/dim2)
  
  # extremos izquierdo
  equisCu<- (ex-1)/dim1
  # Extremo superior
  yeCu<- 1-(yi-1)/dim2
  
  
  
  # Definición de las 4 coordenadas
  extrTot<- list(supIzq=  c(equisCu,yeCu),supDer= c(equisCu+1/dim1,yeCu), infIzq= c(equisCu,yeCu-1/dim2), 
                 infDer=c(equisCu+1/dim1,yeCu-1/dim2) )
  extrTot<- sapply(extrTot, function(x)round(x,max(  pow_x, pow_y)  ) ) %>% as_tibble()
  extrTot<- as.list(extrTot)
  
  return(extrTot)
  
}



# Dispersión ciudades
# Genera arreglo eficientemente disperso de ciudades en el cuadrado
funcionDistrCd<- function(  quant, precis, extremos ){
  
  # quant: nivel de oscuridad, discreto de 0 a gamma. 
  # extremos: 4 límites del cuadrado
  # precis: se generará un grid-2 dimensional delimitado entre los extremos, este grid contará con
  # (precis+1) puntos a lo largo por (precis+1) puntos a lo ancho

  
  
# quant<- 5
# extremos<- list(supIzq=  c(0,1),supDer= c(1, 1), infIzq= c(0, 0), infDer=c(1, 0)  )
# precis<- 10
if(floor(  quant)==quant & quant>0 ){
  
  
  grid_x<- seq(from=max(extremos$supIzq[1], extremos$infIzq[1]), 
             to=max(extremos$supDer[1], extremos$infDer[1]) ,
             by= ((max(extremos$supDer[1], extremos$infDer[1])- max(extremos$supIzq[1], extremos$infIzq[1]) )/precis ) )
  
  grid_y<- seq(from=max(extremos$infIzq[2], extremos$infDer[2]), 
             to=max(extremos$supIzq[2], extremos$supDer[2]) ,
             by= ((max(extremos$supIzq[2], extremos$supDer[2])- max(extremos$infIzq[2], extremos$infDer[2]) )/precis ) )
  
  gridDim_m<- expand.grid(grid_x,grid_y)
  gridDim<-gridDim_m%>% as.matrix()
  
  set.seed(1)
  # distribución eficiente: quant centroides del grid 2-dimensional
  gridPart<- kmeans(gridDim, centers = quant)
  centrosDisp<-gridPart$centers %>% as_tibble()
  obj<- centrosDisp %>% ggplot(aes( Var1, Var2 )   )+ geom_point()  +
    xlim( range(gridDim_m$Var1)   )+ylim( range(gridDim_m$Var1) )#+labs(title = paste0('seed: ', j)) 


}else{
  
  centrosDisp<- tibble(Var1=NA, Var2=NA) %>% filter(!is.na(Var1)   )
  
}

return(centrosDisp)

}



# Construcción de las ciudades a recorrer
# Recorrido por los dim1 x dim2 cuadros generados
for(  k in 1:length(geij) ){
  
  # print(paste('ceil', k))
  osc<- geij[k]
  
  # Asginación k- coordendas i, j 
  cua<-coordGe(k)
  
  # cua<-coordGe_c(k)
  # print(paste('coordenadas x, y: ', cua[1], ', ', cua[2]))
  
  # Generación de extremos del cuadro
  cuadranteEsp<- coordCuadr(cua[1], cua[2])
  
  
  # Genera dispersión de puntos sobre cuadrado
  dfCeil<- funcionDistrCd(osc, 10,cuadranteEsp)
  
  # Incorpora información adicional
  dfCeil<- tibble(numIt=k, coordX=cua[1],coordY=cua[2], dfCeil )
  
  if(k ==1){
    dfCeilF<- dfCeil
  }else{
    dfCeilF<-rbind(dfCeilF, dfCeil)
  }
  
}
dfCeilF<- dfCeilF %>% mutate(rn=row_number())



# Gráfica de puntos generados
dfCeilF %>% ggplot(aes( Var1, Var2 ))+geom_point()+xlim( range(dfCeilF$Var1)   )+
  ylim(  range(dfCeilF$Var2) )+labs(title=paste0(  nombreTitulo,' con puntillismo'  ) )

# Matriz de distancias entre los puntos
distancias<-dist(dfCeilF[, c('Var1', 'Var2')])  %>% as.matrix()

if( reshapeDists ){
  


# Reshape de la matriz de distancias a 
for(kk in 1:nrow(distancias)){
  
  if(kk%%100==0){
    print(paste0(kk, '-ésimo renglón'))
  }
  
  # kk<-1
  distanciasAlt<- tibble(distancias[kk,])
  names(distanciasAlt)<-'dist'
  distanciasAlt<- tibble(partida=kk, dest=1:nrow(distancias), distanciasAlt    )
  
  
  
  if(kk==1){
    distanciasAltF<- distanciasAlt
  }else{
    distanciasAltF<- rbind(distanciasAltF,distanciasAlt )
  }
  
}
 
  distanciasAltF0<- distanciasAltF %>% select(ROW=partida, COL=dest, distance=dist)
  if(habilitaEscrituras){
    nombreDists<- paste0('distCity', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
    # write.csv(distanciasAltF0,'distCityTwitt15_15.csv',row.names = FALSE )
    write.csv(distanciasAltF0,nombreDists,row.names = FALSE )
  }
 
}

if(habilitaEscrituras){
  # write.csv(dfCeilF,'puntosTwitt15_15.csv',row.names = FALSE )
  nombreDots<- paste0('puntos', Hmisc::capitalize(nombreTitulo), dim1,'_', dim2, '.csv'  )
  write.csv(dfCeilF ,  nombreDots,row.names = FALSE )
}


dfCeilF %>% ggplot(aes(Var1, Var2))+geom_point()




# Convierte puntos en matriz
x <- data.frame(dfCeilF %>% select(x=Var1, y=Var2)  ,  row.names = 1:nrow(  dfCeilF )    )

# crea el TSP
etsp <- ETSP(x)
class(etsp)

etsp
# usa métodos
n_of_cities(etsp)
labels(etsp)

# computa gráfica y solución
tour <- solve_TSP(etsp)
plot(etsp, tour, tour_col = "red")


# guarda y luego lee el objeto tipo tsp
nombreTsp<- paste0('etsp_', nombrePng, dim1,'_', dim2, '.tsp')

if(habilitaEscrituras){
  write_TSPLIB(etsp, nombreTsp, precision = 6, inf = NULL, neg_inf = NULL)
  read_TSPLIB(nombreTsp, precision = 0)
  
}


# Interpreta orden de las ciudades en el tour
intTour<-as.numeric(tour)

# Genera reshape de los puntos en una versión que nos permitirá imprimir en ggplot el orden de las conexiones
casoMin<-x%>% mutate(rn=row_number()) 

for(j in 1:(nrow(casoMin)-1) ){
  if(j%%100==0){
    print(j)
  }
  
  casoMinA<- casoMin  %>% filter(rn %in% c(intTour[j], intTour[ (j+1) ] ) )  %>% mutate(paired=j )
  
  if(j==1){
    casoMinF<- casoMinA
  }else{
    casoMinF<- rbind(casoMinF,casoMinA )
  }
  
  
}

if(j==(nrow(casoMin)-1)){
  casoMinA<- casoMin  %>% filter(rn %in% c(intTour[1], intTour[ (j) ] ) )  %>% mutate(paired=(j+1) )
  
  casoMinF<- rbind(casoMinF,casoMinA )
  
}

  
# Escribe con ggplot las conexiones entre puntos indicadas
casoMinF %>% ggplot(aes(x, y))+
  geom_line(aes(group = paired))+labs(title=paste0(nombreTitulo, ' con puntos (ciudades) conectados vía TSP') ) 




