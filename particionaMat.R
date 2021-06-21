library(dplyr)
library(RCurl)
library(colorspace)
library(png)
library(tidyr)
library(ggplot2)

# Número de cuadros-renglón
dim1<- 40
# Número de cuadros-columna
dim2<- 30


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
# Then


rotate <- function(x) t(apply(x, 2, rev))



# matsplitter(mb,4,3)



x <- readPNG( paste0(getwd(),"/try1/twitter.png" )  )
# x: dimensión de pixeles
dim(x)


# Nuevo punto
x0=x[,,1] # will hold the grayscale values divided by 255
# x0=t(apply(x0, 2, rev)) # otherwise the image will be rotated


x0_img<- rotate(x0)
image(x0_img, col  = gray((0:255)/255)) # plot in grayscale



# Parámetros de libertad
libertadadPar<-40
# 
# image(x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
#          -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
#          ], col  = gray((0:255)/255)) # plot in grayscale
# 
# 
# 
# 
# class(x)
# class(x0)
# 
# 
# # x0_mod<- x0[-c((1:88), ( (dim(x0)[1]-(88-1)  )  :dim(x0)[1] )   ),]
# 

# 21-jun####
if(libertadadPar>0){
  x0_mod<- x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
              -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
  ]
}else{
  x0_mod<- x0
}


# dim(x0_mod)


# parámetro 1
analizR<- tibble( pos=1:( nrow( x0_mod )*2  ) ) %>% mutate(
  modR=pos%%dim1,  minT=  min(pos[ modR==0 & pos>=nrow( x0_mod )  ])  )%>% filter(pos==minT)


numR<-  nrow( x0 )-analizR$minT[1]

analizC<- tibble( pos=1:( ncol( x0_mod )*2  ) ) %>% mutate(
  modC=pos%%dim2,  minT=  min(pos[ modC==0 & pos>=ncol( x0_mod )  ])  )%>% filter(pos==minT)
numC<-ncol( x0 )- analizC$minT[1]

# 21-jun####
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


rowN<- tibble(  pos=1:(nrow(x0_modN)) ) %>% mutate(modF=  pos%%dim1==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()
colN<- tibble(  pos=1:(ncol(x0_modN)) ) %>% mutate(modF=  pos%%dim2==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()


x0_modN<- x0_modN[1:rowN,1:colN ]

x0N_img<- rotate(x0_modN)

image(x0N_img, col  = gray((0:255)/255)) # plot in grayscale





x0_esc<- x0_mod %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
x0_escN<- x0_modN %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)




# sapply(x0, function(x)round(x*255))
sapply(x0_esc, class)
class(x0_esc)
length(x0_esc)
dim(x0)[1]* dim(x0)[2]


apply(x0_esc,1, function(x)range(x)  )#%>% View()
apply(x0_esc,2, function(x)range(x)  )




apply(x0_mod,1, function(x)range(x)  )
apply(x0_mod,2, function(x)range(x)  )

# apply(x0,2, function(x)range(x)  ) %>% View()
# sapply(list, function)

# Muestra valores dentro de la escala existentes
valoresExistente<-unique(unlist(x0_esc))
valoresExistente


# Extraemos las matrices disponibles en un array
matrices<-matsplitter(x0_escN,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)
length(matrices)
class(matrices)
dim(  matrices)
# dim(x0_esc)

# Matrices en lista
listMats<- list()
for(  ls in  1:dim(  matrices)[3] ){
  listMats[[ls]]<-   matrices[,,ls]
}
length(listMats)


# Mu
promediosPart<- sapply(listMats, function(x)mean(x)   )
gamma_par<- 7

# Ciudades a computar por cuadro
geij<-  gamma_par  - floor( gamma_par* promediosPart/255 )



# 21-jun####
# opción A: ordenado es por llenado por renglón 
coordGe<- function(num) {
  
  # num<- 28
  # num<-23
  equis<- num%%dim2
  
  if(equis==0){
    equis<- dim2
  }
  
  ye<-(num-equis)/dim2+1
  
  
  coord<- c(equis, ye)
  return(coord)
}

# opción B: ordenado es por llenado por columna 
coordGe_c<- function(num) {
  # num<-23
  ye<- num%%dim1
  
  if(ye==0){
    ye<- dim1
  }
  
  equis<-(num-ye)/dim1+1
  
  equis
  ye
  
  coord<- c(equis, ye)
  return(coord)
}



# Genera los extremos de las cuatro coordenadas del cuadrado x, y
coordCuadr<- function(ex, yi){
  # ex<- 10
  # yi<-1
  
  pow_x<-match(TRUE, round(1/dim1, 1:20) == 1/dim1)
  pow_y<-match(TRUE, round(1/dim2, 1:20) == 1/dim2)
  
  equisCu<- (ex-1)/dim1
  yeCu<- 1-(yi-1)/dim2
  
  
  
  

  extrTot<- list(supIzq=  c(equisCu,yeCu),supDer= c(equisCu+1/dim1,yeCu), infIzq= c(equisCu,yeCu-1/dim2), 
                 infDer=c(equisCu+1/dim1,yeCu-1/dim2) )
  extrTot<- sapply(extrTot, function(x)round(x,max(  pow_x, pow_y)  ) ) %>% as_tibble()
  extrTot<- as.list(extrTot)
  
  return(extrTot)
  
}



# Dispersión ciudades
# Genera arreglo eficiente de ciudades en el cuadrado
funcionDistrCd<- function(  quant, precis, extremos ){
  


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


# for(j in 1:20){
  
# j<-1
# set.seed(j)

set.seed(1)
gridPart<- kmeans(gridDim, centers = quant)
centrosDisp<-gridPart$centers %>% as_tibble()
names(centrosDisp)

obj<- centrosDisp %>% ggplot(aes( Var1, Var2 )   )+ geom_point()  +
  xlim( range(gridDim_m$Var1)   )+ylim( range(gridDim_m$Var1) )#+labs(title = paste0('seed: ', j)) 

# print(obj)

}else{
  
  centrosDisp<- tibble(Var1=NA, Var2=NA) %>% filter(!is.na(Var1)   )
}

return(centrosDisp)

}



# Construcción de la ciudad
for(  k in 1:length(geij) ){
  
  print(paste('ceil', k))
  # k<-11
  # which(geij==3)
  osc<- geij[k]
  
  
  cua<-coordGe(k)
  # cua<-coordGe_c(k)
  print(paste('coordenadas x, y: ', cua[1], ', ', cua[2]))
  
  cuadranteEsp<- coordCuadr(cua[1], cua[2])
  
  # Genera dispersión de puntos sobre cuadrado
  dfCeil<- funcionDistrCd(osc, 10,cuadranteEsp)
  
  
  
  if(k ==1){
    dfCeilF<- dfCeil
  }else{
    dfCeilF<-rbind(dfCeilF, dfCeil)
  }
  
}


# dfCeilF %>% group_by(Var1, Var2) %>%summarise(n()) %>% View()
dfCeilF %>% ggplot(aes( Var1, Var2 ))+geom_point()+xlim( range(dfCeilF$Var1)   )+ylim(  range(dfCeilF$Var2) )
# dfCeilF %>% mutate(Var2=-Var2) %>% ggplot(aes( Var1, Var2 ))+geom_point()+xlim( c(0,1)   )#+ylim(- c(0,1) )




