library(dplyr)
library(RCurl)
library(colorspace)
library(png)
library(tidyr)

dim1<- 10
dim2<- 10

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

# matsplitter(mb,4,3)



x <- readPNG( paste0(getwd(),"/try1/twitter.png" )  )
# x: dimensión de pixeles
dim(x)


# Nuevo punto
x0=x[,,1] # will hold the grayscale values divided by 255
x0=t(apply(x0, 2, rev)) # otherwise the image will be rotated



image(x0, col  = gray((0:255)/255)) # plot in grayscale
image(x0[-c(1:88),], col  = gray((0:255)/255)) # plot in grayscale
image(x0[-c((1:88), (568:dim(x0)[1] )   ),], col  = gray((0:255)/255)) # plot in grayscale
image(x0[-c((1:88), ( (dim(x0)[1]-(88-1)  )  :dim(x0)[1] )   ),], col  = gray((0:255)/255)) # plot in grayscale


# Parámetros de libertad
libertadadPar<-40
image(x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
         -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
         ], col  = gray((0:255)/255)) # plot in grayscale




class(x)
class(x0)


# x0_mod<- x0[-c((1:88), ( (dim(x0)[1]-(88-1)  )  :dim(x0)[1] )   ),]

x0_mod<- x0[-c((1:libertadadPar), ( (dim(x0)[1]-(libertadadPar-1)  )  :dim(x0)[1] )  ),
   -c((1:libertadadPar), ( (dim(x0)[2]-(libertadadPar-1)  )  :dim(x0)[2] )  )
]
dim(x0_mod)


# parámetro 1
analizR<- tibble( pos=1:( nrow( x0_mod )*2  ) ) %>% mutate(
  modR=pos%%dim1,  minT=  min(pos[ modR==0 & pos>=nrow( x0_mod )  ])  )%>% filter(pos==minT)

# analizR<- tibble( pos=1:( nrow( x0_mod )*3  ) ) %>% mutate(
#   modR=pos%%dim1, 
#   minT=  min(pos[ modR==0 & pos>=nrow( x0_mod ) 
#                   # & (nrow( x0 )- pos)%%2 ==0 
#                   ])  )%>% filter(pos==minT)


numR<-  nrow( x0 )-analizR$minT[1]

analizC<- tibble( pos=1:( ncol( x0_mod )*2  ) ) %>% mutate(
  modC=pos%%dim2,  minT=  min(pos[ modC==0 & pos>=ncol( x0_mod )  ])  )%>% filter(pos==minT)
numC<-ncol( x0 )- analizC$minT[1]


x0_modN<- x0[  -c((1:(numR/2) ), ( (dim(x0)[1]-(  (numR/2)  -1)  )  :dim(x0)[1] )  ),
               -c((1: (numC/2) ), ( (dim(x0)[2]-(  (numC/2) -1)  )  :dim(x0)[2] )  )
               ]


rowN<- tibble(  pos=1:(nrow(x0_modN)) ) %>% mutate(modF=  pos%%dim1==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()
colN<- tibble(  pos=1:(ncol(x0_modN)) ) %>% mutate(modF=  pos%%dim1==0 ) %>%
  summarise(cual=max(pos[modF]))%>% as.numeric()


x0_modN<- x0_modN[1:rowN,1:colN ]
image(x0_modN, col  = gray((0:255)/255)) # plot in grayscale





dim(x0)
dim(x0_mod)
dim(x0_modN)



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



matrices<-matsplitter(x0_escN,  dim(x0_modN)[1]/dim1, dim(x0_modN)[2]/dim2)
length(matrices)
class(matrices)
dim(  matrices)
# dim(x0_esc)

# Patrices
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






