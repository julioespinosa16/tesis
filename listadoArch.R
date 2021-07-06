library(dplyr)
library(RCurl)
library(colorspace)
library(png)
library(tidyr)
library(ggplot2)
library(data.table)
library(TSP)
library(stringr)
library(stringi)

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


carpetaCat<- 'instagram'

reshapeDists<- FALSE
habilitaEscrituras<- FALSE 


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






# listado archivos
pngs<-list.files(paste0(getwd(), '/', carpetaCat  ,'/'))
pngs<- pngs[str_detect(pngs, 'png')]

blackWhites<- list()

for(y in 1:length(pngs)  ){
  
  
  # y<-18
nombreTemp<- pngs[y]
# Lectura archivo-imagen

print(paste0(y,': ', nombreTemp))


x <- readPNG( paste0(getwd(),"/", carpetaCat ,"/",nombreTemp )  )
# x: dimensión de pixeles
dim(x)


# Nuevo punto
x0=x[,,1] # will hold the grayscale values divided by 255



# x0<- x0 %>% as_tibble() %>% mutate_all(funs(replace(., .>=0.9, 1))) %>% as.matrix()

# Rotación (para hacer plot en grayscales)

x0_img<- rotate(x0)
# image(x0_img, col  = gray((0:255)/255)) # plot in grayscale


lumP<- mean(x0)
prelInf<- tibble(num=y, nombre=nombreTemp,lumP_prel=lumP, dim1p=dim(x0)[1],  dim2p=dim(x0)[2])

blackWhites[[length(blackWhites)+1]]<- x0


if(y==1){
  prelInfF<- prelInf
}else{
  prelInfF<-rbind(prelInfF, prelInf )
}

}



valorMinimo<- min(c(prelInfF$dim1p), c(prelInfF$dim2p))
blackWhitesA<- list()


for(y in 1:length(pngs)  ){
  
  # y<-18
  nombreTemp<- pngs[y]
  # Lectura archivo-imagen
  
  print(paste0(y,': ', nombreTemp))
   
  
  # x: dimensión de pixeles
  # dim(x)
  
  
  # Nuevo punto
  # x0=x[,,1] # will hold the grayscale values divided by 255
  x0<- blackWhites[[y]]
  dim(x0)
  
  
  
  nuevaDif1<- prelInfF$dim1p[y]-valorMinimo 
  nuevaDif1<- floor(nuevaDif1/2)
  

  if(nuevaDif1>0){
    x0_f<- x0[-c(1:nuevaDif1, (dim(x0)[1] -(nuevaDif1-1)   ):dim(x0)[1]  ),
              
              
    ]
  }else{
    x0_f<- x0
  }

  
  
  nuevaDif2<- prelInfF$dim2p[y]-valorMinimo 
  nuevaDif2<- floor(nuevaDif2/2)
  if(nuevaDif2>0){
    x0_f<- x0_f[,-c(1:nuevaDif2, (dim(x0)[2] -(nuevaDif2-1)   ):dim(x0)[2]  )
              
              
    ]
  }else{
    x0_f<- x0_f
  }
  
  x0_f<- x0_f[1:valorMinimo  ,1:valorMinimo   ]
  
  dim(x0_f)
  
  # x0<- x0 %>% as_tibble() %>% mutate_all(funs(replace(., .>=0.9, 1))) %>% as.matrix()
  
  
  # Rotación (para hacer plot en grayscales)
  x0_img<- rotate(x0_f)
  image(x0_img, col  = gray((0:255)/255)) # plot in grayscale
  
  
  lumP<- mean(x0_f)
  prelInfN<- tibble(num=y, nombre=nombreTemp,lumP_Min=lumP, dim1pMin=dim(x0_f)[1],  dim2pMin=dim(x0_f)[2])
  prelInfN<- merge(prelInfN, prelInfF %>% filter(num==y), by=c('num', 'nombre')  )
  
  
  
  blackWhitesA[[length(blackWhitesA)+1]]<- x0_f
  
  
  if(y==1){
    prelInfFN<- prelInfN
  }else{
    prelInfFN<-rbind(prelInfFN, prelInfN )
  }
  
}





crit<- 'cut'
intv<- 0.05
prelInfFN<- prelInfFN %>% mutate(intervalo='Nulo')


for( q in  seq(from=0, to=1, by=intv)[-1]   ){
  
  # q<- seq(from=0, to=1, by=intv)[-1][1]
  
  listIntv<-prelInfFN %>% filter( between(lumP_Min,q-intv, q )   ) 
  
  
  
  if(   nrow(listIntv)==0 ){
    print(paste0('intervalo ', q-intv,' - ', q, ' nulo' ) )
  }else{
    
    prelInfFN<- prelInfFN %>% mutate(intervalo=ifelse(  nombre %in% listIntv$nombre,
                                      paste0( q-intv,' - ', q) , intervalo )   )
    print(paste0('intervalo ', q-intv,' - ', q, ' no nulo' ) )
    
    for(  ii in 1:nrow(listIntv) ){
      
      
      print(paste0('elemento ', ii))
      
      
      img<- blackWhitesA[[listIntv$num[ii] ]]
      
      
      imgR<- rotate(img)
      image(imgR, col  = gray((0:255)/255), 
            xlab = paste0('eje X; promedio: ',listIntv$lumP_Min[ii]   ),
            ylab = paste0(  'eje Y: ', 'intervalo ', q-intv,' - ', q, ', elemento ', 
                            ii,' img ',listIntv$nombre[ii]   )
            )
      
      
      # ?image(    )
      
    }
    
    
  }
  
  
}







finalDecOutcome<- prelInfFN %>% arrange(lumP_Min)
lInt<-list.files(paste0(getwd(), '/', carpetaCat  ,'/')) 
lInt<- c(lInt, ' hola')

lIntFold<- lInt[ ! str_detect(lInt, '\\.')  ]
lIntFold<- lIntFold[str_ends(lIntFold, '[0-9]+')  ]


lastOut<- stri_extract_last( lIntFold, regex = '[0-9]+'  )
lastOut<- as.integer(lastOut)


if(length(lastOut)==0){
  
  val<- 1
}else{
  val<- max(lastOut)+1
  
}

val<-1

folder<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', val)
dir.create( folder)



# write.csv(finalDecOutcome,paste0(folder, '/', 'parametrosPrev.csv'), row.names = FALSE )

# ?stri_extract_last
# ?str_ends()




selParF<-read.csv(paste0(folder, '/', 'parametrosFin.csv'), stringsAsFactors  = FALSE )


selParF %>% group_by(veredicto) %>% summarise(n())
selParF%>% filter(!veredicto%in%c('n', 'p')  )%>% summarise(n())
selParF%>% filter(!veredicto%in%c('n', 'p')  )%>% group_by(intervalo)%>% summarise(n())




selParF_sel<- selParF %>% filter(!veredicto%in%c('n', 'p')  )%>%
  mutate(intervaloFinal='Nulo', lumP_MinFin=-1)
numsSel<- selParF_sel$num


finalBlackWhites<- blackWhitesA

for( q in numsSel   ){
  
  
  # q<- numsSel[1]
  
  numSel<-selParF_sel  %>%filter(num==q   ) 
  crit<- numSel$veredicto[1]
  print(paste0('número explorado: ', q))
  print(paste0('criterio: ', crit))
  
  if(  crit=='s' ){
    
    selTemp<-numSel %>% mutate(intervaloFinal=intervalo, lumP_MinFin=lumP_Min)
    
    img<- blackWhitesA[[selTemp$num[1] ]]
    
    
    imgR<- rotate(img)
    image(imgR, col  = gray((0:255)/255), 
          xlab = paste0('eje X; promedio: ',selTemp$lumP_MinFin[1]   ),
          ylab = paste0(  'eje Y: ', 'intervalo ', selTemp$intervaloFinal[1], 
                          ii,' img ',selTemp$nombre[1]   )
    )
      
    finalBlackWhites[[selTemp$num[1] ]]<- img
    
    
  }else{
    
    
    if( str_detect(crit, 'minus|plus')  ){
      
      
      numI<-stri_extract_first( crit,regex =  '[0-9]+'  )
      numI<- as.numeric(numI)
      
    }
    
    if( str_detect(crit, 'minus')  ){
      numI<-(-numI)
    }
    
    
    intvDesI<- numSel$intervalo[1]
    intvDesI<-stri_extract_first( intvDesI,regex =  '([0-9]+)(\\.)(([0-9]+))'  )
    intvDesI<- as.numeric(intvDesI)
    intvDesI<- intvDesI+numI*intv
    
    mean(blackWhitesA[[numSel$num[1] ]])
    img<- blackWhitesA[[numSel$num[1] ]] %>% as_tibble()
    sapply(img, class)
    
    img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
      mutate_at(-1,funs(replace(., is.numeric(.),
                            max(c(.+numI*intv, 0 ) )  ))) %>%ungroup()%>% select(-rn) %>%
      as.matrix()
    meanA<- mean(img)
    itMn<- 1
    
    print(paste0('itera acota: ', itMn))
    
    imgR<- rotate(img)
    
    image(imgR, col  = gray((0:255)/255), 
          xlab = paste0('eje X; promedio: ',meanA   ),
          ylab = paste0(  'eje Y: ', 'intervalo ', numSel$intervaloFinal[1], 
                          ii,' img ',numSel$nombre[1], '; it ', itMn  )
    )
    
    
    conEx<- between(meanA, intvDesI,intvDesI+intv)
    
    
    powIntv<-match(TRUE, round(intv  , 1:20) ==intv  )
    paso<- 10^(-powIntv)
    if(numI<0){
      paso<- (-paso)
    }
    
    while(!conEx){
      
      
      
      
      itMn<- itMn+1
      print(paste0('itera acota: ', itMn))
      img<- img %>% as_tibble()
      img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
        mutate_at(-1,funs(replace(., is.numeric(.),
                                  max(c(.+paso, 0 ) )  ))) %>%ungroup()%>% select(-rn) %>%
        as.matrix()
      meanA<- mean(img)
      conEx<- between(meanA, intvDesI,intvDesI+intv)
      
      
      imgR<- rotate(img)
      
      image(imgR, col  = gray((0:255)/255), 
            xlab = paste0('eje X; promedio: ',meanA, '; veredicto: ', conEx   ),
            ylab = paste0(  'eje Y: ', 'intervalo ', numSel$intervaloFinal[1], 
                            ii,' img ',numSel$nombre[1], '; it ', itMn  )
      )
      
      
      
      
      
    }
    
    
    finalBlackWhites[[numSel$num[1] ]]<- img
    
    
    
    
  }
  
  
}


