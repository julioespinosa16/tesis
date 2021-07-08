library(dplyr)
# library(RCurl)
# library(colorspace)
library(png)
library(tidyr)
library(ggplot2)
library(data.table)
library(stringr)
library(stringi)


# Instanciar parámetros ####
# llama nombres de archivos y título de fotos
nombreTitulo<-'Twitter'
subC<-'try1'
nombrePng<-'twitter'

# Carpeta de donde se van a extraer los archivos png con fotos-candidatas
carpetaCat<- 'instagram'

# POR USAR ####
habilitaEscrituras<- FALSE 

# flag de si se trabajará en nueva carpeta
nuevaCarpeta<- FALSE
numCarp<- 1



# Función de rotación
rotate <- function(x) t(apply(x, 2, rev))



# Computar la información de archivos png ####

# listado archivos
pngs<-list.files(paste0(getwd(), '/', carpetaCat  ,'/'))
pngs<- pngs[str_detect(pngs, 'png')]

# Construye lista de arreglos matriciales (considerando que podrían tener distinto tamaño)
blackWhites<- list()


for(y in 1:length(pngs)  ){
  
  
  # y<-18
  nombreTemp<- pngs[y]
  # Lectura archivo-imagen
  
  print(paste0(y,': ', nombreTemp))
  
  x <- readPNG( paste0(getwd(),"/", carpetaCat ,"/",nombreTemp )  )
  # x: dimensión de pixeles
  dim(x)
  
  # Arreglo matricial
  x0=x[,,1] # will hold the grayscale values divided by 255
  
  
  # Rotación y visualización de imagen en grayscales
  x0_img<- rotate(x0)
  # image(x0_img, col  = gray((0:255)/255)) # plot in grayscale
  
  
  lumP<- mean(x0)
  # Acopla información de dimensiones y luminosidad promedio de la imagen en data frame
  prelInf<- tibble(num=y, nombre=nombreTemp,lumP_prel=lumP, dim1p=dim(x0)[1],  dim2p=dim(x0)[2])
  
  
  blackWhites[[length(blackWhites)+1]]<- x0
  
  if(y==1){
    prelInfF<- prelInf
    }else{
      prelInfF<-rbind(prelInfF, prelInf )
    }
  }


# Homologación dimensiones ####
# Construye lista de arreglos matriciales, buscando homologar las dimensiones a la 
# mínima dimensión

valorMinimo<- min(c(prelInfF$dim1p), c(prelInfF$dim2p))
blackWhitesA<- list()


for(y in 1:length(pngs)  ){
  
  
  nombreTemp<- pngs[y]
  # Lectura archivo-imagen
  
  print(paste0(y,': ', nombreTemp))
   
  
  
  # Arreglo matricial a modificar
  x0<- blackWhites[[y]]
  dim(x0)
  
  
  # dimensiones a sustraer en renglón
  nuevaDif1<- prelInfF$dim1p[y]-valorMinimo 
  nuevaDif1<- floor(nuevaDif1/2)
  
  # sustracción
  if(nuevaDif1>0){
    x0_f<- x0[-c(1:nuevaDif1, (dim(x0)[1] -(nuevaDif1-1)   ):dim(x0)[1]  ),
              
              
    ]
  }else{
    x0_f<- x0
  }

  
  # dimensiones a sustraer en columna
  nuevaDif2<- prelInfF$dim2p[y]-valorMinimo 
  nuevaDif2<- floor(nuevaDif2/2)
  
  # Sustracción
  if(nuevaDif2>0){
    x0_f<- x0_f[,-c(1:nuevaDif2, (dim(x0)[2] -(nuevaDif2-1)   ):dim(x0)[2]  )
              
              
    ]
  }else{
    x0_f<- x0_f
  }
  
  x0_f<- x0_f[1:valorMinimo  ,1:valorMinimo   ]
  
  
  
  # Rotación y visualización de imagen en grayscales
  x0_img<- rotate(x0_f)
  image(x0_img, col  = gray((0:255)/255)) # plot in grayscale
  
  
  lumP<- mean(x0_f)
  
  # Acopla información de dimensiones y luminosidad promedio de la imagen (nueva matriz acotada)
  # en data frame
  prelInfN<- tibble(num=y, nombre=nombreTemp,lumP_Min=lumP, dim1pMin=dim(x0_f)[1],  dim2pMin=dim(x0_f)[2])
  prelInfN<- merge(prelInfN, prelInfF %>% filter(num==y), by=c('num', 'nombre')  )
  
  
  
  blackWhitesA[[length(blackWhitesA)+1]]<- x0_f
  
  
  if(y==1){
    prelInfFN<- prelInfN
  }else{
    prelInfFN<-rbind(prelInfFN, prelInfN )
  }
  
}



# Clasificación luminosidad por intervalos####

# Técnica a usar (cortar o expandir)
crit<- 'cut'
# Grid de clasificación (intervalos de luminosidad)
intv<- 0.05
prelInfFN<- prelInfFN %>% mutate(intervalo='Nulo')
powIntv<-match(TRUE, round(intv  , 1:20) ==intv  )

# Iteración por intervalos
for( q in  seq(from=0, to=1, by=intv)[-1]   ){
  
  # q<- seq(from=0, to=1, by=intv)[-1][1]
  
  # Acotar intervalo
  listIntv<-prelInfFN %>% filter( between(lumP_Min,q-intv, q )   ) 
  
  
  
  if(   nrow(listIntv)==0 ){
    print(paste0('intervalo ', q-intv,' - ', q, ' nulo' ) )
  }else{
    
    # Variable intervalo
    prelInfFN<- prelInfFN %>% mutate(intervalo=ifelse(  nombre %in% listIntv$nombre,
                                      paste0( q-intv,' - ', q) , intervalo )   )
    print(paste0('intervalo ', q-intv,' - ', q, ' no nulo' ) )
    
    for(  ii in 1:nrow(listIntv) ){
      
      
      
      print(paste0('elemento ', ii))
      img<- blackWhitesA[[listIntv$num[ii] ]]
      
      
      # plot de las imágenes, ordenadas por intervalo usado
      imgR<- rotate(img)
      image(imgR, col  = gray((0:255)/255), 
            xlab = paste0('eje X; promedio: ',round(listIntv$lumP_Min[ii], powIntv+1   )  ,
            ylab = paste0(  'eje Y: ', 'intervalo ',round( q-intv,  powIntv+1 ),' - ', round(q,  powIntv+1 ), ', elemento ', 
                            ii,' img ',listIntv$nombre[ii]   )
            ))
      
      
      
    }
    
    
  }
  
  
}




finalDecOutcome<- prelInfFN %>% arrange(lumP_Min)

lInt<-list.files(paste0(getwd(), '/', carpetaCat  ,'/')) 


lIntFold<- lInt[ ! str_detect(lInt, '\\.')  ]
lIntFold<- lIntFold[str_ends(lIntFold, '[0-9]+')  ]


lastOut<- stri_extract_last( lIntFold, regex = '[0-9]+'  )
lastOut<- as.integer(lastOut)

if(length(lastOut)==0){
  
  val<- 1
}else{
  val<- max(lastOut)+1
  
}

if( !nuevaCarpeta & numCarp>0 ){
  # val<-1
  val<-numCarp
}


folder<-paste0(getwd(), '/', carpetaCat  ,'/', carpetaCat, 'Prop_', val)
if(nuevaCarpeta){
  dir.create( folder)
  
}



# write.csv(finalDecOutcome,paste0(folder, '/', 'parametrosPrev.csv'), row.names = FALSE )


# Modificaciones finales en la luminosidad ####

# Lectura de información final (completada manualmente)
selParF<-read.csv(paste0(folder, '/', 'parametrosFin.csv'), stringsAsFactors  = FALSE )


selParF %>% group_by(veredicto) %>% summarise(n())
selParF%>% filter(!veredicto%in%c('n', 'p')  )%>% summarise(n())
selParF%>% filter(!veredicto%in%c('n', 'p')  )%>% group_by(intervalo)%>% summarise(n())


# Quedarse con los que inician en 'minus', 'plus', o que valgan 's'
selParF_sel<- selParF %>% filter(!veredicto%in%c('n', 'p')  )%>%
  mutate(intervaloFinal='Nulo', lumP_MinFin=-1)
# Valores de png's sobre los cuales iterar
numsSel<- selParF_sel$num


finalBlackWhites<- blackWhitesA


for( q in numsSel   ){
  
  # q<- numsSel[1]
  # q<-30
  
  # filtrar a la imagen en cuestión
  numSel<-selParF_sel  %>%filter(num==q   ) 
  crit<- numSel$veredicto[1]
  
  
  print(paste0('número explorado: ', q))
  print(paste0('criterio: ', crit))

  # Caso 's': no-modificable
  if(  crit=='s' ){
    
    
    
    selTemp<-numSel %>% mutate(intervaloFinal=intervalo, lumP_MinFin=lumP_Min)
    
    img<- blackWhitesA[[selTemp$num[1] ]]
    
    # desplegar imagen y mencionar luminosidad e intervalo
    
    imgR<- rotate(img)
    image(imgR, col  = gray((0:255)/255), 
          xlab = paste0('eje X; promedio: ',round(selTemp$lumP_MinFin[1],  powIntv+1 )     ),
          ylab = paste0(  'eje Y: ', 'intervalo ', selTemp$intervaloFinal[1], 
                          ii,' img ',selTemp$nombre[1]   )
    )
      
    finalBlackWhites[[selTemp$num[1] ]]<- img
    
    # información final
    selParF_sel<- selParF_sel %>% mutate(
      intervaloFinal=ifelse(  num==q, intervalo,intervaloFinal  ), 
      lumP_MinFin=ifelse( num==q, lumP_Min,lumP_MinFin )
      
      )
    
    
  }else{
    
    # extraer el valor (no. de intervalos a desplazar)
    if( str_detect(crit, 'minus|plus')  ){
      
      
      numI<-stri_extract_first( crit,regex =  '[0-9]+'  )
      numI<- as.numeric(numI)
      
    }
    
    # determinar signo (reducir luminosidad, minus, o aumentarla, plus)
    if( str_detect(crit, 'minus')  ){
      numI<-(-numI)
    }
    
    
    # piso de intervalo actual
    intvDesI<- numSel$intervalo[1]
    intvDesI<-stri_extract_first( intvDesI,regex =  '([0-9]+)(\\.)(([0-9]+))'  )
    intvDesI<- as.numeric(intvDesI)
    
    # piso del intervalo-objetivo 
    intvDesI<- intvDesI+numI*intv
    intvDesI<- round(intvDesI,powIntv+1 )
    
    # paso: reducción o incremento: primer intento
    img<- blackWhitesA[[numSel$num[1] ]] %>% as_tibble()
    
    if(numI<0){
      img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
        mutate_at(-1,funs(replace(., is.numeric(.),
                            max(c(.+numI*intv, 0 ) )  ))) %>%ungroup()%>% select(-rn) %>%
      as.matrix()
    }
    if(numI>0){
      img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
        mutate_at(-1,funs(replace(., is.numeric(.),
                                  min(c(.+numI*intv, 1 ) )  ))) %>%ungroup()%>% select(-rn) %>%
        as.matrix()
    }
    
    
    # nueva luminosidad de la imagen modificada
    meanA<- mean(img)
    itMn<- 1
    
    print(paste0('itera acota: ', itMn))
    
    # desplegar imagen 
    imgR<- rotate(img)
    image(imgR, col  = gray((0:255)/255), 
          xlab = paste0('eje X; promedio: ',round(meanA,  powIntv+1 )   ),
          ylab = paste0(  'eje Y: ', 'intervalo ', numSel$intervaloFinal[1], 
                          ii,' img ',numSel$nombre[1], '; it ', itMn  )
    )
    
    # condición de salida (luminosidad en intervalo)
    conEx<- between(meanA, intvDesI,intvDesI+intv)
    
    # pasos de reducción o incremento hasta llegar al intervalo deseado
    paso<- 10^(-powIntv)
    if(numI<0){
      paso<- (-paso)
    }
    
    while(!conEx){
      
      
      
      
      itMn<- itMn+1
      print(paste0('itera acota: ', itMn))
      
      # paso: reducción o incremento: primer intento
      img<- img %>% as_tibble()
      
      if(numI<0){
        img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
          mutate_at(-1,funs(replace(., is.numeric(.),
                                    max(c(.+paso, 0 ) )  ))) %>%ungroup()%>% select(-rn) %>%
          as.matrix()
      }
      
      if(numI>0){
        img<- img  %>% mutate(rn=row_number()) %>% relocate(rn)%>% group_by(rn)%>%
          mutate_at(-1,funs(replace(., is.numeric(.),
                                    min(c(.+paso, 1 ) )  ))) %>%ungroup()%>% select(-rn) %>%
          as.matrix()
      }
      
      # cómputo de luminosidad de la imagen modificada y de condición de salida 
      # (luminosidad en intervalo)
      meanA<- mean(img)
      conEx<- between(meanA, intvDesI,intvDesI+intv)
      
      
      # desplegar imagen 
      imgR<- rotate(img)
      image(imgR, col  = gray((0:255)/255), 
            xlab = paste0('eje X; promedio: ',round(meanA,  powIntv+1 ), '; veredicto: ', conEx   ),
            ylab = paste0(  'eje Y: ', 'intervalo ', numSel$intervaloFinal[1], 
                            ii,' img ',numSel$nombre[1], '; it ', itMn  )
      )
      
      
      
      
      
    }
    
    # información final, incorporar datos
    selParF_sel<- selParF_sel %>% mutate(
      intervaloFinal=ifelse(  num==q, paste0( round( intvDesI ,  powIntv+1 ) ,' - ', 
                                              round(intvDesI+intv ,  powIntv+1 ) ),intervaloFinal  ),
      lumP_MinFin=ifelse( num==q, meanA,lumP_MinFin )
      
    )
    
    # Guardar imágenes modificadas
    finalBlackWhites[[numSel$num[1] ]]<- img
    
    
  }
  
  
}



# Convertir conjunto de imágenes a un único data frame ####
frameModTot<- tibble()

for(th in 1:length(finalBlackWhites)){
  # th<- 1
  
  print(th)
  arr<- finalBlackWhites[[th]] %>% as_tibble() %>% mutate(num=th) %>% relocate(num)
  
  if(nrow(frameModTot)  ==0 ){
    frameModTot<- arr
  }else{
    frameModTot<- rbind(frameModTot, arr)
  }
  
  
}

frameModTot %>% group_by(num) %>% summarise(n()) #%>% View()
sumF<- selParF_sel %>% mutate(finalSel=TRUE)

sumF %>% group_by(intervaloFinal) %>% summarise(n(), mean(lumP_MinFin))


# sapply(frameModTot, function(x)max(x)  )
# write.csv(frameModTot,paste0(folder, '/', 'finalInf.csv'), row.names = FALSE )
# write.csv(sumF,paste0(folder, '/', 'parametrosSelFin.csv'), row.names = FALSE )

# Despliegue selección final de imágenes ####
# Desplegar todas la imágenes seleccionadas, una junto a otra y ordenado por escala de grises
dimsNew1<- round(sqrt(   nrow(selParF_sel ) ) )
dimsNew2<- ceiling(    nrow(selParF_sel )/dimsNew1 )

dims<- sort(c(dimsNew1, dimsNew2), decreasing = TRUE)


sumFa<-sumF %>% arrange(lumP_MinFin)
# número de pixeles, imágenes cuadradas
largPix<- sumF  %>% ungroup() %>%
  summarise(nf=unique(dim1pMin))  %>% as.numeric()


# Matriz
matEsc<- matrix(NA, nrow=dims[2]*largPix, ncol = dims[1]*largPix)


for(   mm in 1:nrow(sumFa) ){
  
  # llenado de celdas por renglón
  c<- mm%%dims[1]
  
  if(c==0){
    c<- dims[1]
  }
  
  r<-(mm-c)/dims[1]+1
  
  
  # ubicar coordenadas del pixel donde inicia el fotomosaico
  rowI<-(r -1)*largPix+1
  colI<-(c -1)*largPix+1
  
  
  # Incorporar arreglo
  arreglo<- frameModTot %>% filter(num==sumFa$num[mm]) %>% 
    select(-num) %>% as.matrix()
  
  matEsc[rowI:(rowI+largPix-1),colI:(colI+largPix-1)]<-arreglo
  
  
  
}
matEsc[is.na(matEsc)]<- 0

# Despliegue final
imgCat<- rotate(matEsc)
image(imgCat, col  = gray((0:255)/255), 
      xlab = 'eje X',
      ylab = 'eje Y'
)
