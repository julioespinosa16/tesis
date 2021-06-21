
# Convierte csv de x a png con nombre sencillo

rotate <- function(x) t(apply(x, 2, rev))

# apply(x0, 1,  function(x)range(x) ) %>% t() %>% View()


# de x a números
imgX<- read.csv( paste0(getwd(), '/jc_ex/jc equis.csv') , stringsAsFactors = FALSE)
names(imgX)<- paste0( 'Var', 1:ncol(imgX)  )



imgX<- imgX %>% mutate_all( funs(replace(., is.character(.),  1-nchar(.) ))  ) %>%
  mutate_if(is.character, as.numeric)%>% as.matrix()
# sapply(imgX, class)
# 

image(rotate(imgX), col  = gray((0:255)/255)) # plot in grayscale
# image(  rotate(rotate(imgX)), col  = gray((0:255)/255)) # plot in grayscale
# image(  rotate(rotate(rotate(imgX))  ), col  = gray((0:255)/255)) # plot in grayscale
# image(  rotate(rotate(rotate(rotate(imgX)))  ), col  = gray((0:255)/255)) # plot in grayscale
# image(  rotate(rotate(rotate(rotate(rotate(imgX))) )  ), col  = gray((0:255)/255)) # plot in grayscale
