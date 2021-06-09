library(dplyr)

x <- readPNG( paste0(getwd(),"/try1/twitter.png" )  )
# x: dimensión de pixeles
dim(x)


# Nuevo punto
x0=x[,,1] # will hold the grayscale values divided by 255
x0=t(apply(x0, 2, rev)) # otherwise the image will be rotated

image(x0, col  = gray((0:255)/255)) # plot in grayscale
image(x0[-c(1:88),], col  = gray((0:255)/255)) # plot in grayscale
image(x0[-c((1:88), (568:dim(x0)[1]   )),], col  = gray((0:255)/255)) # plot in grayscale


class(x)
class(x0)


x0_esc<- x0 %>% as_tibble() %>% mutate_all( funs(replace(., is.numeric(.) , round(.*255)  ))  ) %>%
  mutate_if(is.numeric, as.integer)
# sapply(x0, function(x)round(x*255))
sapply(x0_esc, class)
class(x0_esc)
length(x0_esc)
dim(x0)[1]* dim(x0)[2]


apply(x0_esc,1, function(x)range(x)  )#%>% View()
apply(x0_esc,2, function(x)range(x)  )




apply(x0,1, function(x)range(x)  )
apply(x0,2, function(x)range(x)  )

# apply(x0,2, function(x)range(x)  ) %>% View()

# sapply(list, function)