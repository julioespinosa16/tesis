
# desplegado de imagen con función alterntiva
# Se usa display, de EBIMAGE


# ?image
# 
# library(lattice)
# # library(EBImage)
# 
# levelplot(matFin_img)
# 
# 
# install.packages("BiocManager") 
# BiocManager::install("EBImage")
# 


library(EBImage)
library(ijtiff)


# display(t(matFin_img), method="raster", all=TRUE)



# Desplegar resultado final

matFin_img<- matFin
# image(matFin_img, col  = gray((0:255)/255),
#       xlab='eje X: final plot'
# )

display(matFin_img, method="raster"
        # , all=TRUE
        )

