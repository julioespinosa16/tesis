#definir renglones-columnas
set ROW; 
set COL; 

#definir indexado compuesto
set LINKS = {ROW,COL};

#define parámetros de distancia
param distance{LINKS} >0;

#define variables de decisión
var Travel {LINKS};

#define función objetivo a minimizar: distancia recorrida
minimize tot_dist: sum{ (i,j) in LINKS} distance[i, j]*Travel[i, j];

#Restricciones para cada ciudad
s.t. 
#restricciones de salida
exits{i in ROW}: 
sum{j in COL}Travel[i, j] =1;

#restricciones de entrada
enters{j in COL}: 
sum{i in COL}Travel[i, j] =1;

