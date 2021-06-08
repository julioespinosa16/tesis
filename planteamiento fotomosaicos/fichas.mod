
#definir tipos de figuras
set FIG; 

#definir renglones-columnas
set ROW; 
set COL; 

#definir indexado compuesto
set COMP = {FIG, ROW, COL};
set CELL = {ROW, COL};

#define parámetros de costo
param cost{COMP} >0;
#define disponibilidad de recursos ('inventario' de figuras)
param quant{FIG} >0;

#define variables de decisión: uso de f sobre (i, j)
var use{COMP};

#define función objetivo a minimizar
#costo de colocar figura tipo f en celda (i, j)
minimize tot_dist: sum{ (f,i,j) in COMP} cost[f,i, j]*use[f,i, j];

#Restricciones
s.t. 
#restricciones de uso de inventario
inv{f in FIG}: 
sum{(i, j) in CELL}use[f, i, j] <= quant[f];

#restricciones llenado de cada celda
cell_fill{(i, j) in CELL}: 
sum{f in FIG}use[f, i, j]  =1;


