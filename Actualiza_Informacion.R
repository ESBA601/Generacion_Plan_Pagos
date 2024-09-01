
# ACTUALIZAR LA FECHA QUE SE NECESITA 
date0 <- as.character(231130)

# ACTUALIZAR LA FECHA QUE SE NECESITA
TC <- filter(IDI, FECHA==fecha0)

# UBICA LA TASA DEL MES
t_cambio = TC$TIPO_CAMBIO

# SEPARA LOS DIAS MESES Y AÑOS
da <- str_sub(date0, 1, 2)
dm <- str_sub(date0, 3, 4)
dd <- str_sub(date0, 5, 6)

# SEPARA LOS DIAS MESES Y AÑOS
fecha0 <- paste(20,da,"-",dm,"-",dd)
fecha0 <- gsub(" ","",fecha0)
fecha0 <- as.Date(fecha0,"%Y-%m-%d")
fecha1 <- as.data.frame(fecha0)

# TAMBIEN SE OBTIENE LA FECHA PRESENTE
VarAux1 <- c()
VarAux1 <- paste(20,da,"-",dm,"-",01)
VarAux1 <- gsub(" ","",VarAux1)
VarAux1 <- as.Date(VarAux1,"%Y-%m-%d")

