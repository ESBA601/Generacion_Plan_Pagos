
# SE SEPARA POR PRODUCTOS Y SE SACA EL NOMBRE DEL ARCHIVO

LiquidacionComercial <- filter(LiquidacionTotal,  TIPO_PRODUCTO == "UVCC") %>%
  mutate( PRESTAMO_NUMERO=paste("PRESTAMO",PRESTAMO), PROPUESTA_NUMERO=paste("PROPUESTA",PROPUESTA)) 

# SE HACE UN BUCLE CON LA INFORMACION

for (i in 1:nrow(LiquidacionComercial)) {
  
# LA PRIMERA ES CUANDO NO HAY VARIACION DEL TIPO DE CAMBIO
  
  if (LiquidacionComercial$MARCADOR[i] == 1) {
    
    # SE CREA NUEVA VARIABLE PARA IDENTIFICAR LAS CUOTAS 
    
    VarAux1 <- c()
    
    VarAux1 <-"CUOTAS_PRESTAMO"
    
    VarAux2 <- c()
    
    VarAux2 <- LiquidacionComercial$PRESTAMO[i]
    
    # SE CREA NUEVA VARIABLE PARA SABER CUANTAS CUOTAS TIENE EL PRESTAMO
    
    VarAux3 <- data.frame(CUOTAS=0:LiquidacionComercial$MESES[i]) 
    
    # SE ASIGINA LAS PRIMERAS VARIABLES IDENTIFICANDO LAS CUOTAS
    
    assign(paste(VarAux1,VarAux2,sep="_"),VarAux3)
    
    # CADA CLIENTE SE LE COLOCA LOS MESES QUE ESTA ACTIVO EL CREDITO
    
    PlanPagos <- cbind(LiquidacionComercial[i,],VarAux3)
    
    # SE CREA NUEVAS VARIABLES   
    
    PlanPagos <- mutate(PlanPagos, DIAS_POR_CUOTAS=CUOTAS*DIAS, FECHA_PAGO= as.Date(FECHA_LIQUIDACION + dmonths(CUOTAS)),
                        MONTO_ORIGINAL_US_=ifelse(PlanPagos$CUOTAS==0, PlanPagos$MONTO_ORIGINAL_US, 0),
                        MONTO_ORIGINAL_BS_=ifelse(PlanPagos$CUOTAS==0, PlanPagos$MONTO_ORIGINAL, 0),
                        #FECHA_LIQUIDACION_=ifelse(PlanPagos$CUOTAS==0, as.Date(PlanPagos$FECHA_LIQUIDACION), 0),
                        #FECHA_PAGO_=ifelse(PlanPagos$CUOTAS!=0, PlanPagos$FECHA_PAGO,00/00/00 )
                        CUOTAS_US_=ifelse(PlanPagos$CUOTAS==0, 0, PlanPagos$CUOTAS_US),
                        CUOTAS_BS_=ifelse(PlanPagos$CUOTAS==0, 0, PlanPagos$CUOTAS_BS))

    # SE ELIMINA ALGUNAS VARIABLES    
        
    PlanPagos <- select(PlanPagos, -PERIODICIDAD_CANT, -PERIODICIDAD, -FECHA_LIQUIDACION, -MESES, -CLIENTES, -DIAS_POR_CUOTAS, -DIAS, -CUOTAS_US, -CUOTAS_BS, -MONTO_ORIGINAL, -MONTO_ORIGINAL_US)
    
    # SE CREA NUEVAS VARIABLES
    
    PlanPagos <- mutate(PlanPagos, CUOTA_US_DEVALUADA=CUOTAS_BS_/TIPO_CAMBIO_PRESENTE, VARIACION_CUOTA_US= CUOTAS_US_ - CUOTA_US_DEVALUADA, CUOTA_VARIACION_BS=TIPO_CAMBIO_PRESENTE*VARIACION_CUOTA_US)

    # SE SELECCIONA LAS VARIABLES PARA LLEVAR AL PLAN DE PAGOS
    
    PlanPagos <- select(PlanPagos , CUOTAS, ID,  NOMBRE_CLIENTE, PROPUESTA, PRESTAMO, TC_LIQUIDACION, MONTO_ORIGINAL_US_, MONTO_ORIGINAL_BS_ , TIPO_CAMBIO_PRESENTE, MARCADOR, FECHA_PAGO, CUOTAS_US_, CUOTAS_BS_, CUOTA_VARIACION_BS)
    
    # SE CREA LAS TABLAS EN EXCEL Y EN EL PROGRAMA    
    
    VarAux1 <- c()  
    
    VarAux1 <-"PLAN_PAGOS"
    
    assign(paste(VarAux1,VarAux2,sep="_"),PlanPagos)
    
    write.xlsx(assign(paste(VarAux1,VarAux2,sep="_"),PlanPagos), paste({LiquidacionComercial$PRESTAMO_NUMERO[i]},'.xlsx'))
    
  } 

# LA SEGUNDA ES CUANDO NO HAY VARIACION DEL TIPO DE CAMBIO
  
  if (LiquidacionComercial$MARCADOR[i] == 0) {
    
    # SE CREA NUEVA VARIABLE PARA IDENTIFICAR LAS CUOTAS 
    
    VarAux1 <- c()
    
    VarAux1 <-"CUOTAS_PRESTAMO"
    
    VarAux2 <- c()
    
    VarAux2 <- LiquidacionComercial$PRESTAMO[i]
    
    # SE CREA NUEVA VARIABLE PARA SABER CUANTAS CUOTAS TIENE EL PRESTAMO
    
    VarAux3 <- data.frame(CUOTAS=0:LiquidacionComercial$MESES[i]) 
    
    # SE ASIGINA LAS PRIMERAS VARIABLES IDENTIFICANDO LAS CUOTAS
    
    assign(paste(VarAux1,VarAux2,sep="_"),VarAux3)
    
    # CADA CLIENTE SE LE COLOCA LOS MESES QUE ESTA ACTIVO EL CREDITO
    
    PlanPagos <- cbind(LiquidacionComercial[i,],VarAux3)

    # SE CREA NUEVAS VARIABLES  
        
    PlanPagos <- mutate(PlanPagos, DIAS_POR_CUOTAS=CUOTAS*DIAS, FECHA_PAGO= as.Date(FECHA_LIQUIDACION + dmonths(CUOTAS)),
                        MONTO_ORIGINAL_US_=ifelse(PlanPagos$CUOTAS==0, PlanPagos$MONTO_ORIGINAL_US, 0),
                        MONTO_ORIGINAL_BS_=ifelse(PlanPagos$CUOTAS==0, PlanPagos$MONTO_ORIGINAL, 0),
                        #FECHA_LIQUIDACION_=ifelse(PlanPagos$CUOTAS==0, as.Date(PlanPagos$FECHA_LIQUIDACION), 0),
                        #FECHA_PAGO_=ifelse(PlanPagos$CUOTAS!=0, PlanPagos$FECHA_PAGO,00/00/00 )
                        CUOTAS_US_=ifelse(PlanPagos$CUOTAS==0, 0, PlanPagos$CUOTAS_US),
                        CUOTAS_BS_=ifelse(PlanPagos$CUOTAS==0, 0, PlanPagos$CUOTAS_BS))
    
    # SE ELIMINA ALGUNAS VARIABLES
    
    PlanPagos <- select(PlanPagos, -PERIODICIDAD_CANT, -PERIODICIDAD, -FECHA_LIQUIDACION, -MESES, -CLIENTES, -DIAS_POR_CUOTAS, -DIAS, -CUOTAS_US, -CUOTAS_BS, -MONTO_ORIGINAL, -MONTO_ORIGINAL_US)
    
    # SE CREA NUEVAS VARIABLES
    
    PlanPagos <- mutate(PlanPagos, CUOTA_US_DEVALUADA=CUOTAS_BS_/TIPO_CAMBIO_PRESENTE, VARIACION_CUOTA_US= CUOTAS_US_ - CUOTA_US_DEVALUADA, CUOTA_VARIACION_BS=0)

    # SE SELECCIONA LAS VARIABLES PARA LLEVAR AL PLAN DE PAGOS     
        
    PlanPagos <- select(PlanPagos , CUOTAS, ID,  NOMBRE_CLIENTE, PROPUESTA, PRESTAMO, TC_LIQUIDACION, MONTO_ORIGINAL_US_, MONTO_ORIGINAL_BS_ , TIPO_CAMBIO_PRESENTE, MARCADOR, FECHA_PAGO, CUOTAS_US_, CUOTAS_BS_, CUOTA_VARIACION_BS)
    
    # SE CREA LAS TABLAS EN EXCEL Y EN EL PROGRAMA   
    
    VarAux1 <- c()
    
    VarAux1 <-"PLAN_PAGOS"
    
    assign(paste(VarAux1,VarAux2,sep="_"),PlanPagos)
    
    write.xlsx(assign(paste(VarAux1,VarAux2,sep="_"),PlanPagos), paste({LiquidacionComercial$PRESTAMO_NUMERO[i]},'.xlsx'))
    
  }
  
}
