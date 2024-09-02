# ELIMINAR LA NOTACION CIENTIFICA 

options(scipen=999) 

# PARA IDENTIFICAR LOS PRESTAMOS ACTIVOS CON CREDITO 

risk_1 <- left_join(RIESGO, CUENTA, by = "CUENTA_CONTABLE") %>% 
  mutate(DIG2 = str_sub(RIESGO$CUENTA_CONTABLE, 4, 5),
         DIG1= str_sub(RIESGO$CUENTA_CONTABLE, 1, 3),
         STUS= ifelse(DIG1=="131","VIG","VEN"),
         DIG0= str_sub(RIESGO$CUENTA_CONTABLE, 1, 1))%>%
  left_join( CARTERA, by = "DIG2")

# SUBCONJUNTO DE LOS CLIENTES ACTIVOS DEL BANCO

risk_str0 <- filter(risk_1, MAR_CUEN == "0")
risk_str1 <- filter(risk_1, DIG0 == "1" & DIG2 != "11" & DIG2 != "02")
risk_str2 <- filter(risk_1, DIG0 == "1" & DIG2 != "11" )

# SE AJUSTA LOS NOMBRES YA QUE SON LOS TIPOS DE CAMBIO DE LIQUIDACION

IDI <- rename(IDI, "FECHA_LIQUIDACION"="FECHA", "TC_LIQUIDACION"="TIPO_CAMBIO")

LiquidacionTotal <- filter(risk_str2,  DIG2 == "35" | DIG2 == "37" | DIG2 == "47") %>%
  select(PERIODICIDAD_CANT, PERIODICIDAD ,FECHA_LIQUIDACION, NOMBRE_CLIENTE, ID, TIPO_PRODUCTO, PRESTAMO, PROPUESTA, MONTO_ORIGINAL) %>%
  left_join( IDI, by = "FECHA_LIQUIDACION") 

LiquidacionTotal <-    mutate(LiquidacionTotal, MESES = ifelse (LiquidacionTotal$PERIODICIDAD=="D", LiquidacionTotal$PERIODICIDAD_CANT/30, LiquidacionTotal$PERIODICIDAD_CANT))

LiquidacionTotal$MESES <- trunc(LiquidacionTotal$MESES)

LiquidacionTotal <-    mutate(LiquidacionTotal, CUOTAS_BS=MONTO_ORIGINAL/MESES,
                              MONTO_ORIGINAL_US=MONTO_ORIGINAL/TC_LIQUIDACION, 
                              CUOTAS_US=CUOTAS_BS/TC_LIQUIDACION,
                              CLIENTES=row_number(),
                              TIPO_CAMBIO_PRESENTE=t_cambio,
                              DIAS=30,
                              MARCADOR=ifelse(TIPO_CAMBIO_PRESENTE>TC_LIQUIDACION,1,0))

LiquidacionComercial <- filter(LiquidacionTotal,  TIPO_PRODUCTO == "UVCC") %>%
  mutate( PRESTAMO_NUMERO=paste("PRESTAMO",PRESTAMO), PROPUESTA_NUMERO=paste("PROPUESTA",PROPUESTA)) 



