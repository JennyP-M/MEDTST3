# Hello, world!
#
#' @title MEDTST3
#'
#' @description Este paquete ayuda a realizar medidas espec�ficas para los datos de testeo
#'
#' @param base,p_model,real_name
#' @param df_base, models=c("Q_30", "Q_50", "Q_70")
#' @param base_testeo, models=c("LIM_INF", "Q_MOD_30", "Q_MOD_50", "Q_MOD_70","LIM_SUP","Q_BESTY","Q_RG3","Q_MKT","Q_FALT")
#' @return NULL
#' @examples ase_fal_sob(DATA,p_model,"Q_REAL")
#' @examples gen_q_besty(BASE01,models = c("P_30", "P_50", "P_70", "P90"))
#' @examples srv_inv(BASE02, models = c("Q_30", "Q_50","Q_70", "Q_FALT"))

#' @export ase_fal_sob

#------1.
##############################
### FUNCION ASE_FAL_SOB ######
##############################

# (Aviso: Se omitiran las tildes)

#  FORMATO DE DATA IMPUT

#   LA FUNCION ADICIONA A LA DATA IMPUT EL RANGO DE ASERTIVIDAD Y CALCULA EL SOBRANTE Y FALTANTE PARA CADA OBSERVACION
#
#   RANGO DE ASERTIVIDAD: CORRESPONDE AL GRUPO AL QUE PERTENECE EL RATIO ENTRE EL VALOR REAL Y EL VALOR PREDICHO
#   SOBRANTE O FALTANTE : CORRESPONDE A LA DESVIACION ENTRE EL VALOR REAL Y EL VALOR PREDICHO
#
#   LA FUNCION PERMITE EVALUAR EL RANGO DE ASERTIVIDAD, FALTANTE Y SOBRANTE DE LOS MODELOS QUE SE LE ESPECIFIQUEN




# FUNCION

# PARAMETROS
# base  : Base de datos con los valores reales y predichos para cada observacion
# p_model : Vector que contiene los nombres de las columnas que almacenan las predicciones de los modelos
#            a evaluar
# real_name : Vector con el nombre de la columna que almacena el valor real


# CODIGO DE LA FUNCION

ase_fal_sob = function(base,p_model,real_name){

  # FALTANTE Y SOBRANTE
  base_1 = base[,c(p_model,real_name)]

  faltante_sobrante<- data.frame(FALT_SBR = apply(base_1[,p_model], 2, function(x){-base_1[,real_name]+x}))

  columns_name = paste("FALT_SBR",p_model,sep = "_")

  names(faltante_sobrante) =  columns_name

  # RANGO DE ASERTIVIDAD
  cal_rango = function(Q){

    Q_1 = ifelse(Q<=0.5,"a) 0-50%",
                 ifelse(Q<=0.8,"b) 50-80%" ,
                        ifelse(Q<=1.2,"c) 80-120%" ,
                               ifelse(Q<=1.8,"d) 120-180%" ,
                                      ifelse(Q<=2.5,"e) 180-250%" ,
                                             "f) >250%" )))))


    return(Q_1)


  }


  ratio <- data.frame(ASERT = apply(base_1[,p_model], 2, function(x){base_1[,real_name]/x}))
  rango <- data.frame(RANGO = apply(ratio, 2, function(x){cal_rango(x)}))

  rango_names = paste("RANGO",p_model,sep = "_")
  names(rango) = rango_names

  datos_finales = cbind(faltante_sobrante,rango)

  final = cbind(base,datos_finales)
  return(final)


}

# DATOS FICTICIOS
DATA = data.frame( DESCRIPCION = c("CAMISETA ANTHONY","VESTIDO JENNY", "POLERA JOSELYN",
                                   "PANTALON PAUL", "CAMISA PAVEL","POLO VLADIMIR", "BLUSA LISBY"),
                   Q_REAL = c(200,100,20,15,350,180,400),
                   Q_30 = c(100,80,2,12,200,140,500),
                   Q_50 = c(150,90,5,28,300,180,600),
                   Q_70 = c(180,102,10,32,400,200,850)
)



p_model= c("Q_30","Q_50","Q_70")

RESULTADO= ase_fal_sob(DATA,p_model,"Q_REAL")

head(RESULTADO)


#' @export gen_q_besty

################################################################
#---------------2.
##############################
### FUNCION Q_BESTY ######
##############################


rm(list = ls())

# Librerías a usar:

library(dplyr)

# La función es:

gen_q_besty<- function(df_base, models=c("Q_30", "Q_50", "Q_70")) {


  n_prod_total <- nrow(df_base)
  nomb_col_pre<- colnames(df_base)
  nomb_col<- nomb_col_pre
  nomb_col[grepl("REAL",nomb_col)]<- "Q_REAL"
  perc<- c("2.5",seq(10,90,by=10),"97.5")
  models_1<- models

  for (i in 1: length(perc)){
    #i=1
    nomb_col[grepl(perc[i],nomb_col)]<- paste0("Q_",perc[i])
    models_1[grepl(perc[i],models_1)]<- paste0("Q_",perc[i])
  }

  colnames(df_base)<- nomb_col

  df_dlt <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0), ]
  df_base <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0)==F, ]

  n_prod_compl <- nrow(df_base)

  n_models <- length(models_1)

  df_base_1_pre<- cbind(df_base[,c("CAMP_CODI_VENT","Q_REAL",models_1)])

  df_base_1<- data.frame(df_base_1_pre,
                         COCIENTE= abs(apply(df_base_1_pre[,3:(2+n_models)],2,function(x){(df_base_1_pre$Q_REAL/x)})-1))

  df_base_2<-df_base_1

  names(df_base_2)

  df_base_3<- data.frame(df_base_2,
                         COL_BESTY= apply(df_base_2[,(3+n_models):ncol(df_base_2)],1, function(x){which.min(x)}))

  n_besty= df_base_3$COL_BESTY + 2

  for (i in 1:nrow(df_base_3)){
    #i=1
    df_base_3$Q_BESTY[i]<- df_base_3[i,n_besty[i]]
  }

  X_RESULTADO <- list()
  X_RESULTADO[["DF_TOT"]] <- df_base
  X_RESULTADO[["DF_DTL"]] <- df_dlt
  X_RESULTADO[["DF_COC"]] <- df_base_1
  X_RESULTADO[["DF_COC_CER"]] <- df_base_2
  X_RESULTADO[["DF_BESTY"]] <- df_base_3$Q_BESTY
  print(paste("Productos totales ingresados:", nrow(df_base), sep=" "))
  print(paste("Productos incompletos eliminados:", nrow(df_dlt), sep=" "))

  df_base_4<- cbind(df_base, Q_BESTY= df_base_3[,c("Q_BESTY")])
  return(df_base_4)
}


# ¿Qué hace la función?
# Calcula arroja el valor de aquel percentil que está más próximo al valor real de la referencia. Este cálculo es individual por referencia.

# Hiperparámetros de la función:
# df_base= Base de datos, el nombre no es estrictamente df_base.
# models= Son los percentiles que ingresarán al cálculo del besty. La función utiliza por defecto el Q_30, Q_50 y Q_70. Pero se pueden espeficar de 2 a más.

# Forma de ingreso de la base de datos
# La base solamente necesita 2 nombres de columnas específicas: "CAMP_CODI_VENT" y al menos una columna que diga "REAL" donde se fija la venta real del SKU. Este puede ser "Q_REAL", "REAL", por ejemplo.
# Además, necesita naturalmente columnas con los percentiles que se deseen, donde tiene que tener al menos los númros de percentiles: 2.5, 10, 20, 30, 40, 50, ... , 90. 97.5, de ahí lo puede acompañar con cualquier otra letra
# De ahí no es necesario particionar la base, podemos tener las columnas que querramos sin importar el orden.

# Ejemplos
# imaginemos una base de datos con la siguiente estructura:

BASE01<- data.frame(PAIS= c(rep("PER",50), rep("COL",50)),
                    CAMP_CODI_VENT=c(paste(rep(c(202001:202010),10),c(1234:1333),sep="_")),
                    CAMP= rep(c(202001:202010),10),
                    DESCRIPCION= paste("SKU", c(1234:1333), sep="_"),
                    CODI_VENT= c(1234:1333),
                    MODELO= "PRENAC",
                    REAL= 1000+(1:100)*50+ rnorm(100,400,200),
                    RG3= 1000+(1:100)*50+ rnorm(100,300,300),
                    MKT= 1000+(1:100)*50+ rnorm(100,300,400),
                    P30= 1000+(1:100)*50+ rnorm(100,100,20),
                    P50= 1000+(1:100)*50+ rnorm(100,300,20),
                    P70= 1000+(1:100)*50+ rnorm(100,500,20),
                    P90= 1000+(1:100)*50+ rnorm(100,600,10)
)


# Lo ideal es que se especifique todo como "Q_" seguido del modelo o de RG3, MKT, o REAL, pero podemos correrlo como está:

# Primero corremos sin identificar los percentiles que necesito, la función me generará el Besty solamente evaluando el P30, P50, P70

BASE02<- gen_q_besty(BASE01)

# También le podemos especificar algun percentil más, por ejemplo que el Besty lo halle también hasta el P90.

BASE03<- gen_q_besty(BASE01,models = c("P_30", "P_50", "P_70", "P90"))

# O también puede ser:
BASE04<- gen_q_besty(BASE01,models = c("Q_30", "Q_50", "Q_70", "Q90"))



#' @export srv_inv

#######################################################################
#3.----------
##############################
### FUNCION SER_INV ######
##############################

rm(list = setdiff(ls(),"gen_q_besty"))
# Librerías a usar:

library(dplyr)

# La función es:
srv_inv<- function(base_testeo, models=c("LIM_INF", "Q_MOD_30", "Q_MOD_50", "Q_MOD_70","LIM_SUP","Q_BESTY","Q_RG3","Q_MKT","Q_FALT")){


  nomb_col_pre<- colnames(base_testeo)
  nomb_col<- nomb_col_pre
  nomb_col[grepl("REAL",nomb_col)]<- "Q_REAL"
  perc<- c("2.5",seq(10,90,by=10),"97.5", "RG3", "MKT", "FALT")
  models_1<- models

  for (i in 1: length(perc)){
    #i=1
    nomb_col[grepl(perc[i],nomb_col)]<- paste0("Q_",perc[i])
    models_1[grepl(perc[i],models_1)]<- paste0("Q_",perc[i])
  }

  colnames(base_testeo)<- nomb_col
  drop_falt<- which(models_1 =="Q_FALT")
  models_2<- models_1[-drop_falt]
  base_testeo<- as.data.frame(base_testeo)

  base_testeo_1<- data.frame(base_testeo,
                             SRV= apply(base_testeo[,models_2], 2, function(x){ifelse(base_testeo$Q_REAL>x, x, base_testeo$Q_REAL)}),
                             SRV_REAL= base_testeo$Q_REAL- base_testeo$Q_FAL,
                             INV= apply(base_testeo[,models_2], 2, function(x){ifelse(base_testeo$Q_REAL>x, 0, x-base_testeo$Q_REAL)})
  )

  nomb_cols_1<- paste0("SRV_",models_2)
  nomb_cols_2<- paste0("INV_",models_2)

  nomb_cols_tot<- c(nomb_col_pre, nomb_cols_1,"SRV_REAL",nomb_cols_2)
  colnames(base_testeo_1)<- nomb_cols_tot


  return(base_testeo_1)

}



# ¿Qué hace la función?
# Calcula las cantidades individuales de la generación del servicio y de inventario (por cada sku)

# Hiperparámetros de la función:
# df_base= Base de datos, el nombre no es estrictamente df_base.
# models= Son los percentiles que ingresarán al cálculo de los imputs para generar el nivel de servicio e inventario La función utiliza por defecto el LIM_INF, Q_30, Q_50, Q_70, LIM_SUP,
# MKT, RG3 y FALT que es para el cálculo del Real. Pero se pueden espeficar los que se deseen

# Forma de ingreso de la base de datos
# La base solamente necesita que al menos una columna que diga "REAL" donde se fija la venta real del SKU. Este puede ser "Q_REAL", "REAL", por ejemplo, y que exista una columna con FALT, que es para el cálculo del servicio REAL.
# Además, necesita naturalmente columnas con los percentiles que se deseen, donde tiene que tener al menos los númros de percentiles: 2.5, 10, 20, 30, 40, 50, ... , 90. 97.5, de ahí lo puede acompañar con cualquier otra letra
# De ahí no es necesario particionar la base, podemos tener las columnas que querramos sin importar el orden.
# También es necesario especificarle los métodos alternativos: Q_MKT, Q_RG3.
# Ejemplos
# imaginemos una base de datos con la siguiente estructura:

BASE01<- data.frame(PAIS= c(rep("PER",50), rep("COL",50)),
                    CAMP_CODI_VENT=c(paste(rep(c(202001:202010),10),c(1234:1333),sep="_")),
                    CAMP= rep(c(202001:202010),10),
                    DESCRIPCION= paste("SKU", c(1234:1333), sep="_"),
                    CODI_VENT= c(1234:1333),
                    MODELO= "PRENAC",
                    Q_REAL= 1000+(1:100)*50+ rnorm(100,400,200),
                    Q_RG3= 1000+(1:100)*50+ rnorm(100,300,300),
                    Q_MKT= 1000+(1:100)*50+ rnorm(100,300,400),
                    LIM_INF= 1000+(1:100)*50+ rnorm(100,50,10),
                    Q_30= 1000+(1:100)*50+ rnorm(100,100,20),
                    Q_50= 1000+(1:100)*50+ rnorm(100,300,20),
                    Q_70= 1000+(1:100)*50+ rnorm(100,500,20),
                    LIM_SUP= 1000+(1:100)*50+ rnorm(100,600,10),
                    Q_FALT= ifelse(rnorm(100,200,100)<0, 0,rnorm(100,200,100))
)


# Lo ideal es que se especifique todo como "Q_" seguido del modelo o de RG3, MKT, o REAL, pero podemos correrlo como está:

# Primero generamos el Q_BESTY, si no lo especificamos nos arrojará el más cercano al real entre P30, P50 y P70
BASE02<- gen_q_besty(BASE01)

# Ahora hacemos el cálculo del servicio e inventario

BASE03<- srv_inv(BASE02)

# Podemos hacer el cálculo solo de algunos percentiles, pero siempre con el Q_FATL para el cálculo del real
BASE04<- srv_inv(BASE02, models = c("Q_30", "Q_50","Q_70", "Q_FALT"))


