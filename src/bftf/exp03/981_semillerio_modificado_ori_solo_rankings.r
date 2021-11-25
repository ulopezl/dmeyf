#Necesita para correr en Google Cloud
#32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#este codigo requiere cambiar como minimo:
#kexperimento --> nro del experimento
#nombre_archivo_bo --> valores de la bo
#karch_dataset --> dataset de la bo

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

require("primes")  #para generar semillas

directory.root <- "~/buckets/b1/"
setwd( directory.root )

#carpeta del experimento de subsampling con variante 005
kexperimento  <- 1007

#leer archivo de ganancia

#version nube
nombre_archivo_bo = "E1007_961_epic_exp_03_solo_rankings_BOlog.txt" #poner el nombre del archivo de bo del experimento
#nombre_archivo_bo = "ganancias005.txt"
ruta_archivo_bo = paste0("./work/E", kexperimento, "/", nombre_archivo_bo)
tabla_bo = fread(ruta_archivo_bo)

#ordenamos decreciente el archivo de ganancias
setorder(tabla_bo, -"ganancia")
mejor_iter = tabla_bo[1 , ]

kscript         <- "981_epic_semillerio"

#dataset correspondiente a la bo
karch_dataset   <- "./datasets/dataset_epic_v951_exp3_solo_ranking.csv.gz"  #el dataset que voy a utilizar

ktest_mes_hasta  <- 202011  #Esto es lo que uso para testing
ktest_mes_desde  <- 202011

## agregamos mes de prediccion para kaggle
kapply_mes       <- c(202101)  #El mes donde debo aplicar el modelo


kgen_mes_hasta   <- 202010  #hasta donde voy a entrenar
kgen_mes_desde   <- 201901  #desde donde voy a entrenar
kgen_meses_malos <- 202006  #el mes que voy a eliminar del entreanamiento

kgen_subsampling <- 1.0     #esto es NO hacer undersampling

campos_malos  <- c()   #aqui se deben cargar todos los campos culpables del Data Drifting

#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work

kresultados  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".txt" )  #archivo donde dejo el resultado

kkaggle       <- paste0("./kaggle/E",kexperimento, "/E",  kexperimento, "_", kscript, "_" ) ## archivo donde queda la salida de la semilla para kaggle

#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#cargo los datos donde voy a aplicar el modelo
dtest  <- copy( dataset[ foto_mes>= ktest_mes_desde &  foto_mes<= ktest_mes_hasta,  ] )

## cargo los datos donde vamos a predecir para kaggle
dapply  <- copy( dataset[  foto_mes %in% kapply_mes ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#agrego la marca de lo que necesito
#SI hago undersampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )

dataset[    foto_mes>= kgen_mes_desde  &
            foto_mes<= kgen_mes_hasta  & 
            !( foto_mes %in% kgen_meses_malos ) &
            ( clase01==1 | vector_azar < kgen_subsampling ),
          generacion:= 1L ]  #donde genero el modelo

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion", "test", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ generacion==1, clase01],
                        free_raw_data= TRUE
                      )

rm( "dataset" )   #libero memoria para el dataset
gc()              #garbage collection


#Estos son los parametros que estan fijos 
param_basicos  <- list( objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                        min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                        #lambda_l1= 0.0,         #por ahora, lo dejo fijo
                        #lambda_l2= 0.0,         #por ahora, lo dejo fijo
                        #max_bin= 31,            #por ahora, lo dejo fijo
                        force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                       )


#Estos hiperparametros salieron de la optimizacion bayesiana del script 961
#ganancia  7272500  ( sobre la mitad de 202011 )
#hiperparametros encontrados en la iteracion bayesiana 56 de un total de 100 inteligentes
param_ganadores  <- list( "learning_rate"= mejor_iter$learning_rate, 
                          "feature_fraction"= mejor_iter$feature_fraction,
                          "min_data_in_leaf"= mejor_iter$min_data_in_leaf,
                          "num_leaves"= mejor_iter$num_leaves,
                          "num_iterations"= mejor_iter$num_iterations,
                          "ratio_corte"= mejor_iter$ratio_corte,
                          "lambda_l1"= mejor_iter$lambda_l1,         
                          "lambda_l2"= mejor_iter$lambda_l2,         
                          "max_bin"= mejor_iter$max_bin            
                        )

#junto ambas listas de parametros en una sola
param_completo  <- c( param_basicos, param_ganadores )


#donde voy a guardar los resultados
tb_resultados  <- data.table( semilla= integer(),
                              subsamping= numeric(),
                              oficial= integer(),
                              meseta= integer(),
                              ganancia= numeric() )

set.seed( 102191 )   #dejo fija esta semilla
CANTIDAD_SEMILLAS  <- 10

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:CANTIDAD_SEMILLAS ]   #me quedo con CANTIDAD_SEMILLAS primos al azar
ksemillas  <- c( 999983, ksemillas )


for(  semillita  in  ksemillas )   #itero por las semillas
{
  gc()
  param_completo$seed  <- semillita   #asigno la semilla a esta corrida

  set.seed( semillita )
  #genero el modelo, los hiperparametros son siempre los mismos, la semilla CAMBIA
  modelo  <- lgb.train( data= dtrain,
                        param= param_completo )

  #aplico el modelo a los datos que elegi para testing  202011
  prediccion  <- predict( modelo, data.matrix( dtest[ , campos_buenos, with=FALSE]) )

  #creo una tabla con las probabilidades y la ganancia de ese registro
  tb_meseta  <- as.data.table( list( "prob"=prediccion,  "gan"=  dtest[ , ifelse( clase_ternaria=="BAJA+2", 48750, - 1250)] ))
  setorder( tb_meseta,  -prob )
  

  #calculo la ganancia  para el ratio de corte original
  pos_corte  <- as.integer( nrow(dtest)* param_completo$ratio_corte )
  
  ganancia   <- tb_meseta[  1:pos_corte, sum(gan) ]

  tb_resultados  <- rbind( tb_resultados, list( semillita, 
                                                kgen_subsampling,
                                                1,  #SI es el punto oficial
                                                pos_corte, 
                                                ganancia ) )  #agrego la ganancia estandar


  ##############################################
  ##aplico el modelo a los datos de kaggle 202101
  prediccion_kaggle  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )
  predsort_kaggle  <- sort(prediccion_kaggle, decreasing=TRUE)
  
  
  pos_corte_kaggle  <- as.integer(param_completo$ratio_corte*nrow(dapply))
  prob_corte_kaggle <- predsort_kaggle[ pos_corte_kaggle ]
  Predicted_kaggle  <- as.integer( prediccion_kaggle > prob_corte_kaggle )
  
  entrega_kaggle  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                   "Predicted"= Predicted_kaggle)  )
  
  
  #genero el archivo para Kaggle
  fwrite( entrega_kaggle, 
          file= paste0(kkaggle, "semilla_", semillita, ".csv" ),
          sep= "," )
  
  ################################################
  
  
  
  
  for( punto_meseta  in seq( 5000, 15000, by=500 ) )  #itero desde 5000 a 15000 , de a 500 
  {
    ganancia  <-  tb_meseta[ 1:punto_meseta, sum(gan) ]   #calculo la ganancia de los mejores punto_meseta registros

    tb_resultados  <- rbind( tb_resultados, list( semillita, 
                                                  kgen_subsampling, 
                                                  0,  #No es el punto oficial
                                                  punto_meseta, 
                                                  ganancia ) )  #agrego la ganancia estandar
  }

  #en cada iteracion GRABO
  fwrite(  tb_resultados,
           file= kresultados,
           sep= "\t" )

}

