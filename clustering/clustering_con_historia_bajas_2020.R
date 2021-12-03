require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/canaproxy_dataset-clustering-andrea-SIN-CANARITOS.csv.gz", stringsAsFactors= TRUE)
gc()

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")

campos_and <- names(dataset)[names(dataset)%like% "AND"]

campos_tend <- paste0(campos_buenos, "_tend6")

campos_buenos <- c(campos_buenos, campos_and, campos_tend)

#campos_buenos <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )

#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico_andrea_1.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter

#hago los boxplots
pdf( paste0( paste0("./cluster_jerarquico_boxplots_andrea.pdf" ) ))

n_vars = length(names(dataset))
for (i in 1:n_vars){
  x = dataset[ , ..i]
  name_var = names(x)
  x = unlist(x)
  y = dataset$cluster2
  boxplot(x ~ y, main = name_var)
}

dev.off()

### buscamos la historia de los clientes que se dieron de baja

#cargo el dataset completo
dataset_completo  <- fread( "datasets/canaproxy_dataset-clustering-andrea-SIN-CANARITOS.csv.gz", stringsAsFactors= TRUE)

clientes = dataset$numero_de_cliente 

#clientes en dataset de bajas, foto mes posterior a enero 2020
dataset_completo_clientes = dataset_completo[numero_de_cliente %in% clientes & foto_mes>=202001 & foto_mes<202101 , ]

length(unique(dataset_completo_clientes$numero_de_cliente))

clientes_clase_blank = unique(dataset_completo_clientes[clase_ternaria=="", numero_de_cliente])

dataset_completo_clientes_filtrado = dataset_completo_clientes[!numero_de_cliente %in% clientes_clase_blank , ]

#ordeno el dataset por meses
setorder(dataset_completo_clientes_filtrado, -foto_mes)

for (i in dataset_completo_clientes_filtrado$numero_de_cliente){
  dataset_completo_clientes_filtrado = dataset_completo_clientes_filtrado[numero_de_cliente == i, 
                                                                          meses_a_la_baja := rownames(dataset_completo_clientes_filtrado[numero_de_cliente == i])]
}

dataset_completo_clientes_filtrado$numero_de_cliente[5]

View(dataset_completo_clientes_filtrado[numero_de_cliente == 4722798 , c("foto_mes", "clase_ternaria", "meses_a_la_baja", "cliente_edad")])

table(dataset_completo_clientes_filtrado$clase_ternaria)


merged = merge(dataset[, c("numero_de_cliente", "cluster2")], dataset_completo_clientes_filtrado)

fwrite(merged, "datasets/bajas_2020_historia_y_clustering.csv.gz")