#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library("ggplot2")
#setwd("~/buckets/b1/")

#elegir directorio de trabajo. debe tener creada una carpeta llamada "plots" y los archivos de ganancias.
#setwd("")

corrida <- list()

## cargar nombre del experimento
experimento_nombre = "Experimento-1"

#cargo el dataset de ganancias en testing
corrida$arch_testing1  <- "ganancias_1003_todas_las_vars.txt"
corrida$arch_testing2  <- "experimento_uli_1001.txt"

#cargo el dataset de ganancias en kaggle
corrida$arch_kaggle1  <- "./work/E5016_991_epic.txt"
corrida$arch_kaggle2  <- "./work/E5018_992_epic.txt"


#leo los datasets
resultados_testing1  <- fread( corrida$arch_testing1 )
resultados_testing2  <- fread( corrida$arch_testing2 )

resultados_kaggle1  <- fread( corrida$arch_kaggle1 )
resultados_kaggle2  <- fread( corrida$arch_kaggle2 )


#divido por un millon para visualizar mas facil
resultados_testing1[   , ganancia  := ganancia/1e6 ]
resultados_testing2[   , ganancia  := ganancia/1e6 ]

resultados_kaggle1[   , ganancia_Public  := ganancia_Public/1e6 ]
resultados_kaggle2[   , ganancia_Public  := ganancia_Public/1e6 ]


## comparacion de ganancias en testing

#test de wilcox
wilcox.test(  resultados_testing1[ oficial==1, ganancia ],
              resultados_testing2[ oficial==1, ganancia ], paired=TRUE )


#creo un dataset con ambas ganancias
experimento1 = data.frame(
  Experimento = rep("Variante1", length(resultados_testing1[ oficial==1, ganancia ])),
  Ganancias = resultados_testing1[ oficial==1, ganancia ]
)

experimento2 = data.frame(
  Experimento = rep("Variante2", length(resultados_testing2[ oficial==1, ganancia ])),
  Ganancias = resultados_testing2[ oficial==1, ganancia ]
)

ganancias_experimentos_testing = rbind(experimento1, experimento2)

#nombre del plot
nombre_plot = paste0("plots/", experimento_nombre, "comp-testing.png")
png(nombre_plot, width = 750, height = 750) 

#ploteo las ganancias en test
ggplot(ganancias_experimentos_testing, 
       aes(x=Experimento,y=Ganancias, fill=Experimento)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme_light()+
  ggtitle(paste0(experimento_nombre, ": comparación de ganancias en testing"))+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=16), #change font size of legend text
        legend.title=element_text(size=16))  #change font size of plot title

dev.off()


