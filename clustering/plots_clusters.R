merged$meses_a_la_baja = as.numeric(merged$meses_a_la_baja)
merged_12 = merged[meses_a_la_baja <= 12 , ]

#dt_agg <- merged[ , .(mean = mean(ctrx_quarter, na.rm=TRUE)), by=.(cluster2,meses_a_la_baja)]

#dt_cast <- dcast(dt_agg, meses_a_la_baja~cluster2, value.var="mean")

#dt_cast$meses_a_la_baja = as.numeric(dt_cast$meses_a_la_baja)
#setorder(dt_cast, -meses_a_la_baja)

# alternative:
dt_cast[ , matplot(-meses_a_la_baja, .SD, type="l", ylab="mean", xlab="Meses antes de la baja", col=2:8, lty=2:8, lwd=2, main="ctrx_quarter"), .SDcols = !"meses_a_la_baja"]
legend("topright", names(dt_cast)[2:8], col=2:8 , lty=2:8 ,cex=0.8, lwd=2)

pdf( paste0( paste0("./cluster_jerarquico_lineplots_andrea_12.pdf" ) ))

n_vars = length(names(merged))
for (i in 1:n_vars){
  x = merged[ , ..i]
  name_var = names(x)
  x = unlist(x)
  dt_agg <- merged[ , .(mean = as.numeric(mean(get(name_var), na.rm=TRUE))), by=.(cluster2,meses_a_la_baja)]
  dt_cast <- dcast(dt_agg, meses_a_la_baja~cluster2, value.var="mean")
  dt_cast$meses_a_la_baja = as.numeric(dt_cast$meses_a_la_baja)
  setorder(dt_cast, -meses_a_la_baja)
  dt_cast = dt_cast[meses_a_la_baja <=12, ]
  #plot
  dt_cast[ , matplot(-meses_a_la_baja, .SD, type="l", ylab="mean", xlab="Meses antes de la baja", col=2:8, lty=2:8, lwd=2, main=name_var), .SDcols = !"meses_a_la_baja"]
  legend("topright", names(dt_cast)[2:8], col=2:8 , lty=2:8 ,cex=0.8, lwd=2)
  
}

dev.off()




### CON MEDIANAS


pdf( paste0( paste0("./cluster_jerarquico_lineplots_andrea_medianas.pdf" ) ))

n_vars = length(names(merged))
for (i in 1:n_vars){
  x = merged[ , ..i]
  name_var = names(x)
  x = unlist(x)
  dt_agg <- merged[ , .(median = as.numeric(median(get(name_var), na.rm=TRUE))), by=.(cluster2,meses_a_la_baja)]
  dt_cast <- dcast(dt_agg, meses_a_la_baja~cluster2, value.var="median")
  dt_cast$meses_a_la_baja = as.numeric(dt_cast$meses_a_la_baja)
  setorder(dt_cast, -meses_a_la_baja)
  #plot
  dt_cast[ , matplot(-meses_a_la_baja, .SD, type="l", ylab="median", xlab="Meses antes de la baja", col=2:8, lty=2:8, lwd=2, main=name_var), .SDcols = !"meses_a_la_baja"]
  legend("topright", names(dt_cast)[2:8], col=2:8 , lty=2:8 ,cex=0.8, lwd=2)
  
}

dev.off()

n_vars = length(names(merged))

x = merged[ , "cliente_antiguedad"]
name_var = names(x)
x = unlist(x)
y = merged$cluster2
boxplot(x ~ y, main = name_var)

