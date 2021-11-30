rm(list=ls())
require("data.table")
setwd("~/buckets/b1/")

karch_dataset  <- "./datasets/canaproxy_vidapropia_ori_vars_profe_and_mas_ranked_YMAS.csv.gz"
nombre_recortado  <- "450_imp_canaproxy_vidapropia_ori_vars_profe_and_mas_ranked_YMAS.csv.gz"
importance_file = './work/impo_vidapropia_ori_vars_profe_and_mas_ranked_YMAS.txt'

importancias_to_filter = fread(importance_file)
vars_imp = importancias_to_filter$Feature[!(importancias_to_filter$Feature %like% 'canarito')]
vars_imp = vars_imp[1:450]
vars_no_filtrar = c("foto_mes", "clase_ternaria", "numero_de_cliente")
vars = c(vars_imp, vars_no_filtrar)
vars = unique(vars)
dataset  <- fread(karch_dataset,select = vars)
#dataset = dataset[,vars, with=FALSE]
filename = paste0('./datasets/',nombre_recortado)
fwrite(dataset,filename)

start_time <- Sys.time()
dataset2 = fread('./datasets/450_imp_canaproxy_vidapropia_ori_vars_profe_and_mas_ranked_YMAS.csv.gz')
end_time <- Sys.time()
end_time - start_time
