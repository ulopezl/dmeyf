dataset_ori_ranking  <- fread( "./datasets/dataset_epic_v951_exp3_ori_y_ranking.csv.gz")
dataset_vars_andrea <- fread("./datasets/canaproxy_vidapropia_01_desde_ori_y_rank_andrea_y_tendymas_para_cbindear.csv.gz")


head_rank = head(dataset_ori_ranking)
head_andrea = head(dataset_vars_andrea)

#me quedo con los nombres de las variables andrea
vars_andrea = colnames(head_andrea)[colnames(head_andrea) %like% "AND_"]
vars_andrea = c(vars_andrea, "foto_mes", "numero_de_cliente")

#pruebo filtrar el head
#head_andrea = head_andrea[ , ..vars_andrea]

#filtro el dataset de verdad
dataset_vars_andrea = dataset_vars_andrea[ , ..vars_andrea]

merged = merge(dataset_ori_ranking, dataset_vars_andrea)

fwrite(merged, "./datasets/canaproxy_vidapropia_01_desde_ori_y_rank_andrea_y_tendymas_merged.csv.gz")