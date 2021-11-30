require("data.table")

dataset = fread('./')
# los que se bajaron en algún momento
bajas_1 = unique(dataset[ clase_ternaria == 'BAJA+1' ]$numero_de_cliente)
length(bajas_1)
# Todos los que fueron continua en algun momento, basicamente todos
continuas = unique(dataset[clase_ternaria == 'CONTINUA' ]$numero_de_cliente)
# los continua que no se bajaron nunca, post 2019
continuas = continuas[!(continuas %in% bajas_1)]

length(continuas)
dim(dataset[ numero_de_cliente %in% bajas_1 ])

#sample_bajas = sample(bajas_1,length(bajas_1)/2,replace=F)
dataset_bajas = dataset[ numero_de_cliente %in% bajas_1]

dataset_cont = dataset[ numero_de_cliente %in% sample(continuas,length(continuas)/50,replace=F) ]
dim(dataset_cont)

sample_dataset = rbind(dataset_cont, dataset_bajas)

dim(sample_dataset)

length(unique(sample_dataset$numero_de_cliente)) == length(unique(dataset_bajas$numero_de_cliente))+length(unique(dataset_cont$numero_de_cliente))

dim(sample_dataset)
dim(dataset)

fwrite(sample_dataset,'./datasets/bajas_1_mas_sample_cont_450_imp_canaproxy_vidapropia_01_desde_ori_y_rank_andrea_y_tendymas_version_post_merge_correcto.csv.gz"')

colnames()
