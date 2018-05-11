#practica 2
# library(readr)
# library(outliers)
# library(graphics)

#llegir datasets 
setwd('/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning')
item_categories <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning/data/item_categories.csv")
shops <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning/data/shops.csv")
items <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/dataset/items.csv")


sales_train_v2 <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning/data/sales_train_v2.csv")
sample_submission <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning/data/sample_submission.csv")
test <- read.csv("~/Documents/UOC/TIPOLOGIA_DADES/Practica2/predict_future_sales_cleaning/data/test.csv")


#Treiem les columnes que no farem servir
sales_data <- sales_train_v2[,-(2)]
sales_data <- sales_data[,-(4)]

#Convertim la data en 2 columnes: mes i any


