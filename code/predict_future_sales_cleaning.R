#practica 2
install.packages("Rcpp")
install.packages("RcppArmadillo")
install.packages("quadprog")
install.packages("fracdiff")
install.packages("curl")
install.packages("forecast")

install.packages("rstan")
install.packages("prophet")

install.packages("mblm")
install.packages("rq")
install.packages("coin")
install.packages("ggpubr")

library(forecast)
library(Rcpp)
library(prophet)

library(Hmisc)
library(dplyr)
library(plyr)





# library(readr)
# library(outliers)
# library(graphics)

##### 1 Descripció dels datasets #####
item_categories <- read.csv("item_categories.csv")
shops <- read.csv("shops.csv")
items <- read.csv("items.csv")
sample_submission <- read.csv("sample_submission.csv")
test <- read.csv("test.csv")

#vendes
vendes <- read.csv("sales_train_v2.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
vendes <- read.csv("sales_train_v2_data_format.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
vendes <- read.csv("dades_totals.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

attach(vendes)
dim(vendes)
summary(vendes)
describe(vendes)
str(vendes)
head(vendes)


str(shops)
head(shops)
str(items)
head(items)



##### 3 Neteja de les dades ######



#tipus dades assignades a cada camp
sapply(vendes, function(x) class(x))

#funció describe del package Hmisc podem analitzar el nombre de variables del data.frame
describe(vendes)
describe(item_categories)
describe(items)

#merge sales i item_categories
library(raster)
# merge per les variables ocumnuescommon variable, here called ''
m <- merge(vendes, items, by='item_id')

#comprovar nuls
sapply(vendes, function(x) sum(is.na(x)))

#comprovar zeros
subset(vendes, date_block_num==0)
length(subset(vendes, shop_id==0))
subset(vendes, shop_id==0)
subset(vendes, item_id==0)
subset(vendes, item_price=0)
subset(vendes, item_cnt_day==0)

subset(vendes, item_price>40000)
subset(vendes, item_id==13403)
subset(items, item_id==13403)
subset(item_categories, item_category_id==16)
subset(vendes, item_cnt_day>300)

binnedCounts(subset(vendes, ITEM_PRICE<10000)[,"item_price", drop=FALSE])
hist(subset(vendes, item_price<3000 & item_cnt_day<6 & item_cnt_day>0))
hist(subset(vendes, item_price<3000 & item_cnt_day==1))


# comprovar outliers
par(mfrow=c(1,5))
boxplot(vendes$date_block_num)
boxplot(vendes$shop_id)
boxplot(vendes$item_id)
boxplot(vendes$item_price)
boxplot(vendes$item_cnt_day)
boxplot (vendes$item_price)
boxplot (vendes$item_cnt_day)
outliersItemId <-boxplot.stats(item_id)$out 
indexItemId <- which( item_id %in% outliersItemId)
length(indexItemId)

outliersItemCnt
# item_price per intervals
binnedCounts(vendes[,"item_price", drop=FALSE])
with(vendes, Hist(item_price, scale="frequency", breaks="Sturges", col="darkgray"))

# utilitzem boxplot.stats per a veure els valors outliers de ITEM_PRICE
outliersItemPrice <-boxplot.stats(item_price)$out 
outliersItemPrice
table(outliersItemPrice)
  
indexItemPrice <- which( item_price %in% outliersItemPrice)
length(indexItemPrice)

# eliminem els registres que contenen outliers de ITEM_PRICE
vendes<-vendes[-indexItemPrice,]

dim(vendes)
subset(vendes, item_price>2124)

# item_cnt_day per intervals
binnedCounts(vendes[,"item_cnt_day", drop=FALSE])
with(vendes, Hist(item_cnt_day, scale="frequency", breaks="Sturges", col="darkgray"))


# utilitzem boxplot.stats per a veure els valors outliers de ITEM_CNT_DAY
outliersItemCnt <-boxplot.stats(item_cnt_day)$out 
outliersItemCnt
  
indexItemCnt <- which( item_cnt_day %in% outliersItemCnt)
length(indexItemCnt)

# eliminem els registres que contenen outliers de ITEM_CNT_DAY
vendes<-vendes[-indexItemCnt,]

dim(vendes)

subset(vendes, item_cnt_day>1)



##### 4 Anàlisi de les dades ######
### Comprovació de la normalitat  ###

# estimar els paràmetres de la distribució normal a partir de la funció fitdistr del paquet MASS, en la variable item_price
require(MASS)
ajust <- fitdistr(item_price,"normal")
ajust

#test Kolmogorov-Smirnov per comprovar la normalitat. Si p<0.05
Ks<- ks.test(item_price, "pnorm", mean =ajust$estimate[1], sd= ajust$estimate[2])
Ks

# estimar els paràmetres de la distribució normal a partir de la funció fitdistr del paquet MASS, en la variable item_cnt_day
require(MASS)
ajust <- fitdistr(item_cnt_day,"normal")
ajust

#test Kolmogorov-Smirnov per comprovar la normalitat. Si p<0.05
Ks<- ks.test(item_cnt_day, "pnorm", mean =ajust$estimate[1], sd= ajust$estimate[2])
Ks

par(mfrow = c(1,1))
shapiro.test(X.ITEM_PRICE.) --sample size must be between 3 and 5000




###### Objectiu 1:  Quins variables influeixen més en el preu dels productes? ######

vendes <- read.csv("dades_totals.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
attach(vendes)
str(vendes)
dim(vendes)
head(vendes)


#funcio sample_frac del package dplyr
require(dplyr)
mostra <- sample_frac(vendes, 0.05, replace = FALSE)
dim(mostra)

cor.test(x = vendes$ITEM_PRICE, y = vendes$ITEM_CNT_DAY,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")


cor.test(x = vendes$ITEM_PRICE, y = vendes$DATE_BLOCK_NUM,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_PRICE, y = vendes$SHOP_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_PRICE, y = vendes$ITEM_CATEGORY_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_PRICE, y = vendes$ITEM_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")


cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$ITEM_PRICE,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")


cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$DATE_BLOCK_NUM,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$SHOP_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$ITEM_CATEGORY_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$ITEM_ID,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")


# utilitzem boxplot.stats per a veure els valors outliers de ITEM_PRICE
outliersCnt <-boxplot.stats(vendes$ITEM_PRICE)$out 
table(outliersCnt)
  
indexCnt <- which( vendes$ITEM_PRICE %in% outliersCnt)
length(indexCnt)

# eliminem els registres que contenen outliers de ITEM_PRICE
vendes<-vendes[-indexCnt,]

dim(vendes)

cor.test(x = vendes$ITEM_PRICE, y = vendes$ITEM_CNT_DAY,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")


cor.test(x = vendes$ITEM_PRICE, y = vendes$DATE_BLOCK_NUM,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

cor.test(x = vendes$ITEM_CNT_DAY, y = vendes$DATE_BLOCK_NUM,
         alternative = "two.sided", conf.level = 0.95, method = "spearman")

#Mijançant diagrama dispersió, comprovem si existeix relació lineal o monotònica.
#Si no hi ha relació, no té sentit calcular aquest tipus de correlacions.
require(MASS)
require(ggplot2)



ggplot(data = vendes, aes(x = ITEM_PRICE, y = DATA_BLOCK_NUM)) +
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersió") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = vendes, aes(x = ITEM_PRICE, y = ITEM_CNT_DAY)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersió") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = vendes, aes(x = ITEM_CNT_DAY, y = DATA_BLOCK_NUM)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersió") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))





###### Objectiu 2: La facturació és superior durant el segon semestre de l’any?  ######

vendes_1semestre <- read.csv("dataset_primer_semestre.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
vendes_2semestre <- read.csv("dataset_segon_semestre.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

str(vendes_1semestre)
str(vendes_2semestre)
head(vendes_1semestre)
head(vendes_2semestre)

#comprovar normalitat dels datasets
# estimar els paràmetres de la distribució normal a partir de la funció fitdistr del paquet MASS, en la variable vendes_1semestre$TOTAL
require(MASS)
ajust <- fitdistr(vendes_1semestre$TOTAL,"normal")
ajust

#test Kolmogorov-Smirnov per comprovar la normalitat. Si p<0.05
Ks<- ks.test(vendes_1semestre$TOTAL, "pnorm", mean =ajust$estimate[1], sd= ajust$estimate[2])
Ks

par(mfrow=c(1,3))
hist(vendes_1semestre$TOTAL, xlab="Semestre 1 - Total", ylab="Freqüència", las=1, main="")
plot(density(vendes_1semestre$TOTAL), xlab="Semestre 1 - Total", ylab="Densitat", las=1, main="")
qqnorm(vendes_1semestre$TOTAL, xlab="Quantils teòrics", ylab="Quantils mostrals", las=1,main="")
qqline(vendes_1semestre$TOTAL)

# estimar els paràmetres de la distribució normal a partir de la funció fitdistr del paquet MASS, en la variable vendes_2semestre$TOTAL
require(MASS)
ajust <- fitdistr(vendes_2semestre$TOTAL,"normal")
ajust

#test Kolmogorov-Smirnov per comprovar la normalitat. Si p<0.05
Ks<- ks.test(vendes_2semestre$TOTAL, "pnorm", mean =ajust$estimate[1], sd= ajust$estimate[2])
Ks

par(mfrow=c(1,3))
hist(vendes_2semestre$TOTAL, xlab="Semestre 2 - Total", ylab="Freqüència", las=1, main="")
plot(density(vendes_2semestre$TOTAL), xlab="Semestre 2 - Total", ylab="Densitat", las=1, main="")
qqnorm(vendes_2semestre$TOTAL, xlab="Quantils teòrics", ylab="Quantils mostrals", las=1,main="")
qqline(vendes_2semestre$TOTAL)

#analitzem la variància amb el test Fligner-Killeen
fligner.test(x = list(vendes_1semestre$TOTAL,vendes_2semestre$TOTAL))

#test Mann–Whitney–Wilcoxon
wilcox.test(x = vendes_1semestre$TOTAL, y = vendes_2semestre$TOTAL, alternative = "less", mu = 0, paired = FALSE, conf.int = 0.95)


#eliminem els outliers de vendes_1semestre
# utilitzem boxplot.stats per a veure els valors outliers de vendes_1semestre$TOTAL
outliers1sem <-boxplot.stats(vendes_1semestre$TOTAL)$out 
  
index1sem <- which( vendes_1semestre$TOTAL %in% outliers1sem)
length(index1sem)

# eliminem els registres que contenen outliers de vendes_1semestre
vendes_1semestre<-vendes_1semestre[-index1sem,]

#eliminem els outliers de vendes_2semestre
# utilitzem boxplot.stats per a veure els valors outliers de vendes_2semestre$TOTAL
outliers2sem <-boxplot.stats(vendes_2semestre$TOTAL)$out 
  
index2sem <- which( vendes_2semestre$TOTAL %in% outliers2sem)
length(index2sem)

# eliminem els registres que contenen outliers de vendes_2semestre
vendes_2semestre<-vendes_2semestre[-index2sem,]

#analitzem la variància amb el test Fligner-Killeen
fligner.test(x = list(vendes_1semestre$TOTAL,vendes_2semestre$TOTAL))

#test Mann–Whitney–Wilcoxon
wilcox.test(x = vendes_1semestre$TOTAL, y = vendes_2semestre$TOTAL, alternative = "less", mu = 0, paired = FALSE, conf.int = 0.95)

boxplot(vendes_1semestre$TOTAL,vendes_2semestre$TOTAL, names=c("semestre1","semestre2"))

#provem el t.test
t.test(vendes_1semestre$TOTAL,vendes_2semestre$TOTAL,alternative = "less")
t.test(vendes_1semestre$TOTAL,vendes_2semestre$TOTAL,alternative = "great")
 




###### Objectiu 3: Crearem models de regressió que permetin predir el preu dels productes ######

vendes <- read.csv("dades_totals.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
attach(vendes)
str(vendes)
dim(vendes)
head(vendes)

#funcio sample_frac del package dplyr per crear una mostra
require(dplyr)
mostra <- sample_frac(vendes, 0.001, replace = FALSE)
dim(mostra)



#Kendall–Theil Sen Siegel nonparametric linear regression
library(mblm)
set.seed(1234)
model1.k <- mblm(ITEM_PRICE ~ DATE_BLOCK_NUM, data=mostra)
summary(model1.k)

model2.k <- mblm(ITEM_PRICE ~ SHOP_ID, data=mostra)
summary(model2.k)

model3.k <- mblm(ITEM_PRICE ~ ITEM_CATEGORY_ID, data=mostra)
summary(model3.k)

model4.k <- mblm(ITEM_PRICE ~ ITEM_CNT_DAY, data=mostra)
summary(model4.k)


model5.k <- mblm(ITEM_PRICE ~ ITEM_CNT_DAY + DATE_BLOCK_NUM, data=mostra)
summary(model5.k)
Only linear models are accepted


model6.k <- mblm(ITEM_PRICE ~ ITEM_CNT_DAY + DATE_BLOCK_NUM + ITEM_CATEGORY_ID, data=mostra)
summary(model6.k)
Only linear models are accepted


newdata <- data.frame(
DATE_BLOCK_NUM = 30
)

predict(model1.k, newdata)

plot(ITEM_PRICE ~ DATE_BLOCK_NUM, data = mostra, pch  = 16)




#Quantile regression
install.packages("quantreg")
library(quantreg)
vendes <- read.csv("dades_totals.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
set.seed(1234)

#funcio sample_frac del package dplyr per crear una mostra
require(dplyr)
mostra <- sample_frac(vendes, 0.001, replace = FALSE)
dim(mostra)
model1_q50 <- rq(ITEM_PRICE ~ DATE_BLOCK_NUM, tau = 0.5, data = mostra)
summary(model1_q50)

model2_q50 <- rq(ITEM_PRICE ~ ITEM_CNT_DAY + DATE_BLOCK_NUM, tau = 0.5, data = mostra)
summary(model2_q50)

model3_q50 <- rq(ITEM_PRICE ~ ITEM_CNT_DAY + DATE_BLOCK_NUM + ITEM_CATEGORY_ID, tau = 0.5, data = mostra)
summary(model3_q50)






