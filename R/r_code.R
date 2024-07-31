#wczytanie danych

data<-read.csv("data_final.csv",header=TRUE,dec=",",sep=";",na.strings="NA")
head(data)
names(data)

#instalacja pakietów
install.packages("stats")
install.packages("car")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("broom")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ISLR")
library(dplyr)
library(ISLR)
library("stats")
library("car")
library("lmstats")
library("ggplot")
library("broom")
library("glmnet")
library("lmtest")
#stworzenie zmiennej do regresji, zawiera wszykstkie regresory 

Reg_1<-data[,3:32]
names(Reg_1)

#zmienna zawierająca tylko gatunki gier

genre<-data[,15:32]

#pierwszy model regresji wyjąsniający ilośc poztywnych ocen, wszytkimi regresorami
Model_1<-lm(data[,33]~., data=Reg_1)
summary(Model_1)

#model wyjasnjacy przewidywana ilosc posaidaczy przez gatunki gier

##Model_2<-lm(data[,14]~.,data=genre)
##ummary(Model_2)
# wynik wskzuje na nie istotnosc parametrow oraz bardzo male dopasowanie 

#redukcja modelu 1

#stworzenie macierzy ziemmnych mogacych byc łącznie nie istotne 

##Redukcja<-Reg_1[, c(1,3,6,8:11)]
##names(Redukcja)

##Model_3<-lm(data[,33]~.,data=Redukcja)
##summary(Model_1)
##summary(Model_3)

##test_f_1<-anova(Model_3,Model_1)
##print(test_f_1)

##SSE_1<-13350^2
##SSE_3<-13350^2

##F_stat<-((SSE_3/SSE_1)-1)*(26373/7)
##F_stat
#zredukowany model charakteryzują się niewiele mniejszym dopsowaniem (R^2) oraz dużo większa statyską F

#testowanie modelu regresji z wyłączniem zmeinnych odpowiedzialnych za gatunek gry 
Reg_2<-Reg_1[,1:12]

summary(Model_4)

#Dalsze analizy zostan przerpowadzone za pomocą najbardziej zredukowanego modelu_3

#Wwykres reszt
##plot(Model_3,which=1)

#wykres wartosci residuów do przewidywanych wartości
##plot(Model_3,which=3)

##plot(Model_3,which = 5)

#metody krokowe 
Model_1_red<-step(Model_1,direction="both")
summary(Model_1_red)
#testowanie heteroskedastycznosci 

# Sprawdź reszty modelu
residuals <- resid(Model_1_red)

# Wykres 
plot(Model_1_red)


#test bp
bptest(Model_1_red)

#występuje hetreoskedatycznosc 

#testowanie wspólninowsoci
vif(Model_1_red)

#występuje wspólinowosć avg_playtime oraz median_playtime 

summary(Model_1_red)
#usuniecie avg_playtime 


nowa_formula <- update(formula(Model_1_red),. ~ . - average_playtime)

# Ponowne dopasowanie modelu z zaktualizowaną formułą
Model_red <- lm(nowa_formula, data = data)
summary(Model_red)
vif(Model_red)
bptest(Model_red)


#usuwanie hetreoskedastycznosci 

#transporamcja positive_ratings na ln

ln_positive_ratings<-log(data[,33])
ln_positive_ratings

#usniecie wartosc -inf

ln_positive_ratings<-log(data[,33] +1)
ln_positive_ratings

#model ze zmieniona zmienna 
Model_ln<-lm(ln_positive_ratings~.,data=Reg_1)
summary(Model_ln)
Model_ln_red<-step(Model_ln,direction="both")
summary(Model_ln_red)
bptest(Model_ln_red)
plot(Model_ln_red)
summary(ln_positive_ratings)
hist(ln_positive_ratings)

#usniecie wartosci odstających 
data_1<-cbind(Reg_1,ln_positive_ratings)
#data_1_ogr<-data_1[data_1$ln_positive_ratings<=10,]

#eksport by usnac recznie outliery
write.csv(data_1,"obrobione_dane_1.csv",row.names=FALSE)
#usniecie ze wszytkich kolumn najbardziej odastajcych wartosci
#ponowne wczytanie zbioru 
data_1_ogr<-read.csv("obrobione_dane_1_po.csv",header=TRUE,dec=",",sep=";",na.strings="NA")

names(data_1_ogr)

#hist(data_1_ogr$ln_positive_ratings)

#utworzenie modelu z zredukowanym zbiorem 
Model_ln_ogr<-lm(data_1_ogr[,31]~.,data=data_1_ogr[,1:30])
summary(Model_ln_ogr)
Model_ln_ogr_red<-step(Model_ln_ogr,direction="both")
summary(Model_ln_ogr_red)
vif(Model_ln_ogr_red)
bptest(Model_ln_ogr_red)
plot(Model_ln_ogr_red)


#poraz kolejny pojawiaj sie wartosci odstające 
#ponowan próba ich usnięcia 
#wczytanie nowego zbioru i nadpisanie modeleu 
data_1_ogr<-read.csv("obrobione_dane_2_po.csv",header=TRUE,dec=",",sep=";",na.strings="NA")
Model_ln_ogr<-lm(data_1_ogr[,31]~.,data=data_1_ogr[,1:30])
summary(Model_ln_ogr)
Model_ln_ogr_red<-step(Model_ln_ogr,direction="both")
summary(Model_ln_ogr_red)
plot(Model_ln_ogr_red)

#jeszcze raz 
#poraz kolejny pojawiaj sie wartosci odstające 
#ponowan próba ich usnięcia 
#przepadnaie zbiowosc w celu zlokalizowaniu wartosc, za którymi mogą być wartosci odstajcce 
hist(data_1_ogr$age)
hist(data_1_ogr$min_owners)
hist(data_1_ogr$median_playtime)
hist(data_1_ogr$achievements)

#usuniecie obserwacji które mają 0 w avg playtime, #czy to ma snens?
#prawdopodobnie nie bo zmienna objasnaia nie jest zerowa dla tych obserawcji
data_1_ogr<-read.csv("obrobione_dane_3_po.csv",header=TRUE,dec=",",sep=";",na.strings="NA")
Model_ln_ogr<-lm(data_1_ogr[,31]~.,data=data_1_ogr[,1:30])
summary(Model_ln_ogr)
Model_ln_ogr_red<-step(Model_ln_ogr,direction="both")
summary(Model_ln_ogr_red)
plot(Model_ln_ogr_red)
bptest(Model_ln_ogr_red)
vif(Model_ln_ogr_red)

#wyniki poporawily sie lecz dalej nie sa zadawaljace 
#zastosownie merody Boxa-Coxa aby lepiej prztransofowmaoc zmienna objasniana


#wczytanie nowego zbioru i nadpisanie modeleu 
data_1_ogr<-read.csv("obrobione_dane_4_po.csv",header=TRUE,dec=",",sep=";",na.strings="NA")
Model_ln_ogr<-lm(data_1_ogr[,31]~.,data=data_1_ogr[,1:30])
summary(Model_ln_ogr)
Model_ln_ogr_red<-step(Model_ln_ogr,direction="both")
summary(Model_ln_ogr_red)
plot(Model_ln_ogr_red)
bptest(Model_ln_ogr_red)
vif(Model_ln_ogr_red)

#wyniki poporawily sie lecz dalej nie sa zadawaljace 
#zastosownie merody Boxa-Coxa aby lepiej prztransofowmaoc zmienna objasniana
install.packages("MASS")
library("MASS")

nowa_zmienna<- exp(data_1_ogr[,31])
boxcox_result <- boxcox(nowa_zmienna ~ 1, data = data_1_ogr)
print(boxcox_result)
# Znalezienie optymalnej wartości lambda
lambda_opt <- boxcox_result$x[which.max(boxcox_result$y)]
print(lambda_opt)

Positve_rating_trasnformed<-((nowa_zmienna^lambda_opt - 1) / lambda_opt)
print(Positve_rating_trasnformed)

#aktualizacja zbioru danych 

print(data_1_ogr[,31])
data_1_ogr<-cbind(data_1_ogr,Positve_rating_trasnformed)
data_1_ogr<-subset(data_1_ogr,select=-ln_positive_ratings)
names(data_1_ogr)
print(data_1_ogr[,31])

print(Positve_rating_trasnformed)
#ponowan estymacja modelu z nowa zmienna objasnina 

Model_tr<-lm(Positve_rating_trasnformed~.,data=data_1_ogr[,1:30])
summary(Model_tr)
Model_tr_red<-step(Model_tr,direction="both")
summary(Model_tr_red)
plot(Model_tr_red)
bptest(Model_tr_red)
vif(Model_tr_red)

#Użycie Modelu lasso aby wyelimocowac wspólniowosc 

#Podzielenie zbioru na treningowy i testowy

x = model.matrix(Positve_rating_trasnformed~.,data_1_ogr)[,-31]
y= data_1_ogr %>%select(Positve_rating_trasnformed) %>%unlist() %>%as.numeric()

set.seed(1)

train = data_1_ogr %>% sample_frac(0.5)
test = data_1_ogr %>% setdiff(train)

x_train = model.matrix(Positve_rating_trasnformed~., train)[,-31]
x_test = model.matrix(Positve_rating_trasnformed~., test)[,-31]

y_train = train %>%
  select(Positve_rating_trasnformed) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(Positve_rating_trasnformed) %>%
  unlist() %>%
  as.numeric()

#trenowanie modelu 
grid = 10^seq(10, -2, length = 100)

lasso_mod = glmnet(x_train, y_train, alpha = 1, lambda = grid)
plot(lasso_mod)

cv.out = cv.glmnet(x_train, y_train, alpha = 1)
plot(cv.out)

#dobór opytmalnej lambdy
bestlam = cv.out$lambda.min 
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) 
mean((lasso_pred - y_test)^2) 
bestlam

# Dopasowanie modelu lasso do pełnego zbioru danych
out = glmnet(x, y, alpha = 1, lambda = grid) 

# Wyświetlanie współczynników przy użyciu lambda wybranego przez CV
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:31,] 
lasso_coef

# Wyświetlanie tylko niezerowych współczynników
lasso_coef[lasso_coef != 0]

#sprawdzenie heteroskedastyczncosci 
plot(lasso_pred,which=2)


