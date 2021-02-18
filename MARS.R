library(earth)
library(lubridate)
Sys.setlocale(category="LC_ALL", locale="Polish")

set.seed(123)

df<-london_merged
df$is_weekend=as.factor(df$is_weekend)
df$is_holiday=as.factor(df$is_holiday)
df$season=as.factor(df$season)
df$weather_code=as.factor(df$weather_code)
df$Hour <- hour(df$timestamp)

names(df) <- c("czas", "ilosc", "temperatura", "temperatura_odczuwalna",
               "wilgotnosc", "predkosc_wiatru", "pogoda", "czy_swieto",       
               "czy_weekend", "pora_roku",  "godzina")

df_nowy <- df[-c(7027, 15834), -1]

testowy<- sample(nrow(df_nowy), nrow(df_nowy)/4)
df_nowy.testowy<-df_nowy[testowy, ]
df_nowy.uczacy<-df_nowy[-testowy, ]


#### model MARS addytwny bez interakcji

mars1 <- earth(formula = ilosc ~ ., data=df_nowy.uczacy, trace=3) 

mars1
summary(mars1)

mars1$bx

# wynik fazy 1
mars1$dirs
mars1$cuts

# przebieg fazy 2
mars1$selected.terms
mars1$prune.terms

plot(mars1$rss.per.subset, xlab = 'Czynniki', ylab='RSS') 
plot(mars1$gcv.per.subset)

# model z interakcjami

mars2 <- earth(formula = ilosc ~ ., data=df_nowy.uczacy, degree=3, trace=3) 
mars2
summary(mars2)

######## model addytywny z lepszymi wartosciami parametrow

#gcv.nk <- sapply(X = 20:50, FUN = function(x) earth(formula = ilosc ~ ., data=df_nowy.uczacy, nk=x)$gcv)
#plot(y=gcv.nk, x=20:50)
#opt_nk <- min(which(gcv.nk == min(gcv.nk)) + 19)

#gcv.minspan <- sapply(X = 1:20, FUN = function(x) earth(formula = ilosc ~ ., data=df_nowy.uczacy, minspan=x)$gcv)
#plot(y=gcv.minspan, x=1:20) # parmetr minspan nie ma wplywu na GCV (dopoki nie osiagnie duzych wartosci, wtedy zwieksza gcv)

### optymalne nk

mars3 <- earth(formula = ilosc ~ ., data=df_nowy.uczacy, minspan=1, nk=37, trace=3)
summary(mars3) # lekkie polepszenie GCV, jednak takze lekko zwieksza RSq

# exhaustive przycinanie

#mars4 <- earth(formula = ilosc ~ ., data=df_nowy.uczacy, minspan=1, pmethod = 'exhaustive', nk=37)
#summary(mars4)

# nie wplywa na miary oceny

###### optymalny model z interakcjami

#gcv.degree <- sapply(X = 1:10, FUN = function(x) earth(formula = ilosc ~ ., data=df_nowy.uczacy, minspan=1, degree = x, nk=37)$gcv)
#plot(y=gcv.degree, x=1:10)
#opt_degree <- min(which(gcv.degree == min(gcv.degree)))

# interakcje powyzej poziomu 2-3 nie zmniejszaja istotnie gcv, najlepszy jest poziom 3

#gcv.nk <- sapply(X = 20:80, FUN = function(x) earth(formula = ilosc ~ ., data=df_nowy.uczacy, nk=x, degree = 3)$gcv)
#plot(y=gcv.nk, x=20:80, ylab='GCV', xlab='nk')
#opt_nk <- min(which(gcv.nk == min(gcv.nk)) + 19) 

# 79 to optymalna maksymalna liczba zmiennych obj. dla modelu z interakcjami (z zakresu 20 do 80 i tej proby uczacej)

#gcv.minspan <- sapply(X = 1:20, FUN = function(x) earth(formula = ilosc ~ ., data=df_nowy.uczacy, minspan=x, degree = 3, nk=opt_nk)$gcv)
#plot(y=gcv.minspan, x=1:20) # parmetr minspan nie ma wplywu na GCV (dopoki nie osiagnie duzych wartosci, wtedy zwieksza gcv)
#opt_minspan <- min(which(gcv.minspan == min(gcv.minspan)))

# nieistotna roznica pomiedzy roznymi wartosciami parametru minspan, najelpsza bylo wartosc 3



mars5 <- earth(formula = ilosc ~ ., data=df_nowy.uczacy, nk=79, degree=3,
               minspan=3) # w raporcie ten model nazywany jest MARS4, poniewaz poprzedni model nie byl udokumentowany

# metoda exhaustive pruning trwa zbyt dlugo przy tym poziomie interakcji oraz
# maksymalnej ilosci transforamcji zmiennych egzogenicznych
summary(mars5)
mars5

# RMSE

RMSE1 <- mean((predict(mars1, newdata = df_nowy.testowy) - df_nowy.testowy$ilosc)^2)^0.5
RMSE2 <- mean((predict(mars2, newdata = df_nowy.testowy) - df_nowy.testowy$ilosc)^2)^0.5
RMSE3 <- mean((predict(mars3, newdata = df_nowy.testowy) - df_nowy.testowy$ilosc)^2)^0.5
RMSE4 <- mean((predict(mars4, newdata = df_nowy.testowy) - df_nowy.testowy$ilosc)^2)^0.5
RMSE5 <- mean((predict(mars5, newdata = df_nowy.testowy) - df_nowy.testowy$ilosc)^2)^0.5

table <- rbind(RMSE1, RMSE2, RMSE3, RMSE4, RMSE5)
table

mean(df_nowy.testowy$ilosc)


plot(mars5, info = T)
plotmo(mars5)

# waznosc zmiennych
evimp(mars5)

miary5 <- rbind(mars5$grsq, mars5$rsq, RMSE5)
rownames(miary5) <- c('GRsq', 'Rsq', 'RMSE')
miary5



