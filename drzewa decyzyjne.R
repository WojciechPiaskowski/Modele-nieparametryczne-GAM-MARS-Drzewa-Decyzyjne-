library(rpart)
library(rpart.plot)
#install.packages('randomForest')
library(randomForest)
#install.packages('caret')
library(caret)


# zaladowac dane, plik weatherAUS.csv
weatherAUS <- read.csv("F:/WP/Studia/magister/sem3/modele nieparametryncze/projekt/weatherAUS.csv")
# dodanie zmiennej miesiac
weatherAUS$month <- format(x=as.Date(weatherAUS$Date), format='%m')
# pozbycie się kolumny date oraz zmiennych ze znaczną ilością braków danych (powyżej 10%)
data <- weatherAUS[, -which(names(weatherAUS) %in% c('Date', 'Evaporation', 'Sunshine', 'Pressure9am', 'Pressure3pm',
                                                     'Cloud9am', 'Cloud3pm', 'Location', 'RainToday'))]
summary(data)

# okolo 25k obserwacji wyrzucony z 145k

data <- na.omit(data)
summary(data)

# histogram istotnych zmiennych

hist((data$RainTomorrow=="Yes")*1, xlab='0 - brak deszczu następnego dnia, 1 - deszcz następnego dnia',
     ylab='ilość', main='')
hist(data$Humidity3pm, ylab='ilość', xlab='wilgotność powietrza o 15:00 dnia poprzedniego', main='')
hist(data$WindGustSpeed, xlab='najwyższa prędkośc wiatru dnia poprzedniego', ylab='ilość', main='')
hist(data$Humidity9am, ylab='ilość', xlab='wilgotność powietrza o 9:00 dnia poprzedniego', main='')

# podzielenie na proby testowe i treningowe

testowe.nr <- sample(x = nrow(data), size = nrow(data)/3, replace = F)

deszcz.test <- data[testowe.nr, ]
deszcz.train <- data[-testowe.nr, ]

drzewo.deszcz <- rpart(RainTomorrow ~ ., data = deszcz.train)

drzewo.deszcz
summary(drzewo.deszcz)

rpart.rules(drzewo.deszcz, style = 'tallw')
rpart.plot(drzewo.deszcz)

yhat_base <- predict(object = drzewo.deszcz, newdata = deszcz.test, type = 'class')

# ustalic optymalne parametry modelu
# maksymalne drzewo oraz przyciete

printcp(drzewo.deszcz)

drzewo.deszcz <- rpart(RainTomorrow ~ ., data = deszcz.train, control = rpart.control(cp = 0))

printcp(drzewo.deszcz)
plotcp(drzewo.deszcz)

bledy <- printcp(drzewo.deszcz)

tmp1 <- which.min(bledy[, "xerror"])  # min błąd w sprawdzaniu krzyżowym
tmp2 <- sum(bledy[tmp1, c("xerror", "xstd", 'xstd', 'xstd')]) # min błąd + odchylenie standardowe
optymalny <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa


drzewo.deszcz.opt <- prune(tree = drzewo.deszcz, cp = bledy[optymalny, 'CP'])
drzewo.deszcz.best <- prune(tree = drzewo.deszcz, cp = bledy[tmp1, 'CP'])

rpart.plot(drzewo.deszcz.opt)
rpart.plot(drzewo.deszcz.best)


# waznosc poszczegolnych predyktorow

drzewo.deszcz.opt$variable.importance
cbind(drzewo.deszcz.opt$variable.importance)
dotchart(sort(drzewo.deszcz.opt$variable.importance, decreasing = F), pch = 16)

# predykcja zbioru testowego

yhat <- predict(object = drzewo.deszcz.opt, newdata = deszcz.test, type = 'class')
yhat_best <- predict(object = drzewo.deszcz.best, newdata = deszcz.test, type = 'class')
yhat_full <- predict(object = drzewo.deszcz, newdata = deszcz.test, type = 'class')
y <- deszcz.test$RainTomorrow

mean(yhat != y)
mean(yhat_best != y)
mean(yhat_base != y)
mean(yhat_full != y)

yhat_p = predict(object = drzewo.deszcz.opt, newdata = deszcz.test, type = 'prob')
head(yhat_p)

hist(yhat_p[y == 'Yes', 'No'])

# Losowy las (domyślne parametry)

RFC <- randomForest(RainTomorrow ~ ., data=deszcz.train, importance=TRUE, ntree=500, mtry=3)
yhat_rfc <-predict(object = RFC, newdata = deszcz.test, type = 'class')

# miary efektwnosci klasyfikiacji
# ze wzgledu na niezbalansowany problem, F1-score będzie lepszą miarą niż trafność

print_meausers <- function(yhat, y, positive) {

  trafnosc <- sum(yhat==y) / length(yhat)
  precyzja <- posPredValue(yhat, y, positive=positive)
  czulosc <- sensitivity(yhat, y, positive=positive)
  F1 <- (2 * precyzja * czulosc) / (precyzja + czulosc)

  table <- (cbind(trafnosc=c(round(trafnosc, 3)),
                  precyzja=c(round(precyzja, 3)),
                  czulosc=c(round(czulosc, 3)),
              F1_score=c(round(F1, 3))))
  return(table)
}

domyslne <- print_meausers(yhat_base, y, "Yes")
cale <- print_meausers(yhat_full, y, "Yes")
optimal <- print_meausers(yhat, y, "Yes")
best <- print_meausers(yhat_best, y, "Yes")
forest <- print_meausers(yhat_rfc, y, "Yes")

table <- rbind(domyslne, cale, optimal, best, forest)

rownames(table) <- c('domyslne','cale','optymalne drzewo',
                     'Najlepsze drzewo', 'Losowy las')
table


#### inny próg (zakładamy większą wagę predykcji deszczu niż dla jego braku)

drzewo.koszt <- rpart(RainTomorrow ~ ., data = deszcz.train, parms=list(loss=c(0, 4, 1, 0)))
bledy <- printcp(drzewo.koszt)
tmp1 <- which.min(bledy[, "xerror"])  # min błąd w sprawdzaniu krzyżowym
tmp2 <- sum(bledy[tmp1, c("xerror", "xstd", 'xstd')]) # min błąd + odchylenie standardowe
opt_koszt <- which(bledy[, "xerror"] < tmp2)[1] # nr optymalnego drzewa
drzewo.koszt <- prune(tree = drzewo.koszt, cp = bledy[opt_koszt, 'CP'])
yhat_koszt <- predict(object = drzewo.koszt, newdata = deszcz.test, type = 'class')
kosztowe <- print_meausers(yhat_koszt, y, "Yes")

table2 <- rbind(table, kosztowe)
rownames(table2) <- c('domyslne','cale','optymalne drzewo',
                     'Najlepsze drzewo', 'Losowy las', 'zwazone drzewo')
table2
