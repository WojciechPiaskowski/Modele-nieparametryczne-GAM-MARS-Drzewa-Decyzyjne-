#wypożyczalnia rowerów
library(mgcv)
library(manipulate)
Sys.setlocale(category="LC_ALL", locale="Polish")
#library(magrittr)
library(lubridate)
#library("plyr")
df<-london_merged
df
#przeglad i zmiana typu zmiennych  
str(df)
df$is_weekend=as.factor(df$is_weekend)
df$is_holiday=as.factor(df$is_holiday)
df$number=as.integer(df$number)
df$number=as.integer(df$number)
df$season=as.factor(df$season)
df$weather_code=as.factor(df$weather_code)
str(df)
#zrobienie porzadku z data
#df$date <- as.character(df$timestamp, format = "%e/%d/%y")
# create month day as a number that we can order as a factor
#df$Month <- as.character(df$timestamp, format = "%m")
#df$month <- as.character(df$timestamp, format = "%m")
#df$Day <- as.character(df$timestamp, format = "%u")
#df$day <- as.character(df$timestamp, format = "%u")
df$Hour <- hour(df$timestamp)
df$Hour
#df$Hour=as.factor(df$Hour)
#df$Hour=as.integer(df$Hour)
df$Hour
#zmiana nazwy zmiennych
names(df)
names(df) <- c("czas", "ilosc", "temperatura", "temperatura_odczuwalna",
               "wilgotnosc", "predkosc_wiatru", "pogoda", "czy_swieto",       
               "czy_weekend", "pora_roku",  "godzina")
df
#podstawowe wykresy, lokalizacja brakow, blednych danych
# a) number
plot(df$ilosc, xlab="nr obserwacji", ylab="ilosc nowych wypozyczen")
boxn<-boxplot(df$ilosc, ylab="ilosc nowych wypozyczen")
boxn #moim zdaniem te liczby sa wiarygodne, ilosc wypozyczen rowerow ok
hist(df$ilosc, breaks=20, col=rainbow(20), xlab="ilosc nowych wypozyczen", ylab="ilosc obserwacji",
     labels=T)
# b) temperature
plot(df$temperatura, xlab="nr obserwacji", ylab="temperatura powietrza") #bledna obserwacja
boxt<-boxplot(df$temperatura, col="green", horizontal=T,
              border="brown", xlab="nr obserwacji", ylab="ilosc nowych wypozyczen")
boxt #teraz usuwam bledna obserwacje, reszte obserwacji zostawiam
plot(df$temperatura[df$temperatura<35], ylab=
       "temperatura", xlab="nr obserwacji")
boxtm<-boxplot(df$temperatura[df$temperatura<35], col="green", horizontal=T,
               border="brown", xlab="temperatura powietrza")
boxtm #moim zdaniem już ok, temperatura wiarygdna
hist(df$temperature[df$temperature<35], breaks=25, col=rainbow(10))
#c)  temperature_feels
plot(df$temperatura_odczuwalna) #bledne obserwacje
boxtf<-boxplot(df$temperatura_odczuwalna, col="green", horizontal=T,
               border="brown")
boxtf
plot(df$temperatura_odczuwalna[df$temperatura_odczuwalna<35], ylab=
       "odczuwalna temperatura", xlab="nr obserwacji")
boxtfm<-boxplot(df$temperatura_odczuwalna[df$temperatura_odczuwalna<35], col="green", horizontal=T,
                border="brown")
boxtfm
hist(df$temperatura_odczuwalna[df$temperatura_odczuwalna<35], breaks=20, col=rainbow(20),
     labels = T, xlab=
       "odczuwalna temperatura", ylab="ilosc obserwacji")
#d) humidity
plot(df$wilgotnosc, xlab="nr obserwacji", ylab="wilgotnosc (w %)") 
boxplot(df$wilgotnosc) #jest ok (?)
hist(df$wilgotnosc, breaks=20, col=rainbow(20), xlab="wilgotnosc (w %)", ylab="czestosc", label=T)
#e) wind_speed (w km/h)
plot(df$predkosc_wiatru, xlab="nr obserwacji", ylab="predkosc wiatru (km/h)")#dwa bledy
boxws<-boxplot(df$predkosc_wiatru, xlab="nr obserwacji", ylab="predkosc_wiatru")
boxws
plot(df$predkosc_wiatru[df$predkosc_wiatru<100], ylab=
       "predkosc wiatru (km/v)", xlab="nr obserwacji")
boxwsm<-boxplot(df$predkosc_wiatru[df$predkosc_wiatru<100], ylab=
                  "predkosc wiatru (km/v)", xlab="nr obserwacji")
boxwsm #chyba ok
hist(df$predkosc_wiatru[df$predkosc_wiatru<100], breaks=12,
     col=rainbow(20), label=T, ylab="czestosc", xlab="predkosc wiatru (km/h)"
)
#f) weather_code, is_holiday, is_weekend, season

plot(df$pogoda, xlab="rodzaj pogody", ylab="czestosc") #pogoda ok,
#1 = Clear ; mostly clear but have some values with haze/fog/patches of fog/ fog in vicinity 
#2 = scattered clouds / few clouds 3 = Broken clouds 4 = Cloudy 
#7 = Rain/ light Rain shower/ Light rain 10 = rain with thunderstorm 
#26 = snowfall
#prop.table(df$pogoda)
plot(df$czy_wakacje,xlab="obserwacja swiateczna", ylab="czestosc") #jest ok
plot(df$czy_weekend, xlab="obserwacja weekendowa", ylab="czestosc") #jest ok
# 1 - jest, 0 - nie jest
plot(df$pora_roku, xlab="pora roku", ylab="czestosc") #jest ok
#0-spring ; 1-summer; 2-fall; 3-winter

#Nie stwierdziem brakow danych, byly dane niewiarygodne (bledne) - te wyrzucam.
#Najpierw modele bez tych wartosci blednych, pozniej bez outlierow...
#nzo_35_100<-df$number[df$temperature_feels<35&df$wind_speed<100]
#tfzo_35_100<-df$temperature_feels[df$temperature_feels<35&df$wind_speed<100]
#hzo_35_100<-df$humidity[df$temperature_feels<35&df$wind_speed<100]
#wszo_35_100<-df$wind_speed[df$temperature_feels<35&df$wind_speed<100]
#szo_35_100<-df$season[df$temperature_feels<35&df$wind_speed<100]
#plot(df$wind_speed[c(-7027,-15834)])
#plot(df$is_holiday)
#plot(df$is_weekend)
#model1<-gam(nzo_35_100 ~ s(tfzo_35_100) + s(hzo_35_100) + s(wszo_35_100)+ szo_35_100,
#              data=df)
df_nowy<-df[df$temperatura_odczuwalna<35&df$predkosc_wiatru<100,]
df_nowy

model1_nowy<-gam(ilosc~s(temperatura_odczuwalna, k=14)+
                   s(predkosc_wiatru, k=13)+s(godzina, k=22)+czy_weekend,
                 data=df_nowy)


model1_liniowy<-gam(ilosc~temperatura_odczuwalna+
                      predkosc_wiatru+godzina+czy_weekend,
                    data=df_nowy)
summary(model1_nowy)
summary(model1_liniowy)

model1_nowy$gcv.ubre
model1_liniowy$gcv.ubre
model1_nowy$aic
model1_liniowy$aic


gam.check(model1_nowy)

#14 13 22

str(df_nowy)

#modelhour<- gam(number~s(Hour), data=df_nowy)
#summary(modelhour)
#gam.check(modelhour)
#plot(modelhour, pages=1,
#     rug=T,
#     residuals = T,
#     scheme = 1,
#     all.terms = T)

summary(model1_nowy)
gam.check(model1_nowy)
plot(model1_nowy, pages=1,
     rug=T,
     residuals = F,
     scheme = 1,
     all.terms = T, ylab="wplyw na ilosc wypozyczen")

model1_nowy$gcv.ubre
model1_nowy$aic
#summary(model1)
#gam.check(model1)
#plot(model1, pages=1,
#     residuals = F,
#     scheme = 1,
#     all.terms = T)
#model1$gcv.ubre
#model1$aic
#model1lin_nowy<-gam(number~temperature_feels+humidity+wind_speed+season,
#                 data=df_nowy)
#summary(model1lin_nowy)
#gam.check(model1lin_nowy)
#plot(model1lin_nowy, pages=1,
#     rug=T,
#     residuals = T,
#     scheme = 0,
#     all.terms = T)
#model1lin_nowy$gcv.ubre
#model1lin_nowy$aic
# proba restrykcyjna - okazuje sie ze nie miala wiekszego znaczenia
#df_out<-df[df$temperature_feels<31.5&df$wind_speed<37,]
#model1_out<-gam(number~s(temperature_feels)+s(humidity)+s(wind_speed)+season,
#                data=df_out)
#summary(model1_out)
#gam.check(model1_out)
#plot(model1_out, pages=1,
#     rug=T,
#     residuals = T,
#     scheme = 1,
#     all.terms = T)
#model1_out$gcv.ubre
#model1_out$aic


#Teraz moge porownac dokladnosc predykcji dla model1_nowy i model1lin_nowy, dzielac
#zbior danych na probe uczaca i testowa
set.seed(123)
testowy<- sample(nrow(df_nowy), nrow(df_nowy)/4)
testowy
df_nowy.testowy<-df_nowy[testowy, ]
df_nowy.uczacy<-df_nowy[-testowy, ]
model1_nowy.u<-gam(ilosc~s(temperatura_odczuwalna, k=14)+s(predkosc_wiatru, k=13)+
                     s(godzina, k=22)+czy_weekend,
                   data=df_nowy.uczacy)
model1_liniowy.u<-gam(ilosc~temperatura_odczuwalna+predkosc_wiatru+godzina+
                        czy_weekend,
                      data=df_nowy.uczacy)
(mean((predict(model1_nowy.u, newdata=df_nowy.testowy)-df_nowy.testowy$ilosc)^2))^(0.5)
(mean((predict(model1_liniowy.u, newdata=df_nowy.testowy)-df_nowy.testowy$ilosc)^2))^(0.5)



#kolejny model
#model_lepszy2<- gam(ilosc~s(temperatura_odczuwalna, by=czy_weekend, k=14)+
#                        s(predkosc_wiatru, k=13)+s(godzina, k=22)+czy_weekend,
#                    data=df_nowy)
#summary(model_lepszy2)
#gam.check(model_lepszy2)
#plot(model_lepszy2,
#     pages=1,
#    rug=T,
#   residuals=T,
#  scheme=1,
# all.terms=T)

model2<- gam(ilosc~s(temperatura_odczuwalna, by=pora_roku, k=15)+s(godzina, k=22)
             +pora_roku,
             data=df_nowy)
summary(model2)
gam.check(model2)
#op <- par(oma=c(5,7,1,1))
plot(model2,
     pages=1,
     rug=T,
     residuals=F,
     scheme=1,
     all.terms=T)
#par(op)
#trzeba tutaj porobić inne interakcje, troche sie tym pobawic
#kolejny model
#model3<- gam(ilosc~te(temperatura_odczuwalna, godzina)+czy_weekend,
#            data=df_nowy)
#summary(model3)
#plot(model3,
#     pages=1,
#     rug=T,
#     residuals=T,
#     scheme=1,
#     all.terms=T)
#manipulate({plot(model3, select=1, scheme=1,
#                 theta=poziom, phi=pion)},
#           poziom=slider(min=-180, max=180, initial=0, step=10),
#           pion=slider(min=-180, max=180, initial=0, step=10))

#manipulate({vis.gam(model3, view=c("temperature_feels", "wind_speed"),se=0, 
#                   ticktype="detail", theta=poziom, phi=pion)},
#         poziom=slider(min=-180, max=180, initial=0, step=10),
#         pion=slider(min=-180, max=180, initial=0, step=10))

#manipulate({vis.gam(model1_nowy, view=c("temperature_feels", "wind_speed"),se=0, 
#                    ticktype="detail", theta=poziom, phi=pion)},
#          poziom=slider(min=-180, max=180, initial=0, step=10),
#          pion=slider(min=-180, max=180, initial=0, step=10))

