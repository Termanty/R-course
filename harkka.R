############################################
# 
# Tilastollinen päättely R-ohjelmistolla
# Harjoitustyö
#
# Tero Mäntylä
# 014093702
#
############################################


##### tehtävä 1. #####

# a)
summamuuttuja <- function(sarakkeet, data, max_puuttuvat=1) {
    ka <- rowMeans(data[sarakkeet], na.rm = TRUE)
    ka[rowSums(is.na(data[sarakkeet])) > max_puuttuvat] <- NA 
    return(ka)
}

# b)
asty <- read.csv2("asiakastyytyvaisyys.csv", stringsAsFactors = FALSE)

asty$ty_kulj <- summamuuttuja(c('K1A1', 'K1A2', 'K1A3'), data=asty)
quantile(asty$ty_kulj, na.rm=TRUE)
median(asty$ty_kulj, na.rm=T)

png(filename = "boxplot.png", width = 640, height = 480)
boxplot(asty$ty_kulj, col='red')
dev.off()

# c)
sarakkeet <- c(paste('K1A', 4:6, sep=''), paste('K2A', 2:6, sep=''))
tyytyvaisyys <- summamuuttuja(sarakkeet, data=asty, max_puuttuvat=5)
summary(tyytyvaisyys)

png(filename = "matkustustyytyv.png", width = 640, height = 480)
hist(tyytyvaisyys, col='red', main="Matkustustyytyväisyys")
dev.off()

# d)
suosittelu <- factor(cut(asty$T16, breaks=c(0,1,2)), labels=c('kyllä','ei'))
median(tyytyvaisyys[suosittelu == 'kyllä'], na.rm=T)
median(tyytyvaisyys[suosittelu == 'ei'], na.rm=T)

png(filename = "suo_tyytyv.png", width = 640, height = 480)
boxplot(tyytyvaisyys[suosittelu == 'kyllä'], main="Suosittelevien tyytyväisyys")
dev.off()

png(filename = "eisuo_tyytyv.png", width = 640, height = 480)
boxplot(tyytyvaisyys[suosittelu == 'ei'], main="Ei suosittelevien tyytyväisyys")
dev.off()

##### tehtävä 2. #####

# a)
alue <- factor(cut(asty$ALUE, breaks=c(49,752,2010)), labels=c('Sisäinen','Seutu'))

# b)
tasmallisyys <- asty$K1A4
alue_X_tasma <- table(tasmallisyys, alue)
prop.table(alue_X_tasma,1)

# c)
mu_seutu <- mean(tasmallisyys[alue == 'Seutu'], na.rm=T)
t.test(tasmallisyys[alue == 'Sisäinen'], mu= mu_seutu)


##### tehtävä 3. #####

# a)
asty$kk <- format(as.Date(asty$PAIVAMAARA), '%m')

# b)
KeskiarvoJaVali <- function(data, m_taso=0.95) {
    tulos <- t.test(data, conf.level= m_taso)
    c(unname(tulos$estimate), tulos$conf.int[1], tulos$conf.int[2])
}

# c)
L55.ty <- tyytyvaisyys[asty$LINJA == 55]
KeskiarvoJaVali(L55.ty, 0.95)

# d)
asty$ty <- tyytyvaisyys
agg <- aggregate(x= asty$ty, by= list(asty$kk), FUN= KeskiarvoJaVali)

png(filename = "ka_vali.png", width = 640, height = 480)
plot(agg$x[,1]~agg$Group.1, type='l', col='red')
lines(agg$x[,2]~agg$Group.1, col='green')
lines(agg$x[,3]~agg$Group.1, col='green')
dev.off()


##### tehtävä 4. #####

# a)
png(filename = "ty_kulj4a.png", width = 640, height = 480)
plot(asty$ty~asty$ty_kulj, col='blue')
dev.off()

# b)
ty <- asty$ty + rnorm(asty$ty, mean= 0, sd= 0.2)
ty_kulj <- asty$ty_kulj + rnorm(asty$ty_kulj, mean= 0, sd= 0.2)

png(filename = "hajakuva.png", width = 640, height = 480)
plot(ty~ty_kulj, col= 'blue', pch= '.', cex= 0.2)

# c)
fit <- lm(asty$ty~asty$ty_kulj)
fit

# d)
curve(fit$coefficients[1] + fit$coefficients[2]*x, from= 1, to= 5.5, add= T, col= 'orange')
dev.off()

##### tehtävä 5. #####

# a)
thetasimu <- function(n) {
    N <- 4 # palloja korisx/Na
    nostoja <- 3 # nostojen määrä palautuksella
    thetas <- floor(runif(n, 0, N+1))
    nostetut_valkoiset <- sapply(thetas, function(x) rbinom(1, nostoja, x/N))
    theta <- thetas[nostetut_valkoiset == 2]
    prop.table(table(theta))
}
simu = thetasimu(100000)

# b)
thetasimu2 <- function(n) {
    N <- 4 # palloja korissa
    nostoja <- 3 # nostojen määrä palautuksella
    thetas <- rbinom(n, N, 0.5)
    nostetut_valkoiset <- sapply(thetas, function(x) rbinom(1, nostoja, x/N))
    theta <- thetas[nostetut_valkoiset == 2]
    prop.table(table(theta))
}
simu2 = thetasimu2(100000)

# c)
simu
max(simu)
simu2
max(simu2)




