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
    ka = rowMeans(data[sarakkeet], na.rm = TRUE)
    ka[rowSums(is.na(data[sarakkeet])) > max_puuttuvat] <- NA 
    return(ka)
}

# b)
asty <- read.csv2("asiakastyytyvaisyys.csv", stringsAsFactors = FALSE)

asty$tyyt_kulj <- summamuuttuja(c('K1A1', 'K1A2', 'K1A3'), data=asty)
quantile(asty$tyyt_kulj, na.rm=TRUE)
mean(asty$tyyt_kulj)
boxplot(asty$tyyt_kulj, col='red')


# c)
sarakkeet = c(paste('K1A', 4:6, sep=''), paste('K2A', 2:6, sep=''))
tyytyvaisyys <- summamuuttuja(sarakkeet, data=asty, max_puuttuvat=5)
summary(tyytyvaisyys)
hist(tyytyvaisyys, col='red', main="Matkustustyytyväisyys")


# d)
suosittelu = factor(cut(asty$T16, breaks=c(0,1,2)), labels=c('kyllä','ei'))
median(tyytyvaisyys[suosittelu == 'kyllä'], na.rm=T)
median(tyytyvaisyys[suosittelu == 'ei'], na.rm=T)
boxplot(tyytyvaisyys[suosittelu == 'kyllä'], main="Suosittelevien tyytyväisyys")
boxplot(tyytyvaisyys[suosittelu == 'ei'], main="Ei suosittelevien tyytyväisyys")


##### tehtävä 2. #####

# a)
alue = factor(cut(asty$ALUE, breaks=c(49,752,2010)), labels=c('Sisäinen','Seutu'))

# b)
tasma = asty$K1A4
alue_X_tasma = table(tasma, alue)
prop.table(alue_X_tasma,1)

# c)
mu_seutu = mean(tasma[alue == 'Seutu'], na.rm=T)
t.test(tasma[alue == 'Sisäinen'], mu= mu_seutu)


##### tehtävä 3. #####

# a)

# b)

# c)

# d)


##### tehtävä 4. #####

##### tehtävä 5. #####

