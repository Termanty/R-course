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
asty$tyytyvaisyys <- summamuuttuja(sarakkeet, data=asty, max_puuttuvat=5)
summary(asty$tyytyvaisyys)
hist(asty$tyytyvaisyys, col='red', main="Matkustustyytyväisyys")


# d)


##### tehtävä 2. #####

##### tehtävä 3. #####

##### tehtävä 4. #####

##### tehtävä 5. #####

