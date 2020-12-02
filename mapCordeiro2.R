#Carregando pacotes necessários para baixar as malhas setoriais
#do IBGE para o munic�pio de Cordeirópolis (CENSO 2010)
library(sf)
library(geobr)

#baixando a malha setorial
geoCordeiro_Set <- read_census_tract(code_tract = 3512407,
                                     year = 2010)

#visualisando o mapa
plot(geoCordeiro_Set)

#Visualisando o data frame do mapa
View(geoCordeiro_Set)

write.csv(geoCordeiro_Set, "D:/Documents/arquivosAnalise/geoCordeiro_Set.csv")

#abrindo o documento Base para os setores de Cordeirópolis
#no meu caso, já estão gravados no meu computador
#para baixar:
#https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html
Setores_Cordeiro <- read.csv(
"D:/Documents/arquivosAnalise/setoresSPsemCapital/excel/Basico_SP2.csv",
 sep = ";")

#visualisando o arquivo
View(Setores_Cordeiro)

#unindo os dois arquivos anteriores
  #primeiro precisamos renomear a primeira coluna de
  #Setores_Cordeiro (pis esta é a variável utilizada para
  #unir os dois arquivos)
names(Setores_Cordeiro)[1] = "code_tract"
#agora podemos unir os dois arquivos
geoCordMap = merge(geoCordeiro_Set, Setores_Cordeiro,
                   by = "code_tract", all.x = T)
#e visualizar
View(geoCordMap)
write.csv(geoCordMap, "D:/Documents/arquivosAnalise/geoCordMap.csv")
#visualizando o mapa apenas utilizando a variável "code_tract"
  #para isto precisamos da função select, do pacot dplyr
library(dplyr)
plot(select(geoCordMap, code_tract))
  #ou, podemos utilizar o ggplot2
library(ggplot2)
  #e criar um data frame somente com esta variável
s <- select(geoCordMap, code_tract)
View(s)
ggplot() +
  geom_sf( data = s, color = "black", 
           aes(fill = code_tract))

#separando somente os setores urbanos
geoCordMapUrbano <- geoCordMap[
  geoCordMap$zone == "URBANO", ]
View(geoCordMapUrbano)
ggplot() +
  geom_sf( data = geoCordMapUrbano, color = "black", 
           aes(fill = code_tract))

#usando bubbles
  #adicionando as coordenadas de latitude e longitude
coordenadas <- read.csv(
  "D:/Documents/arquivosAnalise/coordenadas2.csv",
  sep = ";")
View(coordenadas)
  #unindo coordenadas ao "geoCordMap"
names(coordenadas)[1] = "code_tract"
geoCordMapLatLong = merge(geoCordMap, coordenadas,
                   by = "code_tract", all.x = T)


View(geoCordMapLatLong)
write.csv(geoCordMapLatLong, "D:/Documents/arquivosAnalise/geoCordMapLatLong")
ggplot() +
  geom_sf( data = geoCordMapUrbano, color = "black", 
           aes(fill = code_tract))

ggplot() +
  geom_sf( data = geoCordMapLatLong, color = "black", 
           aes(fill = code_tract)) +
  geom_point(data = geoCordMapLatLong, aes(x = longitude,
                                           y = latitude))

ggplot() +
  geom_sf( data = geoCordMapLatLong, color = "black", 
           aes(fill = code_tract)) +
  geom_point(data = geoCordMapLatLong, aes(x = longitude,
                                           y = latitude,
                                           size = V001,
                                           alpha = code_tract))

#para termos um gr�fico interativo, usamos o plotly
library(plotly)

ggplot() +
  geom_sf( data = geoCordMapLatLong, color = "black", 
           aes(fill = code_tract)) +
  geom_point(data = geoCordMapLatLong, aes(x = longitude,
                                           y = latitude,
                                           size = V001,
                                           alpha = code_tract))
ggplotly()



##MANIPULANDO

#dplyr
count(geoCordMap, zone)

hist(geoCordMap$V001)

sample_n(geoCordMapUrbano, size = 7)

plot(select(geoCordMap, V006))
plot(select(geoCordMap, V005:V010))

Var06_29 <- geoCordMap[29, ]
plot(select(Var06_29, V006))

Var06_28 <- geoCordMap[28, ]
plot(select(Var06_28, V006))

Var06_30 <- geoCordMap[30, ]
plot(select(Var06_30, V006))

filter(geoCordMap, V001 > 200)
plot(filter(geoCordMap, V001 > 200))
plot(select(filter(geoCordMap, V001 > 200), V006))

filter(geoCordMap, zone %in% "RURAL")
plot(filter(geoCordMap, zone %in% "RURAL"))
plot(select(filter(geoCordMap, zone %in% "RURAL"), V006))

c <- geoCordMap %>%
  select(code_tract, V001, V005) %>%
  arrange(V005)
View(c)  

# "desc" é usada para colocar em ordem decrescente
geoCordMap %>%
  select(code_tract, V001, V005) %>%
  arrange(desc(V005))

#mutate é usada para criar um novo índice
geoCordMap %>%
  mutate(novoindice = V002/V001)


geoCordMap %>%
  summarise(mediaMoradorSetor = mean(V003))

geoCordMap %>%
  summarise(avg_Morador = mean(V003),
            min_Morador = min(V003),
            max_Morador = max(V003))
