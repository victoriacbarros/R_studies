# **********************************************************
# ANALISE DE CLUSTER - MUNICIPIOS
# **********************************************************

# Alterar diretorio de trabalho
setwd("C://Users//Victoria//Documents//Pos//Analytics_II")

# Pacote para permitir leitura de dados em arquivo Excel
# install.packages("readxl")
library(readxl)

# Pacotes adicionais para visualizar resultados do k-medias
#install.packages("factoextra")
#install.packages("gridExtra")
library(factoextra) 
library(gridExtra)

# Leitura da base de dados
municipios <- read_excel("Municipios.xlsx", sheet = 'Base de Dados')

# **********************************************************
# ANALISE EXPLORATORIA

# (a) Faca uma breve analise exploratoria da base de dados,
#     a partir das principais medidas de posicao.

summary(municipios[,-1])

# (b) Considerando o histograma das variaveis hab e pib,
#     as distribuicoes sao simetricas?

par(mfrow = c(1,2))
hist(municipios$hab, main = "Densidade demografica")  
hist(municipios$pib, main = "PIB per capita") 

# (c) Considerando as variaveis pop15 e pop60,
#     qual possui a maior variabilidade?

sd(municipios$pop15) / mean(municipios$pop15) * 100
sd(municipios$pop60) / mean(municipios$pop60) * 100

# (d) Existe outlier nas variaveis nas variaveis pop15 e pop60?

par(mfrow = c(1,2))
boxplot(municipios$pop15, col = "darkturquoise", main = "% de habitantes ate 15 anos")
boxplot(municipios$pop60, col = "darkturquoise", main = "$ de habitantes acima de 60 anos")

# **********************************************************
# PADRONIZACAO

# (e) Padronize as variaveis.

municipios_z <- scale(municipios[,-1])

# **********************************************************
# METODO K-MEDIAS

set.seed(12345) # Semente aleatoria fixada, para reprodutibilidade

# (a) Construa segmentacoes por meio do método k-medias,
#     considerando k = 2, 3, 4 ou 5 grupos.
#     Qual quantidade de grupos parece mais adequada?

modelo_k2 <- kmeans(municipios_z, centers = 2, nstart = 25, iter.max = 100)
modelo_k3 <- kmeans(municipios_z, centers = 3, nstart = 25, iter.max = 100)
modelo_k4 <- kmeans(municipios_z, centers = 4, nstart = 25, iter.max = 100)
modelo_k5 <- kmeans(municipios_z, centers = 5, nstart = 25, iter.max = 100)

# Quantidade de observacoes em cada cluster

table(modelo_k2$cluster)
table(modelo_k3$cluster)
table(modelo_k4$cluster)
table(modelo_k5$cluster)

# Visualizacao aproximada dos clusters, reduzindo para 2 dimensoes

G1 <- fviz_cluster(modelo_k2, data = municipios_z, geom = "point", main = "k = 2")
G2 <- fviz_cluster(modelo_k3, data = municipios_z, geom = "point", main = "k = 3")
G3 <- fviz_cluster(modelo_k4, data = municipios_z, geom = "point", main = "k = 4")
G4 <- fviz_cluster(modelo_k5, data = municipios_z, geom = "point", main = "k = 5")
grid.arrange(G1, G2, G3, G4, nrow = 2)
