#https://www.rdatamining.com/examples
## Limpando
rm(list=ls())

library(magrittr) ## for pipe operations
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(arules)

## DIMENSÃO
gamesujo <- read.csv2("C:/Users/User/Downloads/game.csv")
dim(gamesujo)

str(gamesujo)

Vencedor = c()

# loopando para adicionar dados de vencedor ou empate
for (linha in 1:nrow(gamesujo)) {
  if (gamesujo$playerpoints[linha] > gamesujo$computerpoints[linha]){
    Vencedor[linha] = "Jogador"
  } else if (gamesujo$playerpoints[linha] < gamesujo$computerpoints[linha]){
    Vencedor[linha] = "Computador"
  } else {
    Vencedor[linha] = "Empate"
  }
}
Vencedor

# adicionando o Vetor de vencedores como coluna no dataset
gamesujo <- cbind(gamesujo, Vencedor = Vencedor) 
head(gamesujo)

gamesujo <- na.omit(gamesujo)
gamesujo <- distinct(gamesujo)

##Limpando dados
#Retirando colunas que não vamos utilizar
gamesujo = gamesujo[,-c(1,2,3,4,5,6,7,8,9,10,12,15)]
head(gamesujo)

#Transformando valores em colunas para ter valores booleanos
#separrando combos do jogador
gamesujo <- gamesujo %>%
  mutate(`P_Fam-Fuji` = str_detect(playercombos, "Fam-Fuji"),
         `P_Fam-Susuki` = str_detect(playercombos, "Fam-Susuki"),
         `P_Hanami_I-Pai` = str_detect(playercombos, "Hanami I-Pai"),
         `P_Ino-Shika-Cho` = str_detect(playercombos, "Ino-Shika-Cho"),
         `P_Acata` = str_detect(playercombos, "Acata"),
         `P_Tsukimi_I-Pai` = str_detect(playercombos, "Tsukimi I-Pai"),
         `P_Nizoro` = str_detect(playercombos, "Nizoro"),
         `P_Fam-Kiri` = str_detect(playercombos, "Fam-Kiri"),
         `P_Fam-Uma` = str_detect(playercombos, "Fam-Uma"),
         `P_Fam-Momiji` = str_detect(playercombos, "Fam-Momiji"),
         `P_Cosan` = str_detect(playercombos, "Cosan"),
         `P_Oozan` = str_detect(playercombos, "Oozan"),
         `P_Aotan` = str_detect(playercombos, "Aotan"),
         `P_Nanata` = str_detect(playercombos, "Nanata"),
         `P_Tepo` = str_detect(playercombos, "Tepo"),
         `P_Matsu-Kiri-Bozu` = str_detect(playercombos, "Matsu-Kiri-Bozu"),
         `P_Fam-Sakura` = str_detect(playercombos, "Fam-Sakura"),
         `P_Fam-Matsu` = str_detect(playercombos, "Fam-Matsu"),
         `P_Shiko` = str_detect(playercombos, "Shiko"))

summary(gamesujo)

#separando combos do computador
gamesujo <- gamesujo %>%
    mutate(`C_Fam-Fuji` = str_detect(computercombos, "Fam-Fuji"),
           `C_Fam-Susuki` = str_detect(computercombos, "Fam-Susuki"),
           `C_Hanami_I-Pai` = str_detect(computercombos, "Hanami I-Pai"),
           `C_Ino-Shika-Cho` = str_detect(computercombos, "Ino-Shika-Cho"),
           `C_Acata` = str_detect(computercombos, "Acata"),
           `C_Tsukimi_I-Pai` = str_detect(computercombos, "Tsukimi I-Pai"),
           `C_Nizoro` = str_detect(computercombos, "Nizoro"),
           `C_Fam-Kiri` = str_detect(computercombos, "Fam-Kiri"),
           `C_Fam-Uma` = str_detect(computercombos, "Fam-Uma"),
           `C_Fam-Momiji` = str_detect(computercombos, "Fam-Momiji"),
           `C_Cosan` = str_detect(computercombos, "Cosan"),
           `C_Oozan` = str_detect(computercombos, "Oozan"),
           `C_Aotan` = str_detect(computercombos, "Aotan"),
           `C_Nanata` = str_detect(computercombos, "Nanata"),
           `C_Tepo` = str_detect(computercombos, "Tepo"),
           `C_Matsu-Kiri-Bozu` = str_detect(computercombos, "Matsu-Kiri-Bozu"),
           `C_Fam-Sakura` = str_detect(computercombos, "Fam-Sakura"),
           `C_Fam-Matsu` = str_detect(computercombos, "Fam-Matsu"),
           `C_Shiko` = str_detect(computercombos, "Shiko"))
  
summary(gamesujo)

#Retirando colunas que não vamos utilizar
gamesujo = gamesujo[,-c(1,2,4)]
head(gamesujo)
for(i in 1:41){
  gamesujo[,i] <- as.factor(gamesujo[,i])
}
str(gamesujo)


#Separando base de dados para cada inteligencia
RandomPlayer = gamesujo[gamesujo$computeralgorithm == "RandomPlayer",]
head(RandomPlayer)

Greedy = gamesujo[gamesujo$computeralgorithm == "Greedy",]
head(Greedy)

ExpectiMinimaxPlayer = gamesujo[gamesujo$computeralgorithm == "ExpectiMinimaxPlayer",]
head(ExpectiMinimaxPlayer)

NanatanGreedyPlayer = gamesujo[gamesujo$computeralgorithm == "NanatanGreedyPlayer",]
head(NanatanGreedyPlayer)

ShikoGreedyPlayer = gamesujo[gamesujo$computeralgorithm == "ShikoGreedyPlayer",]
head(ShikoGreedyPlayer)

ShikoNanatanGreedyPlayer = gamesujo[gamesujo$computeralgorithm == "ShikoNanatanGreedyPlayer",]
head(ShikoNanatanGreedyPlayer)

###################         ####################
################ RANDOMPLAYER ##################
###################         ####################

#descobrindo regras
rules_RP<- RandomPlayer %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=5, supp=0.13, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))

## Mantendo 3 casas decimais
quality(rules_RP) <- rules_RP %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_RP.sorted <- rules_RP %>% sort(by="lift")
rules_RP.sorted %>% inspect() ## printa as regras
rules_RP.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_RP.matrix <- is.subset(rules_RP, rules_RP)

subset_RP.matrix

subset_RP.matrix[lower.tri(subset_RP.matrix, diag = T)] <- F
redundant_RP <- colSums(subset_RP.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_RP %>% which()

## removendo redundantes 
rules_RP.pruned <- rules_RP.sorted[!redundant_RP]

rules_RP.pruned %>% inspect() ## printa regras

rules_RP.pruned[1:6] %>% inspect() ## printa 6 primeiras regras


###################         ####################
################### Greedy  ####################
###################         ####################

#descobrindo regras
rules_GR<- Greedy %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=6, supp=0.11, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))


##  Mantendo 3 casas decimais
quality(rules_GR) <- rules_GR %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_GR.sorted <- rules_GR %>% sort(by="lift")
rules_GR.sorted %>% inspect() ## printa as reegras
rules_GR.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_GR.matrix <- is.subset(rules_GR, rules_GR)

subset_GR.matrix

subset_GR.matrix[lower.tri(subset_GR.matrix, diag = T)] <- F
redundant_GR <- colSums(subset_GR.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_GR %>% which()

## removendo redundantes 
rules_GR.pruned <- rules_GR.sorted[!redundant_GR]

rules_GR.pruned %>% inspect() ## printa regras
rules_GR.pruned[1:6] %>% inspect() ## printa 6 primeiras regras


###################         ####################
############   ExpectiMinimax  #################
###################         ####################

## descobrindo regras
rules_EMM<- ExpectiMinimaxPlayer %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=6, supp=0.1, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))


## Mantendo 3 casas decimais
quality(rules_EMM) <- rules_EMM %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_EMM.sorted <- rules_EMM %>% sort(by="lift")
rules_EMM.sorted %>% inspect() ## printa as reegras
rules_EMM.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_EMM.matrix <- is.subset(rules_EMM, rules_EMM)

subset_EMM.matrix

subset_EMM.matrix[lower.tri(subset_EMM.matrix, diag = T)] <- F
redundant_EMM <- colSums(subset_EMM.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_EMM %>% which()

##  removendo redundantes 
rules_EMM.pruned <- rules_EMM.sorted[!redundant_EMM]

rules_EMM.pruned %>% inspect() ## printa regras
rules_EMM.pruned[1:6] %>% inspect() ## printa 6 primeiras regras



###################         ####################
##########  NanatanGreedyPlayer  ###############
###################         ####################

## Descobrindo regras
rules_NGP<- NanatanGreedyPlayer %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=6, supp=0.09, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))


## Mantendo 3 casas decimais
quality(rules_NGP) <- rules_NGP %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_NGP.sorted <- rules_NGP %>% sort(by="lift")
rules_NGP.sorted %>% inspect() ## printa as reegras
rules_NGP.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_NGP.matrix <- is.subset(rules_NGP, rules_NGP)

subset_NGP.matrix

subset_NGP.matrix[lower.tri(subset_NGP.matrix, diag = T)] <- F
redundant_NGP <- colSums(subset_NGP.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_NGP %>% which()

## removendo redundantes 
rules_NGP.pruned <- rules_NGP.sorted[!redundant_NGP]

rules_NGP.pruned %>% inspect() ## printa regras
rules_NGP.pruned[1:6] %>% inspect() ## printa 6 primeiras regras


###################         ####################
########### ShikoGreedyPlayer  #################
###################         ####################

## descobrindo regras
rules_SGP<- ShikoGreedyPlayer %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=6, supp=0.1, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))


## Mantendo 3 casas decimais
quality(rules_SGP) <- rules_SGP %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_SGP.sorted <- rules_SGP %>% sort(by="lift")
rules_SGP.sorted %>% inspect() ## printa as reegras
rules_SGP.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_SGP.matrix <- is.subset(rules_SGP, rules_SGP)

subset_SGP.matrix

subset_SGP.matrix[lower.tri(subset_SGP.matrix, diag = T)] <- F
redundant_SGP <- colSums(subset_SGP.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_SGP %>% which()

## removendo redundantes
rules_SGP.pruned <- rules_SGP.sorted[!redundant_SGP]

rules_SGP.pruned %>% inspect() ## printa regras
rules_SGP.pruned[1:6] %>% inspect() ## printa 6 primeiras regras


###################         ####################
######### ShikoNanatanGreedyPlayer  ############
###################         ####################

## Descobrindo regras
rules_SNGP<- ShikoNanatanGreedyPlayer %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=6, supp=0.11, conf=0.8),
  appearance = list(rhs=c("Vencedor=Jogador"), default="lhs"))


## Mantendo 3 casas decimais
quality(rules_SNGP) <- rules_SNGP %>% quality() %>% round(digits=3)

## Ordenando regras pelo Lift
rules_SNGP.sorted <- rules_SNGP %>% sort(by="lift")
rules_SNGP.sorted %>% inspect() ## printa regras
rules_SNGP.sorted %>% length() ## Qtd de regras descobertas

## Encontrando regras redundantes 
subset_SNGP.matrix <- is.subset(rules_SNGP, rules_SNGP)

subset_SNGP.matrix

subset_SNGP.matrix[lower.tri(subset_SNGP.matrix, diag = T)] <- F
redundant_SNGP <- colSums(subset_SNGP.matrix) >= 1

## Descobrindo quais são as redundantes
redundant_SNGP %>% which()

## removendo redundantes
rules_SNGP.pruned <- rules_SNGP.sorted[!redundant_SNGP]

rules_SNGP.pruned %>% inspect() ## printa regras
rules_SNGP.pruned[1:6] %>% inspect() ## printa 6 primeiras regras


#Exportando as regras para o Excel

excel <- c(rules_EMM.pruned[1:5], rules_GR.pruned[1:5], rules_NGP.pruned[1:5], rules_RP.pruned[1:5], rules_SGP.pruned[1:5], rules_SNGP.pruned[1:5])
caminho_arquivo <- file.path("C:/Users/User/Downloads", "regras.csv")
write(excel, caminho_arquivo, sep=";") #export excel


