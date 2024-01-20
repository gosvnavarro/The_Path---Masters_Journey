## Steps to reach the environmental score
## Author: Gabrielle Navarro
## Updated in: 18/10/2021


#####################################################
## OBTENÇÃO E CATEGORAZIÇÃO DA ÉPOCA DE NASCIMENTO ##
#####################################################
# Carregar a tabela e os pacotes para o ambiente
library(readr)
season_of_birth <- read_delim("~/Documents/Gabe/project/database_ambiental/season_of_birth.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Checar o tipo de cada coluna
sapply(season_of_birth, mode)

# Tirar as informações sobre os dias e anos, ficando apenas com os meses
as.data.frame(season_of_birth$childbirthdate <- sub('.[[/]]*', ' ', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('[0-9]*.', ' ', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('[[/]]*.', '_', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('_[0-9]*.', ' ', season_of_birth$childbirthdate))

# Tornar todas as letras maisculas
as.data.frame(season_of_birth$childbirthdate <- toupper(season_of_birth$childbirthdate))

# Criar os grupos: Janeiro a Maio - 0, Junho a dezembro - 0.068
season_of_birth$logs_ODs <- season_of_birth$childbirthdate

as.data.frame(season_of_birth$logs_ODs <- sub('JAN', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('FEB', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('MAR', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('APR', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('MAY', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('JUN', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('JUL', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('AUG', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('SEP', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('OCT', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('NOV', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('DEC', 0.068, season_of_birth$logs_ODs))

# Converter as colunas para 'numeric' e faz a checagem o tipo de cada coluna
season_of_birth$logs_ODs <- as.numeric(as.character(season_of_birth$logs_ODs))
sapply(season_of_birth, mode)

# Salvar arquivo
write.table(season_of_birth, file = '~/Documents/Gabe/project/database_ambiental/season_of_birth.csv', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
write.table(season_of_birth, file = '~/Documents/Gabe/project/database_ambiental/season_of_birth.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

################################################
## OBTENCAO E CATEGORAZICAO DA IDADE PATERNA ##
###############################################

##############################
## NO MOMENTO DO NASCIMENTO ##
# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
idade_paterna_v2 <- read_delim("idade_paterna_v2.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Passo 2 - Checar o tipo de cada coluna (OPCIONAL)
sapply(idade_paterna_v2, mode)

# Passo 3 - Tirar as informações sobre os dias e meses
## pais
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[A-Z]', ' ', idade_paterna_v2$fbirthdate))
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[a-z]', ' ', idade_paterna_v2$fbirthdate))
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[/]', ' ', idade_paterna_v2$fbirthdate))

## filhos
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[A-Z]', ' ', idade_paterna_v2$childbirthdate))
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[a-z]', ' ', idade_paterna_v2$childbirthdate))
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[*]', ' ', idade_paterna_v2$childbirthdate))

# Passo 4 - Converter as colunas para 'numeric e fazer a checagem o tipo de cada coluna
idade_paterna_v2$fbirthdate <- as.numeric(as.character(idade_paterna_v2$fbirthdate))
idade_paterna_v2$childbirthdate <- as.numeric(as.character(idade_paterna_v2$childbirthdate))

sapply(idade_paterna_v2, mode)

# Passo 5 - Cálculo da idade paterna no nascimento
idade_paterna_v2$at_birth <- (idade_paterna_v2$childbirthdate) - (idade_paterna_v2$fbirthdate)

# Passo 6 - Adicionar os odds ratio correspondentes 
idade_paterna_v2$at_birth_odds <- idade_paterna_v2$at_birth

idade_paterna_v2$at_birth_odds[idade_paterna_v2$at_birth_odds < 55] <- 0.25
idade_paterna_v2$at_birth_odds[idade_paterna_v2$at_birth_odds > 55 & idade_paterna_v2$at_birth_odds == 55] <- 0.80

# Passo 7 - Salvar arquivo
write.table(idade_paterna_v2, file = 'idade_paterna_resultados.csv', sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)
write.table(idade_paterna_v2, file = 'idade_paterna_resultados.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

#############################
## NO MOMENTO DA CONCEPCAO ##
# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
library(lubridate)
paternal_age_at_conception <- read_delim("paternal_age_at_conception.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Passo 2 - Trocar os nomes dos meses pelos numeros correspondentes, caso se faca necessario
#           Isso e importante pois o calculo funciona melhor dessa forma
#           Fazer isso tanto para a data de nascimento dos pais quanto para a data dos filhos
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JAN'] <- 01
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'FEB'] <- 02
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'MAR'] <- 03
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'APR'] <- 04
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'MAY'] <- 05
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JUN'] <- 06
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JUL'] <- 07
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'AUG'] <- 08
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'SEP'] <- 09
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'OCT'] <- 10
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'NOV'] <- 11
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'DEC'] <- 12

# Passo 3 - Passar os anos com dois digitos para quatro digitos
library(magrittr)
# Pais
paternal_age_at_conception_OF$fbirthdate <- 
  as.Date(paternal_age_at_conception_OF$fbirthdate, format = "%d/%m/%y") %>% format("19%y%m%d") %>% as.Date("%Y%m%d")

# Filhos
paternal_age_at_conception_OF$childbirthdate <- 
  as.Date(paternal_age_at_conception_OF$childbirthdate, format = "%d/%m/%y") %>% format("20%y%m%d") %>% as.Date("%Y%m%d")

as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2099', '1999', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2098', '1998', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2097', '1997', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2096', '1996', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2095', '1995', paternal_age_at_conception_OF$childbirthdate))

# Passo 4 - Subtrair nove meses antes do nascimento das criancas, para saber o momento da concepcao
library(mondate)
paternal_age_at_conception_OF$concepcao <- mondate(paternal_age_at_conception_OF$childbirthdate) - 9
paternal_age_at_conception_OF$concepcao <- as.Date(paternal_age_at_conception_OF$concepcao)

# Passo 5 - Calcular a idade paterna no momento da concepcao
paternal_age_at_conception_OF$concepcao_idade <- year(strptime(paternal_age_at_conception_OF$concepcao, format = "%Y-%m-%d")) -
  year(strptime(paternal_age_at_conception_OF$fbirthdate, format = "%Y-%m-%d"))

# Passo 6 - Adicionar os odds ratio correspondentes
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade < 55] <- 0.25
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade > 55] <- 0.80
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade == 55] <- 0.80

# Passo 7 - Salvar o arquivo
write.table(paternal_age_at_conception_OF, file = 'idade_paterna_concepcao_resultados.csv', sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)



###################
# CALCULO DO PERS #
###################

# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
library(dplyr)
library(formattable)
ARQUIVO_CALCULO <- read.delim2(file = "arquivo_respostas.csv", header=FALSE)

# Passo 2 - Caso precise renomear as colunas seguir o passo a seguir, 
#           caso não seja necessário seguir para o passo 3
names(ARQUIVO_CALCULO) <- c('ID', 'varamb_1', 'varamb_2', 'varamb_3', 'varamb_4', 'varamb_5', 
                            'varamb_6', 'varamb_7')

# Passo 3 - Substituir os valores 'NA' por 0
ARQUIVO_CALCULO$varamb_1[which(is.na(ARQUIVO_CALCULO$varamb_1))] <- 0
ARQUIVO_CALCULO$varamb_2[which(is.na(ARQUIVO_CALCULO$varamb_2))] <- 0
ARQUIVO_CALCULO$varamb_3[which(is.na(ARQUIVO_CALCULO$varamb_3))] <- 0
ARQUIVO_CALCULO$varamb_4[which(is.na(ARQUIVO_CALCULO$varamb_4))] <- 0
ARQUIVO_CALCULO$varamb_5[which(is.na(ARQUIVO_CALCULO$varamb_5))] <- 0
ARQUIVO_CALCULO$varamb_6[which(is.na(ARQUIVO_CALCULO$varamb_6))] <- 0
ARQUIVO_CALCULO$varamb_7[which(is.na(ARQUIVO_CALCULO$varamb_7))] <- 0
ARQUIVO_CALCULO$varamb_8[which(is.na(ARQUIVO_CALCULO$varamb_8))] <- 0
ARQUIVO_CALCULO$varamb_9[which(is.na(ARQUIVO_CALCULO$varamb_9))] <- 0

# Passo 4 - A fim de evitar erros no passo referente aos odds ratio,
#           os comandos descritos nesse passo são realizados para que 
#           a substituição das respostas positivas pelos odds ratios respectivos desse certo.
ARQUIVO_CALCULO$varamb_1[ARQUIVO_CALCULO$varamb_1 == 1] <- 2
ARQUIVO_CALCULO$varamb_2[ARQUIVO_CALCULO$varamb_2 == 1] <- 2
ARQUIVO_CALCULO$varamb_3[ARQUIVO_CALCULO$varamb_3 == 1] <- 2
ARQUIVO_CALCULO$varamb_4[ARQUIVO_CALCULO$varamb_4 == 1] <- 2
ARQUIVO_CALCULO$varamb_5[ARQUIVO_CALCULO$varamb_5 == 1] <- 2
ARQUIVO_CALCULO$varamb_6[ARQUIVO_CALCULO$varamb_6 == 1] <- 2
ARQUIVO_CALCULO$varamb_7[ARQUIVO_CALCULO$varamb_7 == 1] <- 2
ARQUIVO_CALCULO$varamb_8[ARQUIVO_CALCULO$varamb_8 == 1] <- 2
ARQUIVO_CALCULO$varamb_9[ARQUIVO_CALCULO$varamb_9 == 1] <- 2

# Passo 5 - Checar o tipo de cada coluna
sapply(ARQUIVO_CALCULO, mode)

# Passo 6 - Estipular o OR de cada variavel ambiental para cada individuo
# Varamb_1 - Season of birth 
#            (fazer esse 'for' caso o 'season of birth' tenha sido obtido em conjunto com as outras variaveis)
for(i in ARQUIVO_CALCULO$varamb_1) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_1[ARQUIVO_CALCULO$varamb_1 == 2] <- 0.068
    print('Valor substituido por 0.068')
  }
}

# Varamb_2 - Urbanicidade
for(i in ARQUIVO_CALCULO$varamb_2) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_2[ARQUIVO_CALCULO$varamb_2 == 2] <- 0.54
    print('Valor substituido por 0.54')
  }
}

# Varamb_3 - Uso de drogas
for(i in ARQUIVO_CALCULO$varamb_3) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_3[ARQUIVO_CALCULO$varamb_3 == 2] <- 0.56
    print('Valor substituido por 0.56')
  }
}

# Varamb_4 - Idade paterna ao nascimento
#            (fazer esse 'for' caso o 'paternal age' tenha sido obtido em conjunto com as outras variaveis)
for(i in ARQUIVO_CALCULO$varamb_4) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_4[ARQUIVO_CALCULO$varamb_4 == 2] <- 0.25/0.80
    print('Valor substituido por 0.25/0.80')
  }
}

# Varamb_5 - Obstetric & Pre-natal complications
for(i in ARQUIVO_CALCULO$varamb_5) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_5[ARQUIVO_CALCULO$varamb_5 == 2] <- 0.69
    print('Valor substituido por 0.69')
  }
}

# Varamb_6 -  Abuso físico
for(i in ARQUIVO_CALCULO$varamb_6) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_6[ARQUIVO_CALCULO$varamb_6 == 2] <- 1.08
    print('Valor substituido por 1.08')
  }
}

# Varamb_7 - Abuso sexual
for(i in ARQUIVO_CALCULO$varamb_7) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_7[ARQUIVO_CALCULO$varamb_7 == 2] <- 0.87
    print('Valor substituido por 0.87')
  }
}

# Varamb_8 - Negligencia
for(i in ARQUIVO_CALCULO$varamb_8) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_8[ARQUIVO_CALCULO$varamb_8 == 2] <- 1.06
    print('Valor substituido por 1.06')
  }
}

# Varamb_9 - Perda/Separação dos pais
for(i in ARQUIVO_CALCULO$varamb_9) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_9[ARQUIVO_CALCULO$varamb_9 == 2] <- 0.53
    print('Valor substituido por 0.53')
  }
}

# Passo 7 - Checar o tipo de cada coluna
sapply(ARQUIVO_CALCULO, mode)

# Passo 8 - Criar uma coluna com a soma de todos os OR, resultando assim no pré-PERS
ARQUIVO_CALCULO$SOMA <- rowSums(cbind(ARQUIVO_CALCULO$varamb_1, ARQUIVO_CALCULO$varamb_2, 
                                      ARQUIVO_CALCULO$varamb_3, ARQUIVO_CALCULO$varamb_4, 
                                      ARQUIVO_CALCULO$varamb_5, ARQUIVO_CALCULO$varamb_6, 
                                      ARQUIVO_CALCULO$varamb_7, ARQUIVO_CALCULO$varamb_8, 
                                      ARQUIVO_CALCULO$varamb_9))

# Passo 9 - Calcular o PERS
ARQUIVO_CALCULO$PERS <- (ARQUIVO_CALCULO$SOMA) / 9

# Passo 10 - Salvar a tabela
write.table(ARQUIVO_CALCULO, file = 'arquivo_pers.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
