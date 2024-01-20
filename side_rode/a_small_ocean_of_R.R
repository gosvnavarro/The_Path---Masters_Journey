## Interesting commands in R
## Author: Gabrielle Navarro
## Updated in: 18/10/2021

#####################
# SUBSTITUIR TERMOS #
#####################
as.data.frame(teste_2 <- sub('.[_XX]$', '', tabela_1$coluna_1))
as.data.frame(teste_2 <- sub('.[_]$', '', tabela_1$coluna_1))

teste_2 <- NULL
as.data.frame(teste_2)
write(teste_2, file = 'tabela_final.csv')

#############################################################
# COPIAR UMA COLUNA DE UMA DATA.FRAME PARA OUTRA DATA.FRAME #
#############################################################
data1$one <- data$x

#################################################
# JUNTAR DUAS DATA.FRAME POR UM COLUNA EM COMUM #
#################################################
library(plyr)
tabelas_juntas <- join_all(list(tabela_1, tabela_2), by = "ID")

#########################################################################################
# ADICIONAR UMA NOVA COLUNA EM UM DATA FRAME COM UM VALOR ESPECIFICO E DEPOIS REORDENAR #
#########################################################################################
main$newcol <- rep(1,nrow(main))
main[,c(1,5,2,3,4)]

##################
# DELETAR LINHAS #
##################
tabela_x <- tabela_x[-c(XXX:XXX), ]

################################
# SEPARAR UMA COLUNA EM VARIAS #
################################
library(tidyr)
tabela_z <- separate(tabela_z, col = variant, into = c('C1', 'C2', 'C3', 'C4'), sep = '\\:')

#################################################
# CRIAR UMA NOVA COLUNA A PARTIR DE DUAS OUTRAS #
#################################################
X20544_3_gwas_imputed_v3_both_sexes_v2$SNP <- paste(X20544_3_gwas_imputed_v3_both_sexes_v2$CHR, 
                                                    X20544_3_gwas_imputed_v3_both_sexes_v2$BP,  sep = ':')

####################
# RENOMEAR COLUNAS #
####################
colnames(X20544_3_gwas_imputed_v3_both_sexes_v2)[12] = "BETA"

##########################################
# REORDENAR AS COLUNAS DE UMA DATA.FRAME #
##########################################
col_order <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8')
tabela_a <- tabela_a[, col_order]

##############################
# IDENTIFICACAO DOS OUTLIERS #
##############################
outVals <- boxplot(tabela_b$coluna_1, plot = F)$out #comando para "achar" os outliers
which(tabela_b$coluna_1 %in% outVals_W0) #fazer esse comando caso queira saber o rowIndex
outInd <- tabela_b[match(outVals, tabela_b$coluna_1),]
write.table(outInd, file = 'individuos_outiers', sep = '\t', row.names = F) #salvar a tabela com apenas esses dados

####################################
# ABRIR E MANIPULAR ARQUIVO '.rds' #
####################################
dados <- readRDS('caminho_ate_o_arquivo', refhook = NULL)
write.csv2(dados, file = 'individuos_outiers.csv', sep = '\t', row.names = F) # Salvar esse arquivo em outro formato para facilitar a manipulação do db
