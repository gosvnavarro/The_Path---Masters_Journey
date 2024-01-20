## Basic and applied statistics
## Author: Gabrielle Navarro
## Updated in: 18/10/2021

############
# PACKAGES #
############
library(corrplot)
library(stats)
library(RColorBrewer)
library(Hmisc)
library(fitdistrplus)
library(QuantPsyc)

#######################
# DESCRIPTION OF DATA #
#######################
describe(DADOS_W1_PSYC_ancEUR$CAPE_W1)
describe(DADOS_W1_PSYC_ancEUR$PERS_4)
describe(DADOS_W1_PSYC_ancEUR$PRS_PSYC)

####################
# DENSITY GRAPHICS #
####################
dens_CAPE_W1 <- density(DADOS_W1_PSYC_ancEUR$CAPE_W1)
plot(dens_CAPE_W1, main = "Kernel Density of CAPE_W1 Score")
polygon(dens_CAPE_W1, col = "grey", border = "black")

dens_PERS_4 <- density(DADOS_W1_PSYC_ancEUR$PERS_4)
plot(dens_PERS_4, main = "Kernel Density of PERS_4_4 Score")
polygon(dens_PERS_4, col = "grey", border = "black")

dens_PRS_PSYC <- density(DADOS_W1_PSYC_ancEUR$PRS_PSYC)
plot(dens_PRS_PSYC, main = "Kernel Density of PRS_PSYC Score")
polygon(dens_PRS_PSYC, col = "grey", border = "black")

#########################
# DISTRIBUTION GRAPHICS #
#########################
descdist(DADOS_W1_PSYC_ancEUR$CAPE_W1, discrete = FALSE)
descdist(DADOS_W1_PSYC_ancEUR$PERS_4, discrete = FALSE)
descdist(DADOS_W1_PSYC_ancEUR$PRS_PSYC, discrete = FALSE)

##################
# DATA NORMALITY #
##################
shapiro.test(DADOS_W1_PSYC_ancEUR$CAPE_W1)
shapiro.test(DADOS_W1_PSYC_ancEUR$PERS_4)
shapiro.test(DADOS_W1_PSYC_ancEUR$PRS_PSYC)

################
# CORRELATIONS #
################
# 1. CAPE_W1 x PERS_4
correlacao_1 <- cor.test(DADOS_W1_PSYC_ancEUR$CAPE_W1, DADOS_W1_PSYC_ancEUR$PERS_4, method = "spearman")
correlacao_1 # visualizar o resultado
correlacao_1_p <- correlacao_1$p.value # pegar o valor de p da correlacao
correlacao_1_p
correlacao_1_e <- correlacao_1$estimate # pegar o coeficiente de correlacao
correlacao_1_e

# 2. CAPE_W1 x PRS_PSYC
correlacao_2 <- cor.test(DADOS_W1_PSYC_ancEUR$CAPE_W1, DADOS_W1_PSYC_ancEUR$PRS_PSYC, method = "spearman")
correlacao_2 # visualizar o resultado
correlacao_2_p <- correlacao_2$p.value # pegar o valor de p da correlacao
correlacao_2_p
correlacao_2_e <- correlacao_2$estimate # pegar o coeficiente de correlacao
correlacao_2_e

# 3. PERS_4 x PRS_PSYC + PCs
correlacao_3 <- cor.test(DADOS_W1_PSYC_ancEUR$PERS_4, DADOS_W1_PSYC_ancEUR$PRS_PSYC, method = "spearman")
correlacao_3 # visualizar o resultado
correlacao_3_p <- correlacao_3$p.value # pegar o valor de p da correlacao
correlacao_3_p
correlacao_3_e <- correlacao_3$estimate # pegar o coeficiente de correlacao
correlacao_3_e

# 4. MATRIZ
DADOS_W1_PSYC_ancEUR_SUBSET <- DADOS_W1_PSYC_ancEUR[,c("PRS_PSYC","PC1","PC2","PC3","PC4","PC5","PC6",
                          "PC8","PC9","PC10","PERS_4","CAPE_W1")]

correlacao_4 <- cor(DADOS_W1_PSYC_ancEUR_SUBSET, method = "spearman", use = "everything")
correlacao_4 # visualizar o resultado

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlacao_4, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

###############
# REGRESSIONS #
###############
# 1. CAPE_W1 ~ PRS_PSYC + PC_1 + ... + PC_10 + PERS_4
regressao_pt1 <- lm(DADOS_W1_PSYC_ancEUR$CAPE_W1 ~ DADOS_W1_PSYC_ancEUR$PRS_PSYC + DADOS_W1_PSYC_ancEUR$PC1 + 
                       DADOS_W1_PSYC_ancEUR$PC2 + DADOS_W1_PSYC_ancEUR$PC3 + DADOS_W1_PSYC_ancEUR$PC4 + 
                       DADOS_W1_PSYC_ancEUR$PC5 + DADOS_W1_PSYC_ancEUR$PC6 + DADOS_W1_PSYC_ancEUR$PC7 + 
                       DADOS_W1_PSYC_ancEUR$PC8 + DADOS_W1_PSYC_ancEUR$PC9 + DADOS_W1_PSYC_ancEUR$PC10 + 
                       DADOS_W1_PSYC_ancEUR$PERS_4)
summary(regressao_pt1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt1) # diagnostic plots
lm.beta(regressao_pt1) # valores beta da regressao

# 2. CAPE_W1 ~ PRS_PSYC + PC_1 + ... + PC_10
regressao_pt2 <- lm(DADOS_W1_PSYC_ancEUR$CAPE_W1 ~ DADOS_W1_PSYC_ancEUR$PRS_PSYC + DADOS_W1_PSYC_ancEUR$PC1 + 
                          DADOS_W1_PSYC_ancEUR$PC2 + DADOS_W1_PSYC_ancEUR$PC3 + DADOS_W1_PSYC_ancEUR$PC4 + 
                          DADOS_W1_PSYC_ancEUR$PC5 + DADOS_W1_PSYC_ancEUR$PC6 + DADOS_W1_PSYC_ancEUR$PC7 + 
                          DADOS_W1_PSYC_ancEUR$PC8 + DADOS_W1_PSYC_ancEUR$PC9 + DADOS_W1_PSYC_ancEUR$PC10)
summary(regressao_pt2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt2) # diagnostic plots
lm.beta(regressao_pt2) # valores beta da regressao

# 3. CAPE_W1 ~ PERS_4
regressao_pt3 <- lm(DADOS_W1_PSYC_ancEUR$CAPE_W1 ~ DADOS_W1_PSYC_ancEUR$PERS_4)
summary(regressao_pt3)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt3) # diagnostic plots
lm.beta(regressao_pt3) # valores beta da regressao

# 4. CAPE_W1 ~ (PERS_4 * PRS)
regressao_pt4 <- lm(DADOS_W1_PSYC_ancEUR$CAPE_W1 ~ (DADOS_W1_PSYC_ancEUR$PERS_4 * DADOS_W1_PSYC_ancEUR$PRS_PSYC))
summary(regressao_pt4)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt4) # diagnostic plots
lm.beta(regressao_pt4) # valores beta da regressao

# 5. CAPE_W1 ~ PRS_PSYC + PC_1 + ... + PC_10 + PERS_4 + (PERS_4 * PRS)
regressao_pt5 <- lm(DADOS_W1_PSYC_ancEUR$CAPE_W1 ~ DADOS_W1_PSYC_ancEUR$PRS_PSYC + DADOS_W1_PSYC_ancEUR$PC1 + 
                                                   DADOS_W1_PSYC_ancEUR$PC2 + DADOS_W1_PSYC_ancEUR$PC3 + DADOS_W1_PSYC_ancEUR$PC4 + 
                                                   DADOS_W1_PSYC_ancEUR$PC5 + DADOS_W1_PSYC_ancEUR$PC6 + DADOS_W1_PSYC_ancEUR$PC7 + 
                                                   DADOS_W1_PSYC_ancEUR$PC8 + DADOS_W1_PSYC_ancEUR$PC9 + DADOS_W1_PSYC_ancEUR$PC10 + 
                                                   DADOS_W1_PSYC_ancEUR$PERS_4 + (DADOS_W1_PSYC_ancEUR$PERS_4 * DADOS_W1_PSYC_ancEUR$PRS_PSYC))
summary(regressao_pt5)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt5) # diagnostic plots
lm.beta(regressao_pt5) # valores beta da regressao
