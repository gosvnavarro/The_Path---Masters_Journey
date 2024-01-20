## Different graphics
## Author: Gabrielle Navarro
## Updated in: 18/10/2021

######################
# PLOT - CORRELATION #
######################
# USANDO "ggpubr"
library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

# USANDO "plot"
plot(arquivo_a$col_1, arquivo_a$col_2, main = "1 X 2",
     xlab = "x-axis caption", ylab = "y-axis caption",
     pch = 19, frame = FALSE, xlim = c(0, 0.5), ylim = c(0, 2.5))
abline(lm(arquivo_a$col_2 ~ arquivo_a$col_1, data = arquivo_a), col = "red")

# USANDO 'ggplot'
library(ggplot2)
ggplot(arquivo_b, aes(x = col_1, y = col_2)) + 
  geom_point() +
  labs(title = "Col_1 x Col_2") +
  geom_smooth(method = lm)

#####################################################
# PLOTTING - MULTIPLE REGRESSION, INTEGRATIVE MODEL #
#################### THE COOLEST ####################
library("ggplot2")
ggplot(arquivo_c, aes(y = arquivo_c$col_1, x = arquivo_c$col_2, color = arquivo_c$col_3)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = F, colour = "red")

####################
# DIAGNOSTIC PLOTS #
####################
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fiti)

######################
# SIMPLE SCATTERPLOT #
######################
plot(arquivo_d$col_1, arquivo_d$col_2, 
     main = "Scatterplot Example",
     xlab = "x-axis caption", ylab = "y-axis caption", 
     pch = 19)

abline(lm(arquivo_d$col_1 ~ arquivo_d$col_2), col="red") # regression line (y~x)
lines(lowess(arquivo_d$col_1, arquivo_d$col_2), col="blue") # lowess line (x,y)

########################################################
# 3D SCATTERPLOT WITH COLORING AND VERTICAL DROP LINES #
########################################################
library(scatterplot3d)
library(ggrepel)

rbPal <- colorRampPalette(c('red','blue'))
arquivo_e$Col <- rbPal(10)[as.numeric(cut(arquivo_e$col_2, breaks = 10))]

scatterplot3d(arquivo_e$col_1, arquivo_e$col_2, arquivo_e$col_3, 
              pch = 17, highlight.3d = F,
              type = "p", main = "3D Scatterplot", 
              color = arquivo_e$Col)

ggplot(arquivo_e, aes(x = arquivo_e$col_1, y = Darquivo_e$col_2)) +
  geom_point(color = arquivo_e$Col) +
  labs(title = ' ', x = 'x-axis caption', y = 'y-axis caption')

########################################
# PLOTTING - SUBSET OF A SPECIFIC DATA #
########################################
# Create the subset
test <- subset(arquivo_f, arquivo_f$col_1 < 12000)
test2 <- subset(test, test$col_1 > 0.2567)

# Plot using ggplot
ggplot(test, aes(x = test$col_1, y = test$col_2)) + 
  geom_boxplot() + 
  expand_limits(x = test$col_1, y = c(0,0.6)) +
  labs(title = 'Title', x = 'x-axis caption', y = 'y-axis caption') +
  geom_text_repel(aes(label = ifelse(test$col_2 > 0.38, as.character(test2$col_1), '')), size = 2.5)
