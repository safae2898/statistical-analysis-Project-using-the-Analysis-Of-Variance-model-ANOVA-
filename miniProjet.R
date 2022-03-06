library(multcomp)

data1=read.csv2("C:/Users/el rhaffouli/Downloads/dataAgr.txt",sep=",")
attach(data1)
data1

data1$density = as.factor(data1$density)
M1=data1$density
str(M1)

data1$yield = as.numeric(data1$yield)
M2=data1$yield
str(M2)

data1$block = as.factor(data1$block)
M3=data1$block
str(M3)

data1$fertilizer = as.factor(data1$fertilizer)
M4=data1$fertilizer
str(M4)

str(data1)

library("ggpubr")
ggboxplot(data1, x = "density", y = "yield", color = "density",
          palette = c("#00AFBB", "#E7B800"))

ggline(data1, x = "density", y = "yield", color = "density",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))


boxplot(yield ~ block * density, data=data1, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Yield")


interaction.plot(x.factor = data1$density, trace.factor = data1$block, 
                 response = data1$yield, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "density", ylab="Yield",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))


res.aov2 <- aov(yield ~ block * density, data = data1)
summary(res.aov2)

res.aov3 <- aov(yield ~ block * density, data = data1)
res.aov3 <- aov(yield ~ block + density + fertilizer, data = data1)
summary(res.aov3)

require("dplyr")
group_by(data1, block, density) %>%
  summarise(
    count = n(),
    mean = mean(yield, na.rm = TRUE),
    sd = sd(yield, na.rm = TRUE)
  )

model.tables(res.aov3, type="means", se = TRUE)

install.packages("multcomp")


pairwise.t.test(data1$yield, data1$density,
                p.adjust.method = "BH")

plot(res.aov3, 1)

library(car)
leveneTest(yield ~ block*density, data = data1)

plot(res.aov3, 2)

aov_residuals <- residuals(object = res.aov3)

shapiro.test(x = aov_residuals )
