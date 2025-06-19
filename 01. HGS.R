library (geomorph)
library (abind)
library (viridis)
library (car)
library(plm)
library(corrplot)
library(ggplot2)
library(gridExtra)


# Load and organize data

data <- read.delim("hgs_data.txt")
str(data)

f.cmrd <- data[data$sex=="F" & data$nat == "CMR", ]
m.cmrd <- data[data$sex=="M" & data$nat == "CMR", ]

f.czd <- data[data$sex=="F" & data$nat == "CZ", ]
m.czd <- data[data$sex=="M" & data$nat == "CZ", ]

cmrd <- rbind (f.cmrd,m.cmrd)
czd <- rbind (f.czd,m.czd)


# Load shape data

slids <- read.table ("slidersR.txt")
links <- read.table ("linksR.txt")

fcmr13 <- readland.tps ("F_cmr13_hgs.tps")
mcmr13 <- readland.tps ("M_cmr13_hgs.tps")
fcmr16 <- readland.tps ("F_cmr16.tps")
mcmr16 <- readland.tps ("M_cmr16_hgs.tps")

fcz <- readland.tps ("F_cz16.tps")
mcz <- readland.tps ("M_cz16.tps")


# Organize shape datasets

fcmr <- abind (fcmr13, fcmr16)
mcmr <- abind (mcmr13, mcmr16)

cmr <- abind (fcmr, mcmr)
cz <- abind (fcz, mcz)

f_sh <- abind (fcmr, fcz) 
m_sh <- abind (mcmr, mcz) 

all <- abind (fcmr13, fcmr16, mcmr13, mcmr16, fcz, mcz)


# Procrustes Fit

gpall <- gpagen(all,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)

f_gpall <- gpagen(f_sh,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)

m_gpall <- gpagen(m_sh,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)

cmr_gpall <- gpagen(cmr,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)

cz_gpall <- gpagen(cz,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)


# Subsettings coordinates according to population

pop.sh <- coords.subset(gpall$coords, data$nat)


# Calculate SShD separately for CMR and CZ faces

cmr.sshd <- giveGroupVec(pop.sh$CMR, cmrd$sex)
cz.sshd <- giveGroupVec(pop.sh$CZ, czd$sex) 


# Calculate the level of asymmetry

asall <- ASYM (gpall$coords)


# Calculate correlations between repeated HGS measures for left and right hand 

cor (data[, c('R1','R2','R3')])
cor (data[, c('L1','L2','L3')])

cor(data[data$nat == "CMR", c('R1', 'R2', 'R3')])
cor(data[data$nat == "CZ", c('R1', 'R2', 'R3')])

# Calculate average across measurements of R and L

R.hgs <- rowMeans(data[, c('R1', 'R2', 'R3')], na.rm = TRUE)
L.hgs <- rowMeans(data[, c('L1', 'L2', 'L3')], na.rm = TRUE)
plot(L.hgs,R.hgs)

overal.hgs <- colMeans(rbind(R.hgs, L.hgs))

hgs_dif <- R.hgs - L.hgs


# HGS and HGS Asymmetry

fcmr.hgs <- overal.hgs[data$nat == "CMR" & data$sex == "F"] ## HGS in CMR Women
mcmr.hgs <- overal.hgs[data$nat == "CMR" & data$sex == "M"] ## HGS in CMR Men
fcz.hgs <- overal.hgs[data$nat == "CZ" & data$sex == "F"] ## HGS in CMR Women
mcz.hgs <- overal.hgs[data$nat == "CZ" & data$sex == "M"] ## HGS in CMR Men

fcmr.hgs_dif <- hgs_dif[data$nat == "CMR" & data$sex == "F"] ## HGS Asymmetry in CMR Women
mcmr.hgs_dif <- hgs_dif[data$nat == "CMR" & data$sex == "M"] ## HGS Asymmetry in CMR Men
fcz.hgs_dif <- hgs_dif[data$nat == "CZ" & data$sex == "F"] ## HGS Asymmetry in CZ Women
mcz.hgs_dif <- hgs_dif[data$nat == "CZ" & data$sex == "M"] ## HGS Asymmetry in CZ Men

# Facial characteristics

fcmr.sshd <- cmr.sshd$g1sc ## SShD in CMR Women
mcmr.sshd <- cmr.sshd$g2sc ## SShD in CMR Men
fcz.sshd <- cz.sshd$g1sc ## SShD in CZ Women
mcz.sshd <- cz.sshd$g2sc ## SShD in CZ Men

fcmr.asym <- asall[data$nat == "CMR" & data$sex == "F"] ## Facial Asymmetry in CMR Women
mcmr.asym <- asall[data$nat == "CMR" & data$sex == "M"] ## Facial Asymmetry in CMR Men
fcz.asym <- asall[data$nat == "CZ" & data$sex == "F"] ## Facial Asymmetry in CZ Women
mcz.asym <- asall[data$nat == "CZ" & data$sex == "M"] ## Facial Asymmetry in CZ Men

fcmr.fem_masc <- data$fem_masc[data$nat == "CMR" & data$sex == "F"] ## Femininity in CMR Women
mcmr.fem_masc <- data$fem_masc[data$nat == "CMR" & data$sex == "M"] ## Masculinity in CMR Men
fcz.fem_masc <- data$fem_masc[data$nat == "CZ" & data$sex == "F"] ## Femininity in CZ Women
mcz.fem_masc <- data$fem_masc[data$nat == "CZ" & data$sex == "M"] ## Masculinity in CZ Men

# Age

fcmr.age <- data$age[data$nat == 'CMR' & data$sex == 'F']
mcmr.age <- data$age[data$nat == 'CMR' & data$sex == 'M']
fcz.age <- data$age[data$nat == 'CZ' & data$sex == 'F']
mcz.age <- data$age[data$nat == 'CZ' & data$sex == 'M']


# TESTING CORRELATIONS - CORRELATION MATRICES


# CMR Women

fcmr_corrdata <- data.frame(
  HGS = fcmr.hgs,
  'HGS Asymmetry' = fcmr.hgs_dif,
  SShD = fcmr.sshd,
  'Facial Asymmetry' = fcmr.asym,
  Age = fcmr.age,
  Femininity = fcmr.fem_masc
)

fcmr_cd <- cor(fcmr_corrdata, method = "pearson", use = "complete.obs")
cor.test(fcmr.hgs, fcmr.sshd, method = "pearson") ## non-significant
cor.test(fcmr.hgs_dif, fcmr.sshd, method = "pearson") ## non-significant

corrplot.mixed(fcmr_cd,
               upper = "ellipse",
               lower.col = "#282828",
               tl.pos = "lt",
               tl.col = "#282828",
               tl.cex = 1.1,
               diag = "u",
               mar = c(0, 0, 4, 0)
)

title(main = "Cameroonian Women", adj = 0.2, cex.main = 1.5, font.main = 1)


# CMR Men

mcmr_corrdata <- data.frame(
  HGS = mcmr.hgs,
  'HGS Asymmetry' = mcmr.hgs_dif,
  SShD = mcmr.sshd,
  'Facial Asymmetry' = mcmr.asym,
  Age = mcmr.age,
  Masculinity = mcmr.fem_masc
)

mcmr_cd <- cor(mcmr_corrdata, method = "pearson", use = "complete.obs")
cor.test(mcmr.hgs, mcmr.sshd, method = "pearson") ## non-significant
cor.test(mcmr.hgs_dif, mcmr.sshd, method = "pearson") ## non-significant

corrplot.mixed(mcmr_cd,
               upper = "ellipse",
               lower.col = "#282828",
               tl.pos = "lt",
               tl.col = "#282828",
               tl.cex = 1.1,
               diag = "u",
               mar = c(0, 0, 4, 0)
)

title(main = "Cameroonian Men", adj = 0.2, cex.main = 1.5, font.main = 1)


# CZ Women

f_cz_corrdata <- data.frame(
  HGS = fcz.hgs,
  'HGS Asymmetry' = fcz.hgs_dif,
  SShD = fcz.sshd,
  'Facial Asymmetry' = fcz.asym,
  Age = fcz.age,
  Femininity = fcz.fem_masc
)

f_cz_cd <- cor(f_cz_corrdata, method = "pearson", use = "complete.obs")
cor.test(fcz.hgs, fcz.sshd, method = "pearson") ## non-significant
cor.test(fcz.hgs_dif, fcz.sshd, method = "pearson") ## non-significant


corrplot.mixed(f_cz_cd,
               upper = "ellipse",
               lower.col = "#282828",
               tl.pos = "lt",
               tl.col = "#282828",
               tl.cex = 1.1,
               diag = "u",
               mar = c(0, 0, 4, 0)
)

title(main = "Czech Women", adj = 0.2, cex.main = 1.5, font.main = 1)


# CZ Men

m_cz_corrdata <- data.frame(
  HGS = mcz.hgs,
  'HGS Asymmetry' = mcz.hgs_dif,
  SShD = mcz.sshd,
  'Facial Asymmetry' = mcz.asym,
  Age = mcz.age,
  Masculinity = mcz.fem_masc
)

m_cz_cd <- cor(m_cz_corrdata, method = "pearson", use = "complete.obs")
cor.test(mcz.hgs, mcz.sshd, method = "pearson") ## non-significant
cor.test(mcz.hgs_dif, mcz.sshd, method = "pearson") ## non-significant


corrplot.mixed(m_cz_cd,
               upper = "ellipse",
               lower.col = "#282828",
               tl.pos = "lt",
               tl.col = "#282828",
               tl.cex = 1.1,
               diag = "u",
               mar = c(0, 0, 4, 0)
)

title(main = "Czech Men", adj = 0.2, cex.main = 1.5, font.main = 1)



## SS Type III model

## Set contrasts globally for factors
options(contrasts = c("contr.sum", "contr.poly"))


# HGS ~ AGE AND FACIAL CHARACTERISTICS

## CMR Women

fcmr_gm_type1 <- lm(fcmr.hgs ~ fcmr.age + fcmr.fem_masc + fcmr.sshd + fcmr.asym)
fcmr_gm_type3 <- Anova(fcmr_gm_type1, type = "III")
print(fcmr_gm_type3) # no relation
summary(fcmr_gm_type1)

theme_set(theme_minimal(base_size = 18))

plot_fcmr_gm_type1 <- ggplot(fcmr_gm_type1, aes(x = fcmr.hgs, y = fcmr.sshd)) +
  geom_point(size = 4, shape = 21, 
             fill = "#2c7fb8", color = "black", stroke = 0, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#f03b20", fill = "#fcbba1", size = 1, linetype = "solid") +
  labs(x = "HGS", 
       y = "SShD", 
       title = "Cameroonian Women") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20, color = "#282828"),
    axis.title = element_text(face = "bold", size = 18, color = "#282828"),
    axis.text = element_text(size = 16, color = "#282828"),
    text = element_text(color = "#282828")
  )

print(plot_fcmr_gm_type1)


## CMR Men

mcmr_gm_type1 <- lm(mcmr.hgs ~ mcmr.age + mcmr.fem_masc + mcmr.sshd + mcmr.asym)
mcmr_gm_type3 <- Anova(mcmr_gm_type1, type = "III")
print(mcmr_gm_type3) # no relation
summary(mcmr_gm_type1)

theme_set(theme_minimal(base_size = 18))

plot_mcmr_gm_type1 <- ggplot(mcmr_gm_type1, aes(x = mcmr.hgs, y = mcmr.sshd)) +
  geom_point(size = 4, shape = 21, 
             fill = "#2c7fb8", color = "black", stroke = 0, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#f03b20", fill = "#fcbba1", size = 1, linetype = "solid") +
  labs(x = "HGS", 
       y = "SShD", 
       title = "Cameroonian Men") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20, color = "#282828"),
    axis.title = element_text(face = "bold", size = 18, color = "#282828"),
    axis.text = element_text(size = 16, color = "#282828"),
    text = element_text(color = "#282828")
  )

print(plot_mcmr_gm_type1)


## CZ Women

fcz_gm_type1 <- lm(fcz.hgs ~ fcz.age + fcz.fem_masc + fcz.sshd + fcz.asym)
fcz_gm_type3 <- Anova(fcz_gm_type1, type = "III")
print(fcz_gm_type3) # no relation
summary(fcz_gm_type1)

theme_set(theme_minimal(base_size = 18))

plot_fcz_gm_type1 <- ggplot(fcz_gm_type1, aes(x = fcz.hgs, y = fcz.sshd)) +
  geom_point(size = 4, shape = 21, 
             fill = "#2c7fb8", color = "black", stroke = 0, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#f03b20", fill = "#fcbba1", size = 1, linetype = "solid") +
  labs(x = "HGS", 
       y = "SShD", 
       title = "Czech Women") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20, color = "#282828"),
    axis.title = element_text(face = "bold", size = 18, color = "#282828"),
    axis.text = element_text(size = 16, color = "#282828"),
    text = element_text(color = "#282828")
  )

print(plot_fcz_gm_type1)


## CZ Men

mcz_gm_type1 <- lm(mcz.hgs ~ mcz.age + mcz.fem_masc + mcz.sshd + mcz.asym)
mcz_gm_type3 <- Anova(mcz_gm_type1, type = "III")
print(mcz_gm_type3) # no relation
summary(mcz_gm_type1)

theme_set(theme_minimal(base_size = 18))

plot_mcz_gm_type1 <- ggplot(mcz_gm_type1, aes(x = mcz.hgs, y = mcz.sshd)) +
  geom_point(size = 4, shape = 21, 
             fill = "#2c7fb8", color = "black", stroke = 0, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#f03b20", fill = "#fcbba1", size = 1, linetype = "solid") +
  labs(x = "HGS", 
       y = "SShD", 
       title = "Czech Men") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20, color = "#282828"),
    axis.title = element_text(face = "bold", size = 18, color = "#282828"),
    axis.text = element_text(size = 16, color = "#282828"),
    text = element_text(color = "#282828")
  )

print(plot_mcz_gm_type1)



# HGS ASYMMETRY ~ AGE AND FACIAL CHARACTERISTICS

## CMR Women

fcmr_hgs_dif_gm_type1 <- lm(fcmr.hgs_dif ~ fcmr.age + fcmr.fem_masc + fcmr.sshd + fcmr.asym)
fcmr_hgs_dif_gm_type3 <- Anova(fcmr_hgs_dif_gm_type1, type = "III")
print(fcmr_hgs_dif_gm_type3) ## non-significant
summary(fcmr_hgs_dif_gm_type1)

## CMR Men

mcmr_hgs_dif_gm_type1 <- lm(mcmr.hgs_dif ~ mcmr.age + mcmr.fem_masc + mcmr.sshd + mcmr.asym)
mcmr_hgs_dif_gm_type3 <- Anova(mcmr_hgs_dif_gm_type1, type = "III")
print(mcmr_hgs_dif_gm_type3) ## non-significant
summary(mcmr_hgs_dif_gm_type1)

## CZ Women

fcz_hgs_dif_gm_type1 <- lm(fcz.hgs_dif ~ fcz.age + fcz.fem_masc + fcz.sshd + fcz.asym)
fcz_hgs_dif_gm_type3 <- Anova(fcz_hgs_dif_gm_type1, type = "III")
print(fcz_hgs_dif_gm_type3) ## non-significant
summary(fcz_hgs_dif_gm_type1)

## CZ Men

mcz_hgs_dif_gm_type1 <- lm(mcz.hgs_dif ~ mcz.age + mcz.fem_masc + mcz.sshd + mcz.asym)
mcz_hgs_dif_gm_type3 <- Anova(mcz_hgs_dif_gm_type1, type = "III")
print(mcz_hgs_dif_gm_type3) ## non-significant
summary(mcz_hgs_dif_gm_type1)


## HGS ASYMMETRY ~ FACIAL ASYMMETRY

## CMR Women

fcmr_asym_gm_type1 <- lm(fcmr.hgs_dif ~ fcmr.asym)
fcmr_asym_gm_type3 <- Anova(fcmr_asym_gm_type1, type = "III")
print(fcmr_asym_gm_type3) ## non-significant
summary(fcmr_asym_gm_type1)

## CMR Men

mcmr_asym_gm_type1 <- lm(mcmr.hgs_dif ~ mcmr.asym)
mcmr_asym_gm_type3 <- Anova(mcmr_asym_gm_type1, type = "III")
print(mcmr_asym_gm_type3) ## non-significant
summary(mcmr_asym_gm_type1)

## CZ Women

fcz_asym_gm_type1 <- lm(fcz.hgs_dif ~ fcz.asym)
fcz_asym_gm_type3 <- Anova(fcz_asym_gm_type1, type = "III")
print(fcz_asym_gm_type3) ## non-significant
summary(fcz_asym_gm_type1)

## CZ Men

mcz_asym_gm_type1 <- lm(mcz.hgs_dif ~ mcz.asym)
mcz_asym_gm_type3 <- Anova(mcz_asym_gm_type1, type = "III")
print(mcz_asym_gm_type3) ## non-significant
summary(mcz_asym_gm_type1)