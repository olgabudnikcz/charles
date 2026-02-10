library (abind)
library (geomorph)
library (viridis)
library(ggplot2)


# CUSTOM FUNCTIONS

## CALCULATING GROUP-RELEVANT SHAPESCORES

giveGroupVec <- function(coord,groupvar,iter = 99){
  
  dframe <- geomorph.data.frame(coords = coord, group = groupvar)
  reg1 <- procD.lm(coords ~ group, iter = iter, data = dframe)
  
  print(summary(reg1))
  
  coefficients<-coef(reg1, test = F)
  
  scores <- two.d.array (coord) %*% t(coefficients)
  sc <- scores [,2]
  
  groupvar <- as.numeric(as.factor(groupvar))
  
  g1sc <- subset (sc, groupvar == 1)
  g2sc <- subset (sc, groupvar == 2)
  
  gcoef<-t(coefficients)[,2]
  
  return(list(sc=sc, g1sc=g1sc, g2sc=g2sc, gcoef=gcoef))
}



## LOADING SHAPE DATA

slids <- read.table ("slidersR.txt")
links <- read.table ("linksR.txt")

wdall <- readland.tps ("W-DALL-CamAI.tps")
mdall <- readland.tps ("M-DALL-CamAI.tps")

wflux <- readland.tps ("W-FLUX-CamAI.tps")
mflux <- readland.tps ("M-FLUX-CamAI.tps")

wgem <- readland.tps ("W-Gemini-CamAI.tps")
mgem <- readland.tps ("M-Gemini-CamAI.tps")

wmidj <- readland.tps ("W-MIDJ-CamAI.tps")
mmidj <- readland.tps ("M-MIDJ-CamAI.tps")

wqwen <- readland.tps ("W-Qwen-CamAI.tps")
mqwen <- readland.tps ("M-Qwen-CamAI.tps")

wsd <- readland.tps ("W-SD-CamAI.tps")
msd <- readland.tps ("M-SD-CamAI.tps")

wcmr13 <- readland.tps ("W-CMR13.tps")
mcmr13 <- readland.tps ("M-CMR13.tps")
wcmr16 <- readland.tps ("W-CMR16.tps")
mcmr16 <- readland.tps ("M-CMR16.tps")



## ORGANIZING SHAPE DATASETS

wai <- abind(wdall, wflux, wgem, wmidj, wqwen, wsd)
mai <- abind(mdall, mflux, mgem, mmidj, mqwen, msd)
ai <- abind (wai, mai)
dim(ai)

dall <- abind (wdall, mdall)
flux <- abind (wflux, mflux)
gem <- abind (wgem, mgem)
midj <- abind (wmidj, mmidj)
qwen <- abind (wqwen, mqwen)
sd <- abind (wsd, msd)

wreal <- abind (wcmr13, wcmr16)
mreal <- abind (mcmr13, mcmr16)
real <- abind (wreal, mreal)
dim(real)

w <- abind (wreal, wai)
m <- abind (mreal, mai)
all <- abind (real, ai)
dim(all)



## PERFORMING GENERALIZED PROCRUSTES ANALYSIS

# Generalized Procrustes Analysis (all)
gpall <- gpagen(all,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gpall)

# Generalized Procrustes Analysis (AI)
gp_ai <- gpagen(ai,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_ai) 

# Generalized Procrustes Analysis (DALL-E)
gp_dall <- gpagen(dall,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_dall) 

# Generalized Procrustes Analysis (FLUX)
gp_flux <- gpagen(flux,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_flux) 

# Generalized Procrustes Analysis (GEMINI)
gp_gem <- gpagen(gem,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_gem) 

# Generalized Procrustes Analysis (Midjourney)
gp_midj <- gpagen(midj,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_midj) 

# Generalized Procrustes Analysis (QWEN)
gp_qwen <- gpagen(qwen,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_qwen) 

# Generalized Procrustes Analysis (Stable Diffusion)
gp_sd <- gpagen(sd,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_sd) 

# Generalized Procrustes Analysis (Real)
gp_real <- gpagen(real,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE)
plot(gp_real) 



## CREATING FACTOR VARIABLES

groups <- factor(c(rep("real", dim(real)[3]), rep("AI", dim(ai)[3])))
summary(groups)

face.groups <- factor(c(rep ("real", dim(real)[3]),
                        rep("dall", dim(dall)[3]), 
                        rep("flux", dim(flux)[3]), 
                        rep("gem", dim(gem)[3]),
                        rep("midj", dim(midj)[3]),
                        rep("qwen", dim(qwen)[3]),
                        rep("sd", dim(sd)[3])))
summary(face.groups)

dall.groups <- factor(c(rep("real", dim(real)[3]), rep("dall", dim(dall)[3])))
flux.groups <- factor(c(rep("real", dim(real)[3]), rep("flux", dim(flux)[3])))
gem.groups <- factor(c(rep("real", dim(real)[3]), rep("gem", dim(gem)[3])))
midj.groups <- factor(c(rep("real", dim(real)[3]), rep("midj", dim(midj)[3])))
qwen.groups <- factor(c(rep("real", dim(real)[3]), rep("qwen", dim(qwen)[3])))
sd.groups <- factor(c(rep("real", dim(real)[3]), rep("sd", dim(sd)[3])))

dall.sshd.groups <- factor(c(rep("wdall", dim(wdall)[3]), rep("mdall", dim(mdall)[3])))
flux.sshd.groups <- factor(c(rep("wflux", dim(wflux)[3]), rep("mflux", dim(mflux)[3])))
gem.sshd.groups <- factor(c(rep("wgem", dim(wgem)[3]), rep("mgem", dim(mgem)[3])))
midj.sshd.groups <- factor(c(rep("wmidj", dim(wmidj)[3]), rep("mmidj", dim(mmidj)[3])))
qwen.sshd.groups <- factor(c(rep("wqwen", dim(wqwen)[3]), rep("mqwen", dim(mqwen)[3])))
sd.sshd.groups <- factor(c(rep("wsd", dim(wsd)[3]), rep("msd", dim(msd)[3])))
real.sshd.groups <- factor(c(rep("wreal", dim(wreal)[3]), rep("mreal", dim(mreal)[3])))



## TESTING GROUP SHAPE SCORES ALL AI VS REAL FACES

groups.shapes <- giveGroupVec(gpall$coords, groups) 

df_groups <- data.frame(group = groups, score = groups.shapes$sc) 
ggplot(df_groups, aes(x = group, y = score, fill = group)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  geom_hline(yintercept = 0, colour = "grey80", linetype = "dashed") + 
  geom_boxplot(width = 0.7, colour = "grey30", linewidth = 0.3, alpha = 0.9) + 
  scale_fill_manual(values = c("real" = "lightblue", "AI" = "salmon"), guide = "none") + 
  labs(x = "", y = "", title = "Group Shape Score (AI vs Real Faces)") + 
  theme_classic(base_size = 12) + 
  theme( plot.margin = margin(30, 30, 30, 30), 
         axis.text.x = element_text(angle = 45, hjust = 1, size = 13), 
         axis.text.y = element_text(angle = 90, hjust = 1, size = 11), 
         plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = 15)), 
         axis.line.x = element_line(color = "grey30", linewidth = 0.3), 
         axis.line.y = element_line(color = "grey30", linewidth = 0.3) 
         )


## TESTING GROUP SHAPE SCORES SEPARATE AI MODELS VS REAL FACES

face.groups.shapes <- giveGroupVec(gpall$coords, face.groups)

df_groups <- data.frame(group = face.groups, score = face.groups.shapes$sc)

# создаём вертикальные полосы для каждой модели — real, dall, flux, gem, midj, qwen, sd
bg_df <- data.frame(xmin = seq_len(nlevels(df_groups$group)) - 0.5, xmax = seq_len(nlevels(df_groups$group)) + 0.5,
                    ymin = -Inf, ymax =  Inf, fill_bg = rep(c("grey97", "white"), length.out = nlevels(df_groups$group)))

ggplot(df_groups, aes(x = group, y = score, fill = group)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_rect(data = bg_df, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = bg_df$fill_bg, alpha = 0.9) +
  geom_hline(yintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_boxplot(width  = 0.7, colour = "grey30", linewidth = 0.3, alpha  = 0.9) +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462", "#80B1D3", "#B3DE69", "#FB8072", "#CCEBC5", "#BC80BD"), guide = "none") +
  labs(x = "", y = "", title = "Group Shape Score (AI vs Real Faces)") +
  theme_classic(base_size = 12) +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(angle = 90,  hjust = 1, size = 11),
    plot.title  = element_text(hjust = 0.5, size = 15, margin = margin(b = 15)),
    axis.line.x = element_line(color = "grey30", linewidth = 0.3),
    axis.line.y = element_line(color = "grey30", linewidth = 0.3))



## TESTING SEXUAL FACIAL SHAPE DIMORPHISM - AI MODELS VS REAL FACES

dall.sshd <- giveGroupVec(gp_dall$coords, dall.sshd.groups, iter = 100000)
flux.sshd <- giveGroupVec(gp_flux$coords, flux.sshd.groups, iter = 100000)
gem.sshd <- giveGroupVec(gp_gem$coords, gem.sshd.groups, iter = 100000)
midj.sshd <- giveGroupVec(gp_midj$coords, midj.sshd.groups, iter = 100000)
qwen.sshd <- giveGroupVec(gp_qwen$coords, qwen.sshd.groups, iter = 100000)
sd.sshd <- giveGroupVec(gp_sd$coords, sd.sshd.groups, iter = 100000)
real.sshd <- giveGroupVec(gp_real$coords, real.sshd.groups, iter = 100000)

df_sshd <- data.frame(
  model = c(rep("dall", length(dall.sshd$sc)), rep("flux", length(flux.sshd$sc)),
            rep("gem",  length(gem.sshd$sc)), rep("midj", length(midj.sshd$sc)), 
            rep("qwen", length(qwen.sshd$sc)), rep("sd",   length(sd.sshd$sc)), rep("real", length(real.sshd$sc))),
  sex = c(as.character(dall.sshd.groups), as.character(flux.sshd.groups), as.character(gem.sshd.groups),
          as.character(midj.sshd.groups), as.character(qwen.sshd.groups), as.character(sd.sshd.groups), as.character(real.sshd.groups)),
  score = c(dall.sshd$sc, flux.sshd$sc, gem.sshd$sc, midj.sshd$sc, qwen.sshd$sc, sd.sshd$sc, real.sshd$sc))

df_sshd$sex <- ifelse(grepl("^w", df_sshd$sex), "Women", "Men")
df_sshd$sex <- factor(df_sshd$sex, levels = c("Men", "Women"))
df_sshd$model <- factor(df_sshd$model,
                        levels = c("dall","flux","gem","midj","qwen","sd","real"))

bg_df <- data.frame(xmin = seq_len(nlevels(df_sshd$model)) - 0.5, xmax = seq_len(nlevels(df_sshd$model)) + 0.5,
                    ymin = -Inf, ymax =  Inf, fill_bg = rep(c("grey97", "white"), length.out = nlevels(df_sshd$model)))
ggplot(df_sshd, aes(x = model, y = score, fill = sex)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_rect(data = bg_df, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = bg_df$fill_bg, alpha = 0.9) +
  geom_hline(yintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_boxplot(width = 1, position = position_dodge(width = 0), colour = "grey30", linewidth = 0.3, alpha = 0.9) +
  scale_fill_manual(values = c("Men"   = "lightblue", "Women" = "salmon"), name = "") +
  labs(x = "", y = "", title = "Sexual Facial Shape Dimorphism") +
  theme_classic(base_size = 12) +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(angle = 90, hjust = 1, size = 11),
    plot.title  = element_text(hjust = 0.5, size = 15, margin = margin(b = 15)),
    axis.line.x = element_line(color = "grey30", linewidth = 0.3),
    axis.line.y = element_line(color = "grey30", linewidth = 0.3))



## TESTING MORPHOLOGICAL DISPARITY

morphol.disparity (two.d.array(gpall$coords) ~ groups, groups = groups, 
                   iter = 999, seed = NULL, data = NULL, print.progress = TRUE) ## all AI vs Real

morphol.disparity (two.d.array(gpall$coords) ~ face.groups, groups = face.groups, 
                   iter = 999, seed = NULL, data = NULL, print.progress = TRUE) ## AI Models vs Real



## TESTING DIFFERENCE BETWEEN GROUP MEANS

gtest <- procD.lm (gpall$coords ~ groups, iter = 999)
summary(gtest)

face.group <- coords.subset (gpall$coords, face.groups)

allgtest <- procD.lm (gpall$coords ~ face.groups, iter=999)
summary (allgtest)

dallreal <- abind(face.group$dall,face.group$real)
fluxreal <- abind(face.group$flux,face.group$real)
gemreal <- abind(face.group$gem,face.group$real)
midjreal <- abind(face.group$midj,face.group$real)
qwenreal <- abind(face.group$qwen,face.group$real)
sdreal <- abind(face.group$sd,face.group$real)

dallreal <- procD.lm (dallreal~dall.groups, iter=999)
summary (dallreal)

fluxreal <- procD.lm (fluxreal~flux.groups, iter=999)
summary (fluxreal)

gemreal <- procD.lm (gemreal~gem.groups, iter=999)
summary (gemreal)

midjreal <- procD.lm (midjreal~midj.groups, iter=999)
summary (midjreal)

qwenreal <- procD.lm (qwenreal~qwen.groups, iter=999)
summary (qwenreal)

sdreal <- procD.lm (sdreal~sd.groups, iter=999)
summary (sdreal)



## TESTING THE LEVEL OF FACIAL ASYMMETRY FOR AI VS REAL FACES (ANOVA)

left  <- c(50,51,52,53,54,55,56,57,58, 5,7,12,10,70,15,17,18,20,26,23,21,22,21,25,24,71, 35,46,45,44,34,49,48,47)
right <- c(59,60,61,62,63,64,65,66,67, 4,6,11,9,69,14,16,19,27,33,29,28,30,28,32,31,72, 37,38,39,40,36,41,42,43)
land.pairs <- cbind(left, right)

coords_all <- gpall$coords
ind_all <- factor(seq_len(dim(coords_all)[3]))

fa_res <- bilat.symmetry(A = coords_all, ind = ind_all, land.pairs = land.pairs, object.sym = TRUE, iter = 999)
fa_all <- fa_res$unsigned.AI

df_fa <- data.frame(group = groups, FA = fa_all)

fa_anova <- procD.lm(FA ~ group, data = df_fa, iter = 9999)
summary(fa_anova)

  

## TESTING THE LEVEL OF FACIAL ASYMMETRY FOR AI VS REAL FACES FOR WOMEN AND MEN SEPARATELY

n_wreal <- dim(wreal)[3]
n_mreal <- dim(mreal)[3]
n_wai <- dim(wai)[3]
n_mai <- dim(mai)[3]

sex_all <- c(rep("Women", n_wreal), rep("Men",   n_mreal), rep("Women", n_wai), rep("Men",   n_mai))
sex_all <- factor(sex_all, levels = c("Women","Men"))

df_fa2 <- data.frame(group = groups, sex = sex_all, FA = fa_all)

df_women <- df_fa2[df_fa2$sex == "Women", ]
df_men <- df_fa2[df_fa2$sex == "Men", ]

## FOR WOMEN: REAL vs AI
fa_anova_w <- procD.lm(FA ~ group, data = df_women, iter = 9999)
summary(fa_anova_w)

## FOR MEN: REAL vs AI
fa_anova_m <- procD.lm(FA ~ group, data = df_men, iter = 9999)
summary(fa_anova_m)
