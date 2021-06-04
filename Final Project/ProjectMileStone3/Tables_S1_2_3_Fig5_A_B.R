# Models for the Brain project

library(dplyr)
library(ggplot2)
library(rcompanion)
library(rms)
library(questionr)
library(stargazer)

#### Read data

df_article <- read.csv(file = 'ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv', header= TRUE)

# ARTICLE LEVEL
#### Filters
###### Filter Year [1970-2018]
###### Filter Kp >= 2 and Wp >= 2
df_article = df_article %>% filter(Yp >= 1970)
df_article = df_article %>% filter(Yp <= 2018)
df_article = df_article %>% filter(Kp >= 2)
df_article = df_article %>% filter(nMeSHMain >= 2)
df_article = df_article %>% filter(IRegionRefinedp > 0 & IRegionRefinedp < 7)

df_article$IRegionRefinedp = as.double(df_article$IRegionRefinedp)

df_article <- df_article %>%
    mutate(Regionp = if_else(IRegionRefinedp %in% c(1,2,3),IRegionRefinedp,0))

df_article <- df_article %>%
  mutate(XSACIPp = if_else(((XSAp == XCIPp) & (XCIPp == 1)),1,0))

df_article <- df_article %>%
    mutate(NOTXSACIPp = if_else((XSAp == XCIPp),0,1))

df_article <- df_article %>%
  mutate(NEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp) 
                                     & (NEUROLONGXCIPp == 1)),1,0))

df_article <- df_article %>%
    mutate(NOTNEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp)),0,1))

df_article <- df_article %>%
  mutate(NEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp) 
                                        & (NEUROSHORTXCIPp == 1)),1,0))

df_article <- df_article %>%
  mutate(NOTNEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp)),0,1))

#### Convert Data types
df_article$eidsp = as.factor(df_article$eidsp)
df_article$Yp = as.integer(df_article$Yp)
df_article$Kp = as.integer(df_article$Kp)
df_article$MeanZJp = as.double(df_article$MeanZJp)
df_article$XSAp = as.factor(df_article$XSAp)
df_article$XCIPp = as.factor(df_article$XCIPp)
df_article$NRegp = as.integer(df_article$NRegp)
df_article$NSAp = as.integer(df_article$NSAp)
df_article$NCIPp = as.integer(df_article$NCIPp)
df_article$nMeSHMain = as.integer(df_article$nMeSHMain)
df_article$IRegionRefinedp = as.factor(df_article$IRegionRefinedp)
df_article$logKp = as.double(log(df_article$Kp))
df_article$logMajorMeSHp = as.double(log(df_article$nMeSHMain))
df_article$Regionp = as.factor(df_article$Regionp)
df_article$XSACIPp = as.factor(df_article$XSACIPp)
df_article$NOTXSACIPp = as.factor(df_article$NOTXSACIPp)
df_article$NOTNEUROLONGXSACIPp = as.factor(df_article$NOTNEUROLONGXSACIPp)
df_article$NEUROSHORTXSACIPp = as.factor(df_article$NEUROSHORTXSACIPp)
df_article$NEUROLONGXSACIPp = as.factor(df_article$NEUROLONGXSACIPp)

# FILTER DATA
df_article_bm3 = df_article %>% filter(NOTXSACIPp!=1) 
df_article_bm6 = df_article %>% filter(NOTXSACIPp!=1) 
df_article_dm3 = df_article %>% filter(NOTNEUROLONGXSACIPp!=1)
df_article_dm6 = df_article %>% filter(NOTNEUROLONGXSACIPp!=1) 
df_article_nm3 = df_article %>% filter(NOTNEUROSHORTXSACIPp!=1) 
df_article_nm6 = df_article %>% filter(NOTNEUROSHORTXSACIPp!=1) 

## BROAD Model 1 - for X_SA
model1 <- glm(XSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp, 
              data = df_article, family=binomial(link='logit'))


## BROAD Model 2 - for X_CIP
model2 <- glm(XCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp , 
              data = df_article, family=binomial(link='logit'))

## BROAD Model 3 - for X_SA&CIP

model3 <- glm(XSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp , 
              data = df_article_bm3, family=binomial(link='logit'))

## BROAD Model 4 - for X_SA, Shift after funding
model4 <- glm(XSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp + 
                  IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
              ,data = df_article, family=binomial(link='logit'))

## BROAD Model 5 - for X_CIP, Shift after funding
model5 <- glm(XCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp + 
                IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
              ,data = df_article, family=binomial(link='logit'))

## BROAD Model 6 - for X_SACIP, Shift after funding
model6 <- glm(XSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp  + 
                IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
              ,data = df_article, family=binomial(link='logit'))

## Distant Model 1 - X_SA
model1_d <- glm(NEUROLONGXSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp  + NRegp+ NCIPp, data = df_article, family=binomial(link='logit'))


## Distant Model 2 - NEUROLONGXCIPp
model2_d <- glm(NEUROLONGXCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp  + NRegp+ NSAp, data = df_article, family=binomial(link='logit'))


## Distant Model 3 - Distant X_SA&CIP
model3_d <- glm(NEUROLONGXSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp  + NRegp,  data = df_article_dm3, family=binomial(link='logit'))

## Distant Model 4 -  Distant X_SA , shift after funding
model4_d <- glm(NEUROLONGXSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
                ,data = df_article, family=binomial(link='logit'))

## Distant Model 5 -  Distant X_CIP , shift after funding
model5_d <- glm(NEUROLONGXCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
                ,data = df_article, family=binomial(link='logit'))

## Distant Model 6 -  Distant X_SA&CIP , shift after funding
model6_d <- glm(NEUROLONGXSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
              ,data = df_article_dm6, family=binomial(link='logit'))

## Neighboring Model 1 - Neighboring X_SA
model1_m <- glm(NEUROSHORTXSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp, data = df_article, family=binomial(link='logit'))

## Neighboring Model 2 - Neighboring X_CIPp
model2_m <- glm(NEUROSHORTXCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp, data = df_article, family=binomial(link='logit'))

## Neighboring Model 3 - Neighboring X_SA&CIP
model3_m <- glm(NEUROSHORTXSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp,  data = df_article_nm3, family=binomial(link='logit'))

## Neighboring Model 4 -  Distant X_SA , shift after funding
model4_m <- glm(NEUROSHORTXSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
                ,data = df_article, family=binomial(link='logit'))

## Neighboring Model 5 -  Distant X_CIP , shift after funding
model5_m <- glm(NEUROSHORTXCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
                ,data = df_article, family=binomial(link='logit'))

## Neighboring Model 6 -  Distant X_SA&CIP , shift after funding
model6_m <- glm(NEUROSHORTXSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp  + IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
                ,data = df_article_nm6, family=binomial(link='logit'))
#Table 1
odds.ratio(model1)
exp(model1)
apply(odds.ratio(model1), 2, formatC, format="f", digits=4)
apply(odds.ratio(model2), 2, formatC, format="f", digits=4)
apply(odds.ratio(model3), 2, formatC, format="f", digits=4)
apply(odds.ratio(model4), 2, formatC, format="f", digits=4)
apply(odds.ratio(model5), 2, formatC, format="f", digits=4)
apply(odds.ratio(model6), 2, formatC, format="f", digits=4)

#install.packages("stargazer")
library(stargazer)
stargazer(model1,model2,model3,model4,model5,model6, title="S1 Table", align=F, font.size
= "tiny")

nagelkerke(model1)
#Table S1
summary(model1)
stargazer(list(model1,model2,model3,model4,model5,model6),  
          font.size= "scriptsize",
          keep.stat = c("rsq","n"),
          apply.coef = exp, 
          #coef = mymodels,
          dep.var.labels=c("X_{SA}","X_{CIP}", "X_{SA&CIP}", "X_{SA}","X_{CIP}", "X_{SA&CIP}"),
          covariate.labels=c("y" , "z_{j}", "ln k","ln w", "N_{R}", "N_{CIP}","N_{SA}",
                             "I_{2014+}","I_{R_{NA}}", "I_{R_{EU}}", "I_{R_{AA}}", "I_{R}",
                             "I_{R_{NA}} x I_{2014+}", "I_{R_{EU}} x I_{2014+}",
                             "I_{R_{AA}} x I_{2014+}")
          )

nagelkerke(model1, restrictNobs=T)[2]
#This is for Pseudo.R
#Table S2

apply(odds.ratio(model1_m), 2, formatC, format="f", digits=4)
apply(odds.ratio(model2_m), 2, formatC, format="f", digits=4)
apply(odds.ratio(model3_m), 2, formatC, format="f", digits=4)
apply(odds.ratio(model4_m), 2, formatC, format="f", digits=4)
apply(odds.ratio(model5_m), 2, formatC, format="f", digits=4)
apply(odds.ratio(model6_m), 2, formatC, format="f", digits=4)


exp(coef(model1_m))


stargazer(list(model1_m,model2_m,model3_m,model4_m,model5_m,model6_m), 
          apply.coef = exp, 
          font.size= "scriptsize",
          keep.stat = c("rsq","n"),
          #coef = mymodels,
          dep.var.labels=c("X_{SA}","X_{CIP}", "X_{SA&CIP}", "X_{SA}","X_{CIP}", "X_{SA&CIP}"),
          covariate.labels=c("y" , "z_{j}", "ln k","ln w", "N_{R}", "N_{CIP}","N_{SA}",
                             "I_{2014+}","I_{R_{NA}}", "I_{R_{EU}}", "I_{R_{AA}}", "I_{R}",
                             "I_{R_{NA}} x I_{2014+}", "I_{R_{EU}} x I_{2014+}",
                             "I_{R_{AA}} x I_{2014+}")
          )

#Table S3
exp(coef(model1_d))


stargazer(list(model1_d,model2_d,model3_d,model4_d,model5_d,model6_d),
          apply.coef = exp, 
          font.size= "scriptsize",
          keep.stat = c("rsq","n"),
          #coef = mymodels,
          dep.var.labels=c("X_{SA}","X_{CIP}", "X_{SA&CIP}", "X_{SA}","X_{CIP}", "X_{SA&CIP}"),
          covariate.labels=c("y" , "z_{j}", "ln k","ln w", "N_{R}", "N_{CIP}","N_{SA}",
                             "I_{2014+}","I_{R_{NA}}", "I_{R_{EU}}", "I_{R_{AA}}", "I_{R}",
                             "I_{R_{NA}} x I_{2014+}", "I_{R_{EU}} x I_{2014+}",
                             "I_{R_{AA}} x I_{2014+}")
)

#Plot Sources

# ## BROAD Model 1 - for X_SA
# model1 <- glm(XSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp, 
#               data = df_article, family=binomial(link='logit'))
# 
# 
# ## BROAD Model 2 - for X_CIP
# model2 <- glm(XCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp , 
#               data = df_article, family=binomial(link='logit'))
# 
# ## BROAD Model 3 - for X_SA&CIP
# 
# model3 <- glm(XSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp , 
#               data = df_article_bm3, family=binomial(link='logit'))







#Fig 5 B
library(stargazer)

#Broad Y values
stargazer(model1,model2,model3, 
          apply.coef = exp, 
          apply.se   = exp,
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5A-Broad_Odds.txt")

stargazer(model1_m,model2_m,model3_m, 
          apply.coef = exp, 
          se = list(NULL),
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5A-Neigborh_Odds.txt")


stargazer(model1_d,model2_d,model3_d, 
          apply.coef = exp, 
          se = list(NULL),
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5A-Distant_Odds.txt")



# summary(model2)
#


#Fig5A from TableS1 
library(car)
library(stats)
library(ggplot2)

Yp_5A <- c(1.032, 1.009,1.046, 1.030, 1.002,1.025, 1.033, 1.017,1.043 )
Se_5A <- c(0.0003,0.001,0.001,0.0004,0.001,0.001,0.0004,0.001,0.002)
spinout <-
  data.frame(Beta=100*Yp_5A-100,
             se=100*Se_5A,
             Type=c("Broad","Broad","Broad","Neighboring","Neighboring","Neighboring","Distant","Distant","Distant"),
             Domain=c("SA","CIP","SA_CIP","SA","CIP","SA_CIP","SA","CIP","SA_CIP"))



spinout$Type<-factor(spinout$Type,levels=c("Broad","Neighboring","Distant"))
spinout$Domain<-as.factor(spinout$Domain)

pd = position_dodge(2)

ggplot(spinout, aes(
  x = Type,
  y = Beta,
  color = factor(Domain)
)) +
  geom_errorbar(aes(ymin = Beta-se,  ymax = Beta+se),
    width = .2,  size = .7, position = position_dodge(.5)
  ) +
  geom_point(shape = 15, size = 3, position = position_dodge(.5)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid = element_blank(),
    axis.title.y = element_text(vjust = 1.8),
    axis.title.x = element_text(vjust = -0.5),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(label=c("P(XSA)/P(M)","P(XCIP )/P(M)","P(XSA&CIP )/P(M)"),values = c("grey", "blue", "black")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ylab(" ") + xlab(" ") +
  # labs (title = expression(paste("100", beta, "_y: ", " P(XSA)/P(M)" , " P(XCIP )/P(M)" ," P(XSA&CIP )/P(M)" )), values = c("grey", "blue", "black")) +
  annotate("text", x = c(.72,1.82,2.82), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "grey") +
  annotate("text", x = c(.92,2.02,3.02), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "blue") +
 annotate("text", x = c(1.12,2.22,3.22), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "black")



#Figure 5 B

stargazer(model4,model5,model6, 
          apply.coef = exp, 
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5B-Broad_Odds.txt")

stargazer(model4_m,model5_m,model6_m, 
          apply.coef = exp, 
          se = list(NULL),
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5B-Neigborh_Odds.txt")


stargazer(model4_d,model5_d,model6_d, 
          apply.coef = exp, 
          se = list(NULL),
          font.size= "scriptsize",
          keep.stat = c("rsq","n"), type = "text", out = "5B-Distant_Odds.txt")



I_2014_5B <- c(0.949, 0.754,0.738,     1.029, 0.770,0.795,   0.871, 0.735,0.648 )
SE_5B <- c(0.020,0.016,0.017,    0.016,0.018,0.026,   0.016,0.030,0.047)

spinout <-
  data.frame(Beta=100*I_2014_5B-100,
             se=100*SE_5B,
             Type=c("Broad","Broad","Broad","Neighboring","Neighboring","Neighboring","Distant","Distant","Distant"),
             Domain=c("SA","CIP","SA_CIP","SA","CIP","SA_CIP","SA","CIP","SA_CIP"))



spinout$Type<-factor(spinout$Type,levels=c("Broad","Neighboring","Distant"))
spinout$Domain<-as.factor(spinout$Domain)

pd = position_dodge(2)

ggplot(spinout, aes(
  x = Type,
  y = Beta,
  color = factor(Domain)
)) +
  geom_errorbar(aes(ymin = Beta-se,  ymax = Beta+se),
                width = .2,  size = .7, position = position_dodge(.5)
  ) +
  geom_point(shape = 15, size = 3, position = position_dodge(.5)) +
  theme_bw() +
  theme(
    legend.position = "NaN",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid = element_blank(),
    axis.title.y = element_text(vjust = 1.8),
    axis.title.x = element_text(vjust = -0.5),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(label=c("CIP","SA","SA_CIP"),values = c("grey", "blue", "black")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ylab(" ") +  xlab(" ") +
  labs (title = expression(paste("100 ",gamma, " 2014+" ))) +
  annotate("text", x = c(.72,1.82,2.82), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "grey") +
  annotate("text", x = c(.92,2.02,3.02), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "blue") +
  annotate("text", x = c(1.12,2.22,3.22), y = c(5.5, 5.5, 5.5), label = c("***","***", "***"), size = 6, color = "black")




