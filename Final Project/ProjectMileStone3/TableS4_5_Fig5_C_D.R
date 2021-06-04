# Models for the Brain project

library(dplyr)
library(ggplot2)
library(rcompanion)
library(lmtest)
library(sandwich)
library(plm)
library(RTextTools)

#### Read data

# AUTHOR LEVEL
df_author = read_data('AuthorArticleLevel-RegData-ALLSA_Xc_1_NData_864590_LONGXCIP2.txt')
df_author = df_author %>% filter(NPubsnAuthorIDi >= 10) %>% 
  filter(Kp >= 2) %>% filter(nMeSHMain >= 2) %>% 
  filter(Yp >= 1970 & Yp <= 2018)

df_author$I_2014 <- ifelse(df_author$Yp >= 2014,1,0)

#str(df_author)

df_author <- df_author %>%
  mutate(XSACIPp = if_else(((XSAp == XCIPp) 
                            & (XCIPp == 1)),1,0))
df_author <- df_author %>%
  mutate(NOTXSACIPp = if_else((XSAp == XCIPp),0,1))
df_author <- df_author %>%
  mutate(NEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp) 
                                     & (NEUROLONGXCIPp == 1)),1,0))
df_author <- df_author %>%
  mutate(NOTNEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp)),0,1))

df_author <- df_author %>%
  mutate(NEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp) 
                                      & (NEUROSHORTXCIPp == 1)),1,0))
df_author <- df_author %>%
  mutate(NOTNEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp)),0,1))

df_author$Yp = as.integer(df_author$Yp)
df_author$Kp = as.integer(df_author$Kp)
df_author$MeanZJp = as.double(df_author$MeanZJp)
df_author$NRegp = as.integer(df_author$NRegp)
df_author$NSAp = as.integer(df_author$NSAp)
df_author$NCIPp = as.integer(df_author$NCIPp)
df_author$nMeSHMain = as.integer(df_author$nMeSHMain)
df_author$Tauip = as.integer(df_author$Tauip)
df_author$Zp = as.double(df_author$Zp)

df_author$logKp = as.double(log(df_author$Kp))
df_author$logMajorMeSHp = as.double(log(df_author$nMeSHMain))
# Dummy variables
df_author <- df_author %>%
  mutate(DIRegionRefinedp1 = if_else(IRegionRefinedp == 0,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp2 = if_else(IRegionRefinedp == 1,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp3 = if_else(IRegionRefinedp == 2,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp4 = if_else(IRegionRefinedp == 3,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp5 = if_else(IRegionRefinedp == 4,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp6 = if_else(IRegionRefinedp == 5,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp7 = if_else(IRegionRefinedp == 6,1,0))
df_author <- df_author %>%
  mutate(DIRegionRefinedp8 = if_else(IRegionRefinedp == 7,1,0))

df_author <- df_author %>%
  mutate(DYp1 = if_else(min_pub_year == 1941,1,0))
df_author <- df_author %>%
  mutate(DYp2 = if_else(min_pub_year == 1942,1,0))
df_author <- df_author %>%
  mutate(DYp3 = if_else(min_pub_year == 1943,1,0))
df_author <- df_author %>%
  mutate(DYp4 = if_else(min_pub_year == 1944,1,0))
df_author <- df_author %>%
  mutate(DYp5 = if_else(min_pub_year == 1945,1,0))
df_author <- df_author %>%
  mutate(DYp6 = if_else(min_pub_year == 1946,1,0))
df_author <- df_author %>%
  mutate(DYp7 = if_else(min_pub_year == 1947,1,0))
df_author <- df_author %>%
  mutate(DYp8 = if_else(min_pub_year == 1948,1,0))
df_author <- df_author %>%
  mutate(DYp9 = if_else(min_pub_year == 1949,1,0))
df_author <- df_author %>%
  mutate(DYp10 = if_else(min_pub_year == 1950,1,0))
df_author <- df_author %>%
  mutate(DYp11 = if_else(min_pub_year == 1951,1,0))
df_author <- df_author %>%
  mutate(DYp12 = if_else(min_pub_year == 1952,1,0))
df_author <- df_author %>%
  mutate(DYp13 = if_else(min_pub_year == 1953,1,0))
df_author <- df_author %>%
  mutate(DYp14 = if_else(min_pub_year == 1954,1,0))
df_author <- df_author %>%
  mutate(DYp15 = if_else(min_pub_year == 1955,1,0))
df_author <- df_author %>%
  mutate(DYp16 = if_else(min_pub_year == 1956,1,0))
df_author <- df_author %>%
  mutate(DYp17 = if_else(min_pub_year == 1957,1,0))
df_author <- df_author %>%
  mutate(DYp18 = if_else(min_pub_year == 1958,1,0))
df_author <- df_author %>%
  mutate(DYp19 = if_else(min_pub_year == 1959,1,0))
df_author <- df_author %>%
  mutate(DYp20 = if_else(min_pub_year == 1960,1,0))
df_author <- df_author %>%
  mutate(DYp21 = if_else(min_pub_year == 1961,1,0))
df_author <- df_author %>%
  mutate(DYp22 = if_else(min_pub_year == 1962,1,0))
df_author <- df_author %>%
  mutate(DYp23 = if_else(min_pub_year == 1963,1,0))
df_author <- df_author %>%
  mutate(DYp24 = if_else(min_pub_year == 1964,1,0))
df_author <- df_author %>%
  mutate(DYp25 = if_else(min_pub_year == 1965,1,0))
df_author <- df_author %>%
  mutate(DYp26 = if_else(min_pub_year == 1966,1,0))
df_author <- df_author %>%
  mutate(DYp27 = if_else(min_pub_year == 1967,1,0))
df_author <- df_author %>%
  mutate(DYp28 = if_else(min_pub_year == 1968,1,0))
df_author <- df_author %>%
  mutate(DYp29 = if_else(min_pub_year == 1969,1,0))
df_author <- df_author %>%
  mutate(DYp30 = if_else(min_pub_year == 1970,1,0))
df_author <- df_author %>%
  mutate(DYp31 = if_else(min_pub_year == 1971,1,0))
df_author <- df_author %>%
  mutate(DYp32 = if_else(min_pub_year == 1972,1,0))
df_author <- df_author %>%
  mutate(DYp33 = if_else(min_pub_year == 1973,1,0))
df_author <- df_author %>%
  mutate(DYp34 = if_else(min_pub_year == 1974,1,0))
df_author <- df_author %>%
  mutate(DYp35 = if_else(min_pub_year == 1975,1,0))
df_author <- df_author %>%
  mutate(DYp36 = if_else(min_pub_year == 1976,1,0))
df_author <- df_author %>%
  mutate(DYp37 = if_else(min_pub_year == 1977,1,0))
df_author <- df_author %>%
  mutate(DYp38 = if_else(min_pub_year == 1978,1,0))
df_author <- df_author %>%
  mutate(DYp39 = if_else(min_pub_year == 1979,1,0))
df_author <- df_author %>%
  mutate(DYp40 = if_else(min_pub_year == 1980,1,0))
df_author <- df_author %>%
  mutate(DYp41 = if_else(min_pub_year == 1981,1,0))
df_author <- df_author %>%
  mutate(DYp42 = if_else(min_pub_year == 1982,1,0))
df_author <- df_author %>%
  mutate(DYp43 = if_else(min_pub_year == 1983,1,0))
df_author <- df_author %>%
  mutate(DYp44 = if_else(min_pub_year == 1984,1,0))
df_author <- df_author %>%
  mutate(DYp45 = if_else(min_pub_year == 1985,1,0))
df_author <- df_author %>%
  mutate(DYp46 = if_else(min_pub_year == 1986,1,0))
df_author <- df_author %>%
  mutate(DYp47 = if_else(min_pub_year == 1987,1,0))
df_author <- df_author %>%
  mutate(DYp48 = if_else(min_pub_year == 1988,1,0))
df_author <- df_author %>%
  mutate(DYp49 = if_else(min_pub_year == 1989,1,0))
df_author <- df_author %>%
  mutate(DYp50 = if_else(min_pub_year == 1990,1,0))
df_author <- df_author %>%
  mutate(DYp51 = if_else(min_pub_year == 1991,1,0))
df_author <- df_author %>%
  mutate(DYp52 = if_else(min_pub_year == 1992,1,0))
df_author <- df_author %>%
  mutate(DYp53 = if_else(min_pub_year == 1993,1,0))
df_author <- df_author %>%
  mutate(DYp54 = if_else(min_pub_year == 1994,1,0))
df_author <- df_author %>%
  mutate(DYp55 = if_else(min_pub_year == 1995,1,0))
df_author <- df_author %>%
  mutate(DYp56 = if_else(min_pub_year == 1996,1,0))
df_author <- df_author %>%
  mutate(DYp57 = if_else(min_pub_year == 1997,1,0))
df_author <- df_author %>%
  mutate(DYp58 = if_else(min_pub_year == 1998,1,0))
df_author <- df_author %>%
  mutate(DYp59 = if_else(min_pub_year == 1999,1,0))
df_author <- df_author %>%
  mutate(DYp60 = if_else(min_pub_year == 2000,1,0))
df_author <- df_author %>%
  mutate(DYp61 = if_else(min_pub_year == 2001,1,0))
df_author <- df_author %>%
  mutate(DYp62 = if_else(min_pub_year == 2002,1,0))
df_author <- df_author %>%
  mutate(DYp63 = if_else(min_pub_year == 2003,1,0))
df_author <- df_author %>%
  mutate(DYp64 = if_else(min_pub_year == 2004,1,0))
df_author <- df_author %>%
  mutate(DYp65 = if_else(min_pub_year == 2005,1,0))
df_author <- df_author %>%
  mutate(DYp66 = if_else(min_pub_year == 2006,1,0))
df_author <- df_author %>%
  mutate(DYp67 = if_else(min_pub_year == 2007,1,0))
df_author <- df_author %>%
  mutate(DYp68 = if_else(min_pub_year == 2008,1,0))
df_author <- df_author %>%
  mutate(DYp69 = if_else(min_pub_year == 2009,1,0))
df_author <- df_author %>%
  mutate(DYp70 = if_else(min_pub_year == 2010,1,0))
df_author <- df_author %>%
  mutate(DYp71 = if_else(min_pub_year == 2011,1,0))
df_author <- df_author %>%
  mutate(DYp72 = if_else(min_pub_year == 2012,1,0))
df_author <- df_author %>%
  mutate(DYp73 = if_else(min_pub_year == 2013,1,0))


# FILTER DATA
df_author_bm4 = df_author %>% filter(NOTXSACIPp!=1)
df_author_nm5 = df_author %>% filter(NOTNEUROSHORTXSACIPp!=1)
df_author_dm6 = df_author %>% filter(NOTNEUROLONGXSACIPp!=1)
df_author_nm2 = df_author %>% filter(NOTNEUROSHORTXSACIPp != 1)
df_author_defm3 = df_author %>% filter(NOTNEUROLONGXSACIPp != 1)

#Table S4
## Model 1 - for broad
model21 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + XSAp + XCIPp + 
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author, model = "within", index="nAuthorID")
summary(model21, robust=TRUE)


library(stargazer)
stargazer(model21, model22, model23,align=F,  font.size   = "tiny", type = "text")
## Model 2 - for Neighboring

model22 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + NEUROSHORTXSAp + NEUROSHORTXCIPp +
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author, model = "within", index="nAuthorID")
summary(model22, robust=TRUE)

## Model 3 - for Distant
model23 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + NEUROLONGXSAp + NEUROLONGXCIPp +
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author, model = "within", index="nAuthorID")
summary(model23, robust=TRUE)

## Model 4 - BROAD X_SA&CIP
model24 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + XSACIPp +
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author_bm4, model = "within", index="nAuthorID")
summary(model24, robust=TRUE)

## Model 5 - Neighboring X_SA&CIP
model25 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + NEUROSHORTXSACIPp +
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author_nm5, model = "within", index="nAuthorID")
summary(model25, robust=TRUE)


## Model 6 - for Distant X_SACIP
model26 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + NEUROLONGXSACIPp +
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author_dm6, model = "within", index="nAuthorID")
summary(model26, robust=TRUE)


#Table S5 Models
## Model 1 - for broad effect of Funding
model21f <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_2014 + XSACIPp + (XSACIPp * I_2014)+  
                 SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                 CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                 DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                 DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                 DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                 DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                 DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
               , data = df_author_bm4, model = "within", index="nAuthorID")
summary(model21f, robust=TRUE)

## Model 2 - for broad effect of Funding
model22f <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_2014 + NEUROSHORTXSACIPp + (I_2014 * NEUROSHORTXSACIPp)+
                  SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                  CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                  DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                  DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                  DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                  DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                  DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
                , data = df_author_nm2, model = "within", index="nAuthorID")
summary(model22f, robust=TRUE)

## Model 3 - for broad effect of Funding
model23f <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_2014 + NEUROLONGXSACIPp + (I_2014 * NEUROLONGXSACIPp) +
                  SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                  CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                  DIRegionRefinedp1 +  DIRegionRefinedp2 + DIRegionRefinedp3 +  DIRegionRefinedp4 + DIRegionRefinedp5 +  DIRegionRefinedp6 + DIRegionRefinedp7 +  DIRegionRefinedp8 +
                  DYp1 + DYp2 + DYp3 + DYp4 + DYp5 + DYp6 + DYp7 + DYp8 + DYp9 + DYp10 + DYp11 + DYp12 + DYp13 + DYp14 + DYp15 + DYp16 + DYp17 + DYp18 + DYp19 + DYp20 +
                  DYp21 + DYp22 + DYp23 + DYp24 + DYp25 + DYp26 + DYp27 + DYp28 + DYp29 + DYp30 + DYp31 + DYp32 + DYp33 + DYp34 + DYp35 + DYp36 + DYp37 + DYp38 + DYp39 + DYp40 +
                  DYp41 + DYp42 + DYp43 + DYp44 + DYp45 + DYp46 + DYp47 + DYp48 + DYp49 + DYp50 + DYp51 + DYp52 + DYp53 + DYp54 + DYp55 + DYp56 + DYp57 + DYp58 + DYp59 + DYp60 +
                  DYp61 + DYp62 + DYp63 + DYp64 + DYp65 + DYp66 + DYp67 + DYp68 + DYp69 + DYp70 + DYp71 + DYp72 + DYp73
                , data = df_author_nm2, model = "within", index="nAuthorID")

summary(model23f, robust=TRUE)
#Table 4
#stargazer(model21,model22,model23,model24,model25,model26, align=F, font.size = "tiny")

stargazer(list(model21,model22,model23,model24,model25,model26)) # run this till all values mach
?stargazer()
stargazer(list(model21,model22,model23,model24,model25,model26),  
          font.size= "scriptsize",
          keep.stat = c("rsq","n","f"),
          #coef = mymodels,
          dep.var.labels=c("Z_{p}","Z_{p}", "Z_{p}", "Z_{p}","Z_{p}", "Z_{p}"),
          covariate.labels=c("ln k","ln w", "\tau",
                             "I_{X_{SA}} ",
                             "I_{X_{CIP}} ",
                             "I_{X_{Neighboring,SA}} ", "I_{X_{Neighboring,CIP}}",
                             "I_{X_{Distant,SA}} ", "I_{X_{Distant;CIP}}",
                             "I_{X_{SACIP}} ", "I_{X_{Neighboring,SACIP}}",
                             "I_{X_{Distant;SACIP}}")
)

#Table 5
stargazer(list(model21f,model22f,model23f)) # run this till all values mach

stargazer(list(model21f,model22f,model23f),  
          font.size= "scriptsize",
          keep.stat = c("rsq","n"),
          #coef = mymodels,
          dep.var.labels=c("Z_{p}","Z_{p}", "Z_{p}", "Z_{p}","Z_{p}", "Z_{p}"),
          covariate.labels=c("ln k","ln w", "T", 
                             "I_{X_{Neighboring;SA}} ", "I_{X_{Neighboring;CIP}}",
                             "I_{X_{Distant;SA}} ", "I_{X_{Distant;CIP}}",
                             "I_{X_{SACIP}} ", "I_{X_{Neighboring;SACIP}}",
                             "I_{X_{Distant;SACIP}}")
)



stargazer(model21f,model22f,model23f, align=F, font.size= "tiny")
nagelkerke(model22f)

exp(coef(model21))
exp(coef(model21))
summary(model21)


abc<- c(exp(model21), model22, model23) 

marginal <- lsmeans(model21, "temperature")

CLD = multcomp::cld(marginal,
                    alpha=0.05, 
                    Letters=letters,        # Use lower-case letters for .group
                    adjust="tukey") 

ggplot(CLD,
       aes(x     = temperature,
           y     = lsmean,
           label = .group)) +
  
  geom_point(shape  = 15,
             size   = 4) +
  
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7) +
  
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  
  ylab("Least square mean\n Rating") +
  
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(1.5, 1.5, 1.5),
            color   = "black")


#Fig 5 C velues from Table 4 
# run following to get CI values.
stargazer(model21, model22, model23,align=F, ci=T, font.size   = "tiny", type = "text", 
          out = "Fig5-C_TableS4_withCIs.txt")

stargazer(list(model21,model22,model23,model24,model25,model26), ci = T, 
          type = "text", 
          out = "Fig5-C_TableS4_withCIs.txt",
          font.size= "scriptsize",
          keep.stat = c("rsq","n","f"),
          #coef = mymodels,
          dep.var.labels=c("Z_{p}","Z_{p}", "Z_{p}", "Z_{p}","Z_{p}", "Z_{p}"),
          covariate.labels=c("ln k","ln w", "\tau",
                             "I_{X_{SA}} ",
                             "I_{X_{CIP}} ",
                             "I_{X_{Neighboring,SA}} ", "I_{X_{Neighboring,CIP}}",
                             "I_{X_{Distant,SA}} ", "I_{X_{Distant;CIP}}",
                             "I_{X_{SACIP}} ", "I_{X_{Neighboring,SACIP}}",
                             "I_{X_{Distant;SACIP}}"))



#Fig 5C

i_2014 <-
  data.frame(I_2014=100 * c(0.049,0.073, 0.089,0.070,-0.009, 0.023,0.039,0.034,0.045),
             
             se=100 * c( 0.0026,0.0029,0.0049,0.0003,0.0003,0.0056,0.0028 ,0.0062,0.010),
             se1=100*c(0.0036,0.0046,0.0072,0.0046,0.004,0.008,0.003,0.010,0.015),
             Type=rep(c("Broad","Neighboring","Distant"), each = 3),
             Domain=rep(c("SA","CIP","SA_CIP"), times = 3))


i_2014=mutate(i_2014,min=I_2014-se)
i_2014=mutate(i_2014,max=I_2014+se)


i_2014$Type<-factor(i_2014$Type,levels=c("Broad","Neighboring","Distant"))
i_2014$Domain<-factor(i_2014$Domain,levels=c("SA","CIP","SA_CIP"))

pd = position_dodge(2)

ggplot(i_2014, aes(
  x = Type,
  y = I_2014,
  color = Domain
)) +
  geom_errorbar(
    aes(ymin = I_2014-se1,
        ymax = I_2014+se1),
    width = .2,
    size = .7,
    position = position_dodge(.5)
  ) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_point(shape = 15, size = 3, position = position_dodge(.5)) +
  theme_bw() +
  theme(
    legend.position = "Nan",
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
  ylab(" ") + xlab(" ") +
   labs (title = expression(paste("100 (", sigma, ") ", gamma, x ))) +
  annotate("text", x = c(.72,1.82,2.82), y = c(10, 10, 10), label = c("***","***", "***"), size = 6, color = "grey") +
  annotate("text", x = c(.92,2.02,3.02), y = c(10, 10, 10), label = c("***","***", "***"), size = 6, color = "blue") +
  annotate("text", x = c(1.12,2.22,3.22), y = c(10, 10, 10), label = c("***","***", "***"), size = 6, color = "black")



#Fig 5D


i_2014 <-
  data.frame(I_2014=100 * c(0.157, -0.088, 0.157-0.088,  0.182, -0.160, 0.182-0.160, 0.001, -0.041, 0.001-0.041),
             se=100 * c(0.005,0.007,0.005+0.007,0.006,0.008,0.006+0.008,0.012,0.018,0.012+0.018),
             se1=100*c(0.008,0.009,0.008+0.009,0.009,0.01,0.009+0.01,0.018,0.018,0.018+0.018),
             Type=rep(c("Broad","Neighboring","Distant"), each = 3),
             Domain=rep(c("XSA_CIP_2013","XSA_CIP_2014","XSA_CIP_2013+XSA_CIP_2014"), times = 3))


i_2014=mutate(i_2014,min=I_2014-se)
i_2014=mutate(i_2014,max=I_2014+se)



i_2014$Type<-factor(i_2014$Type,levels=c("Broad","Neighboring","Distant"))
i_2014$Domain<-factor(i_2014$Domain,levels=c("XSA_CIP_2013","XSA_CIP_2014","XSA_CIP_2013+XSA_CIP_2014"))


pd = position_dodge(2)


ggplot(i_2014, aes(
  x = Type,
  y = I_2014,
  color = Domain
)) +
  geom_errorbar(
    aes(ymin = I_2014-se1,
        ymax = I_2014+se1),
    width = .2,
    size = .7,
    position = position_dodge(.5)
  ) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_point(shape = 15, size = 3, position = position_dodge(.5)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid = element_blank(),
    axis.title.y = element_text(vjust = 1.8),
    axis.title.x = element_text(vjust = -0.5),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(label=c("XSA_CIP_2013","XSA_CIP_2014","XSA_CIP_2013+XSA_CIP_2014"),values = c("grey", "blue", "black")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ylab(" ") + xlab(" ") +
  #labs (title = expression(paste("100 (", sigma, ") ", gamma, x ))) +
  annotate("text", x = c(.82,1.82,2.82), y = c(20, 20, 20), label = c("***","***", "***"), size = 6, color = "grey") +
  annotate("text", x = c(1.02,2.02,3.02), y = c(20, 20, 20), label = c("***","***", "***"), size = 6, color = "blue") +
  annotate("text", x = c(1.22,2.22,3.22), y = c(20, 20, 20), label = c("***","***", "***"), size = 6, color = "black")












