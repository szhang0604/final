# setup
library(haven)
library(dplyr)
library(stargazer)

df = read_dta("/Users/serenazhang/Downloads/wls_b_14_03.dta")
head(df)

# changing variable names 
names(df)[names(df) == "yrer74"] <- "income_1974"
names(df)[names(df) == "rp001re"] <- "income_1992"
names(df)[names(df) == "meanrat"] <- "attractiveness"
names(df)[names(df) == "rb003red"] <- "education"
names(df)[names(df) == "sexrsp"] <- "gender"
names(df)[names(df) == "ie020re"] <- "race"
names(df)[names(df) == "bmpin1"] <- "famincome"
names(df)[names(df) == "rfu39jcf"] <- "occ_status"

df <- df[!is.na(df$income_1992),]
df <- df[!is.na(df$attractiveness),]
df <- df[!is.na(df$education),]
df <- df[!is.na(df$famincome),]
df <- df[!is.na(df$occ_status),]

# viewing unique values (checking for invalid values)
unique(df$gender) # drop anything that is not 1 or 2
unique(df$income_1992) # drop <= 0
unique(df$income_1974) # drop <= 0
unique(df$attractiveness)
unique(df$education) # drop <= 0
unique(df$famincome)
unique(df$occ_status)

# dropping invalid responses
df <- df[df$income_1992 > 0, ]
summary(df$income_1992)

df <- df[df$income_1974 > 0, ]
summary(df$income_1974)

df <- df[df$education > 0, ]
summary(df$education)

df <- df[df$famincome > 0, ]
summary(df$famincome)

df <- df[df$occ_status > 0, ]
summary(df$occ_status)

# 1974 income is originally in 100s of dollars, 1992 is not
df$income_1974 <- 100*(df$income_1974)
summary(df$income_1974)

# changing scale of parental income
df$famincome <- 100*(df$famincome)
summary(df$famincome)

# recoding gender variable to m = 0, f = 1
df$gender <- ifelse(df$gender == 1, 0, 1)
unique(df$gender)

lm1 <- lm(income_1992 ~ attractiveness, data = df)  
summary(lm1)

lm2 <- lm(income_1992 ~ income_1974 + attractiveness + education + occ_status + gender + famincome, data = df)
summary(lm2)

# testing for interactions 
df_female <- df[df$gender == 1,] # women only
lm3 <- lm(income_1992 ~ income_1974 + attractiveness + education + occ_status + famincome, data = df_female)
summary(lm3)

df_male <- df[df$gender == 0,]
lm4 <- lm(income_1992 ~ income_1974 + attractiveness + education + occ_status + famincome, data = df_male)
summary(lm4)

## my analysis

# also conditioning on:
names(df)[names(df) == "gwiiq_bm"] <- "iq"
names(df)[names(df) == "rf054jce"] <- "tenure"
# and race

# dropping NA's
df2 <- df[!is.na(df$race),]
df2 <- df2[!is.na(df2$iq), ]
df2 <- df2[!is.na(df2$tenure), ]

# dropping invalid responses
df2 <- df2[df2$race > 0, ]
summary(df2$race)
df2 <- df2[df2$tenure >= 0, ]
summary(df2$tenure)

# logging income variables
hist(df2$income_1992) # income is extremely right skewed
hist(df2$income_1974)
hist(df2$famincome)

df2$income_1992 <- log(df2$income_1992) # so log transform
df2$income_1974 <- log(df2$income_1974)
df2$famincome <- log(df2$famincome)

hist(df2$famincome) # less skew

# changing scale of tenure - ex: 40 years is coded in data as 400.0
summary(df2$tenure)
df2$tenure <- 0.1*(df2$tenure)
summary(df2$tenure)

# recoding white = 0, nonwhite = 1
df2$race <- ifelse(df2$race == 1, 0, 1)
unique(df2$race)

# regress w/ prev variables
lm6 <- lm(income_1992 ~ income_1974 + attractiveness + gender + occ_status + famincome + education, data = df2)
summary(lm6)

# regress income 1992 on everything
lm5 <- lm(income_1992 ~ income_1974 + attractiveness + race + tenure+ education + occ_status + gender + iq + famincome, data = df2)
summary(lm5)

# 1992 on all predictors + interaction term
lm8 <- lm(income_1992 ~ income_1974 + attractiveness + tenure + iq + race + education + occ_status + famincome + gender + gender*attractiveness, data = df2)
summary(lm8)

