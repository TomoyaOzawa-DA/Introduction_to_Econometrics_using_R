########################
### データの読み込み ###
########################

df_fam <- read.csv("Mitaron_convenience.csv")
install.packages("readxl")
library(readxl)
df_map <- read_xlsx("Familymart_mesh.xlsx")

###################
### データ把握 ####
###################

### 基本統計量 ###
summary(df_fam)

## GISプロット ##
install.packages("readxl")
library(readxl)
df_map <- read_xlsx("Familymart_mesh.xlsx")

install.packages("leaflet")
library(leaflet)

map <- leaflet(df_map) %>% 
  addTiles()

map

map %>% 
  addCircles(lng = ~ longitude, lat = ~latitude, color = "green")


#######################
### モデル構築・推定###
#######################

logit1 = glm( Familymart_Entry ~ Familymart_existed + pop, 
              family = binomial(link = "logit"), data = df_fam)

probit1 <- glm( Familymart_Entry ~ Familymart_existed + pop, 
              family = binomial(link = "probit"), data = df_fam)

logit2 = glm( Familymart_Entry ~ Familymart_existed + LAWSON_existed + pop + n_employee, 
               family = binomial(link = "logit"), data = df_fam)

probit2 = glm( Familymart_Entry ~ Familymart_existed + LAWSON_existed + pop + n_employee, 
                 family = binomial(link = "probit"), data = df_fam)

############
### 解釈 ###
############

## 統計的有意 ##
summary(logit1)
summary(probit1)
summary(logit2)
summary(probit2)

## 平均限界効果 ##
install.packages("margins")
library(margins)
margins(logit1)
# 平均限界効果と統計的有意性の両方を同時に確認する
summary(margins(probit1))

## 疑似決定係数 ##
install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(logit1)

## 結果の整理 ##
install.packages("memisc")
library(memisc)

result <- mtable("ロジット" = logit1, "ロジット" = logit2, "プロビット" = probit1, "プロビット" = probit2 )
result














