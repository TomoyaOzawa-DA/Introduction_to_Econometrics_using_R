### データ把握 ###
## 基本統計量を確認してみる
summary(df)

## 欠損値の補完
# dfの欠損値を全て0にする
df[is.na(df)] <- 0

# あるいはdfのmother_eduという列の欠損値を0にする
df$mother_edu[is.na(df$mother_edu)] <- 0


### データの加工 ###
## ダミー変数作成
hist(df$working_hours)
df$working_dummy <- ifelse(df$working_hours>0, 1,0)
summary(df)

## 多重共線性
cor(df)

## 相関係数を図に起こすこともできる
install.packages('psych')
library(psych)
cor.plot(cor(df))

### 計量経済学モデル構築 ###
## 重回帰分析の実装
out2 <- lm(data = df, hourly_wage ~ education + age + working_dummy + mother_edu + father_edu)

### 結果の解釈 ###
## 結果のサマリーを出す
summary(out2)

## 予測誤差を可視化する
plot(fitted(out2), resid(out2))

## VIF統計量を算出する
install.packages("car")
library(car)
# VIF統計量を算出する
vif(out2)


### 番外編 ###
## データの抽出
# dfというデータの中から賃金が0ではないデータを抜き出して新しくdf_newという名とする
df_new <- subset(df, hourly_wage >0)

summary(df_new)
# もう一度相関係数を算出
cor(df_new)

## モデル構築
out3<- lm(data = df_new, hourly_wage ~ education + age + mother_edu + father_edu)
summary(out3)

## 誤差を可視化する
plot(fitted(out3), resid(out3))






