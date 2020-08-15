### データ把握・加工 ###
## 新しい変数の作成
df$number_kids <- df$number_kids_under6 + df$number_kids_6to18
View(df)
head(df)
tail(df)

## ヒストグラムの作成
hist(df$working_hours)
hist(df$number_kids)

## 基本統計量の算出
summary(df)


### 計量経済学モデル構築 ###
## 重回帰分析の実装
out1 <- lm(data = df, hourly_wage ~ education + working_hours + number_kids )


### 結果の解釈 ###
summary(out1)

