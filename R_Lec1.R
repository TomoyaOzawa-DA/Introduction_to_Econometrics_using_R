### データ把握・加工 ###
## データの読み込み
df <- read.csv("female_labor.csv")
df <- read.csv(file.choose())

## グラフを書いて視覚的に把握する
# 賃金
hist(df$hourly_wage)

# 賃金と教育年数の関係性
plot(df$education, df$hourly_wage)


### 計量経済学モデル構築 ###
out <- lm(df$hourly_wage ~ df$education)
out <-lm(data = df, hourly_wage ~ education)


### 結果の解釈 ###
## 結果のサマリー
summary(out)

## 回帰直線を図にプロットする 
plot(df$education, df$hourly_wage)
abline(out)
abline(out, col = "blue")
