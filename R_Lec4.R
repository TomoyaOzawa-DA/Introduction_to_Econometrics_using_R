### データの読み込み ###
df <- read.csv('labsup_edited.csv')

### データ把握 ###
summary(df)

### 計量経済モデル構築 ###

## OLS ##
out_OLS <- lm(data = df, weeksm1 ~ morekids + agem1 + black + hispan + othrace )
summary(out_OLS)

## IV ##
# 操作変数の妥当性#
out_1st <- lm(data = df,  morekids ~ samesex + agem1 + black + hispan + othrace )
summary(out_1st) # F値>10よりOK

# 操作変数の外生性は言葉を尽くす

# IV推定 #
install.packages("AER")
library(AER)
out_iv <- ivreg( weeksm1 ~ morekids + agem1 + black + hispan + othrace  | agem1 + black + hispan + othrace + samesex, data = df)
summary(out_iv)

# 必要なライブラリをインストールする#
install.packages("sem")
library(sem)

# 操作変数法を実施して，その結果をout_ivに格納する#
out_iv <- tsls( weeksm1 ~ morekids + agem1 + black + hispan + othrace , ~samesex + agem1 + black + hispan + othrace, data = df)


### 解釈 ###
# 必要なパッケージをインストールする #
install.packages("memisc")
library(memisc)

result <- mtable("OLS推定" = out_OLS, "IV推定 1st Step" = out_1st, "IV推定" = out_iv )
result
write.mtable(result,file="result_iv.csv",colsep=",")
















