########################
### データの読み込み ###
########################
install.packages("openxlsx")
library(openxlsx)
df_air <- read.xlsx("airfare.xlsx")


#################
### データ把握###
#################

## いつも通りの基本統計量 ##
summary(df_air)

## いつもよりかっこよっく ##
install.packages("ggplot2")
library(ggplot2)

# 運賃についてヒストグラムを書く
g_fare <- ggplot(data = df_air, aes(x = fare)) + geom_histogram()
g_fare

# theme_classic()を指定してあげると、論文用に使えそうです。
g_fare <- g_fare + theme_classic()
g_fare


#################
### データ加工###
#################

## fare の対数をとって外れ値に対応 ##
df_air$ln_fare <- log(df_air$fare)
# ヒストグラムで確認
g_lnfare <- ggplot(data = df_air, aes(x = ln_fare)) + geom_histogram() + theme_classic()
g_lnfare


## パネルデータ分析の前準備 ##
# パネルデータであることをRで認識させるために観測年と観測主体は明らかにする必要がある。 #
# IDは事前に振っておきましたが、以下の2通りがあるかなと思います。 #

## パターン① ##
# 出発地と到着値をくっつければよいか#
# 文字列を足す時はpasteという関数を使うとできます#
df_air$id <- paste (df_air$origin,df_air$destin)

## パターン② ##
# 上から数値を振り分ける#

data_num <- nrow(df_air)

df_air$id_num <- 1

for ( i in 2:data_num){
  if (df_air[i, "id"] != df_air[i-1, "id"]){
    df_air[i, "id_num"] = df_air[i-1, "id_num"] + 1
  }
  else{
    df_air[i, "id_num"] = df_air[i-1, "id_num"]
  }
}


##############################################
### 計量経済学モデル推定・パネルデータ分析 ###
##############################################

## まずは普通にOLSで推定してみましょう##
out_OLS <- lm (ln_fare ~ bmktshr+ dist + passen , data = df_air)
summary(out_OLS)

## パネルデータ分析に必要なパッケージをインストール ##
install.packages("plm")
library(plm)

## パネルデータと認識させる ##
# 観測主体は文字列でも数値でもどっちでも行けるみたいです。#
df_panel <- pdata.frame(df_air, index = c('ID', 'year'))

## 固定効果モデル・変量効果モデルで推定してみましょう##

# 変量効果モデル #
out_random <- plm(ln_fare ~ bmktshr+ dist + passen , data = df_panel, model = "random")
summary(out_random)

# 固定効果モデル #
out_fix <- plm(ln_fare ~ bmktshr+ dist + passen , data = df_panel, model = "within")
summary(out_fix)
fixef(out_fix)
mean(fixef(out_fix))

# ハウスマン検定;固定効果モデルの採択 #
phtest(out_fix, out_random)

# 固定効果モデルに時間効果#
out_fix_time <- plm(ln_fare ~ bmktshr+ dist + passen , data = df_panel, effect = "twoways", model = "within")
summary(out_fix_time)
fixef(out_fix_time)


############
### 解釈 ###
############

#パネルデータ分析の結果はmtableだと動かないみたい。#
install.packages("stargazer")
library(stargazer)
f <-stargazer(out_OLS, out_fix, out_random, out_fix_time, type="text")
write(f, "panel.txt")


