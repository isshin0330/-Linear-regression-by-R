posdata <- read.csv("C:/R/POSdata.csv")
sapply(posdata, class)
posdata$yy<-as.integer(substring(posdata$ymd,1,4))
posdata$mm<-as.integer(substring(posdata$ymd,6,7))
posdata$ym<-as.integer(substring(posdata$ymd,1,4))*100+as.integer(substring(posdata$ymd,6,7))
posdata$prof <- posdata$sales1*50 + posdata$sales2*100 + posdata$sales3*150 + posdata$sales4*150 + 
  posdata$sales5*100 + posdata$sales6*100 + posdata$sales7*150 + posdata$sales8*50 + posdata$sales9*100 + posdata$sales10*150

posdata$ymd<-as.Date(posdata$ymd)
posdata$serial <- as.numeric(rownames(posdata))
library(makedummies)
posdata$holiday2 <- as.factor(posdata$holiday)
posdata$dayofweek2 <- as.factor(posdata$dayofweek)
hd<-makedummies(posdata, basal_level = FALSE, col = "holiday2")
dd<-makedummies(posdata, basal_level = FALSE, col = "dayofweek2")
dumVar<-data.frame(hd, dd)
View(posdata)

#posdata2 売上0の日を除外
#records that have zero salesT are removed from posdata
posdata2<-posdata[(posdata$salesT != 0),]
posdata2$holiday2 <- as.factor(posdata2$holiday)
posdata2$dayofweek2 <- as.factor(posdata2$dayofweek)
hd<-makedummies(posdata2, basal_level = FALSE, col = "holiday2")
dd<-makedummies(posdata2, basal_level = FALSE, col = "dayofweek2")
dumVar2<-data.frame(hd, dd)
View(posdata2)

#posdata3 日で集約
#group by yyyymmdd from posdata2
#values are mean
posdata3<- aggregate(posdata2, list(posdata2$ymd), FUN=mean)
posdata3$ymd <- posdata3$Group.1
posdata3$holiday2 <- as.factor(posdata3$holiday)
posdata3$dayofweek2 <- as.factor(posdata3$dayofweek)
hd<-makedummies(posdata3, basal_level = FALSE, col = "holiday2")
dd<-makedummies(posdata3, basal_level = FALSE, col = "dayofweek2")
dumVar3<-data.frame(hd, dd)
View(posdata3)

#タスク①Ａ地区とＢ地区のお弁当が売れる種類の違いをグラフで表す
par(mfrow = c(2,1))
plot(posdata2[(posdata2$areaid==1),c("ymd","salesT")])  #年と総売り上げに相関は見られない
plot(posdata2[(posdata2$areaid==2),c("ymd","salesT")])　#上に同じ、ほぼ同じグラフ
par(mfrow = c(1,1))

Data <- posdata2[,c("areaid","sales1","sales2","sales3",
                    "sales4","sales5","sales6","sales7",
                    "sales8","sales9","sales10")]
Data <- aggregate(Data, list(Data$areaid), FUN = mean)
Data$Group.1 <- NULL
Data$areaid <- NULL
op<-par(mfrow=c(1,1))
barplot(t(Data),
        col= c(0,0,0,0,0,0,0,0,0,0),
        names.arg = c(1:10))
View(Data)


#タスク②A~E地区でお弁当の売れる種類が類似する地区を調査してグラフで表す
#クラスタ分析して統計的に確かめてみる
par(mfrow = c(1,1))
boxplot(posdata2$salesT~posdata2$areaid,col=posdata2$areaid) #外的条件（ここではエリアID）ごとに勝手に作成してくれる
par(mfrow = c(1,1))

#作業 各弁当の売上比率を計算して項目として追加する
#【参考】データの加工の例 弁当1売上比率の項目追加 例
cwork<- posdata2[(posdata2$areaid==1) |(posdata2$areaid==2) | (posdata2$areaid==3) |(posdata2$areaid==4) | (posdata2$areaid==5),]
cwork$salesR1 <- (cwork$sales1/cwork$salesT)
cwork$salesR2 <- (cwork$sales2/cwork$salesT)
View(cwork)



#タスク③Ａ地区とC地区の違いを調べる。違いの原因をグラフで表す
#---------------------------------------------------------
#作業 各変数の関連を鳥瞰して違いを確認
#【参考】全体売上数と弁当毎の売上数の関係を確認⇒以降の解析は全体の売上数のみを扱う根拠とする
plot(posdata2[(posdata2$areaid==1),c("salesT","sales1","sales2","sales3","sales4","sales5","sales6","sales7","sales8","sales9","sales10")])
plot(posdata2[(posdata2$areaid==3),c("salesT","sales1","sales2","sales3","sales4","sales5","sales6","sales7","sales8","sales9","sales10")])
#【参考】時系列で描画
par(mfrow = c(2,1))
plot(posdata2[(posdata2$areaid==1),c("ymd","salesT")])
plot(posdata2[(posdata2$areaid==3),c("ymd","salesT")])
par(mfrow = c(1,1))
#【参考】曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==3),c("salesT","serial","dayofweek","areaid")])
plot(posdata2[(posdata2$areaid==1),c("salesT","serial","dayofweek","areaid")])

#【参考】天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==1),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])
plot(posdata2[(posdata2$areaid==3),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])
#---------------------------------------------------------
#作業 地区＋曜日で箱ひげ図を描画して２つの地区の違いを考察
#【参考】箱ひげ図
cwork<- posdata2[(posdata2$areaid==1) | (posdata2$areaid==3),]
boxplot(cwork$salesT~cwork$dayofweek)
boxplot(cwork$salesT~cwork$dayofweek+cwork$areaid)


#タスク④C地区とD地区の違いを調べる
#---------------------------------------------------------
#作業 各変数の関連を鳥瞰して違いを確認(タスク③と同様の一連の確認)
#【参考】時系列に描画
par(mfrow = c(2,1))
plot(posdata2[(posdata2$areaid==3),c("ymd","salesT")])
plot(posdata2[(posdata2$areaid==4),c("ymd","salesT")])
par(mfrow = c(1,1))
#【参考】曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==3),c("salesT","serial","dayofweek","areaid")])
plot(posdata2[(posdata2$areaid==4),c("salesT","serial","dayofweek","areaid")])
#【参考】天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==3),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])
plot(posdata2[(posdata2$areaid==4),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#作業 ２つの地区のデータの時系列分析を行う
#【参考】月毎の箱ひげ図を描画
x<-posdata2[(posdata2$areaid==3),]
boxplot(x$salesT~x$ym,col=x$areaid)
x<-posdata2[(posdata2$areaid==4),]
boxplot(x$salesT~x$ym,col=x$areaid)
#【参考】週の周期性を考慮した時系列分析の例
x<-posdata[(posdata$areaid==3),]
ts<-ts(x$salesT,frequency=7,start=c(1,1))
plot(stl(ts,s.window="per"))
x<-posdata[(posdata$areaid==4),]
ts<-ts(x$salesT,frequency=7,start=c(1,1))
plot(stl(ts,s.window="per"))

#作業 ２つの地区のデータで線形回帰分析を行う
#【参考】線形回帰分析の例
par(mfrow = c(2,2))
x<-posdata2[(posdata2$areaid==3 ),]
xx<-lm(x$salesT~x$serial )
summary(xx)
plot(xx)
par(mfrow = c(1,1))
par(mfrow = c(2,2))
x<-posdata2[(posdata2$areaid==4),]
xx<-lm(x$salesT~x$serial )
summary(xx)
plot(xx)
par(mfrow = c(1,1))

#作業 １つ前で行った線形回帰分析では高い相関係数が得られない。理由を考えてみる
#【参考】線形回帰分析の例(解答と同等)
par(mfrow = c(2,2))
x<-posdata2[(posdata2$areaid==4 & posdata2$dayofweek==4),]
xx<-lm(x$salesT~x$serial )
summary(xx)
plot(xx)
par(mfrow = c(1,1))

#タスク⑤F~J地区の売上に対する天候の影響の度合いの違いを調べる
#A地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==1),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==1),]
boxplot(x$salesT~x$ym,col=0, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==3),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==3),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#B地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==2),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==2),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==2),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==2),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#E地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==5),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==5),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==5),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==5),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#F地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==6),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==6),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==6),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==6),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#G地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==7),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==7),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==7),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==7),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#H地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==8),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==8),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==8),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==8),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#I地区
par(mfrow =c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==9), c("ymd", "salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図 
x <- posdata2[(posdata2$areaid==9),]
boxplot(x$salesT~x$ym,col=x$areaid+10, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[posdata2$areaid==9,c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[posdata2$areaid==9,c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])

#J地区
par(mfrow = c(1,2))
#時系列
plot(posdata2[(posdata2$areaid==10),c("ymd","salesT")], xlab="time", ylab="total sales")
#時系列箱ひげ図
x<-posdata2[(posdata2$areaid==10),]
boxplot(x$salesT~x$ym,col=x$areaid, xlab="time", ylab="total sales")
#曜日/時間の影響の有無を調査
plot(posdata2[(posdata2$areaid==10),c("salesT","serial","dayofweek","areaid")])
#天候データの影響の有無を調査
plot(posdata2[(posdata2$areaid==10),c("salesT","temp","precipitation","snowfall","humidity","daylight","windspeed")])
