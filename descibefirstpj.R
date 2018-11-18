# 清除工作环境
cat("\014");rm(list=ls())

dat0=read.csv("weixin.csv",header=T,fileEncoding = "GBK")    #读入数据

category = rep(0,1212)
category[grep(pattern = "办公",dat0[,5])]="办公"
category[grep(pattern = "出行",dat0[,5])]="出行"
category[grep(pattern = "工具",dat0[,5])]="工具"
category[grep(pattern = "购物",dat0[,5])]="购物"
category[grep(pattern = "健康",dat0[,5])]="健康"
category[grep(pattern = "教育",dat0[,5])]="教育"
category[grep(pattern = "金融",dat0[,5])]="金融"
category[grep(pattern = "旅游",dat0[,5])]="旅游"
category[grep(pattern = "母婴",dat0[,5])]="母婴"
category[grep(pattern = "社交",dat0[,5])]="社交"
category[grep(pattern = "生活",dat0[,5])]="生活"
category[grep(pattern = "视频",dat0[,5])]="视频"
category[grep(pattern = "体育",dat0[,5])]="体育"
category[grep(pattern = "音乐",dat0[,5])]="音乐"
category[grep(pattern = "图像",dat0[,5])]="图像"
category[grep(pattern = "阅读",dat0[,5])]="阅读"

dat1 = cbind(dat0,category)
########################################################################
library(ggplot2) 
# 设定绘图颜色
color1 <- rgb(60,179,113, max = 255)
color2 <- rgb(50,205,50, max = 255)
color3 <- rgb(0,255,255, max = 255)
color4 <- rgb(0,206,209, max = 255)
color5 <- rgb(34,139,34, max = 255)
# 绘制对数人气分布直方图
fig2 <-  ggplot(data = dat1,aes(x = log(人气))) + geom_histogram(fill = color1, colour="white", binwidth = 0.5) + theme_bw()  ## 绘制直方图，利用color1填充，不画边框，直方图条宽为10
fig2 <- fig2 + xlab("人气(对数变换)") +  ## x轴标签为"人气"
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴标签字体大小、颜色、位置
fig2 <- fig2 + ylab("频数") +  ## y轴标签为"频数"
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴标签字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴数值字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定y轴数值字体大小、颜色、位置
fig2 <- fig2 + scale_x_continuous(breaks=seq(6, 13, 1)) ## 设定x轴的显示间隔
fig2  ## 显示图片
min(dat1$人气)
dat1$小程序名称[ which(dat1$人气==min(dat1$人气))]
dat1$小程序名称[ which(dat1$人气==max(dat1$人气))]
# 绘制对数人气分布直方图
fig2 <-  ggplot(data = dat1,aes(x = log(人气))) + geom_histogram(fill = color2, colour="white", binwidth = 0.5) + theme_bw()  ## 绘制直方图，利用color1填充，不画边框，直方图条宽为10
fig2 <- fig2 + xlab("人气(对数变换)") +  ## x轴标签为"人气"
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴标签字体大小、颜色、位置
fig2 <- fig2 + ylab("频数") +  ## y轴标签为"频数"
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴标签字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴数值字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定y轴数值字体大小、颜色、位置
fig2 <- fig2 + scale_x_continuous(breaks=seq(6, 13, 1)) ## 设定x轴的显示间隔
fig2  ## 显示图片

fig2 <-  ggplot(data = dat1,aes(x = log(人气))) + geom_histogram(fill = color3, colour="white", binwidth = 0.5) + theme_bw()  ## 绘制直方图，利用color1填充，不画边框，直方图条宽为10
fig2 <- fig2 + xlab("人气(对数变换)") +  ## x轴标签为"人气"
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴标签字体大小、颜色、位置
fig2 <- fig2 + ylab("频数") +  ## y轴标签为"频数"
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴标签字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴数值字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定y轴数值字体大小、颜色、位置
fig2 <- fig2 + scale_x_continuous(breaks=seq(6, 13, 1)) ## 设定x轴的显示间隔
fig2  ## 显示图片

fig2 <-  ggplot(data = dat1,aes(x = log(人气))) + geom_histogram(fill = color4, colour="white", binwidth = 0.5) + theme_bw()  ## 绘制直方图，利用color1填充，不画边框，直方图条宽为10
fig2 <- fig2 + xlab("人气(对数变换)") +  ## x轴标签为"人气"
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴标签字体大小、颜色、位置
fig2 <- fig2 + ylab("频数") +  ## y轴标签为"频数"
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴标签字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴数值字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定y轴数值字体大小、颜色、位置
fig2 <- fig2 + scale_x_continuous(breaks=seq(6, 13, 1)) ## 设定x轴的显示间隔
fig2  ## 显示图片

fig2 <-  ggplot(data = dat1,aes(x = log(人气))) + geom_histogram(fill = color5, colour="white", binwidth = 0.5) + theme_bw()  ## 绘制直方图，利用color1填充，不画边框，直方图条宽为10
fig2 <- fig2 + xlab("人气(对数变换)") +  ## x轴标签为"人气"
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴标签字体大小、颜色、位置
fig2 <- fig2 + ylab("频数") +  ## y轴标签为"频数"
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴标签字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定x轴数值字体大小、颜色、位置
fig2 <- fig2 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5)) ## 设定y轴数值字体大小、颜色、位置
fig2 <- fig2 + scale_x_continuous(breaks=seq(6, 13, 1)) ## 设定x轴的显示间隔
fig2  ## 显示图片
#(一)因变量：人气
#para1在本案例中，人气值的最小值为710,所对应的小程序是超搞笑GIF，去哪儿全球购，评价人数均为1，分属社交类和购物类。
#分析人气值低的原因：用数据表明：购物可能被什么垄断了，社交？？？表明人们用微信小程序进行娱乐社交的较少？
#人气最大值为977582，所对应的小程序是摩拜单车，属于出行交通类，表明小程序的确在交通方面给都市带来了许多便利
#通过人气的直方图（图1-1），可以看到：人气值是呈现右偏分布的。
#具体来说，人气的均值为、中位数为。这一现象符合我们对于小程序使用人气的基本认知，即存在少数用户量极高的小程序，从而拉高了人气的平均水平。
dat10=read.csv("train.csv",header=T,fileEncoding = "utf-8") 
head(dat10)
a=dat10$TF.IDF权重
b=dat10$特征词比例
c=dat10$人气
source("data_outline.R")
dat3 = dat0[,c(2,4,10)]
data_outline(a)
data_outline(b)
data_outline(c)


#下面研究分类的影响：根据类别对人气取平均值
library(dplyr)
#######################
a = summarise(group_by(dat1, category), mean(人气))[1]
b = summarise(group_by(dat1, category), mean(人气))[2]

#在对类别变量进行描述分析时，最常用的方式就是计算不同类别在数据中出现的频数
#，从而得到样本在水平间的分布。
#比如，本例中我们可以通过R中的table()函数计算频数从而得到样本在小说类型上的分布。
#从结果上来看，样本中一共包括了16种小说类型。其中“工具”、“社交”的数量最多，都超过了150个，而“体育”数量最少，仅为19个。 
#可以看出，不同类型的小程序在市场上的占有率差异较大，且大多集中在少数的几种类型。
#这在一定程度上也能反映出市场对不同类型小程序的需求差异。
table(dat1$category)

#描述分析--观察某个定量变量在不同类别上的分布
mycolors <- colorRampPalette(c(color1, color2))(16)  ## 设定绘图颜色
#boxplot(dat1$评价人数~dat1$category,col=mycolors)
#boxplot(dat1$评分~dat1$category,col=mycolors)
#boxplot(log(dat1$人气)~dat1$category,col=mycolors,xlab="小程序类别","log(人气)")
#fig3 = ggplot(data=dat1,aes(x=reorder(category, log(人气),mean),y=log(人气)))+  #按每一等级的平均价格由低到高进行排列
  geom_boxplot(varwidth = T,col="lightblue")  #绘制价格的对数对产品等级的分组箱线图
#fig3
cate = as.vector(unique(dat1$category))
plot_cate = data.frame(cate = cate,
                       cate_number = c(42,124,154,126,49,73,39,47,52,214,98,40,36,76,23,19),
                       popular_mean = c(35946,16622,11698,14918,14999,13297,16448,14797,11554,12472,11314,10583,11684,9982,15776,6942))

# 绘制人气与分类的柱状图，类别的样本个数是柱宽度
names <- as.vector(plot_cate$cate)  ## 将plot_cate的类别变量变为向量，命名为names
slices <- as.vector(plot_cate$popular_mean)  ## 将plot_cate的平均人气变为向量，命名为slices
tmp <- as.vector(plot_cate$cate_number)  ##将plot_cate的类别数量变量变为向量，命名为tmp
mycolors <- colorRampPalette(c(color1, color2))(16)  ## 设定绘图颜色
barplot(beside=TRUE, slices, names.arg = names, border= "transparent",col= mycolors, width = as.vector(plot_cate$cate_number),ylim = c(0,36000), cex.names = 0.98, ylab="人气均值", las = 2, cex.axis = 0.58)  ## 绘制条形图，并设定y轴标签、字体大小、坐标轴类型等参数
barplot(plot_cate[order(plot_cate[,3],decreasing=TRUE),][,3],names.arg=plot_cate[order(plot_cate[,3],decreasing=TRUE),][,1],beside=TRUE,border= "transparent",col= mycolors, width = as.vector(plot_cate$cate_number),ylim = c(0,36000), cex.names = 1.5, ylab="人气均值", las = 2, cex.axis = 0.9)
#在对类别变量进行描述分析时，最常用的方式就是计算不同类别在数据中出现的频数
#，从而得到样本在水平间的分布。
#从结果上来看，样本中一共包括了16种小说类型。出行类的平均人气最高，体育类的人气最低
#其中“工具”、“社交”的数量最多，都超过了150个，而“体育”数量最少，仅为19个。 
#可以看出，不同类型的小程序在市场上的占有率差异较大，且大多集中在少数的几种类型。
#这在一定程度上也能反映出市场对不同类型小程序的需求差异。
mycolors <- colorRampPalette(c(color1, color2))(16)
Class_order<-reorder(dat1$category,dat1$人气,median)
boxplot(dat1$人气~ Class_order,outline=F,las=2,col= mycolors)

#画人气前十
names = as.vector(dat1[1:10,1])
names
barplot(dat1$人气[1:10], names.arg = names,border= "transparent",col= mycolors,ylim = c(0,977582), cex.names = 1.0, ylab="人气值", las = 1, cex.axis = 1.0)
#画类别
counts <- table(dat1$category)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")


#attach(mtcars)
#par(mfrow=c(3,1))
# 绘制人气和发布时间散点图
#mycolors <- colorRampPalette(c(color1, color2))(26)  ## 设定颜色
time = rep(0,1212)
time[grep(pattern = "2016-12",dat0[,8])]="2016-12"
time[grep(pattern = "2017-01",dat0[,8])]="2017-01"
time[grep(pattern = "2017-02",dat0[,8])]="2017-02"
time[grep(pattern = "2017-03",dat0[,8])]="2017-03"
time[grep(pattern = "2017-04",dat0[,8])]="2017-04"
time[grep(pattern = "2017-05",dat0[,8])]="2017-05"
time[grep(pattern = "2017-06",dat0[,8])]="2017-06"
dat3=cbind(dat1,time)
head(time)
fig4 <- ggplot(dat3, aes(x = time, y = 人气)) +  ## 将王朝持续时间映射至x轴，皇帝年龄数据映射至y轴
  geom_point(size=5, fill = "yellow", colour = color2) +  ## 绘制散点图，并设定点的大小和颜色
  theme_bw()  ## 背景为空白
fig4 <- fig4 + xlab("发布时间") +  ## 设定x轴标签 
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig4 <- fig4 + ylab("人气") +  ## 设定y轴标签
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig4 <- fig4 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴数值字体大小、颜色、位置
fig4 <- fig4 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴数值字体大小、颜色、位置
fig4
#2017年1月份发布的人气值较高
#绘制人气和发布时间箱线图
boxplot(varwidth = T,log(人气)~time,data=travel_dat,col="lightblue",ylab="产品价格(对数变换)")
#绘制人气和评价人数散点图
fig5 <- ggplot(dat3, aes(x = 评价人数, y = 人气)) +  ## 将王朝持续时间映射至x轴，皇帝年龄数据映射至y轴
  geom_point(size=5, fill = "yellow", colour = color2) +  ## 绘制散点图，并设定点的大小和颜色
  theme_bw()  ## 背景为空白
fig5 <- fig5 + xlab("评价人数") +  ## 设定x轴标签 
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig5 <- fig5 + ylab("人气") +  ## 设定y轴标签
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig5 <- fig5 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴数值字体大小、颜色、位置
fig5 <- fig5 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴数值字体大小、颜色、位置
fig5

##绘制人气和评分散点图
fig6 <- ggplot(dat3, aes(x = 评分, y = 人气)) +  ## 将王朝持续时间映射至x轴，皇帝年龄数据映射至y轴
  geom_point(size=5, fill = "yellow", colour = color2) +  ## 绘制散点图，并设定点的大小和颜色
  theme_bw()  ## 背景为空白
fig6 <- fig6 + xlab("评分") +  ## 设定x轴标签 
  theme(axis.title.x = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig6 <- fig6 + ylab("人气") +  ## 设定y轴标签
  theme(axis.title.y = element_text(size = 20, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴标签字体大小、颜色、位置
fig6 <- fig6 + theme(axis.text.x = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定x轴数值字体大小、颜色、位置
fig6 <- fig6 + theme(axis.text.y = element_text(size = 15, color = "black", face = "plain", vjust = 0.5, hjust = 0.5))  ## 设定y轴数值字体大小、颜色、位置
fig6
#711个评分为0，是因为这711个的评价人数为0，占大约60%，表明小程序的评价还是较少的
#有评分的，4-5分的占大多数，表明小程序的口碑较好。
#attach(mtcars)
#par(mfrow=c(3,1))
