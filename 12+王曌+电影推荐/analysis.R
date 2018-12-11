library(openxlsx)
d<-read.xlsx("Final_Moive1.xlsx")
{d$budget<-as.numeric(d$budget)
  d$popularity<-as.numeric(d$popularity)
  d$year<-as.numeric(d$year)
  d$revenue<-as.numeric(d$revenue)
  d$runtime<-as.numeric(d$runtime)
  d$vote_average<-as.numeric(d$vote_average)
  d$vote_count<-as.numeric(d$vote_count)}
d<-d[-c(62,1445,1843,2375,2641,2897,3992),]
d1<-d[,2:12]
library(dplyr)
d1<-subset(d1,vote_count<(median(vote_count)+1.5*IQR(vote_count)))

#genre
genres<-strsplit(d1$genres,"\\|")
genres0<-unlist(genres)
sort(table(genres0),decreasing = T)
tab_g<-table(genres0)
genres_high<-names(tab_g[tab_g>500])

{int_Drama<-grep("Drama",d1$genres)
int_Comedy<-grep("Comedy",d1$genres)
int_Thrille<-grep("Thriller",d1$genres)
int_Action<-grep("Action",d1$genres)
int_Romance<-grep("Romance",d1$genres)
int_Adventure<-grep("Adventure",d1$genres)
int_Crime<-grep("Crime",d1$genres)
int_Fiction<-grep("Fiction",d1$genres)
int_Horror<-grep("Horror",d1$genres)
int_Family<-grep("Family",d1$genres)}

d1$Drama<-d1$Comedy<-d1$Thrille<-d1$Action<-d1$Romance<-d1$Adventure<-d1$Crime<-d1$Fiction<-d1$Horror<-d1$Family<-0
{d1$Drama[int_Drama]<-1
d1$Comedy[int_Comedy]<-1
d1$Action[int_Action]<-1
d1$Thrille[int_Thrille]<-1
d1$Adventure[int_Adventure]<-1
d1$Romance[int_Romance]<-1
d1$Family[int_Family]<-1
d1$Horror[int_Horror]<-1
d1$Fiction[int_Fiction]<-1
d1$Crime[int_Crime]<-1}

#actor
actor<-strsplit(d1$actor,"\\|")
actor0<-unlist(actor)
tab_a<-table(actor0)
head(sort(table(actor0),decreasing = T),50)
actor_high<-names(tab_a[tab_a>31])
int_actor<-grep("Bruce Willis|Matt Damon|Nicolas Cage|Robert De Niro|Samuel L. Jackson",d1$actor)
d1$actor_h<-0
d1$actor_h[int_actor]<-1

{int_Robert.De.Niro<-grep("Robert De Niro",d1$actor)
int_Bruce.Willis<-grep("Bruce Willis",d1$actor)
int_Matt.Damon<-grep("Matt Damon",d1$actor)
int_Samuel.L.Jackson<-grep(" Samuel L. Jackson",d1$actor)
int_Nicolas.Cage<-grep("Nicolas Cage",d1$actor)}

d1$Robert.De.Niro<-d1$Bruce.Willis<-d1$Matt.Damon<-d1$Samuel.L.Jackson<-d1$Nicolas.Cage<-0
d1$Robert.De.Niro[int_Bruce.Willis]<-1
d1$Bruce.Willis[int_Bruce.Willis]<-1
d1$Matt.Damon[int_Matt.Damon]<-1
d1$Samuel.L.Jackson[int_Samuel.L.Jackson]<-1
d1$Nicolas.Cage[int_Nicolas.Cage]<-1

#director
director<-strsplit(d1$director,"\\|")
director0<-unlist(director)
head(sort(table(director0),decreasing = T),50)
int_director<-grep("Steven Spielberg|Woody Allen|Martin Scorsese|Clint Eastwood|Robert Rodriguez|Ridley Scott|Spike Lee|Renny Harlin|Steven Soderbergh|Oliver Stone|Tim Burton",d1$director)
d1$director_h<-0
d1$director_h[int_director]<-1

d2<-d1[,c(1:7,12:23)]
fit<-lm(vote_count ~.-vote_count,data=d2)

#split train and test
set.seed(111)
train<-sample(1:nrow(d2),0.8*nrow(d2))
test<-(-train)

#grid
#genres0
set.seed(111)
tuned<-tune.svm(vote_count~.,data=d2[train,],type="eps-regression",cost=4^(-1:3))
tuned
svr1<-svm(vote_count~.,data=d2[train,],type="eps-regression",cost=4)
eva <- predict(svr1, d2[train,-7])
e.eva<-mean((eva-d2[train,7])^2,na.rm = T)
e.eva
pred<-predict(svr1, d2[test,-7])
e.pred<-mean((pred-d2[test,7])^2,na.rm = T)
e.pred
plot(eva,d2[train,7])
plot(pred,d2[test,7])

#genres(pca)
d3<-cbind(d2,genres[,1:9])
d3<-d3[,c(1:7,18:28)]
set.seed(111)
tuned<-tune.svm(vote_count~.,data=d3[train,],type="eps-regression",cost=4^(-1:3))
svr2<-svm(vote_count~.,data=d3[train,],type="eps-regression",cost=1)
eva <- predict(svr2, d3[train,-7])
e.eva<-mean((eva-d3[train,7])^2,na.rm = T)
e.eva
pred<-predict(svr2, d3[test,-7])
e.pred<-mean((pred-d3[test,7])^2,na.rm = T)
e.pred

#xgboost
#
library(xgboost)
xg.m<-xgboost(data = data.matrix(d2[train,-7]),label = d2$vote_count[train],min_child_weight=2,nrounds = 200,eta=0.2)
eva<-predict(xg.m,data.matrix(d2[train,-7]))
e.eva<-mean((eva-d2[train,7])^2,na.rm = T)
e.eva
pred<-predict(xg.m, data.matrix(d2[test,-7]))
e.pred<-mean((pred-d2[test,7])^2,na.rm = T)
e.pred
set.seed(111)
cv<-xgb.cv(data = data.matrix(d2[train,-7]),min_child_weight=2,max_depth=6,label = d2$vote_count[train],nfold = 10,nrounds = 200,eta=0.2,lambda=0.5)
cv
print(cv,verbose = T)
importance_matrix <- xgb.importance(colnames(data.matrix(d2[train,-7])), model = xg.m)
xgb.plot.importance(importance_matrix)
#pca
xg.m<-xgboost(data = data.matrix(d3[train,-7]),label = d2$vote_count[train],min_child_weight=2,nrounds = 100)
eva<-predict(xg.m,data.matrix(d3[train,-7]))
e.eva<-mean((eva-d3[train,7])^2,na.rm = T)
e.eva
pred<-predict(xg.m, data.matrix(d3[test,-7]))
e.pred<-mean((pred-d3[test,7])^2,na.rm = T)
e.pred
set.seed(111)
cv<-xgb.cv(data = data.matrix(d3[train,-7]),min_child_weight=2,max_depth=6,label = d2$vote_count[train],nfold = 10,nrounds = 120)
cv
print(cv,verbose = T)
importance_matrix <- xgb.importance(colnames(data.matrix(d3[train,-7])), model = xg.m)
xgb.plot.importance(importance_matrix)
