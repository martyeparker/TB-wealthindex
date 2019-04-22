
library(cowplot)
library(data.table)
library(scales)
library(survey)

options(survey.lonely.psu="adjust")     #needed as stratum w/1 PSU


## TODO TB 0/1 else NA

load('data/india0506HH.rda')
D1 <- as.data.table(india0506HH)
## D <- fread('india0506HH.csv')
load('data/india1516HH.rda')
D2 <- as.data.table(india1516HH)
names(D1); names(D2)

setdiff(names(D1), names(D2))
setdiff(names(D2), names(D1))
D2[,weighthh:=weight]

nmz <- intersect(names(D2), names(D1))
nmz <- c('year',nmz)

D1[,year:='2005 - 2006']
D2[,year:='2015 - 2016']
DB <- rbind(D1[,nmz,with=FALSE],D2[,nmz,with=FALSE])
DB <- DB[TBhh %in% 0:1]
DB[,TB:=factor(c('No','Yes')[TBhh+1])]  #TODO correct


D1[,.(mean(PC1xDURABLE,na.rm = TRUE),mean(PC1xLessDURABLE,na.rm = TRUE))]
D2[,.(mean(PC1xDURABLE,na.rm = TRUE),mean(PC1xLessDURABLE,na.rm = TRUE))]



## ---------- explicit models ----------
DBR <- DB[!is.na(TB)]
DBR <- DB[!is.na(PC1xLessDURABLE)]
DBR <- DB[!is.na(PC1xDURABLE)]

## use svyglm??

## with year!
mod1 <- lm(PC1xLessDURABLE ~ (PC1xDURABLE + TB +
             HHsize + percentunder15inHH + meanAge + percentMaleHH)*year,
           data=DBR)
mod2 <- lm(PC1xLessDURABLE ~
             PC1xDURABLE * year * (TB +
                            HHsize + percentunder15inHH + meanAge + percentMaleHH
             ),data=DBR)
mod3 <- lm(PC1xLessDURABLE ~ (poly(PC1xDURABLE,2) + TB)*year,data=DBR)
mod4 <- lm(PC1xLessDURABLE ~ poly(PC1xDURABLE,2) * TB * year,data=DBR)
mod5 <- lm(PC1xLessDURABLE/PC1xDURABLE ~ ( TB +
             HHsize + percentunder15inHH + meanAge + percentMaleHH)*year,
           data=DBR)
mod6 <- lm(PC1xLessDURABLE ~ (poly(PC1xDURABLE,2)+HHsize + percentunder15inHH + meanAge + percentMaleHH) * TB * year,data=DBR)


y1 <- predict(mod1,newdata=DBR)
y2 <- predict(mod2,newdata=DBR)
y3 <- predict(mod3,newdata=DBR)
y4 <- predict(mod4,newdata=DBR)
y6 <- predict(mod6,newdata=DBR)
y5 <- predict(mod5,newdata=DBR)*DBR$PC1xDURABLE
DBR[,c('y1','y2','y3','y4','y5','y6'):=list(y1,y2,y3,y4,y5,y6)]


## for lines
xz <- seq(from=-5,to=5,by=.1)
DBRM <- DBR[,.(HHsize=mean(HHsize,na.rm=TRUE),
               percentunder15inHH=mean(percentunder15inHH,na.rm=TRUE),
               meanAge=mean(meanAge,na.rm=TRUE),
               percentMaleHH=mean(percentMaleHH,na.rm=TRUE)),by=year]
DBRM <- DBRM[rep(1:2,each=length(xz))]
DBRM[,PC1xLessDURABLE:=xz]
DBRM[,PC1xDURABLE:=xz]
DBRM <- DBRM[rep(1:nrow(DBRM),2)]
DBRM[,TB:=rep(c('Yes','No'),each=nrow(DBRM)/2)]
y1 <- predict(mod1,newdata=DBRM)
y2 <- predict(mod2,newdata=DBRM)
y3 <- predict(mod3,newdata=DBRM)
y4 <- predict(mod4,newdata=DBRM)
y6 <- predict(mod6,newdata=DBRM)
DBRM[,c('y1','y2','y3','y4','y6'):=list(y1,y2,y3,y4,y6)]



(ii <- AIC(mod1,mod2,mod3,mod4,mod5,mod6))
str(ii)
which.min(ii$AIC)
## which.min(ii$BIC)


LDplot <- ggplot(DBR,aes(PC1xDURABLE,PC1xLessDURABLE,col=TB,lty=TB)) +
   geom_density_2d(h=c(2,2)*1.5,n=60) + 
   scale_color_manual(values=c('black','red'))+
   facet_wrap(~year) + 
   ylim(c(-6,6.5)) +
   xlim(c(-5.5,4.5))+
   xlab('Household durable asset score') +
   ylab('Household less durable asset score')


LDplot2f <- LDplot + geom_line(data=DBRM,aes(PC1xDURABLE,y6,col=TB,lty=TB),lwd=1)
save_plot("outputs/LDplot2f.pdf",
          LDplot2f,
          base_height = 5,
          base_aspect_ratio = 1.5
          )

## need a prediction for DHS based on LD
dhslvls <- c("poorest",
             "poorer",
             "middle",
             "richer",
             "richest")

DBR[year=="2015 - 2016",DHSwealthQ:=dhslvls[as.integer(as.character(DHSwealthQ))]]
DBR[,DHSwealthQ:=factor(DHSwealthQ,levels = dhslvls, ordered=TRUE)]

pm <- lm(data=DBR,DHSwealth ~ (PC1xLessDURABLE + PC1xDURABLE)*year)
DBR[,dhsp:=predict(pm,data=DBR)]
DP <- DBR[,qplot(DHSwealth,dhsp)] + ylab('Prediction') + geom_abline(intercept=0,slope=1,col=2)

save_plot("outputs/DHSpred.png",
          DP,
          base_height = 5,
          base_aspect_ratio = 1.0
          )


## ============

## calculate the same prevalence graph
getPrev1 <- function(dat){
  tmp <- svydesign(id=~cluster,
                   strata=~region+urbanrural,
                   weights=~weighthh,
                   data=dat)
  ans <- svyciprop(~TBhh,tmp)
  ans <- c(as.numeric(ans),attr(ans, "ci"))
  list(mid=ans[1],lo=ans[2],hi=ans[3])
}

v1 <- v2 <- c()
for(i in 1:100){
  print(i)
  ## fake scatter
  DBRF <- copy(DBR)                          #fake data
  (ntb <- DBRF[,sum(TB=='Yes')])
  (Ntb <- nrow(DBRF))
  ntb/Ntb*1e5
  DBRF[,table(DHSwealthQ)]
  (ntby <- DBRF[,sum(TB=='Yes'),by=year])

  ## set TB all No
  DBRF[,TB:=rep('No',nrow(DBRF))]
  ## set sample Yes based on weight
  ## 05/06
  DBRF[,tmpw:=1.0] ## DBRF[,tmpw:=weighthh]
  DBRF[year=="2015 - 2016",tmpw:=0]
  DBRF[sample(x=nrow(DBRF),size=as.integer(ntby[1,2]),prob=tmpw),TB:='Yes']
  ## 15/16
  DBRF[,tmpw:=1.0] ## DBRF[,tmpw:=weighthh]
  DBRF[year=="2005 - 2006",tmpw:=0]
  DBRF[sample(x=nrow(DBRF),size=as.integer(ntby[2,2]),prob=tmpw),TB:='Yes']
  DBRF[,.N,by=.(year,TB)]

  ## predict new LD (needs y6)
  y6 <- predict(mod6,newdata=DBRF)
  DBRF[,PC1xLessDURABLE:=y6]

  ## predict new DHS
  DBRF[,dhsp:=predict(pm,data=DBRF)]

  ## relabel quartile
  tmp1 <- svydesign(id=~cluster,
                    strata=~region+urbanrural,
                    weights=~weighthh,
                    data=DBRF[year=="2005 - 2006"])
  q1 <- svyquantile(x=DBRF[year=="2005 - 2006",dhsp],
                    design = tmp1,quantiles = c(0,.2,.4,.6,.8,1))
  tmp2 <- svydesign(id=~cluster,
                    strata=~region+urbanrural,
                    weights=~weighthh,
                    data=DBRF[year=="2015 - 2016"])
  q2 <- svyquantile(x=DBRF[year=="2015 - 2016",dhsp],
                    design = tmp2,quantiles = c(0,.2,.4,.6,.8,1))


  DBRF[year=="2015 - 2016",DHSwealthQ:=cut(dhsp,breaks=q2,labels=dhslvls)]
  DBRF[year=="2005 - 2006",DHSwealthQ:=cut(dhsp,breaks=q1,labels=dhslvls)]
  DBRF[,DHSwealthQ:=factor(DHSwealthQ,levels = dhslvls, ordered=TRUE)]
  DBRF <- DBRF[!is.na(DHSwealthQ)]
  DBRF[,TBhh:=0]
  DBRF[TB=='Yes',TBhh:=1]
  ## data
  tmpA <- DBRF[,.(TB=getPrev1(.SD)),by=.(DHSwealthQ,year)]
  tmpA[,TB:=unlist(TB)]
  tmpA[,variable:=c('mid','lo','hi')]      #bit frail
  tmpA <- dcast(tmpA,year + DHSwealthQ ~ variable,value.var = 'TB')
  save(tmpA,file=paste0('tmp/',i,'.Rdata'))

  (w1 <- ineq::ineq(tmpA[year=="2005 - 2006",mid],type="Gini"))
  (w2 <- ineq::ineq(tmpA[year=="2015 - 2016",mid],type="Gini"))
  v1 <- c(v1,w1)
  v2 <- c(v2,w2)
}

print(v1)
print(v2)
print(mean(v1)); print(sd(v1)/(10));
## 0.02906063
## 0.001077813 
print(mean(v2)); print(sd(v2)/(10));
## 0.01668227
## 0.000587609

c(mean(v1),mean(v1) - sd(v1)/5,mean(v1) + sd(v1)/5)*1e2
## 2.906063 2.690500 3.121625
c(mean(v2),mean(v2) - sd(v2)/5,mean(v2) + sd(v2)/5)*1e2
## 1.668227 1.550705 1.785749

1e2*ineq::ineq(tmpB[year=="2005 - 2006",mid],type="Gini") #23.54492
1e2*ineq::ineq(tmpB[year=="2015 - 2016",mid],type="Gini") #23.90569


save(v1,file='data/v1.Rdata')
save(v2,file='data/v2.Rdata')

tmpB <- DBR[,.(TB=getPrev1(.SD)),by=.(DHSwealthQ,year)]
tmpB[,TB:=unlist(TB)]
tmpB[,variable:=c('mid','lo','hi')]      #bit frail
tmpB <- dcast(tmpB,year + DHSwealthQ ~ variable,value.var = 'TB')

save(tmpA,file='data/tmpA.Rdata')
save(tmpB,file='data/tmpB.Rdata')

pd <- position_dodge(width = 0.9)
PP2 <- ggplot(tmpB,aes(x=DHSwealthQ,y=mid*1e5,ymin=lo*1e5,ymax=hi*1e5,fill=year)) +
  geom_bar(position=pd,stat='identity') +
  geom_errorbar(position=position_dodge(.9),col=2,width=0) +
  scale_y_continuous(label=comma) +
  scale_fill_grey() + 
  ylab('Household prevalence of self-reported\n tuberculosis (per 100,000)') +
  xlab('DHS wealth quintile')
PP2

save_plot("outputs/PrevPlotE1.pdf",
          PP2,
          base_height = 5,
          base_aspect_ratio = 1.2
          )


PP2 <- PP2 + geom_line(data=tmpA,aes(group=year,lty=year),col='#FF00FF')
PP2

save_plot("outputs/PrevPlotE2.pdf",
          PP2,
          base_height = 5,
          base_aspect_ratio = 1.2
          )

##
load('data/tmpA.Rdata')
load('data/tmpB.Rdata')



## ineq::ineq(tmpA[year=="2005 - 2006",mid],type="Gini")
## ineq::ineq(tmpA[year=="2015 - 2016",mid],type="Gini")
## ineq::ineq(tmpB[year=="2005 - 2006",mid],type="Gini")
## ineq::ineq(tmpB[year=="2015 - 2016",mid],type="Gini")



