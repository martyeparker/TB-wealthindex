library(officer)
library(flextable)
library(magrittr)
library(survey)

load('data/india0506HH.rda')
D1 <- as.data.table(india0506HH)
load('data/india1516HH.rda')
D2 <- as.data.table(india1516HH)

names(D1); names(D2)

setdiff(names(D1), names(D2))
setdiff(names(D2), names(D1))
## nmz <- intersect(names(D2), names(D1))
## nmz <- c('year',nmz)
D1[,year:='2005 - 2006']
D2[,year:='2015 - 2016']
D2[,weighthh:=weight]

## ---------------------------

D1[,TB:=factor(c('No','Yes')[as.integer(TBhh>0)+1])]
D2[,TB:=factor(c('No','Yes')[as.integer(TBhh>0)+1])]
D1[,percenturban:=1e2*as.integer(urbanrural)%%2]   #indicator
D2[,percenturban:=1e2*as.integer(urbanrural)%%2]


## D1
## names(D2)

vrz <- c("TB","urbanrural",
         "HHsize","MalesinHH","FemalesinHH",
         "percentMaleHH","under15inHH","percentunder15inHH",
         "meanAge","sdAge",
         "educationHH","percenturban")

getMN <- function(dat,vr){
  who <- which(!is.na(dat[,vr,with=FALSE]))
  tmp <- svydesign(id=~cluster,
                   strata=~region+urbanrural,
                   weights=~weighthh,
                   data=dat[who])
  ans <- svymean(as.formula(paste0("~",vr)),tmp)
  ans <- c(coef(ans),SE(ans))
  array(ans,dim=c(1,2),dimnames=list(vr,c('mean','SE')))
}

## getMN(D1,'HHsize')                      #test
lanz <- lapply(vrz[-c(1,2)],function(x)getMN(D1,x))
A1 <- do.call('rbind',lanz)
## names(D2)
lanz <- lapply(vrz[-c(1,2)],function(x)getMN(D2,x))
A2 <- do.call('rbind',lanz)

## 
lanz <- lapply(vrz[-c(1,2)],function(x)getMN(D1[TB=='Yes'],x))
Y1 <- do.call('rbind',lanz)
## names(D2)
lanz <- lapply(vrz[-c(1,2)],function(x)getMN(D2[TB=='Yes'],x))
Y2 <- do.call('rbind',lanz)


ntb1 <- D1[,table(DHSwealthQ)]
ytb1 <- D1[TB=='Yes',table(DHSwealthQ)]
ntb2 <- D2[,table(DHSwealthQ)]
ytb2 <- D2[TB=='Yes',table(DHSwealthQ)]


DFN <- data.table(
  qty = c('N','poorest','poorer','middle','richer','richest'),
  all05 = c(sum(ntb1),ntb1),
  tb05 = c(sum(ytb1),ytb1),
  all15 = c(sum(ntb2),ntb2),
  tb15 = c(sum(ytb2),ytb2)
)

fmit <- function(x,y) paste0(sprintf("%.1f",x)," (",sprintf("%.2f",y),")")

fmit(A1[,1],A1[,2])


DFF <- data.table(
  qty = c(row.names(A1)),
  all05 = fmit(A1[,1],A1[,2]),
  tb05 = fmit(Y1[,1],Y1[,2]),
  all15 = fmit(A2[,1],A2[,2]),
  tb15 = fmit(Y2[,1],Y2[,2])
)

DFA <- rbind(DFN,DFF)


ft <- flextable(data = DFA) %>%
  theme_booktabs() %>%
  set_header_labels( qty = "",
                    ntb = "All households (05/06)",
                    ytb = "Households with TB (05/05)",
                    ntb2 = "All households (15/16)",
                    ytb2 = "Households with TB (15/15)"
                    ) %>%
  autofit()


read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "output/epitable3.docx")
