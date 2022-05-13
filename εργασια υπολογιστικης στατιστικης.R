#1o erothma me akrivi elegxo tyxaiopoihshs
#ftiaxno dio dianismata A kai B me tixaia arithmimena mpoukalia krasiou
A<-c(1,2,3)
B<-c(4,5)
#ftiaxno ena dianisma poy na enonei ta prohgoymena dianismata
joint<-c(A,B)
#vrisko toys pithanoys syndyasmoys toy joint gia tis epitixies
choose(5,3)
library(gtools)
comb<-combinations(5,3,v=joint,set=T,repeats.allowed =F)
comb
#vrisko th mesh timh toys
apply(comb,1,mean)  
#vrisko toys pithanoys syndyasmoys apotyxion
comb2<-combinations(5,2,v=joint,set=T,repeats.allowed =F)
comb2
#paraathro oti gia na tairiaksoyn oi syndyasmoy epityxion kai apotyxion
#prepei na antistrafoyn oi grammes toy deyteroy pinaka
comb2b<-matrix(NA,nrow=10,2)
for (i in 0:9){
  comb2b[i+1,]<-comb2[10-i,]
}
comb2b
#vrisko tis meses times ton stoixeion toy 2oy pinaka
apply(comb2b,1,mean)
#vrisko thn akrivi elegxosynarthsh T=|mean(a)-mean(b)|
Tobs<-abs(apply(comb,1,mean)-apply(comb2b,1,mean))
Tobs
#stroggylopoio me 2 dekadika
Tobs<-round(Tobs,2)
#parathro oti to t.test moy dinei diasthma empistosynhs mexri to 1.8
#afoy to tods einai diakrito, psaxno tis times poy einai 
#megalyteres apo 1.67 gia na vro to p-value
t.test(Tobs)
#vrisko thn p-value gia tobs=2.5 
pvalue<-length(which(Tobs>=2.5))/length(Tobs)
pvalue
#h p-value einai megalyterh apo 0.05 synepos den mporo na aporripso 
#thn arxikh ypothesh h opoia einai oti "einai eidikos"

#2o erothma me proseggistiko elegxo tyxaiopoihshs
C<-c(1,2,3,4)
D<-c(5,6,7,8,9,10)
samp<-c(C,D)
choose(10,4)
library(gtools)
#parathro oti mporo na exv 210 syndyasmoys
#thelo na krathso mono toys 99 
comb<-combinations(10,4,v=samp,set=T,repeats.allowed = F)
comb
comb2<-combinations(10,6,v=samp,set=T,repeats.allowed = F)
comb2
#kano toys 2 pinakes symplhromatikoys metaksi toys os pros to deigma mas
comb2b<-matrix(NA,nrow=210,6)
for (i in 0:209){
  for (j in 1:6){
  comb2b[i+1,j]<-comb2[210-i,j]}
}
comb2b
data<-list(comb,comb2b)
b<-999
t<-rep(NA,b)
t2<-t
for(i in 1:b){
  newdata<-sample(1:210,1)
  t[i]<-abs(mean(comb[newdata])-mean(comb2b[newdata]))
  t2<-sum(comb[newdata])
}
tobs<-mean(t)
tobs2<-mean(t2)

print((sum(t>=tobs)+1)/(b+1))
print((sum(t2>=tobs2)+1)/(b+1))

















#pairno ena tyxaio deigma megethous 99 apo toys 210 syndyasmoys
K<-sample(c(1:210),99)
K
#ftiaxno 2 pinakes poy periexoyn mono 99 syndiasmoys kai ta symplhromata toys
comb1a<-matrix(NA,nrow=99,4)
comb2c<-matrix(NA,nrow=99,6)
for(i in 1:99){
  comb1a[i,]<-comb[K[i],]
  comb2c[i,]<-comb2b[K[i],]
}
comb1a
comb2c
#brisko tis meses times
apply(comb1a,1,mean)
apply(comb2c,1,mean)
Tobs2<-abs(apply(comb1a,1,mean)-apply(comb2c,1,mean))
Tobs2<-round(Tobs2,2)
Tobs2
#kano t.test gia na vro to diasthma empistosinhs
t.test(Tobs2)
#to ano akro toy diasthmatos einai to 1.842, opote psaxno ta tobs
#poy einai megalytera apo ayto
pvalue2<-length(which(Tobs2>=1.85))/length(Tobs2)
pvalue2
#an to p value vgainei megalytero apo 0.05 den mporo na aporripso thn arxikh 
#ypothesh h opoia einai oti o dokimasths einai eidikos

#3o epanalhpsh 100 fores
library(gtools)
pvalue3<-numeric(100)
#theto thn pvalue os dianisma gia na sigkrino ta apotelesmata
for (k in 1:100){
  samp<-c(1:10)
  comb<-combinations(10,4,v=samp,set=T,repeats.allowed = F)
  comb2<-combinations(10,6,v=samp,set=T,repeats.allowed = F)
  comb2b<-matrix(NA,nrow=210,6)
  for (i in 0:209){
    for (j in 1:6){
      comb2b[i+1,j]<-comb2[210-i,j]}
  }
  K<-sample(c(1:210),99)
  comb1a<-matrix(NA,nrow=99,4)
  comb2c<-matrix(NA,nrow=99,6)
  for(i in 1:99){
    comb1a[i,]<-comb[K[i],]
    comb2c[i,]<-comb2b[K[i],]
  }
  apply(comb1a,1,mean)
  apply(comb2c,1,mean)
  Tobs2<-abs(apply(comb1a,1,mean)-apply(comb2c,1,mean))
  Tobs2<-round(Tobs2,2)
  y<-t.test(Tobs2)
  #theto thn y lista gia na mporeso na paro 
  #to pano akro toy dianismatos me thn max
  y<-as.list(y)
  x<-max(y$conf.int)
  pvalue3[k]<-length(which(Tobs2>x))/length(Tobs2)
}
pvalue3
#parathro oti ta pvalue einai megalytera apo 0.05
#den plisiazoyn to 1 oste na eimaste sigoyroi gia thn arxikh ypothesh alla oyte 
#kai to 0 gia na thn aporripsoyme
#ta p value einai peripoy oso h pithanothta epityxias toy dokimasth 
#yparxei periptosh na exei parthei lathos elegxosynarthsh 

