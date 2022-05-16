

data<-c(0.18,0.89,0.61,0.23,0.16,0.37,0.29,0.03,
        0.05,0.47,0.24,1.17)

m0<-1
mean(data)
n<-length(data)
tobs<- abs((mean(data) - m0))
tobs
B<-99
t<-rep(NA,B)

for (i in 1:B) {
  x<-rexp(n,m0)
  t[i]<- abs((mean(x) - m0))
}
t
pvalue<- (sum(t>=tobs)+1)/(B+1)
pvalue



data<-c(0,0,0,0,0,0,1,1,1,2,2,2,2,3,3,4)
tobs<- var(data)/mean(data)
tobs

k<-99
t<-rep(NA,k)

for (i in 1:k){
  newdata<- rpois(16,1.3125)
  t[i]<- var(newdata)/mean(newdata)
}
newdata
t
pvalue<-(sum(t>=tobs)+1)/(k+1)
pvalue


b<-1000
th1<-numeric(b)
th2<-th1
for(i in 1:b){
  samp<-rnorm(200,1,1)
  th1[i]<-mean(samp)
  th2[i]<-var(samp)
}
sum((th1-mean(th1))*(th2-mean(th2)))/b


bot<-function(t1,t2,b){
  x<-(1/b)*(sum((k-t1)*(k-t2)))
  print(x)
}

k<-rnorm(20,1,1)
t1<-(1/20)*(sum(k))
t1
t3<-numeric(20)
for(i in 1:20){
  t3[i]<-(1/20)*((k[i])^2-20*t1^2)
}
t3
t2

t2<-sum(t3)
bot(t1,t2,20)

a<-c(67.5, 65.6 ,65.7,59.3,39.8,76.1,73.6,81.6, 75.5,80.3, 54.5,79.1,94.0,80.3,89.6,44.7,82.7,89.7,83.6,84.9,76.3,74.7,68.8,79.3)
b<-c(13,19, 18,12, 20,5,1, 1,2,3, 6, 5,4,8, 1, 3,18,13, 2,2,12,17,26,6)


participation<-c(67.5,65.6,65.7,59.3,39.8,76.1,73.6,81.6,75.5,80.3,54.5,79.1,94,80.3,89.6,44.7,82.7,89.7,83.6,84.9,76.3,74.7,68.8,79.3)
difference<-c(13,19,18,12,20,5,1,1,2,3,6,5,4,8,1,3,18,13,2,2,12,17,26,6)
mean(a)
mean(b)
thi<-(sum((a-mean(a))*(b-mean(b)))/((sum((a-mean(a))^2)*(sum((b-mean(b))^2))))^(1/2))
thi

B<-1000
test<- -cor(a,b)
test
B<-99999
t<-rep(NA,B)
for ( i in 1:B) {
  sampledata<-sample(participation)
  t[i]<-  -cor(sampledata,difference)
}
sampledata
t
pvalue<-(sum(t>=test)+1)/(B+1)
pvalue

CI.p<-c(pvalue-1.96*sqrt(pvalue*(1-pvalue)/B),pvalue+1.96*sqrt(pvalue*(1-pvalue)/B))




x1<-c(64,60,71,61,54,77,81,93,93,51,76,96,77,93,95,54,168,99)
n<-numeric(18)
for(i in 1:18){
x<-sum(x1)-x1[i]
n[i]<-(sum(x1)-x)^2
}
cv<-(1/18)*sum(n)
cv
