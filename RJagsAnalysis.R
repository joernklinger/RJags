# Condition, Ratio and Interaction
# data = read.delim("adultsFinal.csv",sep=";")
data = read.csv2("adultsFinal.csv")

library(rjags)

response = data$Imitation
participantlist = unique(data$Participant)
pnum = match(data$Participant,participantlist)
np=length(unique(pnum))

N = length(data[,1])

y = response


data$ConditionTransparent = 0
data$ConditionNeutral = 0

for (i in seq(1,dim(data)[1],1)) {

	if (data$Condition[i] == 'Transparent') {
		data$ConditionTransparent[i] = 1
	}

	if (data$Condition[i] == 'Semi') {
		data$ConditionNeutral[i] = 1
	}


}


conditionTransparent = data$ConditionTransparent
conditionNeutral = data$ConditionNeutral

ratio = as.vector(scale(data$Ratio))



# CONDITION, RATIO and INTERACTION


P = 6

bugsdata=list(P=P,N=N,y=y,pnum=pnum,np=np,conditionTransparent=conditionTransparent,conditionNeutral=conditionNeutral , ratio=ratio)

init=list( beta=c(0, 0, 0, 0, 0, 0))

modelstring = "

  model{

   for(i in 1 : N){

  	y[i] ~ dbin (p.bound[i], 1)
     	p.bound[i] <- max(0, min(1, p[i]))
     	logit(p[i]) <- Xbeta[i]
	 Xbeta[i] <- beta[1] + beta[2]*conditionNeutral[i] + beta[3]*conditionTransparent[i] + beta[4]*ratio[i] + beta[5]*ratio[i]*conditionNeutral[i] + beta[6]*ratio[i]*conditionTransparent[i] + a.child[pnum[i]]

     }

	for (z in 1:P){
  			     beta[z] ~ dnorm(0.0, 0.0001)

  	       }

  		  for (x in 1:np) {

  					a.child[x] ~ dnorm (0,tau.child.raw)

        			       }
  	 			   tau.child.raw <- pow(sigma.child.raw, -2)
  			           sigma.child.raw ~ dunif (0, 100)


   }"



model=jags.model(textConnection(modelstring), data=bugsdata, n.chains=3)

update(model,n.iter=5000)
# update(model,n.iter=500)
# dic.out = dic.samples(model=model,n.iter=25000,thin=1)
# dic.out = dic.samples(model=model,n.iter=2500,thin=1)

#print(dic.out)
output=coda.samples(model=model, variable.names=c("beta"),n.iter=5000,thin=1)
# output=coda.samples(model=model, variable.names=c("beta"),n.iter=2500,thin=1)
print(summary(output))


getpvals <- function(samp){

pars = names(samp[1,])
for(i in 1:length(samp[1,])){

if(mean(samp[,i]) > 0){

print(paste(pars[i], mean(as.numeric(sort(samp[,i]) < 0 ))))

}
else{

print(paste(pars[i], mean(as.numeric(sort(samp[,i]) > 0 ))))

}

}
}

getpvals(output[[1]])
