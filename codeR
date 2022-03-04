
library('MASS')
library(glmnet)
library(corrplot)

data_train<-read.csv("data_train.csv",sep=",")
data_test<-read.csv("data_test.csv",sep=",")

##############################################
#            PRETRAITEMENT                   #  
##############################################

####### Train ##########

#Liste des variables qui ne sont pas factor
factor<-sapply(data_train,is.factor)
namesnotfactor<-names(factor[factor==FALSE])
namesnotfactor

var_avecNA<-c()
for (i in namesnotfactor) {
  if (sum(is.na(data_train[,i]))>0) {
    var_avecNA<-c(var_avecNA,i)
  }
}

####REMPLIR LES DONNEES MANQUANTES

#On trouve d'abord quelles sont les observations avec des valeurs manquantes
obs_NA<-c()
NA_parvariable<-list()
for (i in var_avecNA) {
  obs_NA<-union(which(is.na(data_train[,i])),obs_NA)
  NA_parvariable[[i]]<-which(is.na(data_train[,i]))
}    #obs_NA contient donc les numeros d'observations qui ont des valeurs manquantes, et NA_parvariable est une liste contenant un vecteur des observations qui ont des valeurs manquantes

#On obtient combien de chaque variable on a des NA
combien_NA<-c()
for (k in var_avecNA) {
  combien_NA[k]<-length(NA_parvariable[[k]])
}
combien_NA

###METHODE 1: regression lineaire pour predire les valeurs

#Fonctions auxiliaires
writemodel<-function(apredire,variables) {
  return(formula(paste(apredire,"~",paste(variables,collapse="+"))))
}
#La fonction suivante remplira les valeurs NA de la base 'data' en faisant une regression par rapport aux autres variables, utilisant les observations qui n'ont pas de NAs dans les variables significatives
predict_NA<-function(data,var_predire) {
  model<-writemodel(var_predire,setdiff(colnames(data),c("charges","id")))
  forwardmodel<-stepAIC(lm(model,data=na.omit(data)),direction="backward",na.action="omit")
  
  #Predictions
  data<-cbind(data,IDOBS=1:dim(data)[1])  
  var_gardees<-attr(forwardmodel$terms,"term.labels")
  
  #Enlever les observations que l'on ne peut pas predire  
  data_pour_predire<-na.omit(data[is.na(data[,var_predire]),(colnames(data) %in% c(var_gardees,"IDOBS") )]) #Les donnees avec NA dans la variable mais sans NA dans les variables qui nous servent pour predire
  
  #Obtenir variables qui sont factor
  types<-sapply(data_pour_predire,class)
  factors<-names(types[types=="factor"])
  
  #On va verifier si les observations a predire ont les 'levels' que l'on peut predire. (Il a fallu faire ca car parfois on ne pouvait pas predire a cause d'un nouvel 'level' dans une observation nouvelle)
  if (length(factors)>0) {
    predictable<-c()
    for (i in 1:nrow(data_pour_predire)) {
      predictable[i]<-prod((as.matrix(data_pour_predire[i,factors]) %in% unlist(forwardmodel$xlevels)))
    }
    data_pour_predire<-data_pour_predire[as.logical(predictable),]
  }
  #On predit les valeurs manquantes  
  predictions<-predict(forwardmodel,newdata=data_pour_predire)
  print("On a predit"); print(length(predictions)); print("nouvelles observations")
  data[data$IDOBS %in% data_pour_predire$IDOBS,var_predire]<-predictions  
  data<-data[,!(colnames(data) %in% c("IDOBS"))]
  return(data)
}


#Effectuer la prediction pour toutes les variables manquantes
data_NApred<-data_train
for (variable in var_avecNA) {
  data_NApred<-predict_NA(data_NApred,variable)
}

#Attention: avec la methode d'au dessus on ne peut predire que les observations qui n'ont qu'une seule valeur NA dans les variables representatives. Il nous en resteront quelques unes sans pouvoir les predire
#Regarder combien d'observations nous restent
for (i in names(data_NApred)) {
  if (sum(is.na(data_NApred[,i]))>0) {
    print(i)
    print(sum(is.na(data_NApred[,i])))
  }
}
dim(na.omit(data_NApred))

#Finalement: remplacer les obs que l'on ne peut pas predire par les valeurs qu'elle a donne
fillNA<-data.frame(alb=3.5,pafi=333.3,bili=1.01,crea=1.01,bun=6.51,wblc=9,urine=2502)
for (i in names(fillNA)) {
  data_NApred[is.na(data_NApred[,i]),i]<-fillNA[,i]
}

#Enlever donnees avec des valeurs manquantes
data_NApred<-na.omit(data_NApred)

#On choisit data_NApred comme l'ensemble d'apprentissage (outil pour predire)
#(ne pas executer cette ligne si on ne veut pas choisir cette base la)
data_train<-data_NApred



###METHODE 2: remplacer directement (sans predire) par les valeurs qu'elle a donne
#On remplit le reste des donnees avec cette methode
fillNA<-data.frame(alb=3.5,pafi=333.3,bili=1.01,crea=1.01,bun=6.51,wblc=9,urine=2502)
for (i in names(fillNA)) {
  data_train[is.na(data_train[,i]),i]<-fillNA[,i]
}

#Enlever donnees avec des valeurs manquantes (s'il en reste)
data_train<-na.omit(data_train)

###METHODE 3: remplacer par les moyennes de chaque variable

fillNA=colMeans(data_train[,var_avecNA],na.rm=T)
for(var in var_avecNA){
	data_train[is.na(data_train[,var]),var]<-fillNA[var]
}

data_train[data_train$urine<0,"urine"]<-0



######## Test ############

#Liste des variables qui ne sont pas factor
factor_test<-sapply(data_test,is.factor)
namesnotfactor_test<-names(factor_test[factor_test==FALSE])
namesnotfactor_test

var_avecNA_test<-c()
for (i in namesnotfactor_test) {
  if (sum(is.na(data_test[,i]))>0) {
    var_avecNA_test<-c(var_avecNA_test,i)
  }
}

#On trouve d'abord quelles sont les observations avec des valeurs manquantes
obs_NA_test<-c()
NA_parvariable_test<-list()
for (i in var_avecNA_test) {
  obs_NA_test<-union(which(is.na(data_test[,i])),obs_NA_test)
  NA_parvariable_test[[i]]<-which(is.na(data_test[,i]))
}    #obs_NA contient donc les numeros d'observations qui ont des valeurs manquantes, et NA_parvariable est une liste contenant un vecteur des observations qui ont des valeurs manquantes

#On obtient combien de chaque variable on a des NA
combien_NA_test<-c()
for (k in var_avecNA_test) {
  combien_NA_test[k]<-length(NA_parvariable_test[[k]])
}
combien_NA_test

##METHODE 1: Effectuer la prediction pour toutes les variables manquantes
test_NApred<-data_test
for (variable in var_avecNA) {
  test_NApred<-predict_NA(test_NApred,variable)
}

#Attention: avec la methode d'au dessus on ne peut predire que les observations qui n'ont qu'une seule valeur NA dans les variables representatives. Il nous en resteront quelques unes sans pouvoir les predire
#Regarder combien d'observations nous restent
for (i in names(test_NApred)) {
  if (sum(is.na(test_NApred[,i]))>0) {
    print(i)
    print(sum(is.na(test_NApred[,i])))
  }
}
dim(na.omit(test_NApred))

#Choisir train_NApred comme la base a utiliser (ne pas executer cette ligne si on ne veut pas choisir cette base la)
data_test<-test_NApred

##METHODE 2:Remplir donnees manquantes
fillNA<-data.frame(alb=3.5,pafi=333.3,bili=1.01,crea=1.01,bun=6.51,wblc=9,urine=2502)
for (i in names(fillNA)) {
  data_test[is.na(data_test[,i]),i]<-fillNA[,i]
}

#Regarder les variables encore avec des valeurs manquantes
for (i in names(data_test)) {
  print(i)
  print(sum(is.na(data_test[,i])))
}

#Predire avtisst (on n'a pas de remplacement et il ne nous reste qu'une observation)
data_test<-predict_NA(data_test,"avtisst")

###METHODE 3: remplacer par les moyennes de chaque variable

#Regarder les variables avec des valeurs manquantes
fillNA_test=colMeans(data_test[,var_avecNA_test],na.rm=T)
for(var in var_avecNA_test){
  data_test[is.na(data_test[,var]),var]<-fillNA_test[var]
}


sum(is.na(data_test))

### Avant de continuer, corriger les valeurs sans aucun sens:

data_test[data_test$urine<0,"urine"]<-0


######## Transformation des variables ############

# fonction pour visualiser charges contre les r?gresseurs transform?s par diff?rentes fonctions

data_train$charges <- log(data_train$charges)


scatter<-function(var,base) {
  par(mfrow=c(2,3))
  R2<-round(summary(lm(base$charges~base[,var]))$adj.r.squared,2) ;  plot(y=base$charges,x=base[,var],main=paste(var," R2=",eval(R2),collapse=""))
  R2<-round(summary(lm(base$charges~log(base[,var])))$adj.r.squared,2) ;  plot(y=base$charges,x=log(base[,var]),main=paste(c("log(",var,") R2=",eval(R2)),collapse=""))
  R2<-round(summary(lm(base$charges~sqrt(base[,var])))$adj.r.squared,2) ;  plot(y=base$charges,x=sqrt(base[,var]),main=paste(c("sqrt(",var,") R2=",eval(R2)),collapse=""))
  R2<-round(summary(lm(base$charges~(base[,var])^2))$adj.r.squared,2) ;  plot(y=base$charges,x=(base[,var])^2,main=paste(c("(",var,")^2 R2=",eval(R2)),collapse=""))
  R2<-round(summary(lm(base$charges~exp(base[,var])))$adj.r.squared,2) ;  plot(y=base$charges,x=exp(base[,var]),main=paste(c("exp(",var,") R2=",eval(R2)),collapse=""))
  R2<-round(summary(lm(base$charges~1/base[,var]))$adj.r.squared,2) ;  plot(y=base$charges,x=1/(base[,var]),main=paste(c("1/(",var,") R2=",eval(R2)),collapse=""))
}

scatter("age",data_train) #Peu significative comme variable..
scatter("slos",data_train)  #La transformer en log semble bien vu le R2
scatter("avtisst",data_train) #Sqrt semble bien
scatter("temp",data_train) #Tres peu explicative
scatter("bili",data_train) #Tres peu explicative
scatter("bun",data_train) #Tres peu explicative
scatter("hday",data_train) #log semble bien

par(mfrow=c(1,1))

data_train$avtisst <- sqrt(data_train$avtisst)
data_train$slos <- log(data_train$slos)
data_train$hday <- log(data_train$hday)

data_test$avtisst <- sqrt(data_test$avtisst)
data_test$slos <- log(data_test$slos)
data_test$hday <- log(data_test$hday)

##############################################
#        CONDITIONNEMENT DE LA MATRICE       #  
##############################################

# Calcul de la matrice de corr?lation

mat_cor<-cor(data_train[,namesnotfactor])
heatmap(abs(mat_cor))
# avec corrplot
corrplot(mat_cor, method = "ellipse")
corrplot(abs(mat_cor), method = "ellipse")

# Corr?lations avec la variable ? expliquer 

mat_cor["charges",]

# valeurs propres de la matrice de corr?lation

vp=eigen(mat_cor)$values
max(vp)/min(vp)   # << 1000

library(car)

# Calcul du VIF

modlin=lm(charges~.,data=data_train[,namesnotfactor])
vif=vif(modlin)
max(vif)  # < 10

##############################################
#        SELECTION DES VARIABLES             #  
##############################################

# Selection backward

back_AIC=stepAIC(lm(charges~.,data=data_train),direction="both")
summary(back_AIC)

# Selection par LASSO

Y<-data_train[,"charges"]
# TOUTES LES VARIABLES
X<-model.matrix(~.,data_train[,!(colnames(data_train) %in% c("charges"))])  
cvLasso<-cv.glmnet(as.matrix(X),as.matrix(Y),family="gaussian",alpha=1)
coef_lasso=coef(cvLasso)
coef_lasso[(coef_lasso[,1]==0),]

# p-values et R?

modlin=lm(charges~.,data=data_train) 
# adjR? = 0.89
modlin2=lm(charges~hday+slos+avtisst,data=data_train)  
# adjR? = 0.84
modlin3=lm(charges~edu+scoma+avtisst+hday+meanbp+temp+bili+bun+adlsc+age+hospdead+slos+d.time+dzgroup,data=data_train)  
#adjR? = 0.86

# Arbre de r?gression

library(rpart)
library(randomForest)
arbre=rpart(charges~.,data=data_train[,!(colnames(data_train) %in% c("id"))])
plot(arbre)
text(arbre)

#foret=randomForest(charges~.,data=data_train,importance=T)
#plot(foret)
#imp1=importance(foret)[,1]
#plot(names(imp1),imp1)
#imp2=importance(foret)[,2]
#imp1=imp1[order(imp1)]
#plot(imp1)
#imp2=imp2[order(imp2)]
#plot(imp2)


##############################################
#      	  ANALYSE DU MODELE              #
##############################################

# on retient le modele qui a le R adj le plus petit
modele=lm(charges~slos+avtisst+hday,data=data_train)
summary(modele)

p=length(modele$coef)
n=nrow(data_train)

##### Diagnostics sur les r?sidus #####

par(mfrow=c(2,2))
plot(modele)

par(mfrow=c(1,1))
influencePlot(modele)

resstu=rstudent(modele)
hii=hatvalues(modele)
plot(data_train[,'slos'],resstu)
plot(data_train[,'hday'],resstu)
plot(data_train[,'avtisst'],resstu)

plot(resstu,hii)
abline(v=-2)
abline(v=2)
abline(h=2*p/n)


qqnorm(resstu)
qqline(resstu)
length(resstu[abs(resstu)>2])

CD=cooks.distance(modele)
plot(CD,type='h')
length(CD[CD>4/n])
length(CD[CD>6/n])  #Seuil propose, faute de normalite des donnees
infl=which(CD>4/n)
#infl=which(CD>6/n)
id_infl=data_train[infl,"id"]

##### Elimination des observations trop influentes

data_train2=data_train[-infl,]
modele2=lm(charges~slos+avtisst+hday,data=data_train2)
summary(modele2)

par(mfrow=c(2,2))
plot(modele2)

par(mfrow=c(1,1))
influencePlot(modele2)   # La c'est mieux

resstu2=rstudent(modele2)
hii2=hatvalues(modele2)
plot(data_train2[,'slos'],resstu2)
plot(data_train2[,'hday'],resstu2)
plot(data_train2[,'avtisst'],resstu2)

plot(resstu2,hii2)
abline(v=-2)
abline(v=2)
abline(h=2*p/nrow(data_train2))

qqnorm(resstu2)
qqline(resstu2)
length(resstu2[abs(resstu2)>2])

CD2=cooks.distance(modele2)
plot(CD2,type='h')
length(CD2[CD2>4/nrow(data_train2)])
length(CD2[CD2>6/nrow(data_train2)])  #Seuil propose, faute de normalite des donnees
#infl2=which(CD2>4/nrow(data_train2))
infl2=which(CD2>6/nrow(data_train2))
id_infl2=data_train2[infl2,"id"]

##############################################
#      	  PREDICTION                     #
##############################################

### Methode a) Modele lineaire lm

y_pred_log=predict(modele2,newdata=data_test)
y_pred=exp(y_pred_log)

#Estimation de l'erreur commise:

#Avec data_train
Y<-data_train2[,"charges"]
X<-model.matrix(~slos+avtisst+hday,data_train2[,!(colnames(data_train) %in% c("charges"))])

cvLasso<-cv.glmnet(as.matrix(X),as.matrix(Y),family="gaussian",alpha=1)
cvRidge<-cv.glmnet(as.matrix(X),as.matrix(Y),family="gaussian",alpha=0)
cvEN<-cv.glmnet(as.matrix(X),as.matrix(Y),family="gaussian",alpha=0.5)

#L'erreur estimee (en echelle logarithmique)
min(cvLasso$cvm)
min(cvRidge$cvm)
min(cvEN$cvm)


### Methode b) (methode choisie): NEURAL NETWORK

library(nnet)
library(MASS)

#On utilise d'abord TUNE pour determiner la taille de la couche cachee et la valeur decay

library(rpart)
library(e1071)

tim<-proc.time()
#Attention: ca prend trop de temps de calcul. Il faut faire la commande (commentee) suivante:
#tune.mod = tune.nnet(charges~., data = data_train2, size = c(1, 3, 5),decay = c(0.1, 0.001, 0.000001), linout=TRUE, na.action = na.omit)
tim<-proc.time()-tim
plot(tune.mod)
summary(tune.mod)

#Prediction
set.seed(123456)
regrnett <- nnet(charges~., data=data_train2, size = 5, decay = 0.001, linout=TRUE, maxit=3000,na.action = na.omit)
prednn<-exp(predict(regrnett, newdata = data_test, type="raw",na.action = na.omit))
# car on a pris le log de la variable Y  donc exp(log(Y)) = Y

#Data_test avec prediction
test_prednn<-data_test
test_prednn$Y_pred<-prednn

#Save as file

test_prednn$id<-as.character(test_prednn$id)
write.table(test_prednn[,c("id","Y_pred")],"prediction_data.csv",row.names=FALSE)





