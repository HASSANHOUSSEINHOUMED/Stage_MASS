### Importation de base de données
library (Rcmdr)
base_de_donnee <- readXL("D:/Mon Stage LASTD/Données/ASERI a analyser.xlsx",
                         rownames=1, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
head(base_de_donnee)

### Test de khi-deux entre la satisfaction par rapport à la quantité de repas et les 
# restaurants fréquentés
attach(base_de_donnee)
quant_rest=table(Restaurant,quantite_repas)
quant_rest
test1=chisq.test(quant_rest)
test1
vcramer1=sqrt((15.678)/(365))
vcramer1

### Test de khi-deux entre la satisfaction par rapport à la qualité de repas et les restaurants 
fréquentés
qualit_rest=table(Restaurant,qualite_repas)
qualit_rest
test2=chisq.test(qualit_rest)
test2
vcramer2=sqrt((8.1352)/(365))
vcramer2

######################################## ACM
### ACM avec axes 1 et 2
library(FactoMineR)
acm=MCA(base_de_donnee[,c(4,7,8)])
acm
round(acm$eig,2)
round(acm$var$coord[,1:2],2)
round(acm$var$contrib[,1:2],2)
round(acm$var$cos[,1:2],2)

### ACM avec axes 1 et 3
acm2=MCA(base_de_donnee[,c(4,7,8)],axes=c(1,3))
acm2
round(acm2$eig,2)
round(acm2$var$coord[,c(1,3)],2)
round(acm2$var$contrib[,c(1,3)],2)
round(acm2$var$cos[,c(1,3)],2)

### Classification non-hiérarchique
distance=dist(acm$ind$coord[,1:3])
classif=hclust(distance,method="ward.D2")
plot(classif)
plot(rev(classif$height)[1:10],type="h",ylab="hauteurs") 
data=base_de_donnee[,c(4,7,8)]
head(data)
classification=kmeans(acm$ind$coord[,1:3],centers=4,nstart=100)
nouv_data=data.frame(data,classe=as.factor(classification$cluster))
catdes(nouv_data,num.var=4)

####################################### Régression logistique multinomiale
## Construction des deux échantillons et modalité de référence
set.seed(111)
selection = sort(sample(nrow(base_de_donnee), nrow(base_de_donnee) * 0.65,replace = 
                          FALSE))
# Échantillon d'apprentissage
echant_appren <- base_de_donnee[selection, ]
nrow(echant_appren)
echant_appren$qualite_repas <- relevel(echant_appren$qualite_repas, ref = "bonn_qualit")

# Échantillon de test
echant_test <- base_de_donnee[-selection, ]
nrow(echant_test)
echant_test$qualite_repas <- relevel(echant_test$qualite_repas, ref = "bonn_qualit")

# Construction du modèle
library(nnet)
regression <- multinom(qualite_repas ~ Restaurant + qualite_service + variete_repas + 
                         quantite_repas + Hygiene, data = echant_appren)
summary(regression)

# Test de significativité
library(car)
test_anova=Anova(regression,type=3,test.statistic = "LR")
test_anova

# Sélectionner le meilleur modèle
require(MASS)
modele.backward <- stepAIC(regression, trace = TRUE, data = echant_appren, direction = 
                             "backward")
modele.backward
summary(modele.backward)

# odds.ratio
library(questionr)
odds.ratio(modele.backward)

# Calcul de probabilité
Y1=41.222816-3.87042-39.897636
Y1
Y2=-9.219892+4.61458+1.904144
Y2
PBQ=(1/(1+exp(-2.54524)+exp(-2.701168)))
PBQ
PMQ=((exp(Y1))/(1+exp(-2.54524)+exp(-2.701168)))
PMQ
PTBQ=((exp(Y2))/(1+exp(-2.54524)+exp(-2.701168)))
PTBQ

# Matrice de confusion
predit=predict(modele.backward,echant_test)
head(predit)
tab=table(predit,echant_test$qualite_repas)
tab
sum(diag(tab))/sum(tab)
prob_erreur=1-sum(diag(tab))/sum(tab)
prob_erreur

# Courbe de roc
library(pROC)
predit1=factor(predit,levels=c("mauvais_qualit","bonn_qualit","tre_bonn_qualit"),labels=c("
1","2","3"))
echant_test$qualite_repas1=factor(echant_test$qualite_repas,levels=c("mauvais_qualit","b
onn_qualit","tre_bonn_qualit"),labels=c("1","2","3"))
roc1<-plot.roc(echant_test$qualite_repas1,as.numeric(predit1),main="Courbe de ROC")
text(0.4,0.2,labels=paste("AUC = ", format.pval(roc1$auc)), adj=c(0,.5),col="red",cex=1)

################################################### AFD
base_de_donnee2=base_de_donnee[,4:9]
str(base_de_donnee2)
set.seed(222)
selection2 = sort(sample(nrow(base_de_donnee2), nrow(base_de_donnee2) * 0.65,replace 
                         = FALSE))
# Échantillon d'apprentissage
echant_appren2 <- base_de_donnee2[selection2, ]
nrow(echant_appren2)

# Échantillon de test
echant_test2 <- base_de_donnee2[-selection2, ]
nrow(echant_test2)

######################### AFDL
library(MASS)
afd=lda(quantite_repas~Restaurant+qualite_service+variete_repas+qualite_repas+Hygiene,
        echant_appren2)
afd

# Histogramme
prediction=predict(afd,echant_appren2)
par(mfrow=c(3,1))
ldahist(data=prediction$x[,1],g=echant_appren2$quantite_repas)
ldahist(data=prediction$x[,2],g=echant_appren2$quantite_repas)
ldahist(data=prediction$x[,3],g=echant_appren2$quantite_repas)

# Bi-plot
plot(afd,col=c("red","blue","green","yellow")[echant_appren2$quantite_repas])

# Matrice de confusion
predit22=predict(afd,echant_test2)$class
matrice=table(predicted=predit22,Actual=echant_test2$quantite_repas)
matrice
correcte=sum(diag(matrice))/sum(matrice)
correcte
1-sum(diag(matrice))/sum(matrice)

####################################### Arbre de décision pour la qualité de repas
base_de_donnee3=base_de_donnee[,4:9]
str(base_de_donnee3)
set.seed(333)
selection3 = sort(sample(nrow(base_de_donnee3), nrow(base_de_donnee3) * 0.65,replace 
                         = FALSE))

# Échantillon d'apprentissage
echant_appren3 <- base_de_donnee3[selection3, ]
nrow(echant_appren3)
echant_appren3$qualite_repas=factor(echant_appren3$qualite_repas,levels=c("bonn_quali
t","mauvais_qualit","tre_bonn_qualit"),labels=c("BQ","MQ","TBQ"))

# Échantillon de test
echant_test3 <- base_de_donnee3[-selection3, ]
nrow(echant_test3)
echant_test3$qualite_repas=factor(echant_test3$qualite_repas,levels=c("bonn_qualit","ma
uvais_qualit","tre_bonn_qualit"),labels=c("BQ","MQ","TBQ"))

# Construction d'arbre de décision
library(party)
arbre=ctree(qualite_repas ~ Restaurant + qualite_service + variete_repas + quantite_repas + 
              Hygiene,echant_appren3)
print(arbre)

# visualisation d'arbre de décision
plot(arbre)

# Matrice de confusion 
pred=predict(arbre,echant_test3)
matrice=table(prediction=pred,Actuel=echant_test3$qualite_repas)
matrice
sum(diag(matrice))/sum(matrice)
1-sum(diag(matrice))/sum(matrice)


#################################### Arbre de décision pour la quantité de repas
base_de_donnee4=base_de_donnee[,4:9]
str(base_de_donnee4)
set.seed(555)
selection4 = sort(sample(nrow(base_de_donnee4), nrow(base_de_donnee4) * 0.65,replace 
                         = FALSE))

# Échantillon d'apprentissage
echant_appren4 <- base_de_donnee4[selection4, ]
nrow(echant_appren4)
echant_appren4$quantite_repas=factor(echant_appren4$quantite_repas,levels=c("pas_sati
sfait" ,"peu_satisfait","satisfait","tre_satisfait"),labels=c("PS","PES","S","TS"))

# Échantillon de test
echant_test4 <- base_de_donnee4[-selection4, ]
nrow(echant_test4)
echant_test4$quantite_repas=factor(echant_test4$quantite_repas,levels=c("pas_satisfait" 
                                                                        ,"peu_satisfait","satisfait","tre_satisfait"),labels=c("PS","PES","S","TS"))
# Construction d'arbre de décision
library(party)
arbre2=ctree( quantite_repas ~ Restaurant + qualite_service + variete_repas + qualite_repas 
              + Hygiene,echant_appren4)
print(arbre2)

# visualisation d'arbre de décision
plot(arbre2)

# Matrice de confusion 
pred22=predict(arbre2,echant_test4)
matrice2=table(prediction=pred22,Actuel=echant_test4$quantite_repas)
matrice2
sum(diag(matrice2))/sum(matrice2)
1-sum(diag(matrice2))/sum(matrice2)
