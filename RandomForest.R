# READ THE DATA 
KrediRisk = read.table(choose.files(), sep ="\t", quote = "",header=TRUE) 

View(KrediRisk)

# Veri setine satir adi ekledik
rownames(KrediRisk) <- paste0("Customer ", seq(from = 1, to = nrow(KrediRisk), by = 1))

# verisetinin boyutu
dim(KrediRisk)

View(KrediRisk)
summary(KrediRisk)

# Kategorik degiskenlerin faktör olarak ve nümerik degiskenlerin numerik degisken olarak tanimlanmasi 

for(i in 1:12){
  if(i == 2 || i == 3 || i == 4 || i == 5 || i == 8 )
    KrediRisk[[i]] <- as.numeric(x = KrediRisk[[i]])
  else
    KrediRisk[[i]] <- as.factor(x = KrediRisk[[i]])
  
}

View(KrediRisk)
summary(KrediRisk)

# Frekans tablosu elde edilir
table(KrediRisk$CRsk)

# Referans deger "Yüksek" kategorisi olarak degistirilir
KrediRisk$CRsk <- relevel(KrediRisk$CRsk, ref="Yuksek")

# Frekans tablosu yeniden kontrol edilir
table(KrediRisk$CRsk)

# Veri setine ait ozet bilgi alinir
summary(KrediRisk)
fix(KrediRisk)

# Numerik niteliklerin normalize edilmesi
orj_KrediRisk <- KrediRisk
install.packages("clusterSim")
library(clusterSim)
KrediRisk$Age <- data.Normalization(KrediRisk$Age,type="n1",normalization="column")
KrediRisk$Chck <- data.Normalization(KrediRisk$Chck,type="n1",normalization="column")
KrediRisk$MEmp <- data.Normalization(KrediRisk$MEmp,type="n1",normalization="column")
KrediRisk$MCust <- data.Normalization(KrediRisk$MCust,type="n1",normalization="column")
KrediRisk$Svgs <- data.Normalization(KrediRisk$Svgs,type="n1",normalization="column")

# Veri setine ait ozet bilgi 
summary(KrediRisk)
# Veri seti rastgele bicimde egitim ve test veri seti olarak ikiye ayrilir
install.packages("caret")
library(caret)
set.seed(1)
#egitimIndisleri <- createDataPartition(y = kalp$kalpHastaligi, times=3, p = .80 list = FALSE) times=3 dediðimizde egitim ve  test setinden 3 defa farklý örnek seçiyor. 
egitimIndisleri <- createDataPartition(y = KrediRisk$CRsk, p = .80, list = FALSE) 
egitim <- KrediRisk[egitimIndisleri,]
test <- KrediRisk[-egitimIndisleri,]

# Egitim ve Test veri setlerinde tahminde kullanilacak nitelikler ve hedef nitelik ayri nesnelere atanir
testNitelikleri <- test[, -12]
testHedefNitelik <- test[[12]]

egitimNitelikleri <- egitim[, -12]
egitimHedefNitelik <- egitim[[12]]



#####################################################################################
##############                     Random Forest                        ##############
#######################################################################################
install.packages("randomForest")
library(randomForest)
egitim$CRsk

rf<-randomForest(CRsk ~., data=egitim)
show(rf)
dim(egitim)
dim(test)

(tahminiSiniflar <- predict(rf, testNitelikleri))

(tablom <- table(tahminiSiniflar, testHedefNitelik, dnn = c("Tahmini Siniflar", "Gercek Siniflar")))

(tp <- tablom[1])
(fp <- tablom[3])
(fn <- tablom[2])
(tn <- tablom[4])

paste0("Dogruluk = ",(dogruluk <- (tp+tn)/sum(tablom)))
paste0("Hata = ",(hata <- 1-dogruluk))
paste0("TPR = ",(TPR <- tp/(tp+fn)))
paste0("SPC = ",(SPC <- tn/(fp+tn)))
paste0("PPV = ",(PPV <- tp/(tp+fp)))
paste0("NPV = ",(NPV <- tn/(tn+fn)))
paste0("FPR = ",(FPR <- fp/sum(tablom)))
paste0("FNR = ",(FNR <- fn/(fn+tp)))
paste0("LR_p = ",(LR_p <- TPR/FPR))
paste0("LR_n = ",(LR_n <- FNR/SPC))
paste0("DOR = ",(DOR <- LR_p/LR_n))
paste0("F_measure = ",(F_measure <- (2*PPV*TPR)/(PPV+TPR)))

Dogruluk<-(tp+tn)/sum(tablom)
Hata<-1-dogruluk
Duyarlilik<-tp/(tp+fn)
Belirleyicilik<-tn/(fp+tn)
Pozitif_Öngörü<-tp/(tp+fp)
Negatif_Öngörü<-tn/(tn+fn)
FPR <- fp/sum(tablom)
FNR <- fn/(fn+tp)
LR_p <- TPR/FPR
LR_n <- FNR/SPC
DOR <- LR_p/LR_n
F_measure <- (2*PPV*TPR)/(PPV+TPR)

olcutler<-c("Dogruluk", "Hata", "Duyarlilik","Belirleyicilik","Pozitif_Öngörü", "Negatif_Öngörü", "F_measure")
degerler<-c(Dogruluk,Hata,Duyarlilik,Belirleyicilik,Pozitif_Öngörü,Negatif_Öngörü,F_measure)
sonuc<-data.frame(olcutler,degerler)
(sonuc<-t.data.frame(sonuc))
