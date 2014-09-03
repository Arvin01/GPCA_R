getwd()
setwd("C:/Users/Sebastian/Documents/Informatique/machineLearning/documenti_memoire/VidalVsSeba/funzioneFinale/PCAImmaginiProvvisorie")


dat = read.table("marks.dat",head=T)
dim(dat)
names(dat)
plot(dat)
pc = princomp(~Stat+Phys,dat)
pc$loading

summary(pc)

