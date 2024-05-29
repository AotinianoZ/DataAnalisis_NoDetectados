load("D:/NADA2/Paper/NADA2/Class Data/Golden2.rda")
View(Golden2)
attach(Golden2)
colnames(Golden2)

meanlead <- mean(blood.Pb)
save.image("D:/NADA2/Paper/NADA2/Training.RData")
load(file = "Training.RData")

#### Storing in Database ####

# 1 
data("ShePyrene") # from a package
attach(ShePyrene)
?ShePyrene

# 2
# Open with the symbol (Load workspace) in the environment (.csv, .txt, .xls, .xlsx)
attach(Oahu)
Oahu$Arsenic_cen <- as.logical(1-LT.0)
save(Oahu,file = "Oahu_d2.RData" )
attach(Oahu)

# We opened LOGISTIC.xlsx from Import Dataset from the Environment

# We opened MPCA_benz.csv from Import Dataset from the Environment

# We opened PbBloodKidney.txt from Import Dataset from the Environment

# We opened PbBloodKidney.txt from Import Dataset from the Environme


# 3
dt <- read.table(file = file.choose(), header = TRUE)
dt <- read.table(file = "Class Data/Example1.txt", header = TRUE)























