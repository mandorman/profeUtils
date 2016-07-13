
library("../utils.R")

# Read Neko
neko <- read_tsv_default(file_name = "~/workspace/RprofeUtils/bigData/NekoAtsumeDatabase.csv")

table("Theme")
nrow(neko)

# Equivalent
head(neko)
nrow(neko[neko$Type == "Gold",])
nrow(subset(neko, Type == "Gold"))

# --------------------------
# Files d'or ... potenciant
# --------------------------
neko_gatsOr <- neko[neko$Type == "Gold",]
colnames(neko_gatsOr)

# ----------------------------------------------------
# Total de peixos or per toy (segur que esta relacionat amb els gats que fan venir)
# CONC: no puc fer venir gats, pero si que puc veure quines toys donen mes rendiment en or ...
# ----------------------------------------------------
aggregate(neko_gatsOr$Gold.Equivalent, by=list(Name=neko_gatsOr$Toy), FUN=sum)

toy_gold <- sort_by_column(aggregate(Gold.Equivalent ~ Toy, neko_gatsOr, sum), col_name = "Gold.Equivalent", des = T)
head(toy_gold, n = 30)

# Wop, aqui no tinc en compte amb quina frequencia he utilitzat quin toy (si una toy hi es
# des dels principi dels temps, es normal que dongui mes or ... cal veure rendiment ...)
head(neko)

head(neko[,c("Toy", "Date")])
# Suposant que cada toy dona al dia alguna cosa ....
# El que puc fer es filtrar duplicats
toy_dia <- as.data.frame(table(neko[,c("Toy", "Date")]))
nrow(toy_dia)
toy_dia <- toy_dia[!duplicated(toy_dia),]
# Ok, esta be ...
table(toy_dia$Toy) # Wow, 191 dies.... ho ha quadrat tot!

# CONCLUSIO (no cal relativitzar res, ja esta ok) :
head(toy_gold, n = 30)

# Hi ha correlacio amb els gats que donen mes or i el toy ?



# ---------------------------------------------





# -----------------------
# Quins gats donen mes or? (ranquejats)
# -----------------------
# Total de peixos or per gat
# -----------------------
aggregate(neko_gatsOr$Gold.Equivalent, by=list(Name=neko_gatsOr$Name), FUN=sum)
# O el que es el mateix ... (i ordenat)
sort_by_column(aggregate(Gold.Equivalent ~ Name, neko_gatsOr, sum), col_name = "Gold.Equivalent", des = T)

# Visites per gat optim ...
sort_by_column(as.data.frame(table(neko_gatsOr$Name)), col_name="Freq", des = T)

# NOTA: Cal diferenciar els gats que donen un rendiment net millor, i el que es mes quantitatiu ...


# Quan donaven or en quin toy estaven? (relacio toy -- gat )





# --------------------------------------------

# Optimize Speed (large data.sets)

library(data.table)
data = data.table(Category=c("First","First","First","Second","Third", "Third", "Second"), 
                  Frequency=c(10,15,5,2,14,20,3))
data[, sum(Frequency), by = Category]
system.time(data[, sum(Frequency), by = Category])
