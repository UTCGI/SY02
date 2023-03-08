notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)
notes <- c(notes, 4)
notes10 <- notes / 2
notes10 > 6 
choix <- notes > 10 
notes[choix]

v<- c(1,2,3,4,5)
notes2 <- v - 2 


min(notes[(notes - floor(notes)) == 0])

v[2] <- 0
v[2:4] <- 1
v[v == 1] <- 2

# Met dans notes2 le vecteur notes avec toutes les valeurs diminuées par 2
notes2 <- notes - 2 

# Renvoie le nombre de valeurs inférieurs à 0
length (notes2 [notes2 < 0])

# Met à 0 toutes les notes inférieurs à 0
notes2[notes2 < 0] <- 0

collection <- c("R", "V", "B", "V")
f <- factor( c("R", "V", "B", "V"))

ADN <- factor(c("A","C","A","A","G","A","T","G","C","C","A","T","T","G","T","C"))
ADN
# [1] A C A A G A T G C C A T T G T C
# Levels: A C G T
nlevels(ADN)
# [1] 4
levels(ADN)
# [1] "A" "C" "G" "T"

# Nombre de A dans le brin d'ADN
length(ADN[ADN == "A"])
# [1] 5
length(ADN[ADN == "T"])
# [1] 4
length(ADN[ADN == "C"])
# [1] 4
length(ADN[ADN == "G"])
# [1] 3


X <- read.csv("data/sy02.data")

length(X) # Nb de colonnes de X 
ncol(X) # Nb de colonnes de X
nrow(X) # Nb de lignes de X
names(X) # Nom des colonnes de X
# [1] "correcteur.median" "median"            "correcteur.final"  "final"             "moyenne"           "resultat"

head(X)
summary(X)

X[1,1] # Extrait le 1er élément
X[,3] # Extrait la 3e colonne
X[1:10,] # Extrait les 10 premières lignes
X[c(1,3),c(1,4)] # Extraire les lignes 1 et 3 et les colonnes 1 et 4

X[,c(2,6)] # Extrait la 2e et la dernière colonne du tableau X

X$median # <=> X[,2]

X[X$median > 10,] # Etudiants ayant + de 10 au médian

mean(X[X$correcteur.median == "EG",2]) # Moyenne des étudiants du médian ayant été corrigé par EG

nrow(X[X$median < X$final,]) / nrow(X) # Proportion d'étudiants qui ont eu une meilleure note au final qu'au médian.

mean(X$final) # Moyenne
sd(X$final) # écart-type
var(X$final) # Variance
median(X$final) # Médiane
max(X$final) 
min(X$final)


summary(X$final)

quantile(X$median)
#0%  25%  50%  75% 100% 
#0.5 10.5 13.5 16.5 20.0 

IQR(X$median) # Etendue inter-quartile (Q3-Q1)

mean(sort(X$median)[11:287]) # Moyenne tronquée d'ordre 10 (moyenne en enlevant les 10 premières et les 1à dernières valeurs)

t <- table(ADN)
barplot(t)

barplot(table(X$correcteur.median)) # Diagramme en bande
# Nb de copies corrigé par correcteur.
# On remrque que ALC a corrigé le plus de copies

boxplot(X$final) # Boite à moustache des notes de final

stem(X$moyenne) # Diagramme en tige et feuille de la moyenne

hist(X$final) # Histogramme des notes du final

histo <- hist(X$final, breaks = c(0,15,20)) # Coupe l'histogramme à 15

histo$density # Renvoie les valeurs de densité présentes sur l'histogramme.

sum (diff(histo$breaks) * histo$density)
histo$breaks # Renvoie les abscices ou il y a un break
diff(histo$breaks) # Renvoie donc la longueur de chaque rectangle
diff(c(1,5,9,2)) # diff renvoie v[i] = v[i+1] - v[i]

plot(X$median , X$final)
plot(final ~ median, data=X)

boxplot(X$final ~ X$correcteur.final) # Boite à moustache en fonction des correcteurs

boxplot(X$final[X$correcteur.final == "DH"] ~ X$correcteur.final[X$correcteur.final == "DH"])

stripchart(X$final ~ X$correcteur.final, data = X) 

stripchart(X$final ~ X$correcteur.final, data = X, method = "jitter") 
