## Copyright (C) 2017-2022 Vincent Goulet
##
## Ce fichier fait partie du projet «Programmer avec R»
## https://gitlab.com/vigou3/programmer-avec-r
##
## Cette création est mise à disposition sous licence
## Attribution-Partage dans les mêmes conditions 4.0
## International de Creative Commons.
## https://creativecommons.org/licenses/by-sa/4.0/

###
### OBJETS R               
###

## NOMS D'OBJETS

## Quelques exemples de noms valides et invalides.
foo <- 5                   # valide
foo.123 <- 5               # valide
foo_123 <- 5               # valide
.foo <- 5                  # valide
123foo <- 5                # invalide; commence par un chiffre
.123foo <- 5               # invalide; point suivi d'un chiffre

## Liste des objets dans l'espace de travail. Les objets dont
## le nom commence par un point sont cachés, comme à la ligne
## de commande Unix.
ls()                       # l'objet '.foo' n'est pas affiché
ls(all.names = TRUE)       # objets cachés aussi affichés

## R est sensible à la casse
foo <- 1
Foo
FOO

## MODES ET TYPES DE DONNÉES

## Le mode d'un objet détermine ce qu'il peut contenir. Les
## vecteurs simples ("atomic") contiennent des données d'un
## seul type.
mode(c(1, 4.1, pi))        # nombres réels
mode(c(2, 1 + 5i))         # nombres complexes
mode(c(TRUE, FALSE, TRUE)) # valeurs booléennes
mode("foobar")             # chaines de caractères

## Par défaut, tous les nombres sont représentés en double
## précision dans R. Il n'y a donc pas de différence entre
## un nombre entier et un nombre réel.
typeof(486)                # nombre réel en double précision
typeof(0.3324)             # idem

## Il est possible de définir des «vrais» entiers dans R en
## faisant suivre la valeur immédiatement de la lettre «L»,
## sans espace. Le mode de ces valeurs est toujours "numeric",
## mais leur type (ou représentation interne) est "integer".
mode(486L)                 # nombre...
typeof(486L)               # ... entier

## La plupart des autres types d'objets sont récursifs. Voici
## quelques autres modes.
mode(seq)                  # une fonction
mode(list(5, "foo", TRUE)) # une liste
mode(expression(x <- 5))   # une expression non évaluée

## LONGUEUR

## La longueur d'un vecteur est égale au nombre d'éléments
## dans le vecteur.
(x <- 1:4)
length(x)

## Une chaine de caractères ne compte que pour un seul
## élément.
(x <- "foobar")
length(x)

## Pour obtenir la longueur de la chaine, il faut utiliser
## nchar().
nchar(x)

## Un objet peut néanmoins contenir plusieurs chaines de
## caractères.
(x <- c("f", "o", "o", "b", "a", "r"))
length(x)

## La longueur peut être 0, auquel cas on a un objet vide,
## mais qui existe.
(x <- numeric(0))          # création du contenant
length(x)                  # l'objet 'x' existe...
x[1] <- 1                  # définition du premier élément

## Si un objet n'existe pas au préalable, il est impossible
## d'affecter directement la valeur d'un élément.
X[1] <- 1                  # impossible, 'X' n'existe pas

## VALEURS SPÉCIALES

## Donnée manquante. 'NA' est un nom réservé pour représenter
## une donnée manquante.
c(65, NA, 72, 88)          # traité comme une valeur
NA + 2                     # tout calcul avec 'NA' donne NA
is.na(c(65, NA))           # test si les données sont NA

## Il arrive souvent de vouloir indicer spécifiquement les
## données manquantes d'un vecteur (pour les éliminer ou pour
## les remplacer par une autre valeur, par exemple).
##
## Pour ce faire, on utilise la fonction 'is.na' et l'indiçage
## par un vecteur booléen.
x <- c(NA, 12, 55, NA, 4)  # vecteur contenant des NA
is.na(x)                   # positions des données manquantes
x[!is.na(x)]               # suppression des données manquantes
x[is.na(x)] <- 0; x        # remplacement des NA par des 0

## Valeurs infinies et indéterminée. 'Inf', '-Inf' et 'NaN'
## sont des noms réservés.
1/0                        # +infini
-1/0                       # -infini
0/0                        # indétermination
x <- c(65, Inf, NaN, 88)   # s'utilisent comme des valeurs
is.finite(x)               # quels sont les nombres réels?
is.nan(x)                  # lesquels sont indéterminés?

## Valeur "néant". 'NULL' est un nom réservé pour représenter
## le néant, rien.
mode(NULL)                 # le mode de 'NULL' est NULL
length(NULL)               # longueur nulle
c(NULL, NULL)              # du néant ne résulte que le néant

## ATTRIBUTS

## Les objets peuvent être dotés d'un ou plusieurs attributs.
data(cars)                 # jeu de données intégré
attributes(cars)           # liste de tous les attributs
attr(cars, "class")        # extraction d'un seul attribut

## L'attribut 'names' conserve les étiquettes des éléments
## d'un vecteur.
x <- 1:24                  # un vecteur
names(x) <- letters[1:24]  # attribution d'étiquettes
x                          # identification facilitée 

###
### MATRICE ET TABLEAU  
###

## Une matrice est un vecteur avec un attribut 'dim' de
## longueur 2 et une classe implicite "matrix". La manière
## naturelle de créer une matrice est avec la fonction
## 'matrix'.
(x <- matrix(1:12, nrow = 3, ncol = 4))
length(x)                  # longueur du vecteur sous-jacent
attributes(x)              # objet muni d'un attribut 'dim'
dim(x)                     # deux dimensions

## Les matrices sont remplies par colonne par défaut. L'option
## 'byrow' permet de les remplir par ligne, si nécessaire.
(x <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE))

## Il n'est pas nécessaire de préciser les deux dimensions de
## la matrice s'il est possible d'en déduire une à partir de
## l'autre et de la longueur du vecteur de données. Les
## expressions ci-dessous sont toutes équivalentes.
matrix(1:12, nrow = 3, ncol = 4)
matrix(1:12, nrow = 3)
matrix(1:12, ncol = 4)

## À l'inverse, s'il n'y a pas assez de données pour remplir
## les dimensions précisées, les données seront recyclées,
## comme d'habitude.
matrix(1, nrow = 3, ncol = 4)
matrix(1:3, nrow = 3, ncol = 4)
matrix(1:4, nrow = 3, ncol = 4, byrow = TRUE)

## Dans l'indiçage des matrices et tableaux, l'indice de
## chaque dimension obéit aux règles usuelles d'indiçage des
## vecteurs.
x[1, 2]                    # élément en position (1, 2)
x[1, -2]                   # 1ère rangée sans 2e colonne
x[c(1, 3), ]               # 1ère et 3e rangées
x[-1, ]                    # supprimer 1ère rangée
x[, -2]                    # supprimer 2e colonne
x[x[, 1] > 2, ]            # lignes avec 1er élément > 2

## Indicer la matrice ou le vecteur sous-jacent est
## équivalent. Utiliser l'approche la plus simple selon le
## contexte.
x[1, 3]                    # l'élément en position (1, 3)...
x[7]                       # ... est le 7e élément du vecteur

## Détail additionnel sur l'indiçage des matrices et tableaux:
## il est aussi possible de les indicer avec une matrice.
## Chaque ligne de la matrice d'indiçage fournit alors la
## position d'un élément à sélectionner.
##
## Consulter au besoin la rubrique d'aide de la fonction '['
## (ou de 'Extract').
x[rbind(c(1, 1), c(2, 2))] # éléments x[1, 1] et x[2, 2]
x[cbind(1:3, 1:3)]         # éléments x[i, i] («diagonale»)
diag(x)                    # idem et plus explicite

## Quelques fonctions pour travailler avec les dimensions des
## matrices.
nrow(x)                    # nombre de lignes
dim(x)[1]                  # idem
ncol(x)                    # nombre de colonnes
dim(x)[2]                  # idem

## Les matrices et les tableaux étant des vecteurs, ils sont
## soumis aux règles usuelles de l'arithmétique vectorielle.
## Certains des opérations qui en résultent ne sont pas
## définies en algèbre linéaire usuelle.
(x <- matrix(1:4, 2))      # matrice 2 x 2
(y <- matrix(3:6, 2))      # autre matrice 2 x 2
5 * x                      # multiplication par une constante
x + y                      # addition matricielle
x * y                      # produit *élément par élément*
x %*% y                    # produit matriciel
x / y                      # division *élément par élément*
x * c(2, 3)                # produit par colonne

## La fonction 'rbind' ("row bind") permet d'«empiler» des
## matrices comptant le même nombre de colonnes.
##
## De manière similaire, la fonction 'cbind' ("column bind")
## permet de concaténer des matrices comptant le même nombre de
## lignes.
##
## Utilisées avec un seul argument, 'rbind' et 'cbind' créent
## des vecteurs ligne et colonne, respectivement. Ceux-ci sont
## rarement nécessaires.
x <- matrix(1:12, 3, 4)    # 'x' est une matrice 3 x 4
y <- matrix(1:8, 2, 4)     # 'y' est une matrice 2 x 4
z <- matrix(1:6, 3, 2)     # 'z' est une matrice 3 x 2

rbind(x, 99)               # ajout d'une ligne à 'x'
rbind(x, y)                # fusion verticale de 'x' et 'y'
cbind(x, 99)               # ajout d'une colonne à 'x'
cbind(x, z)                # concaténation de 'x' et 'z'
rbind(x, z)                # dimensions incompatibles
cbind(x, y)                # dimensions incompatibles
rbind(1:3)                 # vecteur ligne
cbind(1:3)                 # vecteur colonne

## Un tableau (array) est un vecteur avec plus de deux
## dimensions. Pour le reste, la manipulation des tableaux
## est en tous points identique à celle des matrices. Ne pas
## oublier: les tableaux sont remplis de la première dimension
## à la dernière!
(x <- array(1:60, 3:5))    # tableau 3 x 4 x 5
length(x)                  # longueur du vecteur sous-jacent
dim(x)                     # trois dimensions
x[1, 3, 2]                 # l'élément en position (1, 3, 2)...
x[19]                      # ... est le 19e élément du vecteur

## Le tableau ci-dessus est un prisme rectangulaire 3 unités
## de haut, 4 de large et 5 de profond. Indicer ce prisme avec
## un seul indice équivaut à en extraire des «tranches», alors
## qu'utiliser deux indices équivaut à en tirer des «carottes»
## (au sens géologique du terme) ou des tubes. Il est laissé
## en exercice de généraliser à plus de dimensions...
x                          # les cinq matrices
x[, , 1]                   # tranche transversale
x[, 1, ]                   # tranche verticale
x[1, , ]                   # tranche horizontale
x[, 1, 1]                  # carotte de haut en bas
x[1, 1, ]                  # carotte de devant à derrière
x[1, , 1]                  # carotte de gauche à droite
x[1, 1, 1]                 # donnée unique  

###
### APPLICATION POUR LES MATRICES ET LES TABLEAUX  
###

## La fonction 'apply' applique une fonction sur une ou
## plusieurs dimensions d'une matrice ou d'un tableau.
##
## Création d'une matrice et d'un tableau à trois dimensions
## pour les exemples.
m <- matrix(sample(1:100, 20), nrow = 4, ncol = 5)
a <- array(sample(1:100, 60), dim = 3:5)

## Les fonctions 'rowSums', 'colSums', 'rowMeans' et
## 'colMeans' sont des raccourcis pour des utilisations
## fréquentes de 'apply'.
apply(m, 1, sum)           # sommes par ligne
rowSums(m)                 # idem, plus lisible
apply(m, 2, mean)          # moyennes par colonne
colMeans(m)                # idem, plus lisible

## Puisqu'il n'existe pas de fonctions comme 'rowMax' ou
## 'colProds', il faut utiliser 'apply'.
apply(m, 1, max)           # maximums par ligne
apply(m, 2, prod)          # produits par colonne

## L'argument '...' de 'apply' permet de passer des arguments
## à la fonction FUN.
f <- function(x, y) x + 2 * y # fonction à deux arguments
apply(m, 1, f, y = 2)         # argument 'y' passé dans '...'

## Lorsque 'apply' est utilisée sur un tableau, son résultat
## est de dimensions dim(X)[MARGIN], d'où le truc
## mnémotechnique donné dans le texte du chapitre.
apply(a, c(2, 3), sum)     # le résultat est une matrice
apply(a, 1, prod)          # le résultat est un vecteur

## L'utilisation de 'apply' avec les tableaux peut rapidement
## devenir confondante si l'on ne visualise pas les calculs
## qui sont réalisés.
##
## Reprenons ici les exemples du chapitre en montrant comment
## calculer le premier élément de chaque utilisation de
## 'apply'.
##
## Au besoin, réviser l'indiçage des tableaux au chapitre 3.
(x <- array(sample(0:10, 24, rep = TRUE), c(3, 4, 2)))
apply(x, 1, sum)      # sommes des 3 tranches horizontales
sum(x[1, , ])         # équivalent pour la première somme

apply(x, 2, sum)      # sommes des 4 tranches verticales
sum(x[, 1, ])         # équivalent pour la première somme

apply(x, 3, sum)      # sommes des 2 tranches transversales
sum(x[, , 1])         # équivalent pour la première somme

apply(x, c(1, 2), sum) # sommes des 12 carottes horizontales
sum(x[1, 1, ])         # équivalent pour la première somme

apply(x, c(2, 3), sum) # sommes des 8 carottes verticales
sum(x[, 1, 1])         # équivalent pour la première somme

apply(x, c(1, 3), sum) # sommes des 6 carottes transversales
sum(x[1, , 1])         # équivalent pour la première somme  

###
### LISTE  
###

## La liste est l'objet le plus général en R. C'est un objet
## récursif qui peut contenir des objets de n'importe quel
## mode (y compris la liste) et de n'importe quelle longueur.
(x <- list(joueur = c("V", "C", "C", "M", "A"),
           score = c(10, 12, 11, 8, 15),
           expert = c(FALSE, TRUE, FALSE, TRUE, TRUE),
           niveau = 2))
is.vector(x)               # liste est un vecteur...
is.recursive(x)            # ... récursif...
length(x)                  # ... de quatre éléments...
mode(x)                    # ... de mode "list"

## Comme tout autre vecteur, une liste peut être concaténée
## avec un autre vecteur avec la fonction 'c'.
y <- list(TRUE, 1:5)       # liste de deux éléments
c(x, y)                    # liste de six éléments

## Pour initialiser une liste d'une longueur donnée, on
## utilise la fonction 'vector'.
vector("list", 5)

## Les crochets simples [ ] permettent d'extraire un ou
## plusieurs éléments d'une liste. Le résultat est toujours
## une liste, même si l'on extrait un seul élément.
x[c(1, 2)]                 # deux premiers éléments
x[1]                       # premier élément: une liste

## Lorsque l'on veut extraire un, et un seul, élément d'une
## liste et obtenir l'objet lui-même (et non une liste
## contenant l'objet), il faut utiliser les crochets doubles
## [[ ]].
x[[1]]                     # comparer avec ci-dessus

## Jolie fonctionnalité: les crochets doubles permettent
## d'indicer récursivement la liste, c'est-à-dire d'extraire
## un objet de la liste, puis un élément de l'objet, et ainsi
## de suite.
x[[1]][2]                  # 2e élément du 1er élément
x[[c(1, 2)]]               # idem, par indiçage récursif

## Les éléments d'une liste étant généralement nommés (c'est
## une bonne habitude à prendre!), il est souvent plus simple
## et, surtout, plus sûr d'extraire les éléments d'une liste
## par leur étiquette avec l'opérateur $.
x$joueur                   # équivalent à x[[1]]
x$joueur[2]                # équivalent à x[[c(1, 2)]]
x[["expert"]]              # aussi valide, mais peu usité
x$level <- 1               # aussi pour l'affectation

## Une liste peut contenir n'importe quoi...
x[[5]] <- matrix(1, 2, 2)  # ... une matrice...
x[[6]] <- list(0:5, TRUE)  # ... une autre liste...
x[[7]] <- seq              # ... même le code d'une fonction!
x                          # eh ben!
x[[c(6, 1, 3)]]            # de quel élément s'agit-il?

## Il est possible de supprimer un élément d'une liste en lui
## affectant la valeur 'NULL'.
x[[7]] <- NULL; length(x)  # suppression du 7e élément

## Il est parfois utile de convertir une liste en un simple
## vecteur. Les éléments de la liste sont alors «déroulés», y
## compris la matrice en position 5 dans notre exemple (qui
## n'est rien d'autre qu'un vecteur, on s'en souviendra).
unlist(x)                    # remarquer la conversion
unlist(x, recursive = FALSE) # ne pas appliquer aux sous-listes
unlist(x, use.names = FALSE) # éliminer les étiquettes  

###
### APPLICATION POUR LES LISTES ET LES VECTEURS  
###

## FONCTIONS 'lapply' ET 'sapply'

## La fonction 'lapply' applique une fonction à tous les
## éléments d'un vecteur ou d'une liste et retourne une liste,
## peu importe les dimensions des résultats.
##
## La fonction 'sapply' retourne un vecteur ou une matrice, si
## possible.
##
## Somme «interne» des éléments d'une liste.
(x <- list(1:10, c(-2, 5, 6), matrix(3, 4, 5)))
sum(x)                     # erreur
lapply(x, sum)             # sommes internes (liste)
sapply(x, sum)             # sommes internes (vecteur)

## Création de la suite 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, ..., 1,
## 2, ..., 9, 10.
lapply(1:10, seq)          # résultat sous forme de liste
unlist(lapply(1:10, seq))  # résultat sous forme de vecteur

## Soit une fonction calculant la moyenne pondérée d'un
## vecteur. Cette fonction prend en argument une liste de deux
## éléments: 'donnees' et 'poids'.
fun <- function(x)
    sum(x$donnees * x$poids)/sum(x$poids)

## Nous pouvons maintenant calculer la moyenne pondérée de
## plusieurs ensembles de données réunis dans une liste
## itérée.
(x <- list(list(donnees = 1:7,
                poids = (5:11)/56),
           list(donnees = sample(1:100, 12),
                poids = 1:12),
           list(donnees = c(1, 4, 0, 2, 2),
                poids = c(12, 3, 17, 6, 2))))
sapply(x, fun)             # aucune boucle explicite!

## EXEMPLES ADDITIONNELS SUR L'UTILISATION DE L'ARGUMENT
## '...' AVEC 'lapply' ET 'sapply'

## Aux fins des exemples ci-dessous, créons d'abord une liste
## formée de nombres aléatoires.
##
## L'expression ci-dessous fait usage de l'argument '...' de
## 'lapply'. Pouvez-vous la décoder? Nous y reviendrons plus
## loin, ce qui compte pour le moment c'est simplement de
## l'exécuter.
x <- lapply(c(8, 12, 10, 9), sample, x = 1:10, replace = TRUE)

## Soit maintenant une fonction qui calcule la moyenne
## arithmétique des données d'un vecteur 'x' supérieures à une
## valeur 'y'.
##
## Vous remarquerez que cette fonction n'est pas vectorielle
## pour 'y', c'est-à-dire qu'elle n'est valide que lorsque 'y'
## est un vecteur de longueur 1.
fun <- function(x, y) mean(x[x > y])

## Pour effectuer ce calcul sur chaque élément de la liste
## 'x', nous pouvons utiliser 'sapply' plutôt que 'lapply',
## car chaque résultat est de longueur 1.
##
## Cependant, il faut passer la valeur de 'y' à la fonction
## 'fun'. C'est là qu'entre en jeu l'argument '...' de
## 'sapply'.
sapply(x, fun, 7)          # moyennes des données > 7

## Les fonctions 'lapply' et 'sapply' passent tour à tour les
## éléments de leur premier argument comme *premier* argument
## à la fonction, sans le nommer explicitement. L'expression
## ci-dessus est donc équivalente à
##
##   c(fun(x[[1]], 7), ..., fun(x[[4]], 7)
##
## Que se passe-t-il si l'on souhaite passer les valeurs à un
## argument de la fonction autre que le premier? Par exemple,
## supposons que l'ordre des arguments de la fonction 'fun'
## ci-dessus est inversé.
fun <- function(y, x) mean(x[x > y])

## Les règles de pairage des arguments des fonctions en R font
## en sorte que lorsque les arguments sont nommés dans l'appel
## de fonction, leur ordre n'a pas d'importance. Par
## conséquent, un appel de la forme
##
##   fun(x, y = 7)
##
## est tout à fait équivalent à fun(7, x). Pour effectuer les
## calculs
##
##   c(fun(x[[1]], y = 7), ..., fun(x[[4]], y = 7)
##
## avec la liste définie plus haut, il s'agit de nommer
## l'argument 'y' dans '...' de 'sapply'.
sapply(x, fun, y = 7)

## Décodons maintenant l'expression
##
##   lapply(c(8, 12, 10, 9), sample, x = 1:10, replace = TRUE)
##
## qui a servi à créer la liste. La définition de la fonction
## 'sample' est la suivante:
##
##   sample(x, size, replace = FALSE, prob = NULL)
##
## L'appel à 'lapply' est équivalent à
##
##   list(sample(8, x = 1:10, replace = TRUE),
##        ...,
##        sample(9, x = 1:10, replace = TRUE))
##
## Toujours selon les règles d'appariement des arguments, vous
## constaterez que les valeurs 8, 12, 10, 9 seront attribuées
## à l'argument 'size', soit la taille de l'échantillon.
##
## L'expression crée donc une liste comprenant quatre
## échantillons aléatoires de tailles différentes des nombres
## de 1 à 10 pigés avec remise.
##
## Une expression équivalente, quoique moins élégante, aurait
## recours à une fonction anonyme pour replacer les arguments
## de 'sample' dans l'ordre voulu.
lapply(c(8, 12, 10, 9),
       function(x) sample(1:10, x, replace = TRUE))

## La fonction 'sapply' est aussi très utile pour vectoriser
## une fonction qui n'est pas vectorielle. Supposons que l'on
## veut généraliser la fonction 'fun' pour qu'elle accepte un
## vecteur de seuils 'y'.
fun <- function(x, y)
    sapply(y, function(y) mean(x[x > y]))

## Utilisation sur la liste 'x' avec trois seuils.
sapply(x, fun, y = c(3, 5, 7))

## FONCTION 'mapply'

## Application de la fonction 'fun' sur les échantillons de la
## liste 'x' avec un seuil différent pour chacun.
mapply(fun, x, c(3, 5, 7, 7))

## Création de quatre échantillons aléatoires de taille 12.
x <- lapply(rep(12, 4), sample, x = 1:100)

## Moyennes tronquées à 0, 10, 20 et 30 %, respectivement, de
## ces quatre échantillons aléatoires.
mapply(mean, x, 0:3/10)

###
### TABLEAU DE DONNÉES  
###

## Un tableau de données (data frame) est une liste dont les
## éléments sont tous de la même longueur. Il comporte un
## attribut 'dim', ce qui fait qu'il est représenté comme une
## matrice. Cependant, les colonnes peuvent être de modes
## différents.
data.frame(Nom = c("Pierre", "Jean", "Jacques"),
           Age = c(42, 34, 19),
           Fumeur = c(TRUE, TRUE, FALSE))

## R est livré avec plusieurs jeux de données, la plupart sous
## forme de tableaux de données.
data()                     # liste complète

## Nous allons illustrer certaines manipulations des tableaux
## de données avec le jeu de données 'USArrests'.
USArrests                  # jeu de données

## Analyse succincte de l'objet.
mode(USArrests)            # une liste...
length(USArrests)          # ... de quatre éléments...
class(USArrests)           # ... de classe 'data.frame'
dim(USArrests)             # dimensions implicites
names(USArrests)           # titres des colonnes
row.names(USArrests)       # titres des lignes
USArrests[, 1]             # première colonne
USArrests$Murder           # idem, plus simple
USArrests[1, ]             # première ligne

## La fonction 'subset' permet d'extraire des lignes et des
## colonnes d'un tableau de données de manière très intuitive.
##
## Par exemple, nous pouvons extraire ainsi le nombre
## d'assauts dans les états comptant un taux de meurtre
## supérieur à 10.
subset(USArrests, Murder > 10, select = Assault)

###
### FACTEUR  
###

## Les facteurs jouent un rôle important en analyse de
## données, surtout pour classer des données en diverses
## catégories. Les données d'un facteur devraient normalement
## afficher un fort taux de redondance.
##
## Reprenons l'exemple du chapitre.
(grandeurs <-
     factor(c("S", "S", "L", "XL", "M", "M", "L", "L")))
levels(grandeurs)          # catégories
as.integer(grandeurs)      # représentation interne

## Dans le présent exemple, nous pourrions souhaiter que R
## reconnaisse le fait que S < M < L < XL. C'est possible avec
## les facteurs *ordonnés*.
factor(c("S", "S", "L", "XL", "M", "M", "L", "L"),
       levels = c("S", "M", "L", "XL"),
       ordered = TRUE)

###
### APPLICATION POUR LES GROUPES DE DONNÉES
###

## Le jeu de données 'airquality' livré avec R contient les
## mesures quotidiennes de la qualité de l'air à New York
## entre mai et septembre 1973.
?airquality                # rubrique d'aide du jeu de données

## La colonne 'Temp' contient la température du jour et la
## colonne 'Month', le mois (sous forme d'entier de 5 à 9).
##
## La fonction 'tapply' permet de calculer facilement la
## température moyenne par mois.
tapply(airquality$Temp, airquality$Month, mean)

## Équivalent (sauf pour la présentation des résultats).
by(airquality$Temp, airquality$Month, mean)

###
### DATE  
###

## Votre premier réflexe pour représenter une date pourrait
## être d'utiliser simplement une chaine de caractères. Ça
## suffit si les dates sont à intervalles égaux et pour
## identifier les points sur un graphique. Dès lors que vous
## devez tenir compte de l'écart entre les dates ou effectuer
## des calculs avec les dates, il vous faut une solution plus
## robuste.
##
## R utilise la représentation standard des systèmes Unix
## consignée dans la norme POSIX. En simplifiant, le temps y
## est compté (en jours pour une date seule, en secondes pour
## une date et une heure) à partir du 1er janvier 1970.
##
## Une date n'est donc rien d'autre qu'un entier à l'interne
## pour R. C'est ce qui permet d'effectuer facilement des
## calculs et des comparaisons. Cependant, l'objet est muni
## d'un attribut "class" qui fait en sorte de modifier
## l'affichage et l'interaction avec certaines fonctions.
##
## Pour les dates seules, la classe de base est "Date".
d <- "2000-02-29"          # chaine de caractères
d + 1                      # opération invalide
d <- as.Date(d)            # conversion en date
d + 1                      # opération valide; jour suivant
d - 1                      # jour précédent
as.numeric(d)              # nombre de jours depuis 1970-01-01
d - as.Date("1970-01-01")  # vérification
(auj <- Sys.Date())        # date du jour
auj >= d                   # après le 2000-02-29?

## Chose particulièrement utile, la fonction 'seq' est munie
## d'une méthode pour la classe "Date", ce qui permet de
## générer des suites de dates.
##
## Exemple: générer les dates des 10 prochaines semaines à
## partir d'aujourd'hui.
(dixsem <- seq(Sys.Date(), length.out = 10, by = "1 week"))
?seq.Date                  # voir les autres possibilités

## Attention: dans la génération de suites par mois, le jour du mois
## ne change pas sauf si cela résulte en un jour invalide. Cela arrive
## seulement si la date de départ est le 31 du mois. Dans ce cas,
## c'est le jour du mois suivant qui est utilisé.
seq(as.Date("2042-05-31"), length.out = 4, by = "+1 month")

## Un truc pour se prémunir contre cet éventuel problème consiste à
## ajouter un jour à la date de départ avant de générer la suite, puis
## à soustraire un jour ensuite.
##
## Comme ça, si la date de départ est un 31, on génère la suite à
## partir du 1er du mois suivant pour revenir ensuite en arrière; si
## la date de départ est n'importe quel autre jour dans le mois, ça ne
## change rien.
seq(as.Date("2042-05-31") + 1, length.out = 4, by = "+1 month") - 1

## Quelques fonctions d'extraction utiles.
weekdays(dixsem)           # jours de la semaine
months(dixsem)             # mois
quarters(dixsem)           # trimestres

## Pour enregistrer non seulement une date, mais aussi une
## heure, vous devez utiliser les représentations POSIXct et
## POSIXlt. Il s'agit de classes d'objets très puissantes qui
## permettent d'enregistrer une heure jusqu'à la fraction de
## seconde, le fuseau horaire, s'il s'agit de l'heure d'été ou
## non, etc.
?DateTimeClasses           # *tous* les détails

## Les objets POSIXct et POSIXlt représentent un nombre de
## secondes depuis le 1er janvier 1970.
(auj <- Sys.time())        # date et heure courante
auj + 3600                 # une heure plus tard
auj - 24 * 3600            # hier, même heure
auj - as.POSIXct("2000-02-29") # écart entre deux dates
difftime(auj, as.POSIXct("2000-02-29")) # idem

## La classe POSIXlt est une représentation sous forme de liste
## des informations contenues dans un objet POSIXct. Elle est
## surtout utile pour extraire facilement des informations
## d'une date sous forme numérique, ce qui permet ensuite
## d'effectuer des calculs.
##
## N'hésitez pas à convertir d'un type vers un autre.
class(auj)                 # objet POSIXct
auj <- as.POSIXlt(auj)     # conversion en POSIXlt
unclass(auj)               # c'est une liste
auj$hour                   # extraction de l'heure
(m <- auj$mon)             # nombre de mois après janvier
11 - m                     # nombre de mois fin d'année
(y <- auj$year)            # nombre d'année depuis 1900 (!)
y - (2000 - 1900)          # nombre d'années depuis 2000

## La conversion de la classe POSIXct ou POSIXlt vers la
## classe Date laisse tomber l'heure.
d <- as.POSIXct("2000-02-29 10:51:27") # objet POSIXct
as.Date(d)                 # conversion en date seule 

###
### FONCTIONS INTERNES UTILES  
###

## On se donne un vecteur de 16 éléments.
(A <- sample(1:10, 16, replace = TRUE))

## Opérations sur les matrices.
dim(A) <- c(4, 4)          # conversion en une matrice 4 x 4
b <- c(10, 5, 3, 1)        # vecteur quelconque
A                          # matrice 'A'
t(A)                       # transposée
solve(A)                   # inverse
solve(A, b)                # solution de Ax = b
A %*% solve(A, b)          # vérification de la réponse
diag(A)                    # extraction de la diagonale de 'A'
diag(b)                    # matrice diagonale formée avec 'b'
diag(4)                    # matrice identité 4 x 4
(A <- cbind(A, b))         # matrice 4 x 5
nrow(A)                    # nombre de lignes de 'A'
ncol(A)                    # nombre de colonnes de 'A'
rowSums(A)                 # sommes par ligne
colSums(A)                 # sommes par colonne  

###
### FONCTION 'outer'  
###

## La fonction 'outer' applique une fonction (le produit par
## défaut, d'où le nom de la fonction, dérivé de «produit
## extérieur») à toutes les combinaisons des éléments de ses
## deux premiers arguments.
x <- c(1, 2, 4, 7, 10, 12)
y <- c(2, 3, 6, 7, 9, 11)
outer(x, y)                # produit extérieur
x %o% y                    # équivalent plus court

## Pour effectuer un calcul autre que le produit, on spécifie
## la fonction à appliquer en troisième argument. Si la
## fonction est un des opérateurs arithmétiques de base, il
## faut placer le symbole entre guillemets " ".
outer(x, y, "+")           # «somme extérieure»
outer(x, y, "<=")          # toutes les comparaisons possibles
outer(x, y, function(x, y) x + 2 * y) # fonction quelconque  
