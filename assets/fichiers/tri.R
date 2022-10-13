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
### BOUCLES ITÉRATIVES  
###

## Débutons par illustrer les boucles à dénombrement avec des
## boucles triviales qui ne font qu'afficher des valeurs à
## l'écran.
##
## La syntaxe de la déclaration d'une boucle 'for' dans R est
## la suivante:
##
##   for(<variable> in <suite>)
##
## La <variable> est un compteur (ou itérateur) et <suite> est
## une expression qui permet de créer un vecteur (ou une
## liste) des valeurs successives du compteur.
##
## Plus souvent qu'autrement, l'expression fait appel à une
## fonction de génération de suites de valeurs.
for (n in 1:10)
    print(n)

## Je recommande d'utiliser les fonctions 'seq_len' et
## 'seq_along', ou alors 'seq' avec l'argument 'length.out',
## afin de vous prémunir contre le risque de générer une suite
## non vide alors le nombre d'itérations de la bouche devrait
## être zéro.
##
## Voici un exemple où les deux approches sont équivalentes.
n <- 5
for (i in 1:n) print(i)        # approche avec ':'
for (i in seq_len(n)) print(i) # approche avec 'seq_len'

## Si le nombre d'itérations devait toutefois être nul,
## l'approche avec ':' ne donne pas le résultat escompté;
## celle avec 'seq_len', oui.
n <- 0
for (i in 1:n) print(i)        # deux itérations!
for (i in seq_len(n)) print(i) # aucune itération

## De manière équivalente, s'il faut répéter une boucle un
## nombre de fois égal à la longueur d'un vecteur déjà connu,
## la fonction 'seq_along' permet de générer la suite de
## valeurs de manière robuste.
x <- c(5, 32, 57, 42, 0)         # vecteur de longueur 5...
for (i in seq_along(x)) print(i) # ... 5 itérations
x <- numeric(0)                  # vecteur vide...
for (i in seq_along(x)) print(i) # ... 0 itération

## Il y a une petite subtilité avec les boucles 'for' à
## laquelle vous devez faire bien attention: la classe du
## vecteur (ou de la liste) créé par l'expression n'est PAS
## prise en compte.
##
## Vous rencontrerez ce cas si, par exemple, vous souhaitez
## itérer sur un vecteur de dates. Rappelez-vous: une date
## dans R n'est qu'un nombre entier «maquillé» pour être plus
## lisible lorsque présenté à l'écran. (Ce «maquillage» est
## fourni par la classe de l'objet.)
dates <- seq(as.Date("2042-05-09"), by = "+1 month",
             length.out = 6)
dates                      # objet maquillé
unclass(dates)             # sans classe, sans maquillage

## Si l'on essaie d'itérer sur un vecteur de dates, la classe
## (le maquillage) ne suit pas dans la boucle.
for (d in dates)
    print(d)

## Si vous devez utiliser les dates successives sous forme de
## chaines de caractères à l'intérieur de la boucle, vous
## pouvez soit indicer le vecteur de dates dans la boucle,
## soit «cacher» les dates à l'intérieur d'une liste.
for (i in seq_along(dates))
    print(dates[i])
for (d in as.list(dates))
    print(d)

## Passons maintenant aux boucles 'while' et 'repeat'.
##
## Nous allons illustrer leur utilisation avec la méthode
## numérique du point fixe. On dit qu'une valeur x est un
## «point fixe» d'une fonction f si cette valeur satisfait
## l'équation
##
##   x = f(x).
##
## La méthode numérique de recherche du point fixe d'une
## fonction f est simple et puissante: elle consiste à choisir
## une valeur de départ, puis à évaluer successivement f(x),
## f(f(x)), f(f(f(x))), ... jusqu'à ce que la valeur change
## «peu».
##
## L'algorithme est donc très simple:
##
## 1. Choisir une valeur de départ y.
## 2. Calculer x = f(y)
## 3. Si |x - y|/|x| >= e, poser y <- x et retourner à
##    l'étape 2.
## 4. Retourner x.
##
## Avant de poursuivre votre lecture, tentez d'identifier le
## meilleur type de boucle ('for', 'while' ou 'repeat') à
## utiliser pour programmer cet algorithme.

## La méthode de Newton du calcul de la racine carrée par
## approximations successives est un cas spécial de la méthode
## du point fixe. En effet, la racine carrée d'un nombre est
## la valeur positive de y satisfaisant l'équation y^2 = x.
## Cette équation peut se réécrire sous forme de point fixe
## ainsi:
##
##   y = (y + x/y)/2.
##
## Voici une nouvelle mise en oeuvre de la fonction 'sqrt' qui
## utilise la méthode du point fixe. Le critère d'arrêt y est
## exprimé non plus en fonction de l'écart entre 'y'^2 et 'x',
## mais plutôt en fonction de l'écart entre deux
## approximations successives. De plus, la valeur de départ et
## l'erreur d'approximation sont passées en argument à la
## fonction.
##
## Puisqu'il faut au minimum vérifier si la valeur initiale
## est un point fixe, nous utilisons une boucle 'repeat'.
sqrt <- function(x, start = 1, TOL = 1E-10)
{
    repeat
    {
        y <- (start + x/start)/2
        if (abs(y - start)/y < TOL)
            break
        start <- y
    }
    y
}

## Vérifions la validité de la fonction.
sqrt(9, 1)
sqrt(225, 1)
sqrt(3047, 50)

## Formidable. Toutefois, si nous voulions utiliser la méthode
## du point fixe pour résoudre une autre équation, il faudrait
## écrire une nouvelle fonction qui serait pour l'essentiel
## identique, sinon pour le calcul de la fonction
## (mathématique) f(x) pour laquelle nous cherchons le point
## fixe.
##
## Créons donc une fonction de point fixe générale qui prendra
## la fonction mathématique f(x) en argument.
fixed_point <- function(FUN, start, TOL = 1E-10)
{
    repeat
    {
        x <- FUN(start)
        if (abs(x - start)/x < TOL)
            break
        start <- x
    }
    x
}

## Nous pouvons ensuite écrire une nouvelle fonction 'sqrt'
## qui utilise 'fixed_point'. Nous y ajoutons un test de
## validité de l'argument, pour faire bonne mesure.
sqrt <- function(x)
{
    if (x < 0)
        stop("cannot compute square root of negative value")

    fixed_point(function(y) (y + x/y)/2, start = 1)
}

## Validation. Nous obtenons les mêmes résultats que
## précédemment.
sqrt(9)
sqrt(25)
sqrt(3047)

## Suppression de la fonction pour éviter qu'elle n'entre en
## conflit avec celle de R.
rm("sqrt")

## SYNDROME DE LA PLAQUE À BISCUITS

## La fonction ci-dessous calcule les 'nterm' premiers termes
## de la suite de Fibonacci. Elle souffre toutefois du
## Syndrome de la plaque à biscuits. (Identifiez pourquoi.)
fibonacci0 <- function(nterm)
{
    if (nterm < 1)
        stop("'nterm' doit être supérieur ou égal à 1")
    if (nterm == 1)
        return(0)
    x <- c(0, 1)
    for (i in seq_len(nterm - 2))
        x[i + 2] <- x[i + 1] + x[i]
    x
}

## Validation de la fonction
fibonacci0(1)
fibonacci0(2)
fibonacci0(5)

## Une seconde version de la fonction prend garde de d'abord
## créer un vecteur de la bonne longueur pour stocker tous les
## résultats, puis de le remplir graduellement. (Le premier
## terme du vecteur est déjà 0 suite à l'initialisation avec
## 'numeric'.)
fibonacci <- function(nterm)
{
    if (nterm < 1)
        stop("'nterm' doit être supérieur ou égal à 1")
    if (nterm == 1)
        return(0)
    x <- numeric(nterm)
    x[2] <- 1
    for (i in seq_len(nterm - 2))
        x[i + 2] <- x[i + 1] + x[i]
    x
}

## Validation de la fonction
fibonacci(1)
fibonacci(2)
fibonacci(5)

## Avons-nous vraiment gagné en efficacité? En comparant le
## temps requis pour calculer plusieurs valeurs de la suite de
## Fibonacci pour chaque fonction, vous pourrez constater que
## la seconde version est entre trois et quatre fois plus
## rapide!
system.time(fibonacci0(1e6))
system.time(fibonacci(1e6))
