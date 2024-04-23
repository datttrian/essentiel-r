# Apprendre à créer des variables en R , à les effacer et à faire des opérations

# variable algébriqe scalaire
# affecter une valeur 
a <- 5

# pas clair si chiffre négatif
b <--5

# il existe le =
b = 6

# variable algebrique complexe
z <- complex(real=3, imaginary = 2)
Re(z)
Im(z)
#x+x : somme, division, multiplication

#Créer une variable `sherlock` avec la valeur `"Sherlock Holmes"`.
sherlock <- 'Sherlock Holmes'

#Créer une variable `watson` avec la valeur `"Dr. Watson"`
watson <- 'Dr. Watson'

#Créer une variable `sherlock_watson` ayant pour valeur `Sherlock Holmes et Dr. Watson`.
sherlock_watson <- paste(sherlock, watson)

## Pour obtenir la longueur de la chaîne, il faut utiliser
nchar(sherlock_watson)
