# NAIF

## Distance de Levenshtein
#' @import tidyverse
#' @import corpus
#' @import rvest
#' @import tokenizers
#' @import stringr
Levenshtein <- function(str1, str2){
  # conversion en vecteur de caractères pour accéder aux caractères 1 par 1
  str1 <- unlist(strsplit(str1, split = "")) # Est-ce une bonne idée d'enlever les espace
  str2 <- unlist(strsplit(str2, split = ""))

  # longueurs des 2 chaînes de caractères
  n <- length(str1)
  m <- length(str2)

  # cas particuliers : si l'une des 2 chaînes est vide
  if (n == 0){return(m)}
  if (m == 0){return(n)}

  # init de la matrice des distances de levenshtein + coût de substitution ou non (= entier)
  dist <- matrix(0, n+1, m+1)
  substitution <- matrix(0, n, m)

  for (i in 0:n){
    dist[i+1, 1] <- i
  }

  for (j in 0:m){
    dist[1, j+1] <- j
  }

  for (i in 2:(n+1)){
    for (j in 2:(m+1)){
      if (str1[i-1] == str2[j-1]){ # si les caractères sont égaux, pas de substitution
        substitution[i-1, j-1] <- 0
      }else{
        substitution[i-1, j-1] <- 1
      }

      dist[i, j] <- min(dist[i-1, j] + 1, dist[i, j-1] + 1, dist[i-1, j-1] + substitution[i-1, j-1]) # 1°:suppression du caractère de str1; 2°:insertion dans str2; 3°:substitution
    }
  }

  return(dist[n+1, m+1])
}

## Split texte -> vecteur de phrases
#' @import tidyverse
#' @import corpus
#' @import rvest
#' @import tokenizers
#' @import stringr
my_split <- function(texte){
  tokens <- str_to_lower(tokenize_paragraphs(tokenize_sentences(texte)[[1]]))
  return(tokens)
}

## Fonction plagiat
#' @import tidyverse
#' @import corpus
#' @import rvest
#' @import tokenizers
#' @import stringr
plagiat_naif <- function(texte1, texte2){
  texte1 <- my_split(texte1) # candidat
  texte2 <- my_split(texte2) # référence
  desired_length <- length(texte2)
  LevDis <- rep(NA, desired_length)

  nbr_phrases_plagiat <- 0

  # distance de Levenshtein pour chaque phrase
  for (i in 1:length(texte1)){
    for (j in 1:length(texte2)){
      dist <- Levenshtein(texte1[i], texte2[j])
      LevDis[j] <- dist
    }
    if (min(LevDis) < nchar(texte1[i])/2){
      nbr_phrases_plagiat = nbr_phrases_plagiat + 1
    }
  }
  return(paste(paste("Pourcentage de plagiat :", 100 * nbr_phrases_plagiat/length(texte1)), "%"))
}


# OPTI

## Matrice de scores
Scores_matrix <- function(str1, str2, gap_cost){
  n <- length(str1)
  m <- length(str2)

  S <- matrix(0, n+1, m+1)

  for (i in 2:n){
    for (j in 2:m){
      hit_score <- S[i-1, j-1] + 2*(str1[i-1] == str2[j-1])-1 # +1 si mêmes caractères (hit), -1 si différents (=> remplacement)
      deletion_score <- S[i-1,j] + gap_cost # suppression + gap
      insertion_score <- S[i, j-1] + gap_cost # insertion + gap
      S[i,j] <- max(hit_score, deletion_score, insertion_score, 0) # pour ne pas avoir de score négatif
    }
  }
  return(S)
}



## Fonction de traceback
Traceback <- function(scores_matrix){
  p <- which(scores_mat == max(scores_mat), arr.ind = T)[1]
  # ligne
  q <- which(scores_mat == max(scores_mat), arr.ind = T)[2]
  # colonne

  index <- c() # vecteur des indices des mêmes caractères (lecture : <-)
  index <- append(index, p)

  while(scores_mat[p,q] > 0){
    # si toutes les directions = 0 => 'diag' & fin
    if (max(scores_mat[p-1,q], scores_mat[p,q-1], scores_mat[p-1,q-1]) == 0){p <- p-1; q <- q-1; index <- append(index, p); break}

    # si 'up' = max
    if (which.max(list(scores_mat[p-1,q], scores_mat[p,q-1], scores_mat[p-1,q-1])) == 1){p <- p-1; q <- q}

    # si 'left' = max
    if (which.max(list(scores_mat[p-1,q], scores_mat[p,q-1], scores_mat[p-1,q-1])) == 2){p <- p; q <- q-1}

    # si 'diag' = max
    if (which.max(list(scores_mat[p-1,q], scores_mat[p,q-1], scores_mat[p-1,q-1])) == 3){p <- p-1; q <- q-1}

    index <- append(index, p)
  }

  sequence <- c() # vecteur de la séquence similaire
  for (i in index[length(index):1]){ # lire les indices de droite à gauche
    sequence <- append(sequence, str1[i])
  }

  return(sequence)
}

# fonction traceback pour retrouver la s?quence similaire
Traceback <- function(scores_matrix, str1){
  p <- which(scores_matrix == max(scores_matrix), arr.ind = T)[1]
  # ligne
  q <- which(scores_matrix == max(scores_matrix), arr.ind = T)[2]
  # colonne

  index <- c() # vecteur des indices des m?mes caract?res (lecture : <-)
  index <- append(index, p)

  while(scores_matrix[p,q] > 0){
    # si toutes les directions = 0 => 'diag' & fin
    if (max(scores_matrix[p-1,q], scores_matrix[p,q-1], scores_matrix[p-1,q-1]) == 0){p <- p-1; q <- q-1; index <- append(index, p); break}

    # si 'up' = max
    if (which.max(list(scores_matrix[p-1,q], scores_matrix[p,q-1], scores_matrix[p-1,q-1])) == 1){p <- p-1; q <- q}

    # si 'left' = max
    if (which.max(list(scores_matrix[p-1,q], scores_matrix[p,q-1], scores_matrix[p-1,q-1])) == 2){p <- p; q <- q-1}

    # si 'diag' = max
    if (which.max(list(scores_matrix[p-1,q], scores_matrix[p,q-1], scores_matrix[p-1,q-1])) == 3){p <- p-1; q <- q-1}

    index <- append(index, p)
  }
  sequence = str1[rev(index)]

  return(sequence)
}

## Smith-Waterman
Smith_Waterman <- function(str1, str2, gap_cost = -1){
  str1 <- unlist(strsplit(str1, split = ""))
  str2 <- unlist(strsplit(str2, split = ""))

  # Matrice des scores
  scores_mat <- Scores_matrix(str1, str2, gap_cost)

  # Séquence
  sequence <- Traceback(scores_mat)

  return(list(matrix_scores = scores_mat, sequence = sequence))
}
