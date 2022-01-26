#include <Rcpp.h>
using namespace Rcpp;

//' Algorithmes pour la détection de plagiat de texte en C++

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}


/*
Algo Naïf
*/
//' Algorithmes naïf distance de Levenshtein en C++
//'
//' @params 2 chaînes de caractères
//' @return un entier représentant la distance entre les 2 chaînes de caractères
//' @export


/*
Algo Optimisé
*/
//' Fonction changeant les majuscules en minuscules en C++
//'
//' @params 1 caractère
//' @return 1 carcatère
//' @export
// [[Rcpp::export]]
char cpp_to_lowercase(char c)
{
  if (c >= 'A' && c <= 'Z') {
    return c + 32;
  }

  return c;
}


//' Fonction transformant un texte en vecteur de phrases en C++
//'
//' @params 1 chaîne de caractères (longueur texte)
//' @return un vecteur de chaînes de caractères (longueur phrases)
//' @export
// [[Rcpp::export]]
std::vector<std::string> cpp_tokenizer(std::string texte)
{
  for (char &c: texte) {
    c=cpp_to_lowercase(c);
  }
  //texte.erase(std::remove(texte.begin(), texte.end(), ' '), texte.end()); // delete " "

  std::istringstream inFile(texte);

  std::vector<std::string> lines; // This vector will contain all lines.

  for (std::string str; std::getline(inFile, str, '\n');)
  {
    lines.push_back(std::move(str)); // Avoid the copy.
  }

  return lines;
}


//' Fonction pour accéder à la longueur d'une chaîne de caractères en C++
//'
//' @params 1 chaîne de caractères
//' @return un entier représentant la longueur de la chaîne de caractères
//' @export
// [[Rcpp::export]]
int cpp_get_length(std::string texte){
  std::vector<std::string> temp = cpp_tokenizer(texte);
  int l=temp.size();
  return l;
}


//' Fonction distance de Levenshtein en C++
//'
//' @params 2 chaînes de caractères
//' @return un entier représentation la distance entre les 2 chaînes de caractères
//' @export
// [[Rcpp::export]]
int cpp_Levenshtein(std::string str1, std::string str2){

  //std::vector<std::string> v1 = cpp_tokenizer(texte1);
  //std::vector<std::string> v2 = cpp_tokenizer(texte2);

  int n = str1.length();
  int m = str2.length();

  if (n == 0){return m;}
  if (m == 0){return n;}

  NumericMatrix dist(n+1,m+1);
  NumericMatrix substitution(n,m);


  for (int i = 0; i < n; ++i){
    dist(i+1,1)=i;
  }
  for (int j = 0; j < m; ++j){
    dist(1,j+1)=j;
  }

  for (int k = 1; k < n+1; ++k){
    for (int l = 1; l < m+1; ++l){
      if (str1[k-1]==str2[l-1]){
        substitution(k-1,l-1)=0;
      }
      if (str1[k-1]!=str2[l-1]){
        substitution(k-1,l-1)=1;
      }

      dist(k,l) = std::min(dist(k-1,l)+1, std::min(dist(k,l-1)+1,dist(k-1,l-1)+substitution(k-1,l-1)));
    }
  }

  return dist(n,m);
}


//' Fonction évaluant le pourcentage de plagiat en C++
//'
//' @params 2 chaînes de caractères
//' @return un float pourcentage de plagiat entre les 2 chaînes de caractères
//' @export
// [[Rcpp::export]]
int cpp_plagiat_Levenshtein(std::string texte1, std::string texte2){
  std::vector<std::string> v1 = cpp_tokenizer(texte1);
  std::vector<std::string> v2 = cpp_tokenizer(texte2);

  int desired_length = v2.size();

  std::vector<int> LevDis;

  int nbr_phrases_plagiat = 0;

  for (int i = 0; i < v1.size(); ++i){
    for (int j = 0; j < v2.size(); ++j){
      int dist = cpp_Levenshtein(v1[i], v2[j]);
      LevDis.push_back(dist);
    }

    double threshold = v1[i].size()/2;

    //double* ptr = &threshold;

    if (*std::min_element(LevDis.begin(), LevDis.end()) < threshold){
      nbr_phrases_plagiat = nbr_phrases_plagiat + 1;
    }
  }
  int v1size = v1.size();
  double percentage = 100 * nbr_phrases_plagiat/v1size;
  Rcout << "Pourcentage de plagiat : " << percentage << "%\n";
  return 0;
}


/*
Algo Smith-Waterman
*/
//' Fonction créant une matrice de scores en C++
//'
//' @params 2 chaînes de caractères et 1 entier
//' @return une matrice d'entiers
//' @export
// [[Rcpp::export]]
NumericMatrix cpp_scores_matrix(std::string str1, std::string str2, int gap_cost){

  int n = str1.length();

  int m = str2.length();

  NumericMatrix S(n+1,m+1);

  int hit_score, deletion_score, insertion_score;

  for(int i = 1; i < n; ++i){

    for(int j = 1; j < m; ++m){

      if (str1[i-1] == str2[j-1]){
        hit_score = S(i-1, j-1) + 1;
      }

      if (str1[i-1] != str2[j-1]){
        hit_score = S(i-1, j-1) - 1;
      }

      deletion_score = S(i-1,j) + gap_cost;

      insertion_score = S(i, j-1) + gap_cost;

      S(i,j) = std::max(std::max(std::max(hit_score, deletion_score), insertion_score), 0);
    }
  }
  return S ;
}


//' Fonction traceback en C++
//'
//' @params 1 matrice d'entiers et 1 chaîne de caractères
//' @return 1 chaîne de caractères
//' @export
// [[Rcpp::export]]
std::string cpp_Traceback(NumericMatrix scores_matrix, std::string str1){

  int p = scores_matrix.nrow();
  int q = scores_matrix.ncol();

  int row,col,max;

  max = scores_matrix(0,0);
  row = 0;
  col = 0;

  for(int i=0; i < p; ++i){

    for(int j=0; j<4; ++j){

      if (scores_matrix(i,j) > max){

        max = scores_matrix(i,j);
        row = i;
        col = j;

      }
    }
  }

  std::vector<int> ind;
  ind.push_back(row);

  while(scores_matrix(p,q) > 0){

    if (std::max(std::max(scores_matrix(p-1,q), scores_matrix(p,q-1)), scores_matrix(p-1,q-1)) == 0){
      p = p-1;
      q = q-1;
      ind.push_back(p);
    }

    if (std::max(std::max(scores_matrix(p-1,q), scores_matrix(p,q-1)), scores_matrix(p-1,q-1)) == scores_matrix(p-1,q)){
      p = p-1;
      //q = q;
    }

    if (std::max(std::max(scores_matrix(p-1,q), scores_matrix(p,q-1)), scores_matrix(p-1,q-1)) == scores_matrix(p,q-1)){
      //p = p;
      q = q-1;
    }

    if (std::max(std::max(scores_matrix(p-1,q), scores_matrix(p,q-1)), scores_matrix(p-1,q-1)) == scores_matrix(p-1,q-1)){
      p = p-1;
      q = q-1;
    }

    ind.push_back(p);
  }

  // reverse the vector ind
  std::reverse(ind.begin(),ind.end());

  std::string sequence;

  for(int k=0; k<ind.size(); ++k){
    sequence.push_back(str1[k]);
  }
  return sequence;
}


//' Algorithmes Smith-Waterman en C++
//'
//' @params 2 chaînes de caractères et 1 entier
//' @return une liste contenant 1 matrice d'entiers et 1 chaîne de caractères
//' @export
// [[Rcpp::export]]
List cpp_Smith_Waterman(std::string texte1, std::string texte2, int gap_cost){
  for (char &c: texte1){
    c=cpp_to_lowercase(c);
  }

  for (char &c: texte2){
    c=cpp_to_lowercase(c);
  }

  NumericMatrix scores_mat = cpp_scores_matrix(texte1, texte2, gap_cost);

  std::string seq = cpp_Traceback(scores_mat, texte1);

  List z = List::create(scores_mat, seq);

  return z;
}
