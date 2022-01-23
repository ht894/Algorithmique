
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

/*
Algo naïf
*/
// [[Rcpp::export]]
char cpp_to_lowercase(char c)
{
  if (c >= 'A' && c <= 'Z') {
    return c + 32;
  }

  return c;
}


// [[Rcpp::export]]
std::vector<std::string> cpp_tokenizer(std::string texte)
{
  // The function cpp_tokenizer takes a texte as argument and tokenizes it.
  for (char &c: texte) {
    c=cpp_to_lowercase(c);
  }
  texte.erase(std::remove(texte.begin(), texte.end(), ' '), texte.end()); // delete " "

  std::istringstream inFile(texte);

  std::vector<std::string> lines; // This vector will contain all lines.

  for (std::string str; std::getline(inFile, str, '\n');)
  {
    lines.push_back(std::move(str)); // Avoid the copy.
  }

  return lines;
}


// [[Rcpp::export]]
int cpp_get_length(std::string texte){
  std::vector<std::string> temp = cpp_tokenizer(texte);
  int l=temp.size();
  return l;
}


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


// [[Rcpp::export]]
int cpp_plagiat_Levenshtein(std::string texte1, std::string texte2){
  std::vector<std::string> v1 = cpp_tokenizer(texte1);
  std::vector<std::string> v2 = cpp_tokenizer(texte2);

  int desired_length = v2.size();

  NumericVector LevDis(desired_length);

  int nbr_phrases_plagiat = 0;

  for (int i = 0; i < v1.size(); ++i){
    for (int j = 0; j < v2.size(); ++i){
      int dist = cpp_Levenshtein(v1[i], v2[j]);
      LevDis.push_back(dist);
    }

    double threshold = v1[i].size()/2;

    double* ptr = &threshold;

    if (std::min_element(LevDis.begin(), LevDis.end()) < ptr){
      nbr_phrases_plagiat = nbr_phrases_plagiat + 1;
    }
  }
  double percentage = 100 * nbr_phrases_plagiat/v1.size();
  Rcout << "Pourcentage de plagiat : " << percentage << "%\n";
  return 0;
}

/*
Algo amélioré
*/
// [[Rcpp::export]]
NumericMatrix Scores_matrixCpp(std::string str1, std::string str2, int gap_cost){

  int n = str1.length();

  int m = str2.length();

  int hit_score = 0;

  int deletion_score = 0;

  int insertion_score=0;

  NumericMatrix S(n+1,m+1);

  for(int i = 2; i < n; ++i){

    for(int j = 2; i < m; ++m){

      int o = S(i-1,j-1);

      char un =str1[i-1];

      char deux = str2[j-1];

      hit_score =  o + 2*int( un=deux) - 1;

      int p = S(i-1,j);

      deletion_score = p + gap_cost ;

      int k = S(i,j-1);

      insertion_score = k + gap_cost ;

      S(i,j) = std::max(std::max(hit_score, deletion_score), std::max(insertion_score, 0)) ;
    }
  }
  return S ;
}





