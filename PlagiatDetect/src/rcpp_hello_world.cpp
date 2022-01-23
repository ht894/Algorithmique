
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}


// [[Rcpp::export]]
char cpp_to_lowercase(char c)
{
  if (c >= 'A' && c <= 'Z') {
    return c + 32;
  }

  return c;
}


// [[Rcpp::export]]
std::string delUnnecessary(std::string &str)
{
  int size = str.length();
  for(int j = 0; j<=size; j++)
  {
    for(int i = 0; i <=j; i++)
    {
      if(str[i] == ' ' && str[i+1] == ' ')
      {
        str.erase(str.begin() + i);
      }
      else if(str[0]== ' ')
      {
        str.erase(str.begin());
      }
      else if(str[i] == '\0' && str[i-1]== ' ')
      {
        str.erase(str.end() - 1);
      }
    }
  }
  return str;
}


// [[Rcpp::export]]
std::vector<std::string> cpp_tokenizer(std::string texte)
{
  // The function cpp_tokenizer takes a texte as argument and tokenizes it.
  for (char &c: texte) {
    if (c >= 'A' && c <= 'Z') {
      c=c+32; // to lowercase
    }
  }
  std::istringstream inFile(texte);

  std::vector<std::string> lines; // This vector will contain all lines.

  for (std::string str; std::getline(inFile, str, '\n');)
  {
    lines.push_back(std::move(str)); // Avoid the copy.
  }


  for (int i = 0; i <= lines.size(); ++i){
    lines[i]=delUnnecessary(lines[i]);
  }

  return lines;
}




//int cpp_Levenstein(std::vector<std::string> s1, std::vector<std::string> s2){
//
//}


