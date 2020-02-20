#include <iostream>
#include <unordered_map>

using namespace std;

typedef int16_t sym_t;

/* e ::= "..."     symbol
 *
 *
 */

int main() {

  unordered_map<sym_t, string> symbol_table;

  symbol_table.insert( pair<sym_t, string>( 0, "x" ) );

  string name { symbol_table.at( 0 ) };

  constexpr int a = 5;
  constexpr int b = 6;
  constexpr int c = a+b;

  cout << name << '\n' << c << '\n';
  return 0;
}