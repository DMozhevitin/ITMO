#include "term.hpp"
#include <iostream>
#include <algorithm>
#include "binary_search_deluxe.hpp"
#include <functional>
#include "autocomplete.hpp"

using namespace std;

int main() {
    vector<term> a = {term("abcfff", 1), term("abcggg", 1), term("abdffz", 1)};
    cout << "TERMS LIST: " << endl;

    for (const auto &i : a) {
        cout << i.to_string() << endl;
    }

    autocomplete aut(a);
    cout << "All matches of 'abc'" << endl;
    cout << aut.number_of_matches("abc") << endl;
    vector<term> matches = aut.all_matches("abc");
    for  (const auto &i : matches) {
        cout << i << endl;
    }

    cout << endl;

    cout << "All matches of 'ab'" << endl;
    cout << aut.number_of_matches("ab") << endl;
    matches = aut.all_matches("ab");
    for  (const auto &i : matches) {
        cout << i << endl;
    }

    cout << endl;

    cout << "All matches of  'bcd'" << endl;
    cout << aut.number_of_matches("bcd") << endl;
    matches = aut.all_matches("bcd");
    for  (const auto &i : matches) {
        cout << i << endl;
    }
    cout << endl;

    cout << "All matches of  'abcffffffffffffffffff'" << endl;
    cout << aut.number_of_matches("abcffffffffffffffffff") << endl;
    matches = aut.all_matches("abcffffffffffffffffff");
    for  (const auto &i : matches) {
        cout << i << endl;
    }
    cout << endl;

    return 0;
}




