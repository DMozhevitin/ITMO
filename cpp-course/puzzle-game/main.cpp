#include <iostream>
#include "puzzle_game.h"
#include <vector>
#include <random>

using namespace std;

int main() {
    vector<vector<vector<size_t>>> test = {{{1, 2, 3, 4},
                                                   {5, 6, 7, 8},
                                                   {9, 10, 11, 12},
                                                   {13, 14, 0,  15}},

                                           {{1, 2, 3, 4},
                                                   {5, 6, 7, 8},
                                                   {9, 10, 11, 12},
                                                   {13, 14, 15, 0}},

                                           {{1, 2, 3, 4},
                                                   {0, 5, 6, 7},
                                                   {9, 10, 11, 8},
                                                   {13, 14, 15, 12}},

                                           {{1, 2, 3, 0},
                                                   {5, 6, 7, 4},
                                                   {9, 10, 11, 8},
                                                   {13, 14, 15, 12}}};


    for (const auto &i : test) {
        cout << "TEST: " << endl;
        cout << "START BOARD : " << endl;
        for (const auto &j : i) {
            for (const auto &k : j) {
                cout << k << " ";
            }
            cout << endl;
        }

        cout << endl;

        board b(i);
        solver s(b);

        cout << "moves = " << s.moves() << endl;
        for (const auto &it : s) {
            cout << it.to_string() << endl;
        }

        cout << "---------------------\n";
    }

    cout << "RANDOM TEST: \n";
    board b(3);
    cout << "START BOARD: \n" + b.to_string();
    solver s(b);
    cout << "moves = " << s.moves() << endl;
    for (const auto &it : s) {
        cout << it.to_string() << endl;
    }
}