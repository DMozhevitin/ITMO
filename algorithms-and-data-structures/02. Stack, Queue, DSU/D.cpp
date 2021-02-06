#define _FORTIFY_SOURCE 0
#pragma GCC optimize("Ofast")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,popcnt,abm,mmx,tune=native")
#pragma GCC optimize("fast-math")

#include <iostream>
#include <math.h>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <map>
#include <deque>
#include <stack>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 100;

#define x first
#define  y second
#define pb push_back

deque<int> first_half, second_half;

void balance() {
    while (!second_half.empty() && (first_half.empty() || first_half.size()  < second_half.size())) {
        int g = second_half.front();
        second_half.pop_front();
        first_half.push_back(g);
    }
}

void print() {
    cout << "first half = ";
    for (auto i : first_half) {
        cout << i << " ";
    }

    cout << endl;

    cout << "second half = ";
    for (auto j : second_half) {
        cout << j << " ";
    }

    cout << endl;
}

int main() {
    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        char c;
        cin >> c;

        if (c == '+') {
            balance();
            int last;
            cin >> last;
            second_half.push_back(last);
            balance();
        } else if (c == '*') {
            balance();
            int mid;
            cin >> mid;
            second_half.push_front(mid);
            balance();
        } else {
            balance();
            cout << first_half.front() << endl;
            first_half.pop_front();
            balance();
        }
        //print();
    }

    return 0;
}