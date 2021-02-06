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

deque<int> q;

int main() {
//    ios::sync_with_stdio(NULL);
//    cin.tie(NULL);
//    cout.tie(NULL);

    //freopen("antigray.in", "r", stdin);
    //freopen("antigray.out", "w", stdout);

    int n;
    cin >> n;

    for (int i = 0; i < n; i++) {
        int op, id;
        cin >> op;

        if (op == 1) {
            cin >> id;
            q.pb(id);
        }

        if (op == 2) {
            q.pop_front();
        }

        if (op == 3) {
            q.pop_back();
        }

        if (op == 4) {
            cin >> id;
            for (int j = 0; j < q.size(); j++) {
                if (q[j] == id) {
                    cout << j << endl;
                    break;
                }
            }
        }

        if (op == 5) {
            cout << q.front() << endl;
        }
    }
    //fclose(stdin);
    //fclose(stdout);
    ret