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

stack<int> st;
vector<string> ans;
vector<int> tmp;
int main() {
    int n;
    cin >> n;

    int x;
    cin >> x;
    st.push(x);
    ans.emplace_back("push");

    for (int i = 1; i < n; i++) {
        cin >> x;
        while (!st.empty() && x > st.top()) {
            tmp.pb(st.top());
            st.pop();
            ans.emplace_back("pop");
        }
        st.push(x);
        ans.emplace_back("push");
    }

    while (!st.empty()) {
        ans.emplace_back("pop");
        tmp.pb(st.top());
        st.pop();
    }

    for (int i = 1; i < tmp.size(); i++) {
        if (tmp[i] < tmp[i - 1]) {
            return cout << "impossible", 0;
        }
    }

    for (auto elem : ans) {
        cout << elem << endl;
    }
    return 0;
}