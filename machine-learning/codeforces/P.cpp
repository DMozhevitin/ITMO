#include <iostream>
#include <map>
#include <unordered_map>
#include <utility>
#include<vector>
#include <set>
#include <unordered_set>
#include <queue>
#include <algorithm>
#include <numeric>
#include <functional>
#include <cmath>
#include <iomanip>
#include <random>

using namespace std;

typedef long long ll;
typedef long double ld;

int main()
{
    size_t k1, k2, n;
    cin >> k1 >> k2 >> n;

    ld nd = static_cast<ld>(n);

    vector<ld> f1(k1, 0.0);
    vector<ld> f2(k2, 0.0);
    unordered_map<int, unordered_map<int, int>> xy;

    for (size_t i = 0; i < n; i++)
    {
        ll x, y;
        cin >> x >> y;
        x--, y--;
        f1[x]++;
        f2[y]++;
        xy[x][y]++;
    }

    transform(f1.begin(), f1.end(), f1.begin(), [n](ld x) { return x / n; });
    transform(f2.begin(), f2.end(), f2.begin(), [n](ld x) { return x / n; });

    ld ans = n;

    for (auto &it : xy)
    {
        for (auto &jt : it.second)
        {
            ld x = it.first;
            ld y = jt.first;
            ld v = jt.second;

            ld e = f1[x] * f2[y] * nd;
            ld d = v - e;
            ld c = d * d / e;

            ans += c - e;
        }
    }

    cout << setprecision(16) << fixed;
    cout << ans << endl;
    return 0;
}
 