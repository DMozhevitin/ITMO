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

    unordered_map<int, ld> px;
    unordered_map<int, unordered_map<int, ld>> pxy;

    for (size_t i = 0; i < n; i++)
    {
        int x, y;
        cin >> x >> y;
        px[x]++;
        pxy[x][y]++;
    }

    for (auto &it : px)
    {
        it.second /= n;
    }

    for (auto &it : pxy)
    {
        for (auto &jt : it.second)
        {
            jt.second /= n;
        }
    }

    ld ans = 0;
    for (auto &it : pxy)
    {
        for (auto jt : it.second)
        {
            int x = it.first;
            ld v = jt.second;
            ans -= v * (log(v) - log(px[x]));
        }
    }
    cout << setprecision(16) << fixed;
    cout << ans << endl;
    return 0;
}
 