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
typedef pair<long double, long double> point;

int main()
{
    int n, m, k;
    cin >> n >> m >> k;

    vector<vector<int>> class2objnums(m, vector<int>());
    vector<vector<int>> parts(k, vector<int>());

    for (size_t i = 0; i < n; i++)
    {
        int x;
        cin >> x;
        class2objnums[x - 1].push_back(i + 1);
    }


    int j = 0;
    for (size_t i = 0; i < m; i++)
    {
        while (!class2objnums[i].empty())
        {
            parts[j].push_back(class2objnums[i].back());
            class2objnums[i].pop_back();
            j = (j + 1) % k;
        }
    }

    for (size_t i = 0; i < k; i++)
    {
        cout << parts[i].size() << " ";
        for (int item : parts[i])
        {
            cout << item << " ";
        }
        cout << endl;
    }

    return 0;
}
 