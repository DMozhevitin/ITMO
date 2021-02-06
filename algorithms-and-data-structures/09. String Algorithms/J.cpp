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
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <random>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;
typedef unsigned long long ull;
typedef unsigned int uint;

const ll N = 1 * 1e7 + 10;
const ll INF = 1e18 + 100;
const ll NO_EDGE = 100000;
const ll M = 1e3 + 100;
const ull prime = 31;

#define x first
#define y second
#define pb push_back

string s;
vector<int> classes, suffixArray;
int k;

int log_(int n)
{
    int pow = 1, res = 0;
    while (pow < n)
    {
        pow *= 2;
        res++;
    }

    return res;
}

bool cmp0(int i, int j)
{
    return s[i] < s[j];
}

bool cmp1(int i, int j)
{
    return classes[i] < classes[j];
}

bool cmp2(int i, int j)
{
    if (classes[i] == classes[j])
    {
        return cmp1((i + k) % s.size(), (j + k) % s.size());
    }

    return cmp1(i, j);
}

vector<int> createSuffixArray()
{
    vector<int> suffixArray(s.size());
    classes.resize(s.size());

    for (int i = 0; i < s.size(); i++)
    {
        suffixArray[i] = i;
    }

    sort(suffixArray.begin(), suffixArray.end(), cmp0);

    for (int i = 0; i < s.size(); i++)
    {
        classes[i] = s[i] - 'a';
    }


    int log = log_(s.size());

    for (k = 1; k <= (1 << log); k *= 2)
    {
        sort(suffixArray.begin(), suffixArray.end(), cmp2);
        vector<int> newClasses(classes.size());
        newClasses[suffixArray[0]] = 0;

        for (int i = 1; i < s.size(); i++)
        {
            pair<int, int> a = {suffixArray[i - 1], (suffixArray[i - 1] + k) % s.size()},
                    b = {suffixArray[i], (suffixArray[i] + k) % s.size()};

            if (classes[a.x] != classes[b.x] || classes[a.y % s.size()] != classes[b.y % s.size()])
            {
                newClasses[suffixArray[i]] = newClasses[suffixArray[i - 1]] + 1;
            } else
            {
                newClasses[suffixArray[i]] = newClasses[suffixArray[i - 1]];
            }
        }

        classes = newClasses;
    }

    return suffixArray;
}

vector<int> buildLcp(string s, const vector<int> &suffixArray)
{
    vector<int> pos(s.size()), lcp(s.size());
    for (int i = 0; i < s.size() - 1; i++)
    {
        pos[suffixArray[i]] = i;
    }

    int x = 0;
    for (int i = 0; i < s.size() - 1; i++)
    {
        if (x > 0) x--;

        if (pos[i] == s.size() - 1)
        {
            lcp[s.size() - 1] = -1;
            x = 0;
        } else
        {
            int y = suffixArray[pos[i] + 1];
            while (max(i + x, y + x) < s.size() && s[(i + x) % s.size()] == s[(y + x) % s.size()]) x++;
            lcp[pos[i]] = x;
        }
    }

    return lcp;
}

int main()
{
    cin >> s;
    s += '\0';
    suffixArray = createSuffixArray();
    auto lcp = buildLcp(s, suffixArray);

    ll ans = 0;
    for (int i = 1; i < suffixArray.size() - 1; i++)
    {
        ans += (s.size() - suffixArray[i] - 1 - lcp[i]);
    }

    ans += (s.size() - suffixArray.back() - 1);
    cout << ans << endl;

    return 0;
}