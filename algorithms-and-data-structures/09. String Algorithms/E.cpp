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
const ll INF = 1e9 + 100;
const ll NO_EDGE = 100000;
const ll M = 1e3 + 100;
const ull prime = 31;
const char delimiters[] = {'#', '$', '@', '\0', '+', '(', ')', '.', '0', '1'};
#define x first
#define y second
#define pb push_back

string s;
vector<int> classes, suffixArray, t, lcp;
int k;
unordered_set<int> colorsSet;

void build(int v, int left, int right)
{
    if (left == right)
    {
        t[v] = lcp[left];
        return;
    }

    int mid = (left + right) / 2;
    build(v * 2 + 1, left, mid);
    build(v * 2 + 2, mid + 1, right);
    t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}

void update(int i, int x, int v, int left, int right)
{
    if (left == right)
    {
        t[v] = x;
        return;
    }

    int mid = (left + right) / 2;

    if (i <= mid)
    {
        update(i, x, v * 2 + 1, left, mid);
    } else
    {
        update(i, x, v * 2 + 2, mid + 1, right);
    }

    t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

int min(int v, int l, int r, int qleft, int qright)
{
    if (l >= qleft && r <= qright)
    {
        return t[v];
    }

    if (r < qleft || l > qright)
    {
        return INF;
    }

    int mid = (l + r) / 2;

    int min1 = min(v * 2 + 1, l, mid, qleft, qright);
    int min2 = min(v * 2 + 2, mid + 1, r, qleft, qright);
    return min(min1, min2);
}

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
    size_t n, prevIndex = 0;
    cin >> n;

    s = "";
    vector<size_t> colors, indexes;
    size_t color = 0;
    size_t delimiterIndex = 0;
    for (size_t i = 0; i < n; i++)
    {
        string t;
        cin >> t;
        s += (t + delimiters[delimiterIndex]);
        delimiterIndex++;
        indexes.pb(s.size() - 1);
    }

    suffixArray = createSuffixArray();
    lcp = buildLcp(s, suffixArray);
    t.resize(lcp.size() * 4);
    build(0, 0, lcp.size() - 1);
    colors.resize(suffixArray.size());

    for (size_t i = 0; i < suffixArray.size(); i++)
    {
        if (i > indexes[color])
        {
            color++;
        }
        colors[i] = color;
    }

    size_t maxColor = color + 1;
    vector<int> colorsCount(maxColor);
    size_t left = n, right = n - 1;
    int ans = -1;
    int index = -1;

    while (true)
    {
        while (right < suffixArray.size() - 1 && colorsSet.size() < maxColor)
        {
            right++;
            int currentColor = colors[suffixArray[right]];
            colorsSet.insert(currentColor);
            colorsCount[currentColor]++;
        }

        while (colorsSet.size() >= maxColor)
        {
            int x = min(0, 0, lcp.size() - 1, left, right - 1);
            if (x > ans)
            {
                ans = max(ans, x);
                index = suffixArray[left];
            }

            int currentColor = colors[suffixArray[left]];
            colorsCount[currentColor]--;
            if (colorsCount[currentColor] == 0)
            {
                colorsSet.erase(currentColor);
            }

            left++;
        }

        if (right == suffixArray.size() - 1)
        {
            break;
        }
    }

    if (index != -1)
    {
        cout << s.substr(index, ans);
    }
}