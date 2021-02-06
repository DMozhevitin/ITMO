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

const ll N = 1e6 + 10;
const int INF = 1e9 + 100;
const ll NO_EDGE = 100000;
const ll M = 1e3 + 100;
const ull prime = 31;
const size_t alph_size = 27;

#define x first
#define y second
#define pb push_back
#define root 0

vector<int> nodeIndexes;
vector<pair<int, int>> ans;

struct Trie
{
    Trie()
    {
        vertex v;
        v.suffixLink = v.parent = -1;
        v.to.resize(alph_size, -1);
        v.terminal = false;
        t.push_back(v);
    }

    void addString(const string &s, int index)
    {
        int v = 0;
        for (const char &it : s)
        {
            char c = it - 'a';
            if (t[v].to[c] == -1)
            {
                vertex u;
                u.parent = v;
                u.suffixLink = -1;
                u.to.resize(alph_size, -1);
                u.terminal = false;
                u.parentChar = c;

                t[v].to[c] = t.size();
                t.push_back(u);
            }

            v = t[v].to[c];
        }

        nodeIndexes[index] = v;
        t[v].strIndexes.push_back(index);
    }

    void setSuffixLinks()
    {
        queue<int> q;
        q.push(0);

        while (!q.empty())
        {
            int v = q.front();
            setSuffixLink(v, t[v].parentChar);
            q.pop();

            for (char c = 0; c < alph_size; c++)
            {
                if (t[v].to[c] != -1)
                {
                    q.push(t[v].to[c]);
                }
            }
        }

    }

    void markTerminals()
    {
        dfs(root);
    }

    int dfs(int v)
    {
        int terminal = t[v].terminal;

        for (const auto &to : t[v].rsuffixLinks)
        {
            terminal += dfs(to);
        }

        t[v].terminal = terminal;
        return t[v].terminal;
    }

    int next(int v, char c)
    {
        while (v != -1 && t[v].to[c] == -1)
        {
            v = t[v].suffixLink;
        }

        if (v != -1)
        {
            v = t[v].to[c];
        }

        return v == -1 ? root : v;
    }

    void processText(const string &text)
    {
        int v = 0;
        t[v].terminal = true;
        for (int i = 0; i < text.size(); i++)
        {
            char c = text[i] - 'a';
            v = next(v, c);
            if (t[v].first == INF)
            {
                t[v].first = i;
            }

            t[v].last = i;
        }


    }

    void markEntries()
    {
        for (size_t i = 0; i < t.size(); i++)
        {
            int j = i;
            while (j != root)
            {
                t[t[j].suffixLink].first = min(t[j].first, t[t[j].suffixLink].first);
                t[t[j].suffixLink].last = max(t[j].last, t[t[j].suffixLink].last);
                j = t[j].suffixLink;
            }
        }

        for (size_t i = 0; i < t.size(); i++)
        {
            for (const auto &index : t[i].strIndexes)
            {
                ans[index] = {t[i].first, t[i].last};
            }
        }
    }

private:
    struct vertex
    {
        vector<int> to, rsuffixLinks, strIndexes;
        int terminal;
        int first = INF;
        int last = -1;
        int parent, suffixLink;
        char parentChar;
    };

    void setSuffixLink(int v, char c)
    {
        if (v == root)
        {
            t[v].suffixLink = -1;
            return;
        }

        int p = t[v].parent;
        p = t[p].suffixLink;

        while (p != -1 && t[p].to[c] == -1)
        {
            p = t[p].suffixLink;
        }

        if (p == -1)
        {
            t[v].suffixLink = root;
            t[root].rsuffixLinks.push_back(v);

        } else
        {
            int u = t[p].to[c];
            t[v].suffixLink = u;
            t[u].rsuffixLinks.push_back(v);
        }
    }

    vector<vertex> t;
};

int main()
{
    freopen("search6.in", "r", stdin);
    freopen("search6.out", "w", stdout);

    Trie trie;
    size_t n;
    cin >> n;
    nodeIndexes.resize(n);
    ans.resize(n, {INF, -1});
    vector<string> strs(n);

    for (size_t i = 0; i < n; i++)
    {
        string s;
        cin >> s;
        trie.addString(s, i);
        strs[i] = s;
    }

    string text;
    cin >> text;

    trie.setSuffixLinks();
    trie.processText(text);
    trie.markTerminals();
    trie.markEntries();

    for (size_t i = 0; i < n; i++)
    {
        if (ans[i].x == INF)
        {
            cout << "-1 -1\n";
        } else
        {
            cout << ans[i].x - strs[i].size() + 1 << " " << ans[i].y - strs[i].size() + 1 << "\n";
        }

    }
    fclose(stdin);
    fclose(stdout);
}