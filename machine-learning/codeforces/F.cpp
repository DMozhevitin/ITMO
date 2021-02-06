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
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll Q = 2;
    ll n, m, k;
    ll alpha;

    cin >> k;
    vector<ll> lambdas(k);
    for (ll i = 0; i < k; i++)
    {
        cin >> lambdas[i];
    }
    cin >> alpha;
    cin >> n;


    auto class2objcnt = vector<ll>(k, 0);
    auto class2word2prob = vector<map<string, ld>>(k);
    auto class2word2cnt = vector<map<string, ll>> (k);

    set<string> words_distinct;

    for (ll i = 0; i < n; i++)
    {
        ll y;
        ll cnt;
        set<string> words;

        cin >> y;
        y--;
        cin >> cnt;

        class2objcnt[y]++;
        for (ll j = 0; j < cnt; j++)
        {
            string w;
            cin >> w;

            words.emplace(w);
            words_distinct.emplace(w);
        }

        for (const auto &w : words)
        {
            class2word2cnt[y][w]++;
        }
    }

    for (ll i = 0; i < k; i++)
    {
        for (auto &[w, cnt] : class2word2cnt[i])
        {
            ld nom = alpha + cnt;
            ld denom = class2objcnt[i] + Q * alpha;

            class2word2prob[i][w] = nom / denom;
        }
    }


    cin >> m;


    for (ll i = 0; i < m; i++)
    {
        ll cnt;
        cin >> cnt;
        set<string> words;

        for (ll j = 0; j < cnt; j++)
        {
            string w;
            cin >> w;
            words.emplace(w);
        }

        vector<ld> probs;

        ld sum_prob = 0;
        for (ll j = 0; j < k; j++)
        {
            ld nom = class2objcnt[j] * lambdas[j];
            ld denom = n;
            ld prob = nom / denom;

            for (const auto &w : words_distinct)
            {
                ld a, b;
                ld pr;
                if (class2word2prob[j].find(w) != class2word2prob[j].end())
                {
                    pr = class2word2prob[j][w];
                } else
                {
                    a = alpha;
                    b = class2objcnt[j] + Q * alpha;
                    pr = a / b;
                }

                if (words.find(w) != words.end())
                {
                    prob *= pr;
                } else
                {
                    prob *= (1 - pr);
                }
            }

            probs.push_back(prob);
            sum_prob += prob;
        }

        for (ll j = 0; j < k; j++)
        {
            ld ans = probs[j] / sum_prob;
            cout << setprecision(10) << probs[j] / sum_prob << " ";
        }

        cout << endl;

    }

    return 0;
}
 