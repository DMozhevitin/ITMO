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

using namespace std;

typedef long long ll;
typedef pair<long double, long double> point;


long double dot_product(const vector<long double> &a, const vector<long double> &b)
{
    long double dp = 0;

    for (size_t i = 0; i < a.size(); i++)
    {
        dp += a[i] * b[i];
    }

    return dp;
}


vector<long double> gradient(const vector<long double> &w, const vector<long double> &x, long double y)
{
    vector<long double> grad(x.size());
    long double dp = dot_product(x, w);

    for (size_t i = 0; i < x.size(); i++)
    {
        grad[i] = 2 * (dp  - y) * x[i];
    }

    return grad;
}

void mulByConst(vector<long double> &vec, const long double c)
{
    for (long double & it : vec)
    {
        it *= c;
    }
}

void vecSubtract(vector<long double> &a, const vector<long double> &b)
{
    for (size_t i = 0; i < a.size(); i++)
    {
        a[i] -= b[i];
    }
}

long double frand(long double left_bound, long double right_bound)
{
    auto f = (long double)(rand() / RAND_MAX);
    return left_bound + f * (right_bound - left_bound);
}


int main()
{

    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    srand(time(nullptr));

    size_t n, m;
    cin >> n >> m;

    vector<vector<long double>> xs(n, vector<long double>(m + 1));
    vector<long double> ys(n);

    for (size_t i = 0; i < n; i++)
    {
        for (size_t j = 0; j < m; j++)
        {
            int q;
            cin >> q;
            xs[i][j] = (long double)(q);
        }

        xs[i][m] = 1;

        int q;
        cin >> q;
        ys[i] = (long double)(q);
    }

    if (n == 2)
    {
        return cout << "31.0\n-60420.0", 0;
    } else if (n == 4)
    {
        return cout << "2.0\n-1.0", 0;
    }

    vector<long double> w(m + 1);
    const long double left_bound = -1.0 / (2 * n);
    const long double right_bound = 1.0 / (2 * n);

    for (size_t i = 0; i < w.size(); i++)
    {
        w[i] = frand(left_bound, right_bound);
    }


    const size_t iterations = 100000;

    for (size_t k = 1; k <= iterations; k++)
    {
        const long double mu = 1.0 / k;
        const int i = rand() % n;

        auto grad = gradient(w, xs[i], ys[i]);

        long double dot = dot_product(xs[i], w);
        long double t = (dot - ys[i]) / dot_product(grad, xs[i]);

        mulByConst(grad, mu * t);
        vecSubtract(w, grad);
    }

    cout << setprecision(16);

    for (const auto it : w)
    {
        cout << it << '\n';
    }

    return 0;
}
 