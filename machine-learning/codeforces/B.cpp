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
    int k;
    cin >> k;

    int sum = 0;
    vector<ld> tp(k), tn(k), fp(k), fn(k);
    vector<int> class_cnt(k);
    vector<vector<int>> a(k, vector<int>(k));

    for (size_t i = 0; i < k; i++)
    {
        for (size_t j = 0; j < k; j++)
        {
            cin >> a[i][j];
            sum += a[i][j];
            class_cnt[i] += a[i][j];
        }
    }

    for (size_t i = 0; i < k; i++)
    {
        tp[i] = a[i][i];

        for (size_t j = 0; j < k; j++)
        {
            if (j == i) continue;

            fp[i] += a[i][j];
            fn[i] += a[j][i];
        }

        tn[i] = sum - tp[i] - fp[i] - fn[i];
    }

    vector<ld> precision(k), recall(k), f1(k);

    for (size_t i = 0; i < k; i++)
    {
        precision[i] = tp[i] + fp[i] == 0 ? 0 : tp[i] / (tp[i] + fp[i]);
        recall[i] = tp[i] + fn[i] == 0 ? 0 : tp[i] / (tp[i] + fn[i]);
        f1[i] = precision[i] + recall[i] == 0 ? 0 : 2 * (precision[i] * recall[i]) / (precision[i] + recall[i]);
    }


    ld macro_f_measure = 0;
    ld prec_w = 0, recall_w = 0;

    for (size_t i = 0; i < k; i++)
    {
        prec_w += precision[i] * class_cnt[i];
        recall_w += recall[i] * class_cnt[i];
        macro_f_measure += f1[i] * class_cnt[i];
    }

    prec_w /= sum;
    recall_w /= sum;
    macro_f_measure /= sum;
    ld micro_f_measure = prec_w + recall_w == 0 ? 0 : 2 * (prec_w * recall_w) / (prec_w + recall_w);

    cout << micro_f_measure << endl << macro_f_measure << endl;
    return 0;
}
 