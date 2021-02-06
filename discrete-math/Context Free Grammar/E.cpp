#include <iostream>
#include <vector>

using namespace std;

vector<vector<string>> edges;
vector<pair<char, string>> rules;
int indexes[100][100];
bool dp[30][110][110];
bool h[51][110][110][10];

int main() {
    freopen("cf.in", "r", stdin);
    freopen("cf.out", "w", stdout);

    int n;
    char start;

    cin >> n >> start;
    edges.resize(30);
    string tmp;
    getline(cin, tmp);
    for (int i = 0; i < n; i++) {
        string s;
        getline(cin, s);

        if (s.empty()) {
            getline(cin, s);
        }

        char from = s[0];
        string to;

        if (s.back() != '>') {
            for (int j = s.find('>') + 2; j < s.size(); j++) {
                to += s[j];
            }
        } else {
            to = "";
        }

        rules.emplace_back(from, to);
        edges[from - 'A'].emplace_back(to);
        indexes[from - 'A'][edges[from - 'A'].size() - 1] = rules.size() - 1;
    }

    string word;
    cin >> word;

    for (int i = 0; i < word.size(); i++) {
        for (int j = 0; j < rules.size(); j++) {
            pair<char, string> rule = rules[j];

            dp[rule.first - 'A'][i][i + 1] = rule.second.size() == 1 && rule.second[0] == word[i];
            dp[rule.first - 'A'][i][i] = rule.second.empty();
            h[j][i][i][0] = true;
        }
    }

    for (int cnt = 0; cnt < 30; cnt++) {
        for (int m = 0; m <= word.size(); m++) {
            for (int i = 0; i < word.size(); i++) {
                int j = i + m;

                if (j > word.size()) {
                    break;
                }

                for (int k = 1; k <= 5; k++) {
                    for (int ind = 0; ind < rules.size(); ind++) {
                        if (rules[ind].second.size() < k) {
                            continue;
                        }

                        for (int r = i; r <= j; r++) {
                            if (rules[ind].second[k - 1] >= 'a' && rules[ind].second[k - 1] <= 'z') {
                                h[ind][i][j][k] |=
                                        h[ind][i][r][k - 1] && (r == j - 1) && (word[r] == rules[ind].second[k - 1]);
                            } else {
                                h[ind][i][j][k] |= h[ind][i][r][k - 1] && dp[rules[ind].second[k - 1] - 'A'][r][j];
                            }
                        }
                    }
                }

                for (int ind = 0; ind < 26; ind++) {
                    for (int ind1 = 0; ind1 < edges[ind].size(); ind1++) {
                        dp[ind][i][j] |= h[indexes[ind][ind1]][i][j][edges[ind][ind1].size()];
                    }
                }
            }
        }
    }

    if (dp[start - 'A'][0][word.size()]) {
        cout << "yes";
    } else {
        cout << "no";
    }

    fclose(stdin);
    fclose(stdout);
    return 0;
}