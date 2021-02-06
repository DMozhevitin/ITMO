#include "puzzle_game.h"
#include <iostream>
#include <queue>
#include <algorithm>

solver::solver(const board &b) {
    start_board = b;
    transition = {{-1, 0},
                  {0,  1},
                  {1,  0},
                  {0,  -1}};
    _board_goal = board(b.get_goal());
    solve();
}

solver::solver(const solver &s) = default;

solver &solver::operator=(const solver &other)  = default;

const std::vector<board>::iterator solver::begin() {
    return boards.begin();
}

const std::vector<board>::iterator solver::end() {
    return boards.end();
}

void solver::solve() {
    if (!start_board.is_solvable()) {
        return;
    }

    std::priority_queue<std::pair<size_t, board>, std::vector<std::pair<size_t, board>>, comparator> q;
    std::unordered_set<board> used;
    std::unordered_map<board, size_t> distance;

    q.push({0, start_board});
    distance[start_board] = 0;
    while (!q.empty()) {
        std::pair<size_t, board> current = q.top();
        if (current.second.is_goal()) {
            get_parents();
            return;
        }
        q.pop();
        used.insert(current.second);

        std::pair<size_t, size_t> zero = current.second.get_zero();
        for (auto [dx, dy] : transition) {
            size_t new_x = zero.first + dx;
            size_t new_y = zero.second + dy;
            board changed_board = current.second;

            if (check(new_x, new_y)) {
                std::vector<std::vector<size_t>> tmp = changed_board.get_board();
                std::swap(tmp[zero.first][zero.second], tmp[new_x][new_y]);
                changed_board = board(tmp);


                size_t cur_dist = current.first + changed_board.hamming() + changed_board.manhattan();
                if (distance.find(changed_board) == distance.end()) {
                    distance[changed_board] = INT32_MAX;
                }

                if (used.count(changed_board) && cur_dist > distance[changed_board]) {
                    continue;
                }

                if (!used.count(changed_board) || cur_dist < distance[changed_board]) {
                    parent[changed_board] = current.second;
                    distance[changed_board] = cur_dist;
                    q.push({cur_dist, changed_board});
                }

            }
        }
    }
}

void solver::get_parents() {
    board v = _board_goal;
    while (v != start_board) {
        boards.emplace_back(v);
        v = parent[v];
    }
    boards.emplace_back(v);
    reverse(boards.begin(), boards.end());
}

bool solver::check(size_t x, size_t y) const {
    return x >= 0 && x < start_board.size() && y >= 0 && y < start_board.size();
}

size_t solver::moves() const {
    return boards.size();
}

bool solver::comparator::operator()(const std::pair<size_t, board> &a, const std::pair<size_t, board> &b) const {
    return a.first > b.first;
}
