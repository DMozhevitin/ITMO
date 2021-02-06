#pragma once

#include "board.h"
#include<unordered_map>
#include <unordered_set>
#include <queue>

struct solver {
    explicit solver(const board &b);

    solver(const solver &s);

    solver &operator=(const solver &other);

    const std::vector<board>::iterator begin();

    const std::vector<board>::iterator end();

    size_t moves() const;

private:
    struct comparator {
        bool operator()(const std::pair<size_t, board> &a, const std::pair<size_t, board> &b) const;
    };

    std::vector<std::pair<char, char>> transition;
    board start_board;
    board _board_goal;
    std::vector<board> boards;
    std::unordered_map<board, board> parent;

    void solve();

    void get_parents();

    bool check(size_t x, size_t y) const;
};
