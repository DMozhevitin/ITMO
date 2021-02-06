#include "board.h"
#include <algorithm>
#include <random>

board::board() {
    _size = 0;
}

board::board(const board &other) = default;

board::board(const std::vector<std::vector<size_t>> &other) {
    b = other;
    _size = other.size();
    init_goal();
    find_zero();
}

board::board(int size) {
    b.resize(size, std::vector<size_t>(size));
    std::vector<size_t> some_board;
    some_board.resize(size * size);
    _size = size;
    init_goal();
    for (size_t i = 0; i < size * size; i++) {
        some_board[i] = i;
    }
    std::shuffle(some_board.begin(), some_board.end(), std::mt19937(std::random_device()()));
    for (size_t i = 0; i < size * size; i++) {
        size_t x = i / size;
        size_t y = i % size;
        b[x][y] = some_board[i];
        if (!b[x][y]) {
            zero = {x, y};
        }
    }
}

size_t board::size() const {
    return _size;
}

bool board::is_goal() const {
    return b == goal;
}

size_t board::hamming() const {
    size_t hamming_distance = 0;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            if (b[i][j] != goal[i][j]) {
                hamming_distance++;
            }
        }
    }

    return hamming_distance;
}

bool board::is_solvable() const {
    std::vector<size_t> v(_size * _size);
    size_t cnt_inv = 0;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            v[i * _size + j] = b[i][j];
        }
    }

    for (size_t i = 0; i < v.size(); i++) {
        if (v[i]) {
            for (size_t j = 0; j < i; j++) {
                if (v[j] > v[i]) {
                    cnt_inv++;
                }
            }
        }
    }

    for (size_t i = 0; i < v.size(); i++) {
        if (!v[i]) {
            cnt_inv += 1 + i / 4;
        }
    }
    return !(cnt_inv & 1);
}

size_t board::manhattan() const {
    size_t manhattan_distance = 0;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            int need_x = static_cast<int>((b[i][j] - 1) / _size);
            int need_y = static_cast<int>((b[i][j] - 1) % _size);

            if (!b[i][j]) {
                need_x = need_y = static_cast<int>(_size - 1);
            }

            manhattan_distance += abs(static_cast<int>(i) - need_x) + abs(static_cast<int>(j) - need_y);
        }
    }

    return manhattan_distance;
}

std::string board::to_string() const {
    std::string result;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            result += std::to_string(b[i][j]) + ' ';
        }

        result.pop_back();
        result.push_back('\n');
    }

    return result;
}

std::vector<std::vector<size_t>> board::get_goal() const {
    return goal;
}

std::pair<size_t, size_t> board::get_zero() const {
    return zero;
}

board &board::operator=(const board &other) = default;

std::vector<std::vector<size_t>> board::get_board() const {
    return b;
}

const std::vector<size_t> &board::operator[](size_t index) const {
    return b[index];
}

void board::init_goal() {
    goal.resize(_size, std::vector<size_t>(_size));
    for (size_t i = 0; i < _size * _size; i++) {
        goal[i / _size][i % _size] = i + 1;
    }

    goal.back().back() = 0;
}

void board::find_zero() {
    for (int i = 0; i < _size; i++) {
        for (int j = 0; j < _size; j++) {
            if (!b[i][j]) {
                zero = {i, j};
                break;
            }
        }
    }
}

std::ostream &operator<<(std::ostream &out, const board &b) {
    out << b.to_string();
    return out;
}

bool operator==(const board &a, const board &b) {
    return a.b == b.b;
}

bool operator!=(const board &a, const board &b) {
    return a.b != b.b;
}

bool operator <(const board &a, const board &b) {
    return a.b < b.b;
}