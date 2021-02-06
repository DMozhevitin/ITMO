#pragma once
#include <functional>
#include "term.hpp"
#include <vector>

struct binary_search_deluxe {

    binary_search_deluxe() = delete;

    static int
    first_index_of(const std::vector<term> &a, term key, std::function<bool(const term &, const term &)> &comp);

    static int
    last_index_of(const std::vector<term> &a, term key, std::function<bool(const term &, const term &)> &comp);

};


