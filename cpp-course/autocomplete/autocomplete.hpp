#pragma once
#include <vector>
#include "term.hpp"

struct autocomplete {
    explicit autocomplete(std::vector<term> a);

    std::vector<term> all_matches(const std::string &prefix) const;

    size_t number_of_matches(const std::string &prefix) const;

private:
    std::vector<term> terms;
};




