#include <utility>

#include "autocomplete.hpp"
#include "binary_search_deluxe.hpp"
#include "term.hpp"

autocomplete::autocomplete(std::vector<term> a) : terms(std::move(a)) {
    std::stable_sort(terms.begin(), terms.end());
}

size_t autocomplete::number_of_matches(const std::string &prefix) const {
    if (terms.empty()) {
        return 0;
    }

    term key = term(prefix, 0);
    auto f = key.by_prefix_weight_order(prefix.size());
    int first = binary_search_deluxe::first_index_of(terms, key, f);
    int last = binary_search_deluxe::last_index_of(terms, key, f);

    //if list of terms doesn't contains any term with this prefix, result of binary search can be wrong, so I check it
    if (terms[first].get_string().substr(0, prefix.size()) == prefix &&
        terms[last].get_string().substr(0, prefix.size()) == prefix) {
        return last - first + 1;
    } else {
        return 0;
    }
}

std::vector<term> autocomplete::all_matches(const std::string &prefix) const {
    std::vector<term> ans;
    if (terms.empty()) {
        return ans;
    }

    term key = term(prefix, 0);
    auto f = key.by_prefix_weight_order(prefix.size());
    int first = binary_search_deluxe::first_index_of(terms, key, f);
    int last = binary_search_deluxe::last_index_of(terms, key, f);
    //if list of terms doesn't contains any term with this prefix, result of binary search can be wrong, so I check it
    if (terms[first].get_string().substr(0, prefix.size()) == prefix &&
        terms[last].get_string().substr(0, prefix.size()) == prefix) {
        for (size_t i = first; i <= last; i++) {
            ans.emplace_back(terms[i]);
        }
        auto g = key.by_reverse_weight_order();
        std::stable_sort(ans.begin(), ans.end(), g);
    }
    return ans;
}

