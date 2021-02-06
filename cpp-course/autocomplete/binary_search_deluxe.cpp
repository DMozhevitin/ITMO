#include "binary_search_deluxe.hpp"
#include <vector>

int binary_search_deluxe::first_index_of(const std::vector<term> &a, term key,
                                         std::function<bool(const term &, const term &)> &comp) {
    int left = -1;
    int right = a.size() - 1;
    while (right - left > 1) {
        int mid = (left + right) / 2;
        if (comp(key, a[mid])) {
            left = mid;
        } else {
            right = mid;
        }
    }
    return right;
}

int binary_search_deluxe::last_index_of(const std::vector<term> &a, term key,
                                        std::function<bool(const term &, const term &)> &comp) {
    int left = 0;
    int right = a.size();
    while (right - left > 1) {
        int mid = (left + right) / 2;
        if (!comp(key, a[mid])) {
            left = mid;
        } else {
            right = mid;
        }
    }
    return left;
}

