#include <utility>
#include "term.hpp"

term::term() {
    weight = 0;
}

term::term(std::string str, int w) : s(std::move(str)), weight(w) {}

term::term(const term &other) = default;

bool operator<(const term &a, const term &b) {
    return a.s < b.s;
}

bool operator==(const term &a, const term &b) {
    return a.s == b.s;
}

bool operator!=(const term &a, const term &b) {
    return a.s != b.s;
}

bool operator>(const term &a, const term &b) {
    return a.s > b.s;
}

bool operator<=(const term &a, const term &b) {
    return a.s <= b.s;
}

bool operator>=(const term &a, const term &b) {
    return a.s >= b.s;
}

std::function<bool(const term &a, const term &b)> term::by_prefix_weight_order(int r) const {
    return [r](const term &a, const term &b) {
        std::string as = a.s.substr(0, r);
        std::string bs = b.s.substr(0, r);
        return as < bs;
    };
}

std::function<bool(const term &a, const term &b)> term::by_reverse_weight_order() const {
    return [](const term &a, const term &b) { return a.weight > b.weight; };
};

term &term::operator=(const term &other) = default;

std::string term::to_string() const {
    return s + ' ' + std::to_string(weight);
}

std::ostream &operator<<(std::ostream &out, const term &_term) {
    out << _term.to_string();
    return out;
}


term &term::operator=(term &&other) noexcept {
    std::swap(s, other.s);
    std::swap(weight, other.weight);
    return *this;
}

const std::string &term::get_string() const {
    return s;
}


term::term(term &&other) noexcept {
    std::swap(weight, other.weight);
    std::swap(s, other.s);
}


