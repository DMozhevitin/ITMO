#pragma once
#include <iostream>
#include <string>
#include <functional>

struct term {
    term();

    term(std::string str, int weight);

    term(const term &other);

    term(term&& other) noexcept;

    term &operator=(const term &other);

    term &operator=(term&& other) noexcept;

    friend bool operator==(const term &a, const term &b);

    friend bool operator!=(const term &a, const term &b);

    friend bool operator<(const term &a, const term &b);

    friend bool operator<=(const term &a, const term &b);

    friend bool operator>(const term &a, const term &b);

    friend bool operator>=(const term &a, const term &b);

    friend std::ostream &operator<<(std::ostream &out, const term &_term);

    std::string to_string() const;

    std::function<bool(const term& a, const term& b)> by_reverse_weight_order() const ;

    std::function<bool(const term& a, const term& b)> by_prefix_weight_order(int r) const;

    const std::string& get_string() const;
private:
    std::string s;
    int weight;
};



