#include <iostream>
#include "randomized_queue.hpp"

int main() {
    std::string s;
    size_t k;
    std::cin >> k;
    getline(std::cin, s);

    randomized_queue<std::string> q;
    while (getline(std::cin, s)) {
        q.enqueue(s);
    }

    for (size_t i = 0; i < k; i++) {
        std::cout << q.dequeue() << std::endl;
    }

    return 0;
}

