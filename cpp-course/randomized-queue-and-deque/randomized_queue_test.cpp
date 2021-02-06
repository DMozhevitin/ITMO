#include <iostream>
#include "randomized_queue.hpp"

int main()
{
    randomized_queue<int> q;
    q.enqueue(1);
    q.enqueue(2);
    q.enqueue(3);
    q.dequeue();
    q.enqueue(10);

    for (const auto &it : q) {
        std::cout << it << endl;
    }
}


