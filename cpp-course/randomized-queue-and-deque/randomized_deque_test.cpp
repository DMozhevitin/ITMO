#include "randomized_deque.hpp"
#include <iostream>

int main()
{
    deque<int> d;

    d.push_back(3);
    d.push_front(2);
    d.push_back(1);
    d.push_frond(4);

    d.pop_front();
    d.pop_back();

    for (auto it : d) {
        cout << it << endl;
    }
}


