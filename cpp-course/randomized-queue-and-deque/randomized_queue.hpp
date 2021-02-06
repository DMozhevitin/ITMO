#pragma once

#include <random>
#include <algorithm>
#include <stdexcept>

template<typename T>
struct randomized_queue {

    randomized_queue() {
        std::random_device rd;
        gen = std::mt19937(rd());
    }

    ~randomized_queue() = default;

    struct iterator {
        iterator(typename std::vector<T>::iterator x, size_t _sz, int _cur) : it(x), sz(_sz), cur(_cur) {
            p.resize(sz);
            for (size_t i = 0; i < sz; i++) {
                p[i] = i;
            }

            std::shuffle(p.begin(), p.end(), std::mt19937(std::random_device()()));
            p.push_back(-1);
        }

        iterator(const iterator &other) = default;

        iterator& operator++() {
            cur++;
            return *this;
        }

        iterator& operator--() {
            cur--;
            return *this;
        }

        const iterator operator++(int) {
            iterator res = *this;
            res.cur++;
            return res;
        }

        const iterator operator--(int) {
            iterator res = *this;
            res.cur--;
            return res;
        }

        bool operator==(const iterator &other) {
            return it == other.it && p[cur] == other.p[other.cur];
        }

        bool operator!=(const iterator &other) {
            return it == other.it && p[cur] != other.p[other.cur];
        }

        T& operator*() {
            return *(it + p[cur]);
        }

    private:
        typename std::vector<T>::iterator it;
        int cur;
        std::vector<size_t> p;
        size_t sz;
    };

    iterator begin() {
        return iterator(data.begin(), data.size(), 0);
    }

    iterator end() {
        return iterator(data.begin(), data.size(), data.size());
    }

    bool empty() const {
        return data.empty();
    }

    size_t size() const {
        return data.size();
    }

    T &sample() {
        return data[distribution(gen)];
    }

    void enqueue(const T &x) {
        data.emplace_back(x);
        distribute();
    }

    T dequeue() {
        if (empty()) {
            throw std::runtime_error("dequeue from empty randomized_queue");
        }

        size_t ind = distribution(gen);
        T res = data[ind];
        std::swap(data[ind]), data.back());
        data.pop_back();
        distribute();
        return res;
    }


private:
    std::vector<T> data;
    std::uniform_int_distribution<size_t> distribution;
    std::mt19937 gen;

    void distribute() {
        distribution = std::uniform_int_distribution<size_t>(0, data.size() - 1);
    }
};

