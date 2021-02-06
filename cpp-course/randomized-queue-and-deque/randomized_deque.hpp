#pragma once

#include <vector>
#include <stdexcept>

template<typename T>
struct deque {

    deque() {
        _size = 0;
        left = 0;
        right = 0;
        data.resize(10);
    }

    ~deque() = default;

    struct iterator {
        iterator(const typename std::vector<T>::iterator x, size_t _sz, size_t _offset) : it(x),
                                                                                          sz(_sz), offset(_offset) {}

        iterator &operator=(const iterator &other) = default;

        iterator &operator+=(int x) {
            offset += x;
            return *this;
        }

        iterator &operator-=(int x) {
            offset -= x;
            return *this;
        }

        iterator operator+(int x) {
            iterator res = *this;
            return res += x;
        }

        iterator operator-(int x) {
            iterator res = *this;
            return res -= x;
        }

        iterator &operator++() {
            offset++;
            return *this;
        }

        iterator &operator--() {
            offset--;
            return *this;
        }

        iterator operator++(int) {
            iterator res = *this;
            res.offset++;
            return res;
        }

        iterator operator--(int) {
            iterator res = *this;
            res.offset--;
            return res;
        }

        bool operator==(const iterator &other) {
            return (it + (offset % sz) == other.it + (other.offset % other.sz));
        }

        bool operator!=(const iterator &other) {
            return (it + (offset % sz) != other.it + (other.offset % other.sz));
        }

        T &operator*() {
            return *(it + (offset % sz));
        }

    private:
        typename std::vector<T>::iterator it;
        size_t sz;
        size_t offset;
    };

    iterator begin() {
        return iterator(data.begin(), data.size(), left);
    }

    iterator end() {
        return iterator(data.begin(), data.size(), right);
    }

    bool empty() const {
        return _size == 0;
    }

    size_t size() const {
        return _size;
    }

    void push_front(const T &x) {
        ensure_capacity(++_size);
        left = dec(left);
        data[left] = x;
    }

    void push_back(T x) {
        ensure_capacity(++_size);
        data[right] = x;
        right = inc(right);
    }

    void pop_front() {
        if (empty()) {
            throw std::runtime_error("pop from empty deque");
        }

        data[left] = T();
        _size--;
        left = inc(left);
    }


    void pop_back() {
        if (empty()) {
            throw std::runtime_error("pop from empty deque");
        }

        data[dec(right)] = T();
        _size--;
        right = dec(right);
    }

    const T &front() const {
        return data[left];
    }

    T &front() {
        return data[left];
    }

    T &back() {
        return data[dec(right)];
    }

    const T &back() const {
        return data[dec(right)];
    }

private:
    int inc(int x) {
        return static_cast<int>((x + 1) % data.size());
    }

    int dec(int x) {
        if (!x) {
            return static_cast<int>(data.size() - 1);
        } else {
            return x - 1;
        }
    }

    void ensure_capacity(size_t sz) {
        if (data.size() > sz) {
            return;
        }

        std::vector <T> new_data(sz * 2 + 1);
        size_t index = 0;
        for (int i = left; i != right; i = inc(i)) {
            new_data[index] = data[i];
            index++;
        }
        data = new_data;
        left = 0;
        right = static_cast<int>(index);
    }

    std::vector <T> data;
    int left;
    int right;
    size_t _size;
};


