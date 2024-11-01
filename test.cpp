#include "vector.h"
#include <cassert>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <vector>

struct S {
    int val;
    explicit S(int val) : val(val) {}
    bool operator==(const S&) const = default;
};

struct DefaultConstructible {};
struct OnlyMovable {
    OnlyMovable(int) {}
    OnlyMovable(const OnlyMovable&) = delete;
    OnlyMovable& operator=(const OnlyMovable&) = delete;
    OnlyMovable(OnlyMovable&&) = default;
    OnlyMovable& operator=(OnlyMovable&&) = default;
};
struct ThrowMovable {
    ThrowMovable(int) {}
    ThrowMovable(const ThrowMovable&) = default;
    ThrowMovable(const OnlyMovable&&) {
        throw std::logic_error{"should not be called"};
    }
    ThrowMovable& operator=(const ThrowMovable&) = default;
    ThrowMovable& operator=(ThrowMovable&&) {
        throw std::logic_error{"should not be called"};
    }
};

struct UnexpectedThrow {
    static size_t constructed;
    UnexpectedThrow() {
        if (rand() < RAND_MAX / 100)
            throw "ha-ha";
        ++constructed;
    }
    UnexpectedThrow(const UnexpectedThrow&) {
        if (rand() < RAND_MAX / 100)
            throw "ha-ha";
        ++constructed;
    }
    UnexpectedThrow& operator=(const UnexpectedThrow&) {
        if (rand() < RAND_MAX / 100)
            throw "ha-ha";
        return *this;
    }
    ~UnexpectedThrow() { --constructed; }
};

size_t UnexpectedThrow::constructed = 0;

template<typename T>
struct InputIterator {
    T iter;
    using difference_type = std::iterator_traits<T>::difference_type;
    using value_type = std::iterator_traits<T>::value_type;
    using pointer = std::iterator_traits<T>::pointer;
    using reference = std::iterator_traits<T>::reference;
    using iterator_category = std::input_iterator_tag;
    InputIterator& operator++() {
        ++iter;
        return *this;
    }
    auto operator*() const { return *iter; }
    auto operator++(int) {
        auto copy = *iter;
        ++iter;
        return copy;
    }
    auto operator->() const { return iter.operator->(); }
    bool operator==(const InputIterator&) const = default;
};

vector<S> my_vector;
std::vector<S> true_vector;

void test(auto lambda) {
    static_assert(std::is_same_v<decltype(lambda(my_vector)),
                                 decltype(lambda(true_vector))>);
    using Ret = decltype(lambda(my_vector));
    if constexpr (std::is_same_v<Ret, void>) {
        lambda(my_vector);
        lambda(true_vector);
    } else {
        assert(lambda(my_vector) == lambda(true_vector));
    }
}

int main() {
    static_assert(sizeof(my_vector) == sizeof(true_vector));

    std::vector<S> to_insert;
    for (size_t i = 0; i < 1000000; ++i) {
        int r1 = rand(), r2 = rand();
        assert(my_vector.size() == true_vector.size());
        size_t size = my_vector.size();
        switch (rand() % 16) {
        default:
            test([r1](auto& v) { return v.push_back(S(r1)); });
            break;
        case 0:
            test([r1](auto& v) { return v.emplace_back(r1); });
            break;
        case 1: {
            to_insert.clear();
            for (size_t i = 0, end = rand() % 100; i < end; ++i)
                to_insert.emplace_back(rand());
            test([r1, size, &to_insert](auto& v) {
                return v.insert(v.begin() + r1 % (size + 1), to_insert.begin(),
                                to_insert.end()) -
                       v.begin();
            });
        } break;
        case 2:
            test([](auto& v) { v.shrink_to_fit(); });
            break;
        case 3:
            test([r1, r2, size](auto& v) {
                auto m1 = std::min(r1 % (size + 1), r2 % (size + 1));
                auto m2 = std::max(r1 % (size + 1), r2 % (size + 1));
                return v.erase(v.begin() + m1, v.begin() + m2) - v.begin();
            });
            break;
        case 4:
            test([](auto& v) {
                auto copy = v;
                std::swap(copy, v);
            });
            break;
        case 5:
            if (size > 0)
                test([](auto& v) { v.pop_back(); });
            break;
        }
        assert(my_vector.size() == true_vector.size());
        size = my_vector.size();
        for (size_t i = 0; i < size; ++i) {
            assert(my_vector[i] == true_vector[i]);
        }
    }

    // simple tests
    S vals[] = {S(1), S(2), S(3)};
    vector<S> s1{S(1), S(2)}, s2{S(3), S(4)};
    vector<OnlyMovable> om1{}, om2{};
    vector<ThrowMovable> tm1{ThrowMovable(1)}, tm2{ThrowMovable(2)};

    // construct
    { vector<S> v; }
    { vector<S> v((std::allocator<S>())); }
    { vector<S> v(4, S(42), std::allocator<S>()); }
    {
        vector<DefaultConstructible> v(4,
                                       std::allocator<DefaultConstructible>());
    }
    {
        vector<S> v(vals, vals + std::size(vals), std::allocator<S>());
        vector<S> v2(InputIterator(vals), InputIterator(vals + std::size(vals)),
                     std::allocator<S>());
        vector<S> v3(v);
        vector<S> v4(std::move(v));
    }
    { vector<S>{S(1), S(2)}; }

    // =
    s1 = s2;
    s2 = {S(1), S(2)};
    s1 = std::move(s2);
    om1 = std::move(om2);
    tm1 = std::move(tm2);

    s2.assign(5, S(5));
    s2.assign({S(2), S(3)});
    s2.assign(InputIterator(vals), InputIterator(vals + std::size(vals)));
    s1[0] = S(137);
    om1[0] = OnlyMovable(0);
    {
        ThrowMovable some(0);
        tm1[0] = some;
    }
    s1.reserve(10);
    om1.reserve(10);
    tm1.reserve(10);

    s2.clear();
    om2.clear();
    tm2.clear();

    s2.resize(5, S(5));

    std::vector<UnexpectedThrow> try_insert;
    try_insert.reserve(100);

    // unexpected throws
    for (size_t i = 0; i < 100; ++i)
        try {
            try_insert.emplace_back();
        } catch (...) {
            try_insert.clear();
            i = 0;
        }

    vector<UnexpectedThrow> v;
    for (size_t i = 0; i < 1000000; ++i)
        try {
            int r1 = rand(), r2 = rand();
            size_t size = v.size();
            size_t act = rand() % 16;
            switch (act) {
            default:
                v.emplace_back();
                break;
            case 1:
                v.insert(v.begin() + r1 % (size + 1), try_insert.begin(),
                         try_insert.end());
                break;
            case 2:
                v.shrink_to_fit();
                break;
            case 3: {
                auto m1 = std::min(r1 % (size + 1), r2 % (size + 1));
                auto m2 = std::max(r1 % (size + 1), r2 % (size + 1));
                v.erase(v.begin() + m1, v.begin() + m2);
            }; break;
            case 4: {
                auto copy = v;
                std::swap(copy, v);
            }; break;
            case 5:
                if (size > 0)
                    v.pop_back();
                break;
            }
            assert(UnexpectedThrow::constructed ==
                   try_insert.size() + v.size());
        } catch (...) {
            v.clear();
        }
}
