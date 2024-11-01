#include <cstddef>
#include <iterator>
#include <limits>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace detail {

template<typename T, typename size_type = size_t>
struct copy_iterator {
    const T* val;
    size_type index;
    using difference_type = ssize_t;
    using value_type = T;
    using pointer = const T*;
    using reference = const T&;
    using iterator_category = std::random_access_iterator_tag;
    constexpr copy_iterator& operator++() {
        ++index;
        return *this;
    }
    constexpr const T& operator++(int) {
        ++index;
        return *val;
    }
    constexpr const T& operator*() const { return *val; }
    constexpr bool operator==(const copy_iterator& other) const {
        return index == other.index;
    }
    void operator+(size_t i) { index += i; }
    void operator-(size_t i) { index -= i; }
    ssize_t operator-(const copy_iterator& other) const {
        return index - other.index;
    }
};
template<typename T, typename size_type = size_t>
struct construct_iterator {
    size_type index;
    constexpr void operator++() { ++index; }
    constexpr T operator*() const { return T(); }
    constexpr bool operator==(const construct_iterator& other) const {
        return index == other.index;
    }
};
template<typename T>
constexpr bool is_input_iter =
    std::is_same_v<typename std::iterator_traits<T>::iterator_category,
                   std::input_iterator_tag>;

} // namespace detail

template<typename T, typename Allocator = std::allocator<T>>
class vector {
  private:
    using alloc_trait = std::allocator_traits<Allocator>;

  public:
    using value_type = T;
    using allocator_type = Allocator;
    using size_type = typename alloc_trait::size_type;
    using difference_type = typename alloc_trait::difference_type;
    using reference = T&;
    using const_reference = const T&;
    using pointer = typename alloc_trait::pointer;
    using const_pointer = typename alloc_trait::const_pointer;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  private:
    using copy_iter = detail::copy_iterator<T, size_type>;
    using construct_iter = detail::construct_iterator<T, size_type>;
    using move_iter =
        std::conditional_t<std::is_nothrow_move_constructible_v<T> ||
                               !std::is_copy_constructible_v<T>,
                           std::move_iterator<iterator>, iterator>;

  private:
    pointer data_ = nullptr;
    size_type size_ = 0;
    size_type capacity_ = 0;
    [[no_unique_address]] allocator_type allocator_;

  private:
    pointer allocate(size_type size) {
        if (size == 0)
            return nullptr;
        return allocator_.allocate(size);
    }
    void deallocate(pointer p, size_type size) {
        if (size == 0)
            return;
        allocator_.deallocate(p, size);
    }
    template<typename... Args>
    void construct(pointer p, Args&&... args) {
        alloc_trait::construct(allocator_, p, std::forward<Args>(args)...);
    }
    void destroy(pointer p) { alloc_trait::destroy(allocator_, p); }
    template<typename Iter>
    pointer copy(size_type capacity, Iter begin, Iter end) {
        pointer data = allocate(capacity);
        size_type i = 0;
        try {
            for (; begin != end; ++begin, ++i) {
                construct(data + i, *begin);
            }
        } catch (...) {
            for (size_type j = 0; j < i; ++j) {
                destroy(data + j);
            }
            deallocate(data, capacity);
            throw;
        }
        return data;
    }

  public:
    constexpr vector() noexcept(noexcept(allocator_type())) = default;
    constexpr explicit vector(const allocator_type& allocator)
        : allocator_(allocator) {}
    constexpr vector(size_type count, const_reference value,
                     const allocator_type& allocator = allocator_type())
        : capacity_(count), allocator_(allocator) {
        data_ = copy(count, copy_iter{std::addressof(value), 0},
                     copy_iter{nullptr, count});
    }
    explicit vector(size_type count, const Allocator& alloc = Allocator())
        : allocator_(alloc) {
        data_ = copy(count, construct_iter{0}, construct_iter{count});
    }

    template<std::input_iterator InputIt>
    constexpr vector(InputIt first, InputIt last,
                     const Allocator& alloc = Allocator())
        : allocator_(alloc) {
        if constexpr (detail::is_input_iter<InputIt>) {
            for (; first != last; ++first) {
                emplace_back(*first);
            }
        } else {
            if constexpr (detail::is_input_iter<InputIt>) {
                size_ = capacity_ = last - first;
            } else {
                for (InputIt current = first; current != last;
                     ++current, ++size_)
                    ;
                capacity_ = size_;
            }
            data_ = copy(capacity_, first, last);
        }
    }

    constexpr vector(const vector& other)
        : vector(other.begin(), other.end(),
                 alloc_trait::select_on_container_copy_construction(
                     other.allocator_)) {}
    constexpr vector(const vector& other, const allocator_type& alloc)
        : vector(other.begin(), other.end(), alloc) {}
    constexpr vector(vector&& other)
        : data_(other.data_), size_(other.size_), capacity_(other.capacity_),
          allocator_(std::move(other.allocator_)) {
        other.data_ = nullptr;
        other.size_ = 0;
        other.capacity_ = 0;
    }
    constexpr vector(vector&& other, const allocator_type& alloc)
        : data_(other.data_), size_(other.size), capacity_(other.capacity_),
          allocator_(alloc) {
        other.data_ = nullptr;
        other.size_ = 0;
        other.capacity_ = 0;
    }
    constexpr vector(std::initializer_list<T> init,
                     const allocator_type& alloc = allocator_type())
        : vector(init.begin(), init.end(), alloc) {}

    ~vector() {
        clear();
        deallocate(data_, capacity_);
    }

    vector& operator=(vector&& other) {
        if (this == &other)
            return *this;
        clear();
        if (alloc_trait::propagate_on_container_move_assignment::value &&
            allocator_ != other.allocator_) {
            deallocate(data_, capacity_);
            allocator_ = std::move(other.allocator_);
            capacity_ = other.size_;
            size_ = other.size_;
            data_ = copy(capacity_, move_iter(other.begin()),
                         move_iter(other.end()));
            other.clear();
        } else {
            size_ = other.size_;
            other.size_ = 0;
            std::swap(capacity_, other.capacity_);
            std::swap(data_, other.data_);
        }
        return *this;
    }
    constexpr vector& operator=(const vector&);
    vector& operator=(std::initializer_list<value_type> list) {
        clear();
        reserve(list.size());
        for (auto cur = list.begin(); cur != list.end(); ++cur, ++size_)
            construct(data_ + size_, *cur);
        return *this;
    }

    constexpr reference operator[](size_type index) { return data_[index]; }
    constexpr const_reference operator[](size_type index) const {
        return data_[index];
    }

    constexpr void assign(size_type count, const T& value) {
        assign(copy_iter(&value, 0), copy_iter(&value, count));
    }
    template<std::input_iterator InputIt>
    constexpr void assign(InputIt first, InputIt last) {
        *this = vector(first, last);
    }
    constexpr void assign(std::initializer_list<T> list) {
        assign(list.begin(), list.end());
    }

    constexpr allocator_type get_allocator() const noexcept {
        return allocator_;
    }
  private:
    void bound_check(size_type index) {
        if (index < size_type(0) || index >= size_)
            throw std::out_of_range{"vector index is out of range"};
    }

  public:
    reference at(size_type index) {
        bound_check(index);
        return operator[](index);
    }
    const_reference at(size_type index) const {
        bound_check(index);
        return operator[](index);
    }
    constexpr reference front() { return data_[0]; }
    constexpr const_reference front() const { return data_[0]; }
    constexpr reference back() { return data_[size_ - 1]; }
    constexpr const_reference back() const { return data_[size_ - 1]; }
    constexpr pointer data() { return data_; }
    constexpr const_pointer data() const { return data_; }
    constexpr iterator begin() { return data_; }
    constexpr const_iterator begin() const { return data_; }
    constexpr iterator end() { return data_ + size_; }
    constexpr const_iterator end() const { return data_ + size_; }
    constexpr reverse_iterator rbegin() {
        return reverse_iterator(data_ + size_ - 1);
    }
    constexpr const_iterator rbegin() const {
        return const_reverse_iterator(data_ + size_ - 1);
    }
    constexpr reverse_iterator rend() { return reverse_iterator(data_ - 1); }
    constexpr const_iterator rend() const {
        return const_reverse_iterator(data_ - 1);
    }
    constexpr size_type size() const { return size_; }
    constexpr bool empty() const { return !size_; }
    constexpr size_type max_size() const {
        return std::numeric_limits<difference_type>::max();
    }
    constexpr void reserve(size_type new_cap) {
        if (new_cap <= capacity_)
            return;
        pointer new_data = copy(new_cap, move_iter(begin()), move_iter(end()));
        size_type size = size_;
        clear();
        deallocate(data_, capacity_);
        size_ = size;
        capacity_ = new_cap;
        data_ = new_data;
    }
    constexpr size_type capacity() const { return capacity_; }
    constexpr void shrink_to_fit() {
        if (size_ == capacity_)
            return;
        pointer new_data = copy(size_, move_iter(begin()), move_iter(end()));
        size_t old_size = size_;
        clear();
        deallocate(data_, capacity_);
        data_ = new_data;
        size_ = old_size;
        capacity_ = old_size;
    }
    constexpr void clear() {
        for (size_type i = 0; i < size_; ++i) {
            destroy(data_ + i);
        }
        size_ = 0;
    }
    constexpr iterator insert(const_iterator pos, const T& value) {
        return insert(pos, 1, value);
    }
    constexpr iterator insert(const_iterator pos, T&& value) {
        return insert(pos, move_iter(&value), move_iter(&value + 1));
    }
    constexpr iterator insert(const_iterator pos, size_type count,
                              const T& value) {
        insert(pos, copy_iter(&value, 0), copy_iter(&value, count));
    }
    template<std::input_iterator InputIt>
    constexpr iterator insert(const_iterator pos_iter, InputIt first,
                              InputIt last) {
        const size_type pos = pos_iter - begin();
        if (first == last)
            return pos + begin();
        if constexpr (detail::is_input_iter<InputIt>) {
            vector<T> to_insert(first, last);
            return insert(pos_iter, to_insert.begin(), to_insert.end());
        } else {
            const size_type old_size = size_;
            const size_type insert_size = last - first;
            reserve(std::max<size_type>(size_ + insert_size, 2 * size_));
            size_type count = 0;
            // construct from input
            if (pos + insert_size > size_) {
                count = pos + insert_size - size_;
                for (InputIt cur = last - count; cur != last; ++cur)
                    emplace_back(std::move_if_noexcept(*cur));
            }
            // construct from data_
            for (size_type i = 0; i < insert_size - count; ++i)
                emplace_back(std::move_if_noexcept(data_[size_ - insert_size]));
            // copy till the end
            if (old_size > 0)
                for (size_type i = old_size - 1; i >= pos + insert_size; --i)
                    data_[i] = std::move_if_noexcept(data_[i - insert_size]);
            // copy beginning if It
            for (size_type i = 0; i < insert_size - count; ++i, ++first)
                data_[pos + i] = std::move_if_noexcept(*first);
            return pos + begin();
        }
    }
    constexpr iterator insert(const_iterator pos,
                              std::initializer_list<T> list) {
        return insert(pos, list.begin(), list.end());
    }
    constexpr iterator erase(const_iterator pos) { return erase(pos, pos + 1); }
    constexpr iterator erase(const_iterator first, const_iterator last) {
        if (first == last)
            return first - begin() + begin();
        size_type difference = last - first;
        for (size_type i = first - data_; i < size_ - difference; ++i)
            data_[i] = std::move_if_noexcept(data_[i + difference]);
        for (size_type i = 0; i < difference; ++i)
            pop_back();
        return first - begin() + begin();
    }
    template<typename... Args>
    constexpr reference emplace_back(Args&&... args) {
        if (size_ == capacity_)
            reserve(std::max<size_type>(1, 2 * capacity_));
        construct(data_ + size_, std::forward<Args>(args)...);
        ++size_;
        return back();
    }
    constexpr void push_back(const T& value) {
        emplace_back(value);
    }
    constexpr void push_back(T&& value) {
        emplace_back(std::move(value));
    }
    constexpr void pop_back() {
        destroy(data_ + size_ - 1);
        --size_;
    }
    constexpr void resize(size_type count) { resize(count, value_type()); }
    constexpr void resize(size_type count, const value_type& value) {
        if (count <= size_) {
            for (size_type i = count; i < size_; ++i)
                destroy(data_ + i);
            size_ = count;
            return;
        }
        reserve(count);
        for (; size_ < count; ++size_)
            construct(data_ + size_, value);
    }
    constexpr void swap(vector& other) {
        if (alloc_trait::propagate_on_container_swap::value &&
            allocator_ != other.allocator_) {
            vector copy_this(std::move(this), other.allocator_);
            vector copy_other(std::move(other), allocator_);
            *this = std::move(copy_other);
            *other = std::move(copy_this);
        } else {
            std::swap(data_, other.data_);
            std::swap(size_, other.size_);
            std::swap(capacity_, other.capacity_);
            std::swap(allocator_, other.allocator_);
        }
    }
};

template<typename T, typename Alloc>
constexpr vector<T, Alloc>&
vector<T, Alloc>::operator=(const vector<T, Alloc>& other) {
    if (this == &other)
        return *this;
    clear();
    if constexpr (alloc_trait::propagate_on_container_copy_assignment::value) {
        deallocate(data_, capacity_);
        allocator_ = other.allocator_;
        capacity_ = 0;
        data_ = nullptr;
    }
    reserve(other.size_);
    for (size_type i = 0; i < other.size_; ++i)
        construct(data_ + i, other[i]);
    return *this;
}

template<typename T, typename Alloc>
constexpr bool operator==(const vector<T, Alloc>& lhs,
                          const vector<T, Alloc>& rhs) {
    return lhs.size() == rhs.size() &&
           std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}
template<typename T, typename Alloc>
constexpr auto operator<=>(const vector<T, Alloc>& lhs,
                           const vector<T, Alloc>& rhs) {
    return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(),
                                                  rhs.begin(), rhs.end());
}
