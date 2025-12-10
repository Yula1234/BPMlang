#pragma once

#if defined(__GNUC__) || defined(__clang__)
    #define G_LIKELY(x) __builtin_expect(!!(x), 1)
    #define G_UNLIKELY(x) __builtin_expect(!!(x), 0)
    #define G_ALWAYS_INLINE __attribute__((always_inline)) inline
    #define G_NOINLINE __attribute__((noinline))
    #define G_RESTRICT __restrict__
    #define G_ASSUME_ALIGNED(ptr) __builtin_assume_aligned((ptr), alignof(T))
#elif defined(_MSC_VER)
    #define G_LIKELY(x) (x)
    #define G_UNLIKELY(x) (x)
    #define G_ALWAYS_INLINE __forceinline
    #define G_NOINLINE __declspec(noinline)
    #define G_RESTRICT __restrict
    #define G_ASSUME_ALIGNED(ptr) (ptr)
#else
    #define G_LIKELY(x) (x)
    #define G_UNLIKELY(x) (x)
    #define G_ALWAYS_INLINE inline
    #define G_NOINLINE
    #define G_RESTRICT
    #define G_ASSUME_ALIGNED(ptr) (ptr)
#endif

template <typename T>
class GFastVector {
public:
    using value_type = T;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = T*;
    using const_iterator = const T*;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;

private:
    T* G_RESTRICT m_begin = nullptr;
    T* G_RESTRICT m_end = nullptr;
    T* G_RESTRICT m_cap = nullptr;

public:
    G_ALWAYS_INLINE GFastVector() noexcept = default;

    explicit GFastVector(size_type count) {
        if (count > 0) {
            alloc_raw(count);
            default_init(m_begin, count);
            m_end = m_begin + count;
        }
    }

    GFastVector(size_type count, const T& value) {
        if (count > 0) {
            alloc_raw(count);
            T* G_RESTRICT ptr = m_begin;
            for (size_type i = 0; i < count; ++i) {
                 if constexpr (std::is_trivially_copyable_v<T>) *ptr = value;
                 else new (ptr) T(value);
                 ++ptr;
            }
            m_end = m_begin + count;
        }
    }

    GFastVector(std::initializer_list<T> init) {
        size_type n = init.size();
        if (n > 0) {
            alloc_raw(n);
            copy_mem(m_begin, init.begin(), n);
            m_end = m_begin + n;
        }
    }

    template <typename InputIt, typename = std::enable_if_t<!std::is_integral_v<InputIt>>>
    GFastVector(InputIt first, InputIt last) {
        if constexpr (std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category>) {
            size_type n = static_cast<size_type>(last - first);
            if (n > 0) {
                alloc_raw(n);
                if constexpr (std::is_pointer_v<InputIt> && std::is_trivially_copyable_v<T>) {
                     __builtin_memcpy(m_begin, first, n * sizeof(T));
                } else {
                    T* G_RESTRICT ptr = m_begin;
                    while (first != last) new (ptr++) T(*first++); 
                }
                m_end = m_begin + n;
            }
        } else {
            while (first != last) push_back(*first++);
        }
    }

    GFastVector(const GFastVector& other) {
        size_type n = static_cast<size_type>(other.m_end - other.m_begin);
        if (n > 0) {
            alloc_raw(n);
            copy_mem(m_begin, other.m_begin, n);
            m_end = m_begin + n;
        }
    }

    GFastVector(GFastVector&& other) noexcept 
        : m_begin(other.m_begin), m_end(other.m_end), m_cap(other.m_cap) 
    {
        other.m_begin = nullptr;
        other.m_end = nullptr;
        other.m_cap = nullptr;
    }

    GFastVector& operator=(const GFastVector& other) {
        if (this != &other) {
            size_type n = static_cast<size_type>(other.m_end - other.m_begin);
            if (n > capacity()) {
                destroy_range(m_begin, m_end);
                alloc_raw(n);
                copy_mem(m_begin, other.m_begin, n);
                m_end = m_begin + n;
            } else {
                size_type cur = size();
                if (n <= cur) {
                    copy_mem(m_begin, other.m_begin, n);
                    destroy_range(m_begin + n, m_end);
                    m_end = m_begin + n;
                } else {
                    copy_mem(m_begin, other.m_begin, cur);
                    T* src = other.m_begin + cur;
                    T* dst = m_end;
                    T* last = other.m_end;
                    while (src != last) new (dst++) T(*src++);
                    m_end = m_begin + n;
                }
            }
        }
        return *this;
    }

    GFastVector& operator=(GFastVector&& other) noexcept {
        if (this != &other) {
            destroy_range(m_begin, m_end);
            m_begin = other.m_begin;
            m_end = other.m_end;
            m_cap = other.m_cap;
            other.m_begin = nullptr;
            other.m_end = nullptr;
            other.m_cap = nullptr;
        }
        return *this;
    }

    ~GFastVector() {
        if constexpr (!std::is_trivially_destructible_v<T>) {
            destroy_range(m_begin, m_end);
        }
    }

    [[nodiscard]] G_ALWAYS_INLINE reference operator[](size_type index) noexcept {
        return m_begin[index];
    }
    [[nodiscard]] G_ALWAYS_INLINE const_reference operator[](size_type index) const noexcept {
        return m_begin[index];
    }
    [[nodiscard]] G_ALWAYS_INLINE size_type size() const noexcept {
        return static_cast<size_type>(m_end - m_begin);
    }
    [[nodiscard]] G_ALWAYS_INLINE bool empty() const noexcept {
        return m_begin == m_end;
    }
    [[nodiscard]] G_ALWAYS_INLINE size_type capacity() const noexcept {
        return static_cast<size_type>(m_cap - m_begin);
    }
    [[nodiscard]] G_ALWAYS_INLINE pointer data() noexcept { return m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE const_pointer data() const noexcept { return m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE reference front() noexcept { return *m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE const_reference front() const noexcept { return *m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE reference back() noexcept { return *(m_end - 1); }
    [[nodiscard]] G_ALWAYS_INLINE const_reference back() const noexcept { return *(m_end - 1); }
    [[nodiscard]] G_ALWAYS_INLINE iterator begin() noexcept { return m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE const_iterator begin() const noexcept { return m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE const_iterator cbegin() const noexcept { return m_begin; }
    [[nodiscard]] G_ALWAYS_INLINE iterator end() noexcept { return m_end; }
    [[nodiscard]] G_ALWAYS_INLINE const_iterator end() const noexcept { return m_end; }
    [[nodiscard]] G_ALWAYS_INLINE const_iterator cend() const noexcept { return m_end; }

    G_ALWAYS_INLINE void push_back(const T& value) {
        if (G_UNLIKELY(m_end == m_cap)) grow_slow();
        if constexpr (std::is_trivially_copyable_v<T>) *m_end = value;
        else new (m_end) T(value);
        ++m_end;
    }

    G_ALWAYS_INLINE void push_back(T&& value) {
        if (G_UNLIKELY(m_end == m_cap)) grow_slow();
        if constexpr (std::is_trivially_copyable_v<T>) *m_end = value;
        else new (m_end) T(std::move(value));
        ++m_end;
    }

    template<typename... Args>
    G_ALWAYS_INLINE reference emplace_back(Args&&... args) {
        if (G_UNLIKELY(m_end == m_cap)) grow_slow();
        new (m_end) T(std::forward<Args>(args)...);
        return *(m_end++);
    }

    G_ALWAYS_INLINE void pop_back() {
        --m_end;
        if constexpr (!std::is_trivially_destructible_v<T>) m_end->~T();
    }

    void clear() {
        destroy_range(m_begin, m_end);
        m_end = m_begin;
    }

    void reserve(size_type new_cap) {
        if (new_cap > capacity()) reallocate(new_cap);
    }

    void resize(size_type new_size) {
        size_type cur = size();
        if (new_size > cur) {
            if (G_UNLIKELY(new_size > capacity())) reallocate(new_size);
            default_init(m_end, new_size - cur);
            m_end = m_begin + new_size;
        } else {
            destroy_range(m_begin + new_size, m_end);
            m_end = m_begin + new_size;
        }
    }

    void resize_uninitialized(size_type new_size) {
        if (new_size > capacity()) reallocate(new_size);
        m_end = m_begin + new_size;
    }

    void assign(size_type count, const T& value) {
        clear();
        if (count > capacity()) reallocate(count);
        T* G_RESTRICT ptr = m_begin;
        for (size_type i = 0; i < count; ++i) {
             if constexpr (std::is_trivially_copyable_v<T>) *ptr = value;
             else new (ptr) T(value);
             ++ptr;
        }
        m_end = m_begin + count;
    }

    template <typename InputIt, typename = std::enable_if_t<!std::is_integral_v<InputIt>>>
    void assign(InputIt first, InputIt last) {
        clear();
        if constexpr (std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category>) {
             size_type n = static_cast<size_type>(last - first);
             if (n > 0) {
                if (n > capacity()) reallocate(n);
                if constexpr (std::is_pointer_v<InputIt> && std::is_trivially_copyable_v<T>) {
                     __builtin_memcpy(m_begin, first, n * sizeof(T));
                } else {
                    T* G_RESTRICT ptr = m_begin;
                    while (first != last) new (ptr++) T(*first++);
                }
                m_end = m_begin + n;
             }
        } else {
             while (first != last) push_back(*first++);
        }
    }

    iterator insert(const_iterator pos, const T& value) { return insert_val(pos, value); }
    iterator insert(const_iterator pos, T&& value) { return insert_val(pos, std::move(value)); }

    template <typename InputIt, typename = std::enable_if_t<!std::is_integral_v<InputIt>>>
    iterator insert(const_iterator pos, InputIt first, InputIt last) {
        difference_type offset = pos - m_begin;
        
        if constexpr (std::is_base_of_v<std::forward_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category>) {
            size_type count = std::distance(first, last);
            if (G_UNLIKELY(count == 0)) return m_begin + offset;

            if (static_cast<size_type>(m_cap - m_end) < count) {
                size_type min_needed = size() + count;
                size_type next_cap = capacity() ? capacity() * 2 : 8;
                if (next_cap < min_needed) next_cap = min_needed;
                reallocate(next_cap);
            }

            T* G_RESTRICT ptr = m_begin + offset;
            
            if (ptr == m_end) {
                if constexpr (std::is_trivially_copyable_v<T> && std::is_pointer_v<InputIt>) {
                    __builtin_memcpy(ptr, first, count * sizeof(T));
                } else {
                    while (first != last) new (ptr++) T(*first++);
                }
                m_end += count;
            } else {
                if constexpr (std::is_trivially_copyable_v<T>) {
                     __builtin_memmove(ptr + count, ptr, (m_end - ptr) * sizeof(T));
                     if constexpr (std::is_pointer_v<InputIt>) {
                         __builtin_memcpy(ptr, first, count * sizeof(T));
                     } else {
                         while (first != last) *ptr++ = *first++;
                     }
                } else {
                    T* src = m_end - 1;
                    T* dest = m_end + count - 1;
                    T* stop = ptr;
                    while (src >= stop) {
                         if (dest >= m_end) new (dest) T(std::move(*src));
                         else *dest = std::move(*src);
                         --src; --dest;
                    }
                    while (first != last) {
                        if (ptr >= m_end) new (ptr) T(*first++);
                        else *ptr = T(*first++);
                        ++ptr;
                    }
                }
                m_end += count;
            }
            return m_begin + offset;
        } else {
            size_type current_off = offset;
            while (first != last) {
                insert(m_begin + current_off, *first++);
                current_off++;
            }
            return m_begin + offset;
        }
    }

    iterator erase(const_iterator pos) {
        iterator p = const_cast<iterator>(pos);
        if constexpr (std::is_trivially_copyable_v<T>) {
            __builtin_memmove(p, p + 1, (m_end - p - 1) * sizeof(T));
        } else {
            T* end_minus_1 = m_end - 1;
            for (T* curr = p; curr != end_minus_1; ++curr) {
                *curr = std::move(*(curr + 1));
            }
            end_minus_1->~T();
        }
        --m_end;
        return p;
    }

    iterator erase(const_iterator first, const_iterator last) {
        if (first == last) return const_cast<iterator>(last);
        iterator p_first = const_cast<iterator>(first);
        iterator p_last = const_cast<iterator>(last);
        difference_type n = p_last - p_first;
        
        if constexpr (std::is_trivially_copyable_v<T>) {
            __builtin_memmove(p_first, p_last, (m_end - p_last) * sizeof(T));
        } else {
            T* dst = p_first;
            T* src = p_last;
            while (src != m_end) *dst++ = std::move(*src++);
            destroy_range(dst, m_end);
        }
        m_end -= n;
        return p_first;
    }

    void swap(GFastVector& other) noexcept {
        T* t_begin = m_begin; m_begin = other.m_begin; other.m_begin = t_begin;
        T* t_end = m_end; m_end = other.m_end; other.m_end = t_end;
        T* t_cap = m_cap; m_cap = other.m_cap; other.m_cap = t_cap;
    }

    friend bool operator==(const GFastVector& lhs, const GFastVector& rhs) {
        size_type s = static_cast<size_type>(lhs.m_end - lhs.m_begin);
        if (s != static_cast<size_type>(rhs.m_end - rhs.m_begin)) return false;
        if constexpr (std::is_integral_v<T> || std::is_pointer_v<T>) {
            return s == 0 || __builtin_memcmp(lhs.m_begin, rhs.m_begin, s * sizeof(T)) == 0;
        } else {
            const T* l = lhs.m_begin;
            const T* r = rhs.m_begin;
            for (; s > 0; --s, ++l, ++r) if (*l != *r) return false;
            return true;
        }
    }
    friend bool operator!=(const GFastVector& lhs, const GFastVector& rhs) { return !(lhs == rhs); }

private:
    G_NOINLINE void grow_slow() {
        size_type cap = capacity();
        reallocate(cap ? cap * 2 : 8);
    }

    void reallocate(size_type new_cap) {
        const size_t current_bytes = (m_cap - m_begin) * sizeof(T);
        const size_t new_bytes = new_cap * sizeof(T);

        if (m_begin && g_GlobalArena->try_expand(m_begin, current_bytes, new_bytes)) {
            m_cap = m_begin + new_cap;
            return;
        }

        T* new_begin = static_cast<T*>(G_ASSUME_ALIGNED(g_GlobalArena->alloc<T>(new_cap)));
        size_type count = size();
        
        if (count) {
            if constexpr (std::is_trivially_copyable_v<T>) {
                __builtin_memcpy(new_begin, m_begin, count * sizeof(T));
            } else {
                T* dst = new_begin;
                T* src = m_begin;
                T* end = m_end;
                while (src != end) {
                    new (dst++) T(std::move(*src));
                    src->~T();
                    ++src;
                }
            }
        }
        
        m_begin = new_begin;
        m_end = new_begin + count;
        m_cap = new_begin + new_cap;
    }

    G_ALWAYS_INLINE void alloc_raw(size_type n) {
        m_begin = static_cast<T*>(G_ASSUME_ALIGNED(g_GlobalArena->alloc<T>(n)));
        m_end = m_begin;
        m_cap = m_begin + n;
    }

    G_ALWAYS_INLINE void destroy_range(T* first, T* last) {
        if constexpr (!std::is_trivially_destructible_v<T>) {
            while (first != last) {
                first->~T();
                ++first;
            }
        }
    }

    G_ALWAYS_INLINE void default_init(T* ptr, size_type count) {
        if constexpr (!std::is_trivially_default_constructible_v<T>) {
            for (size_type i = 0; i < count; ++i) new (ptr + i) T();
        } else {
            __builtin_memset(ptr, 0, count * sizeof(T));
        }
    }

    G_ALWAYS_INLINE void copy_mem(T* dst, const T* src, size_type count) {
        if constexpr (std::is_trivially_copyable_v<T>) {
            __builtin_memcpy(dst, src, count * sizeof(T));
        } else {
            for (size_type i = 0; i < count; ++i) new (dst + i) T(src[i]);
        }
    }

    template<typename U>
    iterator insert_val(const_iterator pos, U&& value) {
        difference_type offset = pos - m_begin;
        if (G_UNLIKELY(m_end == m_cap)) grow_slow();
        
        T* ptr = m_begin + offset;
        if (ptr == m_end) {
            new (m_end) T(std::forward<U>(value));
            ++m_end;
        } else {
            new (m_end) T(std::move(*(m_end - 1)));
            ++m_end;
            if constexpr (std::is_trivially_copyable_v<T>) {
                __builtin_memmove(ptr + 1, ptr, (m_end - 1 - ptr - 1) * sizeof(T));
            } else {
                T* dst = m_end - 1;
                T* src = m_end - 2;
                while (dst > ptr) *dst-- = std::move(*src--);
            }
            *ptr = std::forward<U>(value);
        }
        return ptr;
    }
};