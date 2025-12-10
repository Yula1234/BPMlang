#pragma once

#if defined(__GNUC__) || defined(__clang__)
    #define S_LIKELY(x) __builtin_expect(!!(x), 1)
    #define S_UNLIKELY(x) __builtin_expect(!!(x), 0)
    #define S_FORCE_INLINE __attribute__((always_inline)) inline
    #define S_NOINLINE __attribute__((noinline))
    #define S_RESTRICT __restrict__
    #define S_ASSUME_ALIGNED(ptr) __builtin_assume_aligned((ptr), 1)
#elif defined(_MSC_VER)
    #define S_LIKELY(x) (x)
    #define S_UNLIKELY(x) (x)
    #define S_FORCE_INLINE __forceinline
    #define S_NOINLINE __declspec(noinline)
    #define S_RESTRICT __restrict
    #define S_ASSUME_ALIGNED(ptr) (ptr)
#else
    #define S_LIKELY(x) (x)
    #define S_UNLIKELY(x) (x)
    #define S_FORCE_INLINE inline
    #define S_NOINLINE
    #define S_RESTRICT
    #define S_ASSUME_ALIGNED(ptr) (ptr)
#endif

class GFastString {
public:
    using value_type = char;
    using traits_type = std::char_traits<char>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using pointer = char*;
    using const_pointer = const char*;
    using reference = char&;
    using const_reference = const char&;
    using iterator = char*;
    using const_iterator = const char*;

    static constexpr size_type npos = static_cast<size_type>(-1);

private:
    char* m_data; 
    size_type m_size;
    size_type m_cap;

public:
    S_FORCE_INLINE GFastString() noexcept : m_data(nullptr), m_size(0), m_cap(0) {}

    GFastString(const char* str) : m_data(nullptr), m_size(0), m_cap(0) {
        if (str) {
            size_type len = __builtin_strlen(str);
            if (len > 0) {
                init_alloc(len);
                std::memcpy(m_data, str, len);
                m_data[len] = '\0';
            }
        }
    }

    GFastString(const char* str, size_type len) : m_data(nullptr), m_size(0), m_cap(0) {
        if (len > 0 && str) {
            init_alloc(len);
            std::memcpy(m_data, str, len);
            m_data[len] = '\0';
        }
    }

    GFastString(const std::string& str) : m_data(nullptr), m_size(0), m_cap(0) {
        size_type len = str.size();
        if (len > 0) {
            init_alloc(len);
            std::memcpy(m_data, str.data(), len);
            m_data[len] = '\0';
        }
    }

    GFastString(std::string_view str) : m_data(nullptr), m_size(0), m_cap(0) {
        size_type len = str.size();
        if (len > 0) {
            init_alloc(len);
            std::memcpy(m_data, str.data(), len);
            m_data[len] = '\0';
        }
    }

    GFastString(size_type count, char ch) : m_data(nullptr), m_size(0), m_cap(0) {
        if (count > 0) {
            init_alloc(count);
            std::memset(m_data, ch, count);
            m_data[count] = '\0';
        }
    }

    // LTO-Safe Copy Constructor
    GFastString(const GFastString& other) {
        // Shallow copy first. This tricks LTO into knowing 'this' is initialized if 'other' was.
        std::memcpy(this, &other, sizeof(GFastString));
        
        // Now perform deep copy if necessary
        if (m_size > 0 && m_data) {
            // Allocate new memory for our own copy
            init_alloc(m_size);
            std::memcpy(m_data, other.m_data, m_size);
            m_data[m_size] = '\0';
        } else {
            // Reset to clean state if empty or invalid
            m_data = nullptr;
            m_size = 0;
            m_cap = 0;
        }
    }

    GFastString(GFastString&& other) noexcept {
        std::memcpy(this, &other, sizeof(GFastString));
        other.m_data = nullptr;
        other.m_size = 0;
        other.m_cap = 0;
    }

    GFastString& operator=(const GFastString& other) {
        if (this != &other) assign(other.m_data, other.m_size);
        return *this;
    }

    GFastString& operator=(GFastString&& other) noexcept {
        if (this != &other) {
            m_data = other.m_data;
            m_size = other.m_size;
            m_cap = other.m_cap;
            other.m_data = nullptr;
            other.m_size = 0;
            other.m_cap = 0;
        }
        return *this;
    }

    GFastString& operator=(const char* str) {
        if (str) assign(str, __builtin_strlen(str));
        else clear();
        return *this;
    }

    GFastString& operator=(std::string_view str) { assign(str.data(), str.size()); return *this; }
    GFastString& operator=(const std::string& str) { assign(str.data(), str.size()); return *this; }

    explicit operator const char*() const noexcept { return m_data ? m_data : ""; }
    operator std::string_view() const noexcept { return std::string_view(m_data, m_size); }
    operator std::string() const { return std::string(data(), size()); }

    [[nodiscard]] S_FORCE_INLINE const char* c_str() const noexcept { return m_data ? m_data : ""; }
    [[nodiscard]] S_FORCE_INLINE const char* data() const noexcept { return m_data; }
    [[nodiscard]] S_FORCE_INLINE char* data() noexcept { return m_data; }
    [[nodiscard]] S_FORCE_INLINE size_type size() const noexcept { return m_size; }
    [[nodiscard]] S_FORCE_INLINE size_type length() const noexcept { return m_size; }
    [[nodiscard]] S_FORCE_INLINE bool empty() const noexcept { return m_size == 0; }
    [[nodiscard]] S_FORCE_INLINE size_type capacity() const noexcept { return m_cap; }
    
    [[nodiscard]] S_FORCE_INLINE char& operator[](size_type index) { return m_data[index]; }
    [[nodiscard]] S_FORCE_INLINE const char& operator[](size_type index) const { return m_data[index]; }
    [[nodiscard]] S_FORCE_INLINE char& operator[](int index) { return m_data[index]; }
    [[nodiscard]] S_FORCE_INLINE const char& operator[](int index) const { return m_data[index]; }

    [[nodiscard]] S_FORCE_INLINE iterator begin() noexcept { return m_data; }
    [[nodiscard]] S_FORCE_INLINE const_iterator begin() const noexcept { return m_data; }
    [[nodiscard]] S_FORCE_INLINE const_iterator cbegin() const noexcept { return m_data; }
    [[nodiscard]] S_FORCE_INLINE iterator end() noexcept { return m_data + m_size; }
    [[nodiscard]] S_FORCE_INLINE const_iterator end() const noexcept { return m_data + m_size; }
    [[nodiscard]] S_FORCE_INLINE const_iterator cend() const noexcept { return m_data + m_size; }
    [[nodiscard]] S_FORCE_INLINE char& front() { return m_data[0]; }
    [[nodiscard]] S_FORCE_INLINE const char& front() const { return m_data[0]; }
    [[nodiscard]] S_FORCE_INLINE char& back() { return m_data[m_size - 1]; }
    [[nodiscard]] S_FORCE_INLINE const char& back() const { return m_data[m_size - 1]; }

    void clear() {
        m_size = 0;
        if(m_data) m_data[0] = '\0';
    }

    void reserve(size_type new_cap) {
        if (new_cap > m_cap) reallocate(new_cap);
    }

    void assign(const char* str, size_type len) {
        if (len > m_cap) reallocate(len);
        if (len > 0 && str) std::memcpy(m_data, str, len);
        m_size = len;
        if(m_data) m_data[len] = '\0';
    }

    iterator erase(const_iterator pos) {
        char* p = const_cast<char*>(pos);
        std::memmove(p, p + 1, m_data + m_size - (p + 1));
        --m_size;
        m_data[m_size] = '\0';
        return p;
    }

    iterator erase(const_iterator first, const_iterator last) {
        if (first == last) return const_cast<char*>(last);
        char* p_first = const_cast<char*>(first);
        char* p_last = const_cast<char*>(last);
        size_type count = p_last - p_first;
        std::memmove(p_first, p_last, m_data + m_size - p_last);
        m_size -= count;
        m_data[m_size] = '\0';
        return p_first;
    }

    GFastString& erase(size_type pos = 0, size_type count = npos) {
        if (pos >= m_size) return *this;
        if (count == npos || pos + count >= m_size) {
            m_size = pos;
        } else {
            std::memmove(m_data + pos, m_data + pos + count, m_size - (pos + count));
            m_size -= count;
        }
        if (m_data) m_data[m_size] = '\0';
        return *this;
    }

    S_FORCE_INLINE GFastString& append(const char* str, size_type len) {
        if (len == 0) return *this;
        size_type needed = m_size + len;
        if (S_UNLIKELY(needed > m_cap)) grow(needed);
        std::memcpy(m_data + m_size, str, len);
        m_size = needed;
        m_data[needed] = '\0';
        return *this;
    }

    S_FORCE_INLINE GFastString& append(const char* str) { return append(str, __builtin_strlen(str)); }
    S_FORCE_INLINE GFastString& append(std::string_view str) { return append(str.data(), str.size()); }
    S_FORCE_INLINE GFastString& append(const std::string& str) { return append(str.data(), str.size()); }
    S_FORCE_INLINE GFastString& append(const GFastString& str) { return append(str.data(), str.size()); }

    S_FORCE_INLINE GFastString& operator+=(char ch) {
        if (S_UNLIKELY(m_size + 1 > m_cap)) grow(m_size + 1);
        m_data[m_size++] = ch;
        m_data[m_size] = '\0';
        return *this;
    }

    S_FORCE_INLINE GFastString& operator+=(const char* str) { return append(str); }
    S_FORCE_INLINE GFastString& operator+=(const GFastString& str) { return append(str.data(), str.size()); }
    S_FORCE_INLINE GFastString& operator+=(std::string_view str) { return append(str.data(), str.size()); }
    S_FORCE_INLINE GFastString& operator+=(const std::string& str) { return append(str.data(), str.size()); }

    S_FORCE_INLINE void push_back(char ch) { *this += ch; }
    void pop_back() { if (m_size) m_data[--m_size] = '\0'; }

    void resize(size_type n, char ch = '\0') {
        if (n > m_size) {
            if (n > m_cap) reallocate(n);
            std::memset(m_data + m_size, ch, n - m_size);
        }
        m_size = n;
        if(m_data) m_data[n] = '\0';
    }

    size_type find(char ch, size_type pos = 0) const noexcept {
        if (pos >= m_size) return npos;
        const char* p = static_cast<const char*>(std::memchr(m_data + pos, ch, m_size - pos));
        return p ? static_cast<size_type>(p - m_data) : npos;
    }

    size_type find(const char* str, size_type pos = 0) const noexcept {
        if (pos >= m_size) return npos;
        size_type len = __builtin_strlen(str);
        if (len == 0) return pos;
        if (len > m_size - pos) return npos;
        if (len == 1) return find(str[0], pos);
        const char* haystack = m_data + pos;
        size_type n = m_size - pos;
        while (n >= len) {
            const char* match = static_cast<const char*>(std::memchr(haystack, str[0], n - len + 1));
            if (!match) return npos;
            if (std::memcmp(match, str, len) == 0) return static_cast<size_type>(match - m_data);
            size_type advance = (match - haystack) + 1;
            haystack += advance;
            n -= advance;
        }
        return npos;
    }

    size_type find(const GFastString& str, size_type pos = 0) const noexcept { return find(str.c_str(), pos); }

    GFastString substr(size_type pos = 0, size_type count = npos) const {
        if (pos >= m_size) return GFastString();
        size_type rcount = (count == npos || pos + count > m_size) ? (m_size - pos) : count;
        return GFastString(m_data + pos, rcount);
    }

    int compare(const char* str) const noexcept {
        return __builtin_strcmp(m_data ? m_data : "", str ? str : "");
    }

    int compare(const GFastString& other) const noexcept {
        size_type min_len = m_size < other.m_size ? m_size : other.m_size;
        int res = (m_size == 0 && other.m_size == 0) ? 0 : std::memcmp(m_data ? m_data : "", other.m_data ? other.m_data : "", min_len);
        if (res == 0) return (m_size < other.m_size) ? -1 : (m_size > other.m_size ? 1 : 0);
        return res;
    }

    int compare(size_type pos1, size_type count1, const GFastString& str) const {
        if (pos1 > m_size) return -1;
        size_type rcount = (count1 == npos || pos1 + count1 > m_size) ? (m_size - pos1) : count1;
        size_type min_len = (rcount < str.m_size) ? rcount : str.m_size;
        int res = (min_len == 0) ? 0 : std::memcmp(m_data + pos1, str.m_data, min_len);
        if (res != 0) return res;
        return (rcount < str.m_size) ? -1 : (rcount > str.m_size ? 1 : 0);
    }
    
    int compare(size_type pos1, size_type count1, const char* s) const {
        if (pos1 > m_size) return -1;
        size_type rcount = (count1 == npos || pos1 + count1 > m_size) ? (m_size - pos1) : count1;
        size_type s_len = __builtin_strlen(s);
        size_type min_len = (rcount < s_len) ? rcount : s_len;
        int res = (min_len == 0) ? 0 : std::memcmp(m_data + pos1, s, min_len);
        if (res != 0) return res;
        return (rcount < s_len) ? -1 : (rcount > s_len ? 1 : 0);
    }

    bool operator==(const GFastString& other) const { return m_size == other.m_size && (m_size == 0 || std::memcmp(m_data, other.m_data, m_size) == 0); }
    bool operator!=(const GFastString& other) const { return !(*this == other); }
    bool operator<(const GFastString& other) const { return compare(other) < 0; }
    
    friend bool operator==(const GFastString& lhs, const char* rhs) { return lhs.compare(rhs) == 0; }
    friend bool operator==(const char* lhs, const GFastString& rhs) { return rhs.compare(lhs) == 0; }
    friend bool operator!=(const GFastString& lhs, const char* rhs) { return lhs.compare(rhs) != 0; }
    friend bool operator!=(const char* lhs, const GFastString& rhs) { return rhs.compare(lhs) != 0; }

    friend GFastString operator+(const GFastString& lhs, const GFastString& rhs) {
        GFastString res;
        size_type needed = lhs.m_size + rhs.m_size;
        res.init_alloc(needed);
        if(lhs.m_size) std::memcpy(res.m_data, lhs.m_data, lhs.m_size);
        if(rhs.m_size) std::memcpy(res.m_data + lhs.m_size, rhs.m_data, rhs.m_size);
        res.m_data[needed] = '\0';
        return res;
    }
    
    friend GFastString operator+(const GFastString& lhs, const char* rhs) {
        GFastString res;
        size_type r_len = __builtin_strlen(rhs);
        size_type needed = lhs.m_size + r_len;
        res.init_alloc(needed);
        if(lhs.m_size) std::memcpy(res.m_data, lhs.m_data, lhs.m_size);
        if(r_len) std::memcpy(res.m_data + lhs.m_size, rhs, r_len);
        res.m_data[needed] = '\0';
        return res;
    }

    friend GFastString operator+(const char* lhs, const GFastString& rhs) {
        GFastString res;
        size_type l_len = __builtin_strlen(lhs);
        size_type needed = l_len + rhs.m_size;
        res.init_alloc(needed);
        if(l_len) std::memcpy(res.m_data, lhs, l_len);
        if(rhs.m_size) std::memcpy(res.m_data + l_len, rhs.m_data, rhs.m_size);
        res.m_data[needed] = '\0';
        return res;
    }
    
    friend GFastString operator+(const GFastString& lhs, const std::string& rhs) {
        GFastString res;
        size_type r_len = rhs.size();
        size_type needed = lhs.m_size + r_len;
        res.init_alloc(needed);
        if(lhs.m_size) std::memcpy(res.m_data, lhs.m_data, lhs.m_size);
        if(r_len) std::memcpy(res.m_data + lhs.m_size, rhs.data(), r_len);
        res.m_data[needed] = '\0';
        return res;
    }

private:
    S_NOINLINE void grow(size_type min_cap) {
        size_type new_cap = (m_cap == 0) ? 32 : m_cap * 2;
        if (new_cap < min_cap) new_cap = min_cap;
        reallocate(new_cap);
    }

    void reallocate(size_type new_cap) {
        size_type req = new_cap + 1;
        if (m_data && g_GlobalArena->try_expand(m_data, m_cap + 1, req)) {
            m_cap = new_cap;
            return;
        }
        char* new_data = static_cast<char*>(g_GlobalArena->alloc<char>(req));
        if (m_size > 0 && m_data) std::memcpy(new_data, m_data, m_size);
        m_data = new_data;
        m_cap = new_cap;
    }

    void init_alloc(size_type cap) {
        m_data = static_cast<char*>(g_GlobalArena->alloc<char>(cap + 1));
        m_cap = cap;
        m_size = cap;
    }
};

inline std::ostream& operator<<(std::ostream& os, const GFastString& str) {
    return os.write(str.data(), str.size());
}

namespace std {
    template <> struct hash<GFastString> {
        std::size_t operator()(const GFastString& s) const noexcept {
            const char* ptr = s.data();
            const char* end = ptr + s.size();
            std::size_t hash;
            if constexpr (sizeof(std::size_t) == 8) {
                hash = 14695981039346656037ULL;
                while (ptr != end) {
                    hash ^= static_cast<std::size_t>(*ptr++);
                    hash *= 1099511628211ULL;
                }
            } else {
                hash = 2166136261U;
                while (ptr != end) {
                    hash ^= static_cast<std::size_t>(*ptr++);
                    hash *= 16777619U;
                }
            }
            return hash;
        }
    };
}