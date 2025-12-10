#pragma once


#if defined(_WIN32) || defined(_WIN64)
   
#else
    #include <sys/mman.h>
    #include <unistd.h>
    #ifndef MAP_HUGETLB
        #define MAP_HUGETLB 0x40000
    #endif
    #ifndef MAP_POPULATE
        #define MAP_POPULATE 0x08000
    #endif
#endif

#if defined(__GNUC__) || defined(__clang__)
    #define LIKELY(x) __builtin_expect(!!(x), 1)
    #define UNLIKELY(x) __builtin_expect(!!(x), 0)
    #define FORCE_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
    #define LIKELY(x) (x)
    #define UNLIKELY(x) (x)
    #define FORCE_INLINE __forceinline
#else
    #define LIKELY(x) (x)
    #define UNLIKELY(x) (x)
    #define FORCE_INLINE inline
#endif

class ArenaAllocator {
public:
    explicit ArenaAllocator(const size_t max_num_bytes)
        : m_size { max_num_bytes }
        , m_buffer { nullptr }
        , m_cursor { nullptr }
        , m_limit { nullptr }
    {
        allocate_os_memory(max_num_bytes);
        m_cursor = m_buffer;
        m_limit  = m_buffer + max_num_bytes;
    }

    ArenaAllocator(const ArenaAllocator&) = delete;
    ArenaAllocator& operator=(const ArenaAllocator&) = delete;

    ArenaAllocator(ArenaAllocator&& other) noexcept
        : m_size { std::exchange(other.m_size, 0) }
        , m_buffer { std::exchange(other.m_buffer, nullptr) }
        , m_cursor { std::exchange(other.m_cursor, nullptr) }
        , m_limit { std::exchange(other.m_limit, nullptr) }
    {
    }

    ~ArenaAllocator() {
        free_os_memory();
    }

    template <typename T>
    [[nodiscard]] 
    FORCE_INLINE
    T* alloc(size_t count = 1) {
        const uintptr_t align_mask = alignof(T) - 1;
        const uintptr_t size = sizeof(T) * count;

        uintptr_t current_addr = reinterpret_cast<uintptr_t>(m_cursor);
        uintptr_t aligned_addr = (current_addr + align_mask) & ~align_mask;
        uintptr_t next_cursor  = aligned_addr + size;

        if (UNLIKELY(next_cursor > reinterpret_cast<uintptr_t>(m_limit))) {
            throw std::bad_alloc{}; 
        }

        m_cursor = reinterpret_cast<std::byte*>(next_cursor);
        return reinterpret_cast<T*>(aligned_addr);
    }

    template <typename T, typename... Args>
    [[nodiscard]] 
    FORCE_INLINE
    T* emplace(Args&&... args) {
        T* ptr = alloc<T>();
        new (ptr) T(std::forward<Args>(args)...);
        return ptr;
    }

    [[nodiscard]] size_t freely() const noexcept { return static_cast<size_t>(m_limit - m_cursor); }
    [[nodiscard]] size_t used() const noexcept { return static_cast<size_t>(m_cursor - m_buffer); }

    [[nodiscard]] FORCE_INLINE bool try_expand(void* old_ptr, size_t old_size, size_t new_size) noexcept {
        std::byte* old_byte_ptr = static_cast<std::byte*>(old_ptr);
        if (LIKELY(old_byte_ptr + old_size == m_cursor)) {
            size_t needed = new_size - old_size;
            if (LIKELY(m_cursor + needed <= m_limit)) {
                m_cursor += needed;
                return true;
            }
        }
        return false;
    }
    
    void reset() noexcept { m_cursor = m_buffer; }

private:
    size_t m_size;
    std::byte* m_buffer;
    std::byte* m_cursor;
    std::byte* m_limit;

    void allocate_os_memory(size_t size) {
#if defined(_WIN32) || defined(_WIN64)
        void* ptr = VirtualAlloc(nullptr, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
        if (!ptr) throw std::bad_alloc{};
        
        m_buffer = static_cast<std::byte*>(ptr);
        
        volatile std::byte* touch = m_buffer;
        const size_t page_size = 4096;
        for (size_t i = 0; i < size; i += page_size) {
            touch[i] = std::byte{0};
        }
#else
        void* ptr = mmap(nullptr, size, PROT_READ | PROT_WRITE, 
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE | MAP_HUGETLB, -1, 0);

        if (ptr == MAP_FAILED) {
             ptr = mmap(nullptr, size, PROT_READ | PROT_WRITE, 
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, -1, 0);
        }
        
        if (ptr == MAP_FAILED) throw std::bad_alloc{};
        m_buffer = static_cast<std::byte*>(ptr);
#endif
    }

    void free_os_memory() {
        if (!m_buffer) return;
#if defined(_WIN32) || defined(_WIN64)
        VirtualFree(m_buffer, 0, MEM_RELEASE);
#else
        munmap(m_buffer, m_size);
#endif
    }
};

static uint8_t arena_obj_mem[sizeof(ArenaAllocator)];
inline ArenaAllocator* g_GlobalArena = new (arena_obj_mem) ArenaAllocator((1024 * 1024) * 256);

template <typename T>
class GlobalArenaAllocator {
public:
    using value_type = T;

    GlobalArenaAllocator() noexcept = default;

    template <typename U>
    GlobalArenaAllocator(const GlobalArenaAllocator<U>&) noexcept {}

    [[nodiscard]] 
    FORCE_INLINE
    T* allocate(std::size_t n) {
#ifdef NDEBUG
        return g_GlobalArena->alloc<T>(n);
#else
        assert(g_GlobalArena != nullptr);
        return g_GlobalArena->alloc<T>(n);
#endif
    }

    FORCE_INLINE
    void deallocate(T*, std::size_t) noexcept {}

    template<class U> bool operator==(const GlobalArenaAllocator<U>&) const noexcept { return true; }
    template<class U> bool operator!=(const GlobalArenaAllocator<U>&) const noexcept { return false; }
};



#if defined(__GFAST_VECTOR__)
#include "gfast_vector.hpp"
template <typename T>
using GVector = GFastVector<T>;
#else
template <typename T>
using GVector = std::vector<T, GlobalArenaAllocator<T>>;
#endif

#if defined(__GFAST_STRING__)
#include "gfast_string.hpp"
using GString = GFastString;
#else
using GString = std::basic_string<char, std::char_traits<char>, GlobalArenaAllocator<char>>;
#endif


#if defined(__GFLAT_MAP__)
#include "flat_map.hpp"
template <typename Key, typename Value, typename Hasher = std::hash<Key>, typename Equal = std::equal_to<Key>>
using GMap = GFlatMap<Key, Value, Hasher, Equal>;
#else
template <typename Key, typename Value>
using GMap = std::unordered_map<
    Key, Value, std::hash<Key>, std::equal_to<Key>, 
    GlobalArenaAllocator<std::pair<const Key, Value>>
>;
#endif

template <typename Key, typename Hash = std::hash<Key>, typename KeyEqual = std::equal_to<Key>>
using GSet = std::unordered_set<Key, Hash, KeyEqual, GlobalArenaAllocator<Key>>;

using GStringStream  = std::basic_stringstream<char, std::char_traits<char>, GlobalArenaAllocator<char>>;
using GOStringStream = std::basic_ostringstream<char, std::char_traits<char>, GlobalArenaAllocator<char>>;
using GIStringStream = std::basic_istringstream<char, std::char_traits<char>, GlobalArenaAllocator<char>>;

template <typename T>
using GDeque = std::deque<T, GlobalArenaAllocator<T>>;