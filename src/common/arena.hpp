#pragma once

class ArenaAllocator {
public:
    explicit ArenaAllocator(const size_t max_num_bytes)
        : m_size { max_num_bytes }
        , m_buffer { new std::byte[max_num_bytes] }
        , m_offset { m_buffer }
    {
    }

    ArenaAllocator(ArenaAllocator& other)
    :   m_size(other.m_size),
        m_buffer(other.m_buffer),
        m_offset(other.m_offset)
    {
    }
    ArenaAllocator& operator=(const ArenaAllocator&) = delete;

    ArenaAllocator(ArenaAllocator&& other) noexcept
        : m_size { std::exchange(other.m_size, 0) }
        , m_buffer { std::exchange(other.m_buffer, nullptr) }
        , m_offset { std::exchange(other.m_offset, nullptr) }
    {
    }

    ArenaAllocator& operator=(ArenaAllocator&& other) noexcept
    {
        std::swap(m_size, other.m_size);
        std::swap(m_buffer, other.m_buffer);
        std::swap(m_offset, other.m_offset);
        return *this;
    }

    template <typename T>
    [[nodiscard]] T* alloc(size_t count = 1)
    {
        size_t remaining_num_bytes = m_size - static_cast<size_t>(m_offset - m_buffer);
        auto pointer = static_cast<void*>(m_offset);
        
        size_t size_to_alloc = sizeof(T) * count;

        const auto aligned_address = std::align(alignof(T), size_to_alloc, pointer, remaining_num_bytes);
        
        if (aligned_address == nullptr) {
            throw std::bad_alloc {};
        }

        m_offset = static_cast<std::byte*>(aligned_address) + size_to_alloc;
        
        return static_cast<T*>(aligned_address);
    }

    template <typename T, typename... Args>
    [[nodiscard]] T* emplace(Args&&... args)
    {
        const auto allocated_memory = alloc<T>();
        return new (allocated_memory) T { std::forward<Args>(args)... };
    }

    ~ArenaAllocator()
    {
        // No destructors are called for the stored objects. Thus, memory
        // leaks are possible (e.g. when storing std::vector objects or
        // other non-trivially destructable objects in the allocator).
        // Although this could be changed, it would come with additional
        // runtime overhead and therefore is not implemented.
        delete[] m_buffer;
    }

private:
    size_t m_size;
    std::byte* m_buffer;
    std::byte* m_offset;
};

ArenaAllocator* g_GlobalArena = new ArenaAllocator((1024 * 1024) * 256);

template <typename T>
class GlobalArenaAllocator {
public:
    using value_type = T;

    GlobalArenaAllocator() noexcept = default;

    template <typename U>
    GlobalArenaAllocator(const GlobalArenaAllocator<U>&) noexcept {}

    [[nodiscard]] T* allocate(std::size_t n) {
        assert(g_GlobalArena != nullptr && "Global arena is not initialized!");
        
        return g_GlobalArena->alloc<T>(n);
    }

    void deallocate(T*, std::size_t) noexcept {
    }

    template<class U>
    bool operator==(const GlobalArenaAllocator<U>&) const noexcept { return true; }

    template<class U>
    bool operator!=(const GlobalArenaAllocator<U>&) const noexcept { return false; }
};


template <typename T>
using GVector = std::vector<T, GlobalArenaAllocator<T>>;

using GString = std::basic_string<char, std::char_traits<char>, GlobalArenaAllocator<char>>;

template <typename Key, typename Value>
using GMap = std::unordered_map<
    Key, 
    Value, 
    std::hash<Key>, 
    std::equal_to<Key>, 
    GlobalArenaAllocator<std::pair<const Key, Value>>
>;

template <
    typename Key,
    typename Hash = std::hash<Key>,
    typename KeyEqual = std::equal_to<Key>
>
using GSet = std::unordered_set<
    Key, 
    Hash, 
    KeyEqual, 
    GlobalArenaAllocator<Key>
>;

using GStringStream = std::basic_stringstream<
    char,
    std::char_traits<char>,
    GlobalArenaAllocator<char>
>;

using GOStringStream = std::basic_ostringstream<
    char,
    std::char_traits<char>,
    GlobalArenaAllocator<char>
>;

using GIStringStream = std::basic_istringstream<
    char,
    std::char_traits<char>,
    GlobalArenaAllocator<char>
>;

template <typename T>
using GDeque = std::deque<
    T,
    GlobalArenaAllocator<T>
>;