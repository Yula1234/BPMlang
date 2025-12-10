

#define RESTRICT __restrict__

template <typename Key, typename Value, typename Hasher = std::hash<Key>, typename Equal = std::equal_to<Key>>
class GFlatMap {
public:
    using value_type = std::pair<Key, Value>;
    
    using HashAllocator = GlobalArenaAllocator<size_t>;
    using DataAllocator = GlobalArenaAllocator<value_type>;

private:
    static FORCE_INLINE size_t mix(size_t h) {
        h ^= h >> 16;
        h *= 0x85ebca6b;
        h ^= h >> 13;
        h *= 0xc2b2ae35;
        h ^= h >> 16;
        return LIKELY(h != 0) ? h : 1;
    }

public:
    struct Iterator {
        size_t* hash_ptr;
        size_t* hash_end;
        value_type* data_ptr;

        FORCE_INLINE Iterator(size_t* h_ptr, size_t* h_end, value_type* d_ptr) 
            : hash_ptr(h_ptr), hash_end(h_end), data_ptr(d_ptr) 
        {
            scan();
        }

        FORCE_INLINE void scan() {
            while (hash_ptr < hash_end && *hash_ptr == 0) {
                hash_ptr++;
                data_ptr++;
            }
        }

        FORCE_INLINE Iterator& operator++() {
            hash_ptr++;
            data_ptr++;
            scan();
            return *this;
        }

        FORCE_INLINE bool operator!=(const Iterator& other) const { return hash_ptr != other.hash_ptr; }
        FORCE_INLINE bool operator==(const Iterator& other) const { return hash_ptr == other.hash_ptr; }
        FORCE_INLINE value_type& operator*() { return *data_ptr; }
        FORCE_INLINE value_type* operator->() const { return data_ptr; }
    };

    GFlatMap(size_t initial_cap = 0) {
        if (initial_cap > 0) reserve(initial_cap);
    }
    
    GFlatMap(std::initializer_list<value_type> init) {
        reserve(init.size());
        for (const auto& p : init) insert(p);
    }

    GFlatMap(const GFlatMap& other) {
        if (other.m_capacity > 0) {
            allocate(other.m_capacity);
            for (size_t i = 0; i < other.m_capacity; ++i) {
                if (other.m_hashes[i] != 0) {
                    insert_direct(other.m_hashes[i], other.m_data[i]);
                }
            }
        }
    }

    GFlatMap& operator=(const GFlatMap& other) {
        if (this != &other) {
            destroy();
            if (other.m_capacity > 0) {
                allocate(other.m_capacity);
                for (size_t i = 0; i < other.m_capacity; ++i) {
                    if (other.m_hashes[i] != 0) {
                        insert_direct(other.m_hashes[i], other.m_data[i]);
                    }
                }
            }
        }
        return *this;
    }

    GFlatMap(GFlatMap&& other) noexcept 
        : m_hashes(other.m_hashes), m_data(other.m_data), 
          m_capacity(other.m_capacity), m_mask(other.m_mask), 
          m_size(other.m_size), m_threshold(other.m_threshold) {
        other.m_hashes = nullptr;
        other.m_data = nullptr;
        other.m_size = 0;
        other.m_capacity = 0;
    }

    GFlatMap& operator=(GFlatMap&& other) noexcept {
        if (this != &other) {
            destroy();
            m_hashes = other.m_hashes;
            m_data = other.m_data;
            m_capacity = other.m_capacity;
            m_mask = other.m_mask;
            m_size = other.m_size;
            m_threshold = other.m_threshold;
            
            other.m_hashes = nullptr;
            other.m_data = nullptr;
            other.m_size = 0;
            other.m_capacity = 0;
        }
        return *this;
    }

    ~GFlatMap() { destroy(); }

    FORCE_INLINE Iterator begin() const { return Iterator(m_hashes, m_hashes + m_capacity, m_data); }
    FORCE_INLINE Iterator end() const { return Iterator(m_hashes + m_capacity, m_hashes + m_capacity, m_data + m_capacity); }

    FORCE_INLINE Iterator find(const Key& key) const {
        if (UNLIKELY(m_size == 0)) return end();

        const size_t hash = mix(Hasher{}(key));
        size_t idx = hash & m_mask;
        const size_t mask = m_mask;
        
        const size_t* RESTRICT hashes = m_hashes;
        const value_type* RESTRICT data = m_data;

        while (true) {
            const size_t h = hashes[idx];
            
            if (h == 0) return end();

            if (h == hash && Equal{}(data[idx].first, key)) {
                return Iterator(const_cast<size_t*>(&hashes[idx]), 
                                const_cast<size_t*>(hashes + m_capacity), 
                                const_cast<value_type*>(&data[idx]));
            }

            idx = (idx + 1) & mask;
        }
    }

    FORCE_INLINE void insert(const value_type& val) { emplace(val.first, val.second); }

    template<typename K, typename V>
    FORCE_INLINE void emplace(K&& key, V&& val) {
        if (UNLIKELY(m_size >= m_threshold)) grow();

        const size_t hash = mix(Hasher{}(key));
        size_t idx = hash & m_mask;
        const size_t mask = m_mask;
        
        size_t* RESTRICT hashes = m_hashes;
        value_type* RESTRICT data = m_data;

        while (true) {
            const size_t h = hashes[idx];

            if (h == 0) {
                hashes[idx] = hash;
                new (&data[idx]) value_type(std::forward<K>(key), std::forward<V>(val));
                m_size++;
                return;
            }

            if (h == hash && Equal{}(data[idx].first, key)) {
                data[idx].second = std::forward<V>(val);
                return;
            }

            idx = (idx + 1) & mask;
        }
    }
    
    FORCE_INLINE Value& operator[](const Key& key) {
        if (UNLIKELY(m_size >= m_threshold)) grow();

        const size_t hash = mix(Hasher{}(key));
        size_t idx = hash & m_mask;
        const size_t mask = m_mask;
        
        size_t* RESTRICT hashes = m_hashes;
        value_type* RESTRICT data = m_data;

        while (true) {
            const size_t h = hashes[idx];

            if (h == 0) {
                hashes[idx] = hash;
                new (&data[idx]) value_type(key, Value{});
                m_size++;
                return data[idx].second;
            }

            if (h == hash && Equal{}(data[idx].first, key)) {
                return data[idx].second;
            }

            idx = (idx + 1) & mask;
        }
    }

    void reserve(size_t n) {
        if (n == 0) return;
        size_t new_cap = 16;
        while (new_cap < n * 2) new_cap <<= 1;
        if (new_cap > m_capacity) rehash(new_cap);
    }

    size_t size() const { return m_size; }
    size_t capacity() const { return m_capacity; }
    bool empty() const { return m_size == 0; }
    void clear() { destroy(); }

private:
    size_t* m_hashes = nullptr;
    value_type* m_data = nullptr;
    size_t m_capacity = 0;
    size_t m_mask = 0;
    size_t m_size = 0;
    size_t m_threshold = 0;

    FORCE_INLINE void insert_direct(size_t hash, const value_type& val) {
        size_t idx = hash & m_mask;
        const size_t mask = m_mask;
        while (m_hashes[idx] != 0) {
            idx = (idx + 1) & mask;
        }
        m_hashes[idx] = hash;
        new (&m_data[idx]) value_type(val);
        m_size++;
    }

    FORCE_INLINE void insert_move_direct(size_t hash, value_type&& val) {
        size_t idx = hash & m_mask;
        const size_t mask = m_mask;
        while (m_hashes[idx] != 0) {
            idx = (idx + 1) & mask;
        }
        m_hashes[idx] = hash;
        new (&m_data[idx]) value_type(std::move(val));
        m_size++;
    }

    void destroy() {
        if (m_hashes) {
            for (size_t i = 0; i < m_capacity; ++i) {
                if (m_hashes[i] != 0) m_data[i].~value_type();
            }
            HashAllocator().deallocate(m_hashes, m_capacity);
            DataAllocator().deallocate(m_data, m_capacity);
            m_hashes = nullptr;
            m_data = nullptr;
            m_size = 0;
            m_capacity = 0;
        }
    }

    void allocate(size_t cap) {
        size_t new_cap = 16;
        while (new_cap < cap) new_cap <<= 1;
        
        m_hashes = HashAllocator().allocate(new_cap);
        std::memset(m_hashes, 0, new_cap * sizeof(size_t));
        
        m_data = DataAllocator().allocate(new_cap);
        
        m_capacity = new_cap;
        m_mask = new_cap - 1;
        m_threshold = new_cap >> 1;
    }

    void rehash(size_t new_cap) {
        size_t* old_hashes = m_hashes;
        value_type* old_data = m_data;
        size_t old_cap = m_capacity;

        m_hashes = HashAllocator().allocate(new_cap);
        std::memset(m_hashes, 0, new_cap * sizeof(size_t));
        m_data = DataAllocator().allocate(new_cap);
        
        m_capacity = new_cap;
        m_mask = new_cap - 1;
        m_threshold = new_cap >> 1;
        m_size = 0;

        if (old_hashes) {
            for (size_t i = 0; i < old_cap; ++i) {
                if (old_hashes[i] != 0) {
                    insert_move_direct(old_hashes[i], std::move(old_data[i]));
                    old_data[i].~value_type();
                }
            }
            HashAllocator().deallocate(old_hashes, old_cap);
            DataAllocator().deallocate(old_data, old_cap);
        }
    }

    void grow() { 
        rehash(m_capacity == 0 ? 16 : m_capacity * 2); 
    }
};