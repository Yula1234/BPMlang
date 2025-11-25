#define UNIMPLEMENTED \
    do { \
        fprintf(stderr, "%s:%d: %s is not implemented yet\n", \
                __FILE__, __LINE__, __func__); \
        abort(); \
    } while(0)

#define HEAP_CAP_BYTES 256000
#define HEAP_CAP_WORDS (HEAP_CAP_BYTES / sizeof(uintptr_t))

size_t HEAP_SIZE = HEAP_CAP_WORDS;
uintptr_t heap[HEAP_CAP_WORDS] = {0};
const uintptr_t *stack_base = 0;

static uintptr_t* heap_top   = heap;
static uintptr_t* heap_limit = heap + HEAP_CAP_WORDS;

#define CHUNK_LIST_CAP 2048
#define MAX_ALLOCED_NC 64U

void *memalloc(size_t size_bytes);
void memfree(void *ptr);
void heap_collect();

// ==========================================
// 3. MEMORY MANAGEMENT IMPLEMENTATION
// ==========================================

typedef struct {
    uintptr_t *start;
    size_t size;
} Chunk;

typedef struct {
    size_t count;
    Chunk chunks[CHUNK_LIST_CAP];
} Chunk_List;

Chunk_List alloced_chunks = {0};
Chunk_List freed_chunks = {
    .count = 1,
    .chunks = { [0] = {.start = heap, .size = HEAP_CAP_WORDS } },
};
Chunk_List tmp_chunks = {0};

bool reachable_chunks[CHUNK_LIST_CAP] = {0};
void *to_free[CHUNK_LIST_CAP] = {0};
size_t to_free_count = 0;
uint32_t __all_heap_collects = 0U;

// Helper functions for chunks
uint32_t chunk_list_sizes(const Chunk_List *list) {
    uint32_t res = 0U;
    for (int i = 0;i < list->count;++i) {
        res += list->chunks[i].size;
    }
    return res;
}

// Найти индекс чанка, содержащего указатель p (или -1)
int chunk_list_find_containing(const Chunk_List* list, const uintptr_t* p) {
    size_t lo = 0;
    size_t hi = list->count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        uintptr_t* start = list->chunks[mid].start;
        uintptr_t* end   = start + list->chunks[mid].size;
        if (p < start) {
            hi = mid;
        } else if (p >= end) {
            lo = mid + 1;
        } else {
            return (int)mid;
        }
    }
    return -1;
}

bool need_heap_collect(const Chunk_List* list) {
    if(list->count > MAX_ALLOCED_NC) return true;
    if(chunk_list_sizes(list) > 1024U) return true;
    return false;
}

void chunk_list_insert(Chunk_List *list, void *start, size_t size) {
    assert(list->count < CHUNK_LIST_CAP);
    list->chunks[list->count].start = start;
    list->chunks[list->count].size  = size;

    for (size_t i = list->count;
            i > 0 && list->chunks[i].start < list->chunks[i - 1].start;
            --i) {
        const Chunk t = list->chunks[i];
        list->chunks[i] = list->chunks[i - 1];
        list->chunks[i - 1] = t;
    }
    list->count += 1;
}

void chunk_list_merge(Chunk_List *dst, const Chunk_List *src) {
    dst->count = 0;
    for (size_t i = 0; i < src->count; ++i) {
        const Chunk chunk = src->chunks[i];
        if (dst->count > 0) {
            Chunk *top_chunk = &dst->chunks[dst->count - 1];
            if (top_chunk->start + top_chunk->size == chunk.start) {
                top_chunk->size += chunk.size;
            } else {
                chunk_list_insert(dst, chunk.start, chunk.size);
            }
        } else {
            chunk_list_insert(dst, chunk.start, chunk.size);
        }
    }
}

void chunk_list_dump(const Chunk_List *list, const char *name) {
    printf("%s Chunks (%zu):\n", name, list->count);
    for (size_t i = 0; i < list->count; ++i) {
        printf("  start: %p, size: %zu\n", (void*) list->chunks[i].start, list->chunks[i].size);
    }
}

int chunk_list_find(const Chunk_List *list, uintptr_t *ptr) {
    for (size_t i = 0; i < list->count; ++i) {
        if (list->chunks[i].start == ptr) return (int) i;
    }
    return -1;
}

void chunk_list_remove(Chunk_List *list, size_t index) {
    assert(index < list->count);
    for (size_t i = index; i < list->count - 1; ++i) {
        list->chunks[i] = list->chunks[i + 1];
    }
    list->count -= 1;
}

void dump_all_chunks() {
    puts("---------------------------------");
    chunk_list_dump(&alloced_chunks, "allocated");
    puts("");
    chunk_list_dump(&freed_chunks, "freed");
    puts("---------------------------------");
}

// Memalloc implementation
void *memalloc(size_t size_bytes) {
    // Размер в словах (uintptr_t), выровненный вверх
    size_t size_words = (size_bytes + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
    if (size_words == 0) size_words = 1;

    // 1) Попытка быстрого bump-alloc в статическом heap[]
    if (size_words <= (size_t)(heap_limit - heap_top)) {
        uintptr_t* ptr = heap_top;
        heap_top += size_words;
        chunk_list_insert(&alloced_chunks, ptr, size_words);
        return ptr;
    }

    // 2) Падаем в GC + попытка взять из freed_chunks (heap-память после GC)
    if (size_words < HEAP_CAP_WORDS) {
        // Один раз собираем мусор при нехватке
        static int in_gc = 0;
        if (!in_gc) {
            in_gc = 1;
            heap_collect();
            in_gc = 0;
        }

        // Сольём и отсортируем свободные чанки
        chunk_list_merge(&tmp_chunks, &freed_chunks);
        freed_chunks = tmp_chunks;

        // Ищем подходящий свободный чанк
        for (size_t i = 0; i < freed_chunks.count; ++i) {
            Chunk chunk = freed_chunks.chunks[i];
            if (chunk.size >= size_words) {
                chunk_list_remove(&freed_chunks, i);

                size_t tail = chunk.size - size_words;
                chunk_list_insert(&alloced_chunks, chunk.start, size_words);

                if (tail > 0) {
                    chunk_list_insert(&freed_chunks,
                                      chunk.start + size_words,
                                      tail);
                }
                return chunk.start;
            }
        }
    }

    // 3) Fallback: большие или безнадёжные аллокации идут через malloc
    void* mptr = malloc(size_bytes);
    if (!mptr) return NULL;

    chunk_list_insert(&alloced_chunks, (uintptr_t*)mptr, size_words);
    HEAP_SIZE += size_words;
    return mptr;
}

// Memfree implementation
void memfree(void *ptr) {
    if (ptr != NULL) {
        const int index = chunk_list_find(&alloced_chunks, ptr);
        if(index < 0) {
            __bpm_exception* exc;
            if (__BpmDoubleExceptionTypeId != 0) {
                __DoubleFreeException* __bpm_struct_exc = (__DoubleFreeException*)memalloc(sizeof(__DoubleFreeException));
                __bpm_struct_exc->__addr = (int32_t)ptr;
                exc = __bpm_allocate_exception(__BpmDoubleExceptionTypeId, (int32_t)__bpm_struct_exc, (__what_t)__bpm_double_free_exception_what);
            } else {
                exc = __bpm_allocate_exception(3, (uintptr_t)ptr, (__what_t)__bpm_double_free_exception_what);
            }
            __bpm_throw(exc);
            return;
        }
        assert(ptr == alloced_chunks.chunks[index].start);
        chunk_list_insert(&freed_chunks,
                          alloced_chunks.chunks[index].start,
                          alloced_chunks.chunks[index].size);
        chunk_list_remove(&alloced_chunks, (size_t) index);
    }
}

// Garbage Collector
static void mark_region(const uintptr_t *start, const uintptr_t *end) {
    for (; start < end; start += 1) {
        const uintptr_t *p = (const uintptr_t *) *start;
        if (!p) continue;

        int idx = chunk_list_find_containing(&alloced_chunks, p);
        if (idx >= 0) {
            if (!reachable_chunks[idx]) {
                reachable_chunks[idx] = true;
                Chunk chunk = alloced_chunks.chunks[idx];
                mark_region(chunk.start, chunk.start + chunk.size);
            }
        }
    }
}

void heap_collect() {
    __all_heap_collects += 1;
    const uintptr_t *stack_start = (const uintptr_t*)__builtin_frame_address(0);
    memset(reachable_chunks, 0, sizeof(reachable_chunks));
    mark_region(stack_start, stack_base + 1);
    to_free_count = 0;
    for (size_t i = 0; i < alloced_chunks.count; ++i) {
        if (!reachable_chunks[i]) {
            assert(to_free_count < CHUNK_LIST_CAP);
            to_free[to_free_count++] = alloced_chunks.chunks[i].start;
        }
    }
    for (size_t i = 0; i < to_free_count; ++i) {
        memfree(to_free[i]);
    }
}