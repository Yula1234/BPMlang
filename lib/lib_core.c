#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>

// ==========================================
// 1. GLOBALS & CONSTANTS
// ==========================================

FILE* get_cstdout() { return stdout; }
FILE* get_cstderr() { return stderr; }
FILE* get_cstdin()  { return stdin; }

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

#define CHUNK_LIST_CAP 2048
#define MAX_ALLOCED_NC 64U

#define TRACEBACK_SIZE_LIMIT 1024

// ==========================================
// 2. FORWARD DECLARATIONS (PROTOTYPES)
// ==========================================

// Типы исключений и функций
typedef char*(*__what_t)(void*);

typedef struct __bpm_exception_30 {
    int32_t __type_id;
    uintptr_t __val;
    __what_t __what;
} __bpm_exception;

typedef struct __BPMDoubleFreeException {
    int32_t __addr;
    uintptr_t __bstub1;
    __what_t __bstub2;
} __DoubleFreeException;

typedef struct __BPMRecursionException {
    int32_t __bstub;
    uintptr_t __bstub1;
    __what_t __bstub2;
} __RecursionException;

typedef struct __BPMSigSegvException {
    int32_t __addr;
    uintptr_t __bstub1;
    __what_t __bstub2;
} __SigSegvException;

// Прототипы функций исключений
__bpm_exception* __bpm_allocate_exception(int32_t __tid, uintptr_t __v, __what_t __wh);
void __bpm_throw(__bpm_exception* __exception);
char* __bpm_double_free_exception_what(void* __val);
char* __bpm_recursion_exception_what(void* __val);

// Прототипы аллокатора
void *memalloc(size_t size_bytes);
void memfree(void *ptr);
void heap_collect();

// Глобальные переменные исключений (extern)
extern int32_t __BpmDoubleExceptionTypeId;
extern int32_t __BpmRecursionExceptionTypeId;
extern int32_t __BpmSigSegvExceptionTypeId;

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
    if(need_heap_collect(&alloced_chunks)) heap_collect();
    const size_t size_words = ((size_bytes + (sizeof(uintptr_t) - 1) / sizeof(uintptr_t)) + 4);
    
    if(size_words >= HEAP_CAP_WORDS) {
        void* ptr = malloc(size_words * 4);
        chunk_list_insert(&alloced_chunks, ptr, size_words);
        HEAP_SIZE += size_words;
        return ptr;
    }

    if (size_words > 0) {
        chunk_list_merge(&tmp_chunks, &freed_chunks);
        freed_chunks = tmp_chunks;

        for (size_t i = 0; i < freed_chunks.count; ++i) {
            const Chunk chunk = freed_chunks.chunks[i];
            if (chunk.size >= size_words) {
                chunk_list_remove(&freed_chunks, i);

                const size_t tail_size_words = chunk.size - size_words;
                chunk_list_insert(&alloced_chunks, chunk.start, size_words);

                if (tail_size_words > 0) {
                    chunk_list_insert(&freed_chunks, chunk.start + size_words, tail_size_words);
                }
                return chunk.start;
            }
        }
    }
    return NULL;
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
        for (size_t i = 0; i < alloced_chunks.count; ++i) {
            Chunk chunk = alloced_chunks.chunks[i];
            if (chunk.start <= p && p < chunk.start + chunk.size) {
                if (!reachable_chunks[i]) {
                    reachable_chunks[i] = true;
                    mark_region(chunk.start, chunk.start + chunk.size);
                }
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

// ==========================================
// 4. EXCEPTION & TRACEBACK IMPLEMENTATION
// ==========================================

char* __bpm_double_free_exception_what(void* __val) {
    static char buf[128];
    snprintf(buf, sizeof(buf), "double free of pointer %p", __val);
    return buf;
}

char* __bpm_recursion_exception_what(void* __val) {
    return "the traceback size has exceeded 1024 elements.";
}

typedef struct bpm_func_t {
    char* name;
} func_t;

typedef struct bpm_traceback_t {
    func_t funcs[TRACEBACK_SIZE_LIMIT];
    int32_t count;
} traceback_t;

traceback_t traceback;

void traceback_push(char* name) {
    if(traceback.count >= TRACEBACK_SIZE_LIMIT) {
        __bpm_exception* exc;
        if (__BpmRecursionExceptionTypeId != 0) {
            __RecursionException* __bpm_struct_exc = (__RecursionException*)memalloc(sizeof(__RecursionException));
            exc = __bpm_allocate_exception(__BpmRecursionExceptionTypeId, (int32_t)__bpm_struct_exc, (__what_t)__bpm_recursion_exception_what);
        } else {
            exc = __bpm_allocate_exception(3, 0, (__what_t)__bpm_recursion_exception_what);
        }
        __bpm_throw(exc);
    }
    traceback.funcs[traceback.count].name = name;
    traceback.count += 1;
}

void traceback_pop() {
    traceback.count -= 1;
}

void traceback_print() {
    printf("Traceback (most recent call last):\n");
    
    int limit = traceback.count;
    if (limit > TRACEBACK_SIZE_LIMIT) limit = TRACEBACK_SIZE_LIMIT; 
    if (limit < 0) limit = 0;

    for(int i = 0; i < limit; ++i) {
        printf("    at %s(...)\n", traceback.funcs[i].name);
    }
    putc('\n', stdout);
}

jmp_buf __exception_bufs[1024];
int32_t __exception_bufs_lvl = 0;

__bpm_exception* __current_exception = NULL;

int32_t __exception_handled[1024] = {-1};
int32_t __exception_handle_lvl = 0;

void** __type_id_table;

__bpm_exception* __bpm_allocate_exception(int32_t __tid, uintptr_t __v, __what_t __wh) {
    __bpm_exception* __exception = (__bpm_exception*)memalloc(sizeof(__bpm_exception));
    __exception->__type_id = __tid;
    __exception->__val = __v;
    __exception->__what = __wh;
    return __exception;
}

void __bpm_terminate() {
    traceback_print();
    if(__current_exception == NULL) {
        printf("process terminated.\n");
        abort();
    }
    size_t table_index = __current_exception->__type_id;
    __what_t __what = __current_exception->__what;
    char* instance = __type_id_table[table_index];
    printf("    process terminated after throwing of instance `%s`.\n", instance);
    if(__what != NULL) {
        printf("        %s::what(): %s.\n", instance, __what((void*)(__current_exception->__val)));
    }
    abort();
}

void __bpm_start_catch(size_t __tid) {
    __exception_handled[++__exception_handle_lvl] = __tid;
}

void __bpm_end_catch() {
    __exception_handled[__exception_handle_lvl--] = -1;
}

void __bpm_throw(__bpm_exception* __exception) {
    if (__current_exception != NULL) {
        printf("FATAL ERROR: Exception thrown while handling another exception (Double Fault).\n");
        printf("    Current exception address: %p\n", (void*)__current_exception->__val);
        printf("    New exception address:     %p\n", (void*)__exception->__val);
        abort();
    }

    __current_exception = __exception;
    while (__exception_bufs_lvl > 0) {
        int32_t expected_type = __exception_handled[__exception_handle_lvl];

        if (expected_type == __exception->__type_id) {
            __bpm_end_catch(); 
            longjmp(__exception_bufs[__exception_bufs_lvl--], 1);
        }
        __exception_handle_lvl--;
        __exception_bufs_lvl--;
    }
    __bpm_terminate();
}

uintptr_t __bpm_get_current_exception() {
    __bpm_exception* exc = __current_exception;
    __current_exception = NULL;
    assert(exc != NULL);
    return exc->__val;
}

int32_t __bpm_exception_throwed() {
    if(__current_exception != NULL) return 1;
    return 0;
}

char* __sigsegv_wh_exception(void* addr) {
    static char buf[128];
    snprintf(buf, sizeof(buf), "segmentation fault at address %p", addr);
    return buf;
}

char* __wexcp_wh_exception(void* code_ptr) {
    static char buf[128];
    DWORD code = (DWORD)(uintptr_t)code_ptr;
    snprintf(buf, sizeof(buf), "windows exception code 0x%08lx",
             (unsigned long)code);
    return buf;
}

LONG WINAPI unhandled_exception_filter(struct _EXCEPTION_POINTERS *ep)
{
    DWORD code = ep->ExceptionRecord->ExceptionCode;
    __bpm_exception* exc;

    if (code == EXCEPTION_ACCESS_VIOLATION) {
        void *bad_addr = (void*)ep->ExceptionRecord->ExceptionInformation[1];
        if (__BpmSigSegvExceptionTypeId != 0) {
            __SigSegvException* __bpm_struct_exc = (__SigSegvException*)memalloc(sizeof(__SigSegvException));
            __bpm_struct_exc->__addr = (int32_t)bad_addr;
            exc = __bpm_allocate_exception(__BpmSigSegvExceptionTypeId, (int32_t)__bpm_struct_exc, (__what_t)__sigsegv_wh_exception);
        } else {
            exc = __bpm_allocate_exception(3, (uintptr_t)bad_addr, (__what_t)__sigsegv_wh_exception);
        }
    } else {
        exc = __bpm_allocate_exception(
            3,
            (uintptr_t)code,
            __wexcp_wh_exception 
        );
    }

    __bpm_throw(exc);
    return EXCEPTION_EXECUTE_HANDLER;
}

void __bpm_set_sigsegv_handler(void)
{
    SetUnhandledExceptionFilter(unhandled_exception_filter);
}