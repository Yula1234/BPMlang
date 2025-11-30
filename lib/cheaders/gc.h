#pragma once

// ========================================================
//  simple mark-sweep GC with API:
//      void* memalloc(size_t size);
//      void  memfree(void* ptr);
//  + explicit gc_collect(), init with gc_set_stack_base().
// ========================================================

typedef struct GcHeader {
    struct GcHeader* next;
    size_t           size;
    unsigned char    mark;
} GcHeader;

static GcHeader* gc_objects = NULL;

static const uintptr_t* gc_stack_base = NULL;

static size_t gc_allocated_bytes     = 0;
static size_t gc_object_count        = 0;
static size_t gc_bytes_before_collect = 4 * 1024 * 1024; 
static size_t gc_max_objects_before_collect = 100000;    

void gc_set_stack_base(const void* base) {
    gc_stack_base = (const uintptr_t*)base;
}

static void gc_clear_marks(void) {
    for (GcHeader* h = gc_objects; h != NULL; h = h->next) {
        h->mark = 0;
    }
}

static int gc_ptr_in_object(GcHeader* h, const void* p) {
    const uintptr_t addr   = (uintptr_t)p;
    const uintptr_t start  = (uintptr_t)(h + 1);
    const uintptr_t end    = start + h->size;
    return (addr >= start && addr < end);
}

static void gc_mark_region(const uintptr_t* start, const uintptr_t* end);

static void gc_mark_object(GcHeader* h) {
    if (h->mark) return;
    h->mark = 1;

    uintptr_t* ob_start = (uintptr_t*)(h + 1);
    uintptr_t* ob_end   = (uintptr_t*)((uint8_t*)(h + 1) + h->size);
    gc_mark_region(ob_start, ob_end);
}

static void gc_mark_region(const uintptr_t* start, const uintptr_t* end) {
    for (const uintptr_t* slot = start; slot < end; ++slot) {
        void* cand = (void*)(*slot);
        if (!cand) continue;

        for (GcHeader* h = gc_objects; h != NULL; h = h->next) {
            if (!h->mark && gc_ptr_in_object(h, cand)) {
                gc_mark_object(h);
                break;
            }
        }
    }
}

void gc_collect(void) {
    if (!gc_stack_base) {
        return;
    }

    gc_clear_marks();

    uintptr_t dummy = 0;
    const uintptr_t* top = (const uintptr_t*)&dummy;
    const uintptr_t* lo  = (top < gc_stack_base) ? top : gc_stack_base;
    const uintptr_t* hi  = (top < gc_stack_base) ? gc_stack_base : top;

    gc_mark_region(lo, hi + 1);

    GcHeader** link = &gc_objects;
    while (*link) {
        GcHeader* h = *link;
        if (!h->mark) {
            *link = h->next;
            gc_allocated_bytes -= h->size;
            gc_object_count--;
            free(h);
        } else {
            link = &h->next;
        }
    }

    gc_bytes_before_collect = gc_allocated_bytes + 4 * 1024 * 1024;
    gc_max_objects_before_collect = gc_object_count + 100000;
}

void* memalloc(size_t size) {
    if (size == 0) {
        size = 1;
    }

    if ((gc_allocated_bytes + size > gc_bytes_before_collect) ||
        (gc_object_count + 1 > gc_max_objects_before_collect))
    {
        gc_collect();
    }

    GcHeader* h = (GcHeader*)malloc(sizeof(GcHeader) + size);
    if (!h) {
        gc_collect();
        h = (GcHeader*)malloc(sizeof(GcHeader) + size);
        if (!h) {
            return NULL;
        }
    }

    h->size = size;
    h->mark = 0;
    h->next = gc_objects;
    gc_objects = h;

    gc_allocated_bytes += size;
    gc_object_count++;

    return (void*)(h + 1);
}

void memfree(void* ptr) {
    if (!ptr) return;

    GcHeader* target = (GcHeader*)ptr - 1;

    GcHeader** link = &gc_objects;
    while (*link) {
        if (*link == target) {
            *link = target->next;
            gc_allocated_bytes -= target->size;
            gc_object_count--;
            free(target);
            return;
        }
        link = &((*link)->next);
    }

    free(ptr);
}

void __bpm_gc_dump_state() {
    printf("===== GC state dump =====\n");
    printf("  objects:           %zu\n", gc_object_count);
    printf("  allocated bytes:   %zu\n", gc_allocated_bytes);
    printf("  next GC at bytes:  %zu\n", gc_bytes_before_collect);
    printf("  next GC at objs:   %zu\n", gc_max_objects_before_collect);
    printf("  stack_base:        %p\n", (const void*)gc_stack_base);
    printf("  objects list:\n");

    size_t idx = 0;
    for (GcHeader* h = gc_objects; h != NULL; h = h->next, ++idx) {
        void* user_ptr = (void*)(h + 1);
        printf("    [%zu] ptr=%p size=%zu mark=%u\n",
                idx,
                user_ptr,
                h->size,
                (unsigned)h->mark);
    }

    printf("===== end of GC state =====\n");
}