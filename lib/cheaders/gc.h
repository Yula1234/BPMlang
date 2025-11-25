#pragma once

// ========================================================
//  Простая консервативная mark-sweep GC c API:
//      void* memalloc(size_t size);
//      void  memfree(void* ptr);
//  + явный gc_collect(), инициализация корней через gc_set_stack_base().
// ========================================================

typedef struct GcHeader {
    struct GcHeader* next;  // связный список всех объектов
    size_t           size;  // размер пользовательской области (байт)
    unsigned char    mark;  // 0/1 — бит "живой"
} GcHeader;

static GcHeader* gc_objects = NULL;

// База стека — адрес "ниже" всех будущих кадров.
static const uintptr_t* gc_stack_base = NULL;

// Порог для запуска GC по байтам / количеству объектов
static size_t gc_allocated_bytes     = 0;
static size_t gc_object_count        = 0;
static size_t gc_bytes_before_collect = 4 * 1024 * 1024;  // 4 МБ для примера
static size_t gc_max_objects_before_collect = 100000;     // 100k объектов

// Внешний интерфейс:
//   - вызвать один раз в старте программы, чтобы GC знал верх стека.
void gc_set_stack_base(const void* base) {
    gc_stack_base = (const uintptr_t*)base;
}

// Пометить/снять метку на всех объектах
static void gc_clear_marks(void) {
    for (GcHeader* h = gc_objects; h != NULL; h = h->next) {
        h->mark = 0;
    }
}

// Проверка: p лежит внутри [h+1, h+1 + size)
static int gc_ptr_in_object(GcHeader* h, const void* p) {
    const uintptr_t addr   = (uintptr_t)p;
    const uintptr_t start  = (uintptr_t)(h + 1);
    const uintptr_t end    = start + h->size;
    return (addr >= start && addr < end);
}

// Вперёд объявляем, чтобы можно было вызывать из gc_mark_object
static void gc_mark_region(const uintptr_t* start, const uintptr_t* end);

// Пометить объект живым и просканировать его содержимое на наличие указателей
static void gc_mark_object(GcHeader* h) {
    if (h->mark) return;
    h->mark = 1;

    // Сканируем всю пользовательскую область как массив uintptr_t
    uintptr_t* ob_start = (uintptr_t*)(h + 1);
    uintptr_t* ob_end   = (uintptr_t*)((uint8_t*)(h + 1) + h->size);
    gc_mark_region(ob_start, ob_end);
}

// Сканируем регион памяти [start, end), интерпретируя каждое слово как возможный указатель
static void gc_mark_region(const uintptr_t* start, const uintptr_t* end) {
    for (const uintptr_t* slot = start; slot < end; ++slot) {
        void* cand = (void*)(*slot);
        if (!cand) continue;

        for (GcHeader* h = gc_objects; h != NULL; h = h->next) {
            if (!h->mark && gc_ptr_in_object(h, cand)) {
                gc_mark_object(h);
                // после gc_mark_object(h) объект помечен и его внутренняя память уже просканирована
                break;
            }
        }
    }
}

// Публичная точка входа для явного сбора мусора
void gc_collect(void) {
    if (!gc_stack_base) {
        // Без знания базы стека мы не можем корректно сканировать,
        // поэтому в дебаге лучше упасть. В релизе можно просто return.
        // assert(!"gc_stack_base is not set. Call gc_set_stack_base()");
        return;
    }

    gc_clear_marks();

    // Сканируем стек: от текущего кадра до gc_stack_base.
    // В x86 (cdecl) стек растёт вниз, поэтому:
    //   stack_top = адрес локальной переменной (ниже по памяти),
    //   gc_stack_base — выше по памяти.
    //
    // Но чтобы не ломать мозг, сделаем просто "min/max":
    uintptr_t dummy = 0;
    const uintptr_t* top = (const uintptr_t*)&dummy;
    const uintptr_t* lo  = (top < gc_stack_base) ? top : gc_stack_base;
    const uintptr_t* hi  = (top < gc_stack_base) ? gc_stack_base : top;

    gc_mark_region(lo, hi + 1);

    // TODO: по уму нужно сканировать и глобалы/статические данные,
    // но для начала ограничимся стеком.

    // Сбор: удаляем все объекты с mark == 0
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

    // Сдвигаем пороги (простая эвристика)
    gc_bytes_before_collect = gc_allocated_bytes + 4 * 1024 * 1024;
    gc_max_objects_before_collect = gc_object_count + 100000;
}

// memalloc: обёртка над malloc + учёт в GC
void* memalloc(size_t size) {
    if (size == 0) {
        size = 1; // нулевая аллокация — всегда 1 байт
    }

    // Пороговый запуск GC
    if ((gc_allocated_bytes + size > gc_bytes_before_collect) ||
        (gc_object_count + 1 > gc_max_objects_before_collect))
    {
        gc_collect();
    }

    GcHeader* h = (GcHeader*)malloc(sizeof(GcHeader) + size);
    if (!h) {
        // Попробуем один раз собрать мусор и повторить
        gc_collect();
        h = (GcHeader*)malloc(sizeof(GcHeader) + size);
        if (!h) {
            // Реальный OOM
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

// memfree: явное освобождение (опциональное в GC-системе)
void memfree(void* ptr) {
    if (!ptr) return;

    GcHeader* target = (GcHeader*)ptr - 1;

    // Ищем в списке и удаляем
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

    // Если дошли сюда — ptr не из GC-кучи.
    // Можно либо:
    //  - assert(0);
    //  - либо просто free(ptr).
    //
    // Чтобы быть дружелюбными к уже имеющемуся коду, сделаем free().
    // (Например, если кто-то вызвал memfree() от обычного malloc.)
    free(ptr);
}

// Дамп текущего состояния GC: список объектов и базовые счётчики.
// Если out == NULL, пишем в stderr.
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