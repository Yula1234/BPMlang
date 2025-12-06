FILE* get_cstdout() { return stdout; }
FILE* get_cstderr() { return stderr; }
FILE* get_cstdin()  { return stdin; }

#define TRACEBACK_SIZE_LIMIT 1024

// ==========================================
// 2. FORWARD DECLARATIONS (PROTOTYPES)
// ==========================================


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

        if (expected_type == __exception->__type_id || expected_type == 3) {
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

int32_t __eh_proc_lvl_stack[1024];
int32_t __eh_proc_lvl_top = -1;

void __bpm_proc_enter(char* name) {
    traceback_push(name);
    __eh_proc_lvl_stack[++__eh_proc_lvl_top] = __exception_bufs_lvl;
}

void __bpm_proc_leave(void) {
    assert(__eh_proc_lvl_top >= 0);
    traceback_pop();
    int32_t base = __eh_proc_lvl_stack[__eh_proc_lvl_top--];

    while (__exception_bufs_lvl > base) {
        __bpm_end_catch();
        __exception_bufs_lvl--;
    }
}