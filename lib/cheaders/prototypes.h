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

__bpm_exception* __bpm_allocate_exception(int32_t __tid, uintptr_t __v, __what_t __wh);
void __bpm_throw(__bpm_exception* __exception);
char* __bpm_double_free_exception_what(void* __val);
char* __bpm_recursion_exception_what(void* __val);

extern int32_t __BpmDoubleExceptionTypeId;
extern int32_t __BpmRecursionExceptionTypeId;
extern int32_t __BpmSigSegvExceptionTypeId;

int32_t __traceback_saved[1024];