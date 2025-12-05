
namespace fasm_dll {
struct FasmLineHeader {
    char* file_path;
    int line_number;
    union {
        int file_offset;
        long long macro_offset;
    };
    FasmLineHeader* macro_line;
};

struct FasmState {
    int condition;
    union {
        int output_length;
        int error_code;
    };
    union {
        unsigned char* output_data;
        FasmLineHeader* error_line;
    };
};

typedef int (*fasm_Assemble_t)(const char* source, unsigned char* memory, int memory_size, int passes, int display_pipe);

bool compile_via_fasm_dll(const GString& source_code, const fs::path& obj_file_path) {
    HMODULE fasm_dll = LoadLibraryA("fasm.dll");
    if (!fasm_dll) {
        std::cerr << "Error: Could not load fasm.dll. Make sure it is in the compiler directory.\n";
        return false;
    }

    FARPROC raw_proc = GetProcAddress(fasm_dll, "fasm_Assemble");
    
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wcast-function-type"
        fasm_Assemble_t fasm_Assemble = reinterpret_cast<fasm_Assemble_t>(raw_proc);
    #pragma GCC diagnostic pop

    if (!fasm_Assemble) {
        std::cerr << "Error: Corrupted fasm.dll (function fasm_Assemble not found).\n";
        FreeLibrary(fasm_dll);
        return false;
    }

    const int MEMORY_SIZE = 1024 * 1024 * 16; 
    unsigned char* memory = new unsigned char[MEMORY_SIZE];

    int result = fasm_Assemble(source_code.c_str(), memory, MEMORY_SIZE, 100, 0);

    if (result != 0) {
        FasmState* state = reinterpret_cast<FasmState*>(memory);
        std::cerr << "FASM Error code: " << state->error_code << "\n";
        delete[] memory;
        FreeLibrary(fasm_dll);
        return false;
    }

    FasmState* state = reinterpret_cast<FasmState*>(memory);
    
    std::ofstream out_obj(obj_file_path.c_str(), std::ios::binary);
    out_obj.write(reinterpret_cast<const char*>(state->output_data), state->output_length);
    out_obj.close();

    delete[] memory;
    FreeLibrary(fasm_dll);
    return true;
}
}

namespace static_fasm {

constexpr int FASM_OK = 0;
constexpr int FASM_MEMORY_SIZE = 1024 * 1024 * 16;

struct FasmState {
    int condition;
    union {
        int output_length;
        int error_code;
    };
    union {
        char* output_data;
        int error_line_hdr;
    };
};

extern "C" {
    int __stdcall fasm_Assemble(
        const char* source,
        void* memory,
        int memory_size,
        int passes_limit,
        int display_pipe
    ) asm("_fasm_Assemble");
}

extern "C" intptr_t _get_osfhandle(int fd);

inline bool compile_via_fasm(const GString& source_code, const fs::path& obj_file_path) {
    static char* fasm_memory = nullptr;

    if (__builtin_expect(!fasm_memory, 0)) {
        fasm_memory = g_GlobalArena->alloc<char>(FASM_MEMORY_SIZE);
    }

    fasm_Assemble(
        source_code.c_str(), 
        fasm_memory, 
        FASM_MEMORY_SIZE, 
        100, 
        0
    );

    FasmState* state = reinterpret_cast<FasmState*>(fasm_memory);

    if (__builtin_expect(state->condition != FASM_OK, 0)) {
        std::cerr << "Fasm: error, code: " << state->error_code << '\n';
        return false;
    }

    HANDLE hFile = CreateFileW(
        obj_file_path.c_str(),       
        GENERIC_WRITE,               
        0,                         
        NULL,                         
        CREATE_ALWAYS,                
        FILE_ATTRIBUTE_TEMPORARY |   
        FILE_ATTRIBUTE_NORMAL,      
        NULL
    );

    if (hFile == INVALID_HANDLE_VALUE) {
        std::cerr << "Fasm: could not create file\n"; 
        return false;
    }

    DWORD bytesWritten;
    BOOL writeResult = WriteFile(
        hFile, 
        state->output_data, 
        static_cast<DWORD>(state->output_length), 
        &bytesWritten, 
        NULL
    );

    CloseHandle(hFile);

    if (!writeResult || bytesWritten != static_cast<DWORD>(state->output_length)) {
        std::cerr << "Fasm: failed to write all bytes to file\n";
        return false;
    }

    return true;
}

}