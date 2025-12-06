
GVector<GString>* split_string(const GString& str, const GString& delimiter) {
    GVector<GString>* strings = g_GlobalArena->emplace<GVector<GString>>();
    GString::size_type pos = 0;
    GString::size_type prev = 0;
    while((pos = str.find(delimiter, prev)) != GString::npos) {
        strings->push_back(str.substr(prev, pos - prev));
        prev = pos + delimiter.size();
    }
    strings->push_back(str.substr(prev));
    return strings;
}

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

constexpr int FASM_OK                          = 0;
constexpr int FASM_WORKING                     = 1;
constexpr int FASM_ERROR                       = 2 ;
constexpr int FASM_INVALID_PARAMETER           = -1;
constexpr int FASM_OUT_OF_MEMORY               = -2;
constexpr int FASM_STACK_OVERFLOW              = -3;
constexpr int FASM_SOURCE_NOT_FOUND            = -4;
constexpr int FASM_UNEXPECTED_END_OF_SOURCE    = -5;
constexpr int FASM_CANNOT_GENERATE_CODE        = -6;
constexpr int FASM_FORMAT_LIMITATIONS_EXCEDDED = -7;
constexpr int FASM_WRITE_FAILED                = -8;
constexpr int FASM_INVALID_DEFINITION          = -9;


constexpr int FASMERR_FILE_NOT_FOUND                      = -101;
constexpr int FASMERR_ERROR_READING_FILE                  = -102;
constexpr int FASMERR_INVALID_FILE_FORMAT                 = -103;
constexpr int FASMERR_INVALID_MACRO_ARGUMENTS             = -104;
constexpr int FASMERR_INCOMPLETE_MACRO                    = -105;
constexpr int FASMERR_UNEXPECTED_CHARACTERS               = -106;
constexpr int FASMERR_INVALID_ARGUMENT                    = -107;
constexpr int FASMERR_ILLEGAL_INSTRUCTION                 = -108;
constexpr int FASMERR_INVALID_OPERAND                     = -109;
constexpr int FASMERR_INVALID_OPERAND_SIZE                = -110;
constexpr int FASMERR_OPERAND_SIZE_NOT_SPECIFIED          = -111;
constexpr int FASMERR_OPERAND_SIZES_DO_NOT_MATCH          = -112;
constexpr int FASMERR_INVALID_ADDRESS_SIZE                = -113;
constexpr int FASMERR_ADDRESS_SIZES_DO_NOT_AGREE          = -114;
constexpr int FASMERR_DISALLOWED_COMBINATION_OF_REGISTERS = -115;
constexpr int FASMERR_LONG_IMMEDIATE_NOT_ENCODABLE        = -116;
constexpr int FASMERR_RELATIVE_JUMP_OUT_OF_RANGE          = -117;
constexpr int FASMERR_INVALID_EXPRESSION                  = -118;
constexpr int FASMERR_INVALID_ADDRESS                     = -119;
constexpr int FASMERR_INVALID_VALUE                       = -120;
constexpr int FASMERR_VALUE_OUT_OF_RANGE                  = -121;
constexpr int FASMERR_UNDEFINED_SYMBOL                    = -122;
constexpr int FASMERR_INVALID_USE_OF_SYMBOL               = -123;
constexpr int FASMERR_NAME_TOO_LONG                       = -124;
constexpr int FASMERR_INVALID_NAME                        = -125;
constexpr int FASMERR_RESERVED_WORD_USED_AS_SYMBOL        = -126;
constexpr int FASMERR_SYMBOL_ALREADY_DEFINED              = -127;
constexpr int FASMERR_MISSING_END_QUOTE                   = -128;
constexpr int FASMERR_MISSING_END_DIRECTIVE               = -129;
constexpr int FASMERR_UNEXPECTED_INSTRUCTION              = -130;
constexpr int FASMERR_EXTRA_CHARACTERS_ON_LINE            = -131;
constexpr int FASMERR_SECTION_NOT_ALIGNED_ENOUGH          = -132;
constexpr int FASMERR_SETTING_ALREADY_SPECIFIED           = -133;
constexpr int FASMERR_DATA_ALREADY_DEFINED                = -134;
constexpr int FASMERR_TOO_MANY_REPEATS                    = -135;
constexpr int FASMERR_SYMBOL_OUT_OF_SCOPE                 = -136;
constexpr int FASMERR_USER_ERROR                          = -140;
constexpr int FASMERR_ASSERTION_FAILED                    = -141;


GMap<int, const char*> fasm_condition_to_string = {
    {FASM_OK, "ok"},
    {FASM_WORKING, "working"},
    {FASM_ERROR, "error"},
    {FASM_INVALID_PARAMETER, "invalid parameter"},
    {FASM_OUT_OF_MEMORY, "out of memory"},
    {FASM_STACK_OVERFLOW, "stack overflow"},
    {FASM_SOURCE_NOT_FOUND, "source not found"},
    {FASM_UNEXPECTED_END_OF_SOURCE, "unexpected end of source"},
    {FASM_CANNOT_GENERATE_CODE, "cannot generate code"},
    {FASM_FORMAT_LIMITATIONS_EXCEDDED, "format limitations excedded"},
    {FASM_WRITE_FAILED, "write failed"},
    {FASM_INVALID_DEFINITION, "invalid definition"},
};

GMap<int, const char*> fasm_error_to_string = {
    {FASMERR_FILE_NOT_FOUND, "file not_found"},
    {FASMERR_ERROR_READING_FILE, "error reading file"},
    {FASMERR_INVALID_FILE_FORMAT, "invalid file format"},
    {FASMERR_INVALID_MACRO_ARGUMENTS, "INVALID MACRO ARGUMENTS"},
    {FASMERR_INCOMPLETE_MACRO, "incomplete macro"},
    {FASMERR_UNEXPECTED_CHARACTERS, "UNEXPECTED CHARACTERS"},
    {FASMERR_INVALID_ARGUMENT, "invalid argument"},
    {FASMERR_ILLEGAL_INSTRUCTION, "illegal instruction"},
    {FASMERR_INVALID_OPERAND, "invalid operand"},
    {FASMERR_INVALID_OPERAND_SIZE, "invalid operand size"},
    {FASMERR_OPERAND_SIZE_NOT_SPECIFIED, "operand size not specified"},
    {FASMERR_OPERAND_SIZES_DO_NOT_MATCH, "operand sizes do not match"},
    {FASMERR_INVALID_ADDRESS_SIZE, "invalid address size"},
    {FASMERR_ADDRESS_SIZES_DO_NOT_AGREE, "address sizes do not agree"},
    {FASMERR_DISALLOWED_COMBINATION_OF_REGISTERS, "disallowed combination of registers"},
    {FASMERR_LONG_IMMEDIATE_NOT_ENCODABLE, "long immediate not encodable"},
    {FASMERR_RELATIVE_JUMP_OUT_OF_RANGE, "relative jump out of range"},
    {FASMERR_INVALID_EXPRESSION, "invalid expression"},
    {FASMERR_INVALID_ADDRESS, "invalid address"},
    {FASMERR_INVALID_VALUE, "invalid value"},
    {FASMERR_VALUE_OUT_OF_RANGE, "value out of range"},
    {FASMERR_UNDEFINED_SYMBOL, "undefined symbol"},
    {FASMERR_INVALID_USE_OF_SYMBOL, "invalid use of symbol"},
    {FASMERR_NAME_TOO_LONG, "name too long"},
    {FASMERR_INVALID_NAME, "invalid name"},
    {FASMERR_RESERVED_WORD_USED_AS_SYMBOL, "reserved word used as symbol"},
    {FASMERR_SYMBOL_ALREADY_DEFINED, "symbol already defined"},
    {FASMERR_MISSING_END_QUOTE, "missing end quote"},
    {FASMERR_MISSING_END_DIRECTIVE, "missing end directive"},
    {FASMERR_UNEXPECTED_INSTRUCTION, "unexpected instruction"},
    {FASMERR_EXTRA_CHARACTERS_ON_LINE, "extra characters on line"},
    {FASMERR_SECTION_NOT_ALIGNED_ENOUGH, "section not aligned enough"},
    {FASMERR_SETTING_ALREADY_SPECIFIED, "setting already specified"},
    {FASMERR_DATA_ALREADY_DEFINED, "data already defined"},
    {FASMERR_TOO_MANY_REPEATS, "too many repeats"},
    {FASMERR_SYMBOL_OUT_OF_SCOPE, "symbol out of scope"},
    {FASMERR_USER_ERROR, "user error"},
    {FASMERR_ASSERTION_FAILED, "assertion failed"},
};

constexpr int FASM_MEMORY_SIZE = 1024 * 1024 * 16;

struct FasmLineHeader {
    char* file_path;
    int line_number;
    union {
        int file_offset;
        int macro_calling_line;
    };
    int macro_line;
};

struct FasmState {
    int condition;
    union {
        int output_length;
        int error_code;
    };
    union {
        char* output_data;
        FasmLineHeader* error_line_hdr;
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
        if(state->condition == FASM_ERROR) {
            GVector<GString>* source_lines = split_string(source_code, "\n");
            int line_offs = state->error_line_hdr->line_number;
            std::cerr << "Fasm: " << "[" << line_offs << "] " << source_lines->operator[](line_offs - 1) << std::endl;
            std::cerr << "    error - " << fasm_error_to_string[state->error_code] << std::endl;
        } else {
            std::cerr << "Fasm: error " << fasm_condition_to_string[state->condition] << std::endl;
        }
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