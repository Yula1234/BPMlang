#pragma once
#include "ir.hpp"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <utility>

namespace iropt {

// ================================
// Информация о функции в IR
// ================================
struct FunctionInfo {
    std::string name;
    size_t      start;  // индекс инструкции с Label (в prog.instrs)
    size_t      end;    // индекс первой инструкции СЛЕДУЮЩЕЙ функции или prog.instrs.size()
};

// ================================
// Вспомогательные функции
// ================================

// Считаем ли мы эту метку входом функции
inline bool is_function_label_name(const std::string& name) {
    if (name.empty()) return false;

    // Локальные базовые блоки (L0, L1, ...) не считаем функциями
    if (name[0] == 'L') {
        return false;
    }

    // Внутренние рет-лейблы вида "__ns@proc@ret"
    if (name.size() >= 4) {
        const std::string suffix = "@ret";
        if (name.compare(name.size() - suffix.size(), suffix.size(), suffix) == 0) {
            return false;
        }
    }

    return true;
}

// Собрать список функций по IRProgram
inline std::vector<FunctionInfo> collect_functions(const IRProgram& prog) {
    const auto& instrs = prog.instrs;
    std::vector<std::pair<std::string, size_t>> func_labels; // (name, index)

    for (size_t i = 0; i < instrs.size(); ++i) {
        const IRInstr& ins = instrs[i];
        if (ins.op == IROp::Label && ins.a.kind == OperandKind::Label) {
            const std::string& lbl = ins.a.name;
            if (is_function_label_name(lbl)) {
                func_labels.emplace_back(lbl, i);
            }
        }
    }

    std::vector<FunctionInfo> functions;
    if (func_labels.empty()) return functions;

    const size_t N = instrs.size();
    functions.reserve(func_labels.size());

    for (size_t i = 0; i < func_labels.size(); ++i) {
        const auto& cur  = func_labels[i];
        size_t start_idx = cur.second;
        size_t end_idx   = (i + 1 < func_labels.size() ? func_labels[i + 1].second : N);
        FunctionInfo fi{ cur.first, start_idx, end_idx };
        functions.push_back(fi);
    }

    return functions;
}

// Построить множество достижимых функций из "main"
inline std::unordered_set<std::string>
compute_reachable_functions(const IRProgram& prog,
                            const std::vector<FunctionInfo>& funcs)
{
    std::unordered_set<std::string> reachable;
    std::vector<std::string>        worklist;

    std::unordered_map<std::string, const FunctionInfo*> name_to_func;
    name_to_func.reserve(funcs.size());
    for (const auto& f : funcs) {
        name_to_func[f.name] = &f;
    }

    auto mark = [&](const std::string& fname) {
        auto itf = name_to_func.find(fname);
        if (itf == name_to_func.end()) return;
        if (reachable.insert(fname).second) {
            worklist.push_back(fname);
        }
    };

    // 1. Корень — main
    mark("main");

    // 2. BFS/DFS по графу вызовов (по символическим операндам)
    while (!worklist.empty()) {
        std::string cur = worklist.back();
        worklist.pop_back();

        auto itf = name_to_func.find(cur);
        if (itf == name_to_func.end()) continue;
        const FunctionInfo& fi = *itf->second;

        for (size_t i = fi.start; i < fi.end; ++i) {
            const IRInstr& ins = prog.instrs[i];

            auto scan_operand = [&](const Operand& op) {
                if (op.kind == OperandKind::Symbol) {
                    // Любое упоминание symbolOp(name) считаем использованием функции
                    mark(op.name);
                }
            };

            scan_operand(ins.a);
            scan_operand(ins.b);
            scan_operand(ins.c);
        }
    }

    return reachable;
}

// Удалить из IRProgram все функции, которые не достижимы из main
inline void remove_unused_functions(IRProgram& prog) {
    std::vector<FunctionInfo> funcs = collect_functions(prog);
    if (funcs.empty()) return;

    std::unordered_set<std::string> reachable =
        compute_reachable_functions(prog, funcs);

    const size_t N = prog.instrs.size();
    std::vector<bool> kill(N, false);

    for (const auto& f : funcs) {
        // Никогда не удаляем main по соображениям безопасности
        if (f.name == "main") continue;

        if (reachable.find(f.name) == reachable.end()) {
            for (size_t i = f.start; i < f.end; ++i) {
                kill[i] = true;
            }
        }
    }

    std::vector<IRInstr> new_instrs;
    new_instrs.reserve(N);
    for (size_t i = 0; i < N; ++i) {
        if (!kill[i]) {
            new_instrs.push_back(std::move(prog.instrs[i]));
        }
    }

    prog.instrs.swap(new_instrs);
}

// ================================
// Простейшие peephole-оптимизации
// ================================
//
// Здесь мы сознательно делаем ТОЛЬКО максимально безопасные вещи,
// которые не зависят от анализа живости регистров/флагов.
//
// Примеры:
//   - удаляем mov r, r (ничего не делает и не меняет флаги в x86)
//

// Удаляем инструкции вида: mov reg, reg
inline void peephole_noops(IRProgram& prog) {
    std::vector<IRInstr> out;
    out.reserve(prog.instrs.size());

    for (const auto& ins : prog.instrs) {
        bool remove = false;

        if (ins.op == IROp::Mov &&
            ins.a.kind == OperandKind::Reg &&
            ins.b.kind == OperandKind::Reg &&
            ins.a.reg == ins.b.reg)
        {
            // mov eax, eax и т.п. — реально no-op, не меняет флагов
            remove = true;
        }

        if (!remove) {
            out.push_back(ins);
        }
    }

    prog.instrs.swap(out);
}

// Главная точка входа оптимизатора
inline void optimize_ir(IRProgram& prog) {
    // 1. Удаляем неиспользуемые функции
    remove_unused_functions(prog);

    // 2. Простые peephole-оптимизации
    peephole_noops(prog);
}

} // namespace iropt