
HANDLE hConsole;

static inline void __normal_console() {
	SetConsoleTextAttribute(hConsole, 7);
}

static inline void __red_console() {
	SetConsoleTextAttribute(hConsole, FOREGROUND_RED);
}

static inline void __light_blue_console() {
	SetConsoleTextAttribute(hConsole, FOREGROUND_INTENSITY | FOREGROUND_GREEN | FOREGROUND_BLUE);
}

static inline void __white_console() {
	SetConsoleTextAttribute(hConsole, FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
}

class DiagnosticManager {
public:
	void DiagnosticMessage(Token tok,
	                       const GString& header,
	                       const GString& msg,
	                       const int col_inc,
	                       bool show_expansion)
	{
	    auto normalize_kind = [](const GString& h) -> GString {
	        if (h == "NOTE" || h == "note")        return "note";
	        if (h == "WARNING" || h == "warning")  return "warning";
	        if (h == "ERROR" || h == "error")      return "error";
	        return h;
	    };

	    GString kind = normalize_kind(header);

	    auto set_color_for_kind = [](const GString& k) {
	        if (k == "error") {
	            __red_console();
	        } else if (k == "warning") {
	            SetConsoleTextAttribute(
	                hConsole,
	                FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY
	            );
	        } else if (k == "note") {
	            __light_blue_console();
	        } else {
	            __normal_console();
	        }
	    };

	    auto reset_color = []() {
	        __normal_console();
	    };

	    auto get_line_safely = [&](const GString& file, int line) -> GString {
	        auto it = m_lines.find(file);
	        if (it == m_lines.end()) return GString{};
	        auto& vec = it->second;
	        if (line <= 0 || static_cast<size_t>(line) > vec.size()) return GString{};
	        return vec[line - 1];
	    };

	    auto print_header_line = [&](const Token& t, const GString& kind_, const GString& message) {
	        reset_color();
	        __white_console();
	        std::cout << t.file << ":" << t.line << ":" << t.col << ": ";
	        reset_color();
	        set_color_for_kind(kind_);
	        std::cout << kind_ << ": ";
	        reset_color();
	        std::cout << message << "\n";
	    };

	    auto print_source_with_caret = [&](const Token& t, int extra_col_inc) {
		    GString orig_line = get_line_safely(t.file, t.line);
		    if (orig_line.empty()) return;

		    GString line = orig_line;
		    int col = t.col;

		    while (!line.empty() && (line[0] == ' ' || line[0] == '\t')) {
		        line.erase(line.begin());
		        col -= 1;
		    }
		    if (col < 1) col = 1;

		    std::cout << " " << line << "\n";

		    int caret_pos = col - 1 + extra_col_inc;
		    if (caret_pos < 0) caret_pos = 0;

		    std::cout << " ";
		    for (int i = 0; i < caret_pos; ++i) {
		        std::cout << " ";
		    }

		    set_color_for_kind(kind);
		    std::cout << "^\n";
		    reset_color();
		};

	    print_header_line(tok, kind, msg);
	    print_source_with_caret(tok, col_inc);

		Token inexp = tok;
	    if (show_expansion) {
	        while (inexp.expand.has_value()) {
	            Token exp = *(inexp.expand.value());

	            GString note_msg = (exp.type == TokenType_t::_include)
	                ? "in file included from here"
	                : "expanded from here";

	            __white_console();
	            std::cout << exp.file << ":" << exp.line << ":" << exp.col << ": ";
	            __light_blue_console();
	            std::cout << "note: ";
	            __normal_console();
	            std::cout << note_msg << "\n";

	            GString line = get_line_safely(exp.file, exp.line);
	            if (!line.empty()) {
	                while (!line.empty() && (line[0] == ' ' || line[0] == '\t')) {
	                    line.erase(line.begin());
	                    exp.col -= 1;
	                }
	                if (exp.col < 1) exp.col = 1;

	                std::cout << " " << line << "\n";

	                int caret_pos = exp.col - 1;
	                if (caret_pos < 0) caret_pos = 0;

	                std::cout << " ";
	                for (int i = 0; i < caret_pos; ++i) {
	                    std::cout << " ";
	                }
	                __red_console();
	                std::cout << "^\n";
	                __normal_console();
	            }

	            inexp = exp;
	        }
	    }
	}

	void DiagnosticMessage(Token tok,
                       const GString& header,
                       const GString& msg,
                       const int col_inc)
	{
	    DiagnosticMessage(tok, header, msg, col_inc, true);
	}

	void save_file(GString fname, GVector<GString> lines) {
		m_lines[std::move(fname)] = std::move(lines);
	}

private:
	GMap<GString, GVector<GString>> m_lines {};
};