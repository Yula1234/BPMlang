enum class ProcAttr;
struct InterfaceMethod;

struct NodeTermIntLit {
	Token int_lit;
};

struct NodeTermStrLit {
	GString str_lit;
};

struct NodeTermIdent {
	Token ident;
};

struct NodeExpr;

struct NodeTermAmpersand {
	Token def;
	NodeExpr* expr;
};

struct NodeTermParen {
	NodeExpr* expr;
};

struct NodeTermCall {
	Token def; 
	GString name;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeTermNmCall {
	Token def; 
	GString name;
	GString nm;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeTermSizeof {
	Token def;
	DataType type;
	std::optional<NodeExpr*> expr;
};

struct NodeTermTypeid {
	Token def;
	std::optional<DataType> ptype;
	NodeExpr* expr;
};

struct NodeTermRd {
	Token def;
	size_t size;
	NodeExpr* expr;
};

struct NodeTermCast {
	Token def;
	DataType type;
	NodeExpr* expr;
};

struct NodeTermCastTo {
	Token def;
	NodeExpr* to;
	NodeExpr* expr;
};

struct NodeTermLine {
	Token def;
};

struct NodeTermCol {
	Token def;
};

struct NodeTermFile {
	Token def;
};

struct NodeTermDrvalue {
	Token def;
	NodeExpr* expr;
};

struct NodeTermType {
	Token def;
	DataType type;
};

struct NodeScope;

struct NodeTermExprStmt {
	Token def;
	NodeScope* scope;
	NodeExpr* expr;
};

struct NodeTermPop {};

struct NodeTermCtEval {
	Token def;
	NodeExpr* expr;
};

struct NodeTermCtMdefined {
	bool value;
};

struct NodeTermUnref {
	Token def;
	NodeExpr* expr;
};

struct NodeTermMtCall {
	Token def;
	NodeExpr* mt;
	GString name;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeTermNmIdent {
    Token def;
    GString nm;
    GString name;
};

struct NodeBinExprAdd {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprMulti {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprSub {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprDiv {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprLess {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprAbove {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprEqEq {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprNotEq {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprMod {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprAnd {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprOr {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprShl {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprShr {
	NodeExpr* lhs;
	NodeExpr* rhs;
};

struct NodeBinExprArgs {
	GVector<NodeExpr*> args;
};

struct NodeBinExprDot {
	NodeExpr* lhs; // object
	NodeExpr* rhs; // field (must be a NodeTermIdent*)
};

struct NodeBinExprIndex {
    NodeExpr* lhs; // massive
    NodeExpr* rhs; // index
};

struct NodeBinExpr {
	Token def;
	std::variant<NodeBinExprAdd*, NodeBinExprMulti*, NodeBinExprSub*, NodeBinExprDiv*, NodeBinExprEqEq*, NodeBinExprLess*, NodeBinExprAbove*, NodeBinExprArgs*, NodeBinExprNotEq*, NodeBinExprMod*, NodeBinExprDot*, NodeBinExprAnd*, NodeBinExprOr*, NodeBinExprShl*, NodeBinExprShr*, NodeBinExprIndex*> var;
};

struct NodeTerm {
	std::variant<NodeTermIntLit*, NodeTermStrLit*, NodeTermIdent*, NodeTermParen*, NodeTermCall*, NodeTermRd*, NodeTermAmpersand*, NodeTermCast*, NodeTermSizeof*, NodeTermTypeid*, NodeTermLine*, NodeTermCol*, NodeTermFile*, NodeTermType*, NodeTermExprStmt*, NodeTermPop*, NodeTermCastTo*, NodeTermCtEval*, NodeTermNmCall*, NodeTermCtMdefined*, NodeTermUnref*, NodeTermMtCall*, NodeTermDrvalue*, NodeTermNmIdent*> var;
};

struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
    mutable std::optional<DataType> cached_type = std::nullopt;
};

struct NodeStmtExit {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtReturn {
	Token def;
	std::optional<NodeExpr*> expr;
};

struct NodeStmtLetNoAssign {
	Token ident;
	DataType type;
};

struct NodeStmtLet {
	Token ident;
	NodeExpr* expr {};
	std::optional<DataType> type;
};

struct NodeStmt;

struct NodeScope {
	GVector<NodeStmt*> stmts;
};

struct NodeIfPred;

struct NodeIfPredElif {
	NodeExpr* expr {};
	NodeScope* scope {};
	std::optional<NodeIfPred*> pred;
};

struct NodeIfPredElse {
	NodeScope* scope;
};

struct NodeIfPred {
	std::variant<NodeIfPredElif*, NodeIfPredElse*> var;
};

struct NodeStmtIf {
	NodeExpr* expr {};
	NodeScope* scope {};
	std::optional<NodeIfPred*> pred;
};

struct NodeStmtAssign {
	Token def;
	NodeExpr* lvalue {};
	NodeExpr* expr {};
};

struct NodeStmtWhile {
	NodeScope* scope {};
	NodeExpr* expr {};
};

struct TypeConstraint {
    GString type_param;
    DataType iface_type;
};

struct NodeStmtProc {
	GString name;
	Token def;
	DataType rettype;
	GVector<ProcAttr> attrs;
	GVector<std::pair<GString, DataType>> params;
	NodeScope* scope {};
	bool prototype = false;
	GVector<GString>* templates;
	GVector<TypeConstraint> constraints;
};

struct NodeStmtCall {
	Token def;
	GString name;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeStmtNmCall {
	Token def;
	GString name;
	GString nm;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeStmtStore {
	Token def;
	NodeExpr* expr;
	NodeExpr* ptr;
	size_t size;
};

struct NodeStmtBuffer {
	Token def;
	GString name;
	NodeExpr* size;
};

struct NodeStmtCextern {
	GString name;
};

struct NodeStmtAsm {
	GString code;
};

struct Field {
	GString name;
	DataType type;
	size_t nth;
	bool operator==(Field& two) {
		return type == two.type;
	}
};

bool operator==(const Field& one, const Field& two) {
	return one.type == two.type;
}

struct NodeStmtStruct {
	Token def;
	GString name;
	GVector<std::pair<GString, DataType>> fields;
	std::optional<GString> __allocator;
	GVector<GString> temps;
	bool temp;
	std::optional<DataType> parent; 
};

struct NodeStmtInterface {
    Token def;
    GString name;
    GVector<GString> temps;
    GVector<InterfaceMethod> methods;
};

struct NodeStmtDelete {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtBreak {
	Token def;
};

struct NodeStmtIncBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtDecBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtMulBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtDivBy {
	Token def;
	NodeExpr* expr;
	NodeExpr* lvalue;
};

struct NodeStmtOninit {
	Token def;
	NodeScope* scope;
};

struct NodeStmtPushOnStack {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtStaticAssert {
	Token def;
	GString msg;
	NodeExpr* condition;
};

struct NodeStmtCompileTimeIf {
	Token def;
	NodeExpr* condition;
	NodeScope* _if;
	std::optional<NodeScope*> _else;
};

struct NodeStmtNamespace {
	Token def;
	GString name;
	NodeScope* scope;
};

struct NodeStmtImpl {
	Token def;
	GString name;
	NodeScope* scope;
	GVector<GString> temps;
	GVector<GString> inst;
};

struct NodeStmtMtCall {
	Token def;
	NodeExpr* mt;
	GString name;
	std::optional<NodeExpr*> args;
	GVector<DataType> targs;
};

struct NodeStmtConst {
	Token def;
	GString name;
	NodeExpr* expr;
};

struct NodeStmtTypedef {
	Token def;
	DataType type;
	GString name;
};

struct NodeStmtRaise {
	Token def;
	NodeExpr* expr;
};

struct NodeStmtTry {
	Token def;
	NodeScope* _try;
	NodeScope* _catch;
	DataType type;
	GString name;
};

struct NodeStmtFor {
    NodeStmt* init;  
    NodeExpr* cond;  
    NodeStmt* step;  
    NodeScope* scope;
};

struct NodeStmtForeach {
    Token var_name;
    NodeExpr* expr;
    NodeScope* scope;
};

struct NodeStmtEnum {
    Token def;
    GString name;
    GVector<std::pair<GString, int>> members;
};

struct NodeStmt {
	std::variant<NodeStmtExit*, NodeStmtLet*,
				NodeScope*, NodeStmtIf*,
				NodeStmtAssign*,NodeStmtAsm*,
				NodeStmtProc*, NodeStmtCall*,
				NodeStmtWhile*,NodeStmtReturn*,
				NodeStmtStore*,NodeStmtBuffer*,
				NodeStmtCextern*,NodeStmtStruct*,
				NodeStmtDelete*,NodeStmtLetNoAssign*,
				NodeStmtBreak*,NodeStmtIncBy*,
				NodeStmtDecBy*,NodeStmtMulBy*,
				NodeStmtDivBy*,NodeStmtInterface*,
				NodeStmtOninit*,NodeStmtPushOnStack*,
				NodeStmtStaticAssert*,NodeStmtCompileTimeIf*,
				NodeStmtNamespace*,NodeStmtNmCall*,
				NodeStmtMtCall*, NodeStmtConst*,
				NodeStmtTypedef*,NodeStmtImpl*,
				NodeStmtRaise*, NodeStmtTry*,
				NodeStmtFor*, NodeStmtForeach*,
				NodeStmtEnum*> var;
};

struct NodeProg {
	GVector<NodeStmt*> stmts {};
};