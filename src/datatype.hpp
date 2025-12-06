enum class SimpleDataType {
	_int,
	ptr,
	_void,
	any,
	_char,
	_constexpr,
	proc_ptr,
};

struct BaseDataType {
	bool is_object = false;
	std::variant<SimpleDataType, GString> type;
	bool link = false;
	bool rvalue = false;
	size_t ptrlvl = 0ULL;

	bool is_simple() const {
		return !is_object;
	}
	SimpleDataType getsimpletype() const {
		return std::get<SimpleDataType>(this->type);
	}
	GString getobjectname() const {
		return std::get<GString>(this->type);
	}
	GString sign() const {
		GString res;
		
		if(ptrlvl > 0) res += "P" + GString(std::to_string(ptrlvl).c_str());
		if(link) res += "R";
		if(rvalue) res += "V";

		if(is_object) {
			GString name = getobjectname();
			res += GString(std::to_string(name.length()).c_str()) + name;
		} else {
			switch(getsimpletype()) {
			case SimpleDataType::_int:   res += "i"; break;
			case SimpleDataType::ptr:    res += "p"; break;
			case SimpleDataType::_void:  res += "v"; break;
			case SimpleDataType::any:    res += "a"; break;
			case SimpleDataType::_char:  res += "c"; break;
			case SimpleDataType::_constexpr: res += "x"; break;
			case SimpleDataType::proc_ptr:   res += "f"; break;
			default: res += "u"; break;
			}
		}
		return res;
	}
	GString to_string() const {
		GString dop(this->link ? "&" : "");
		for(size_t i = 0;i < ptrlvl;++i) {
			dop += '*';
		}
		if(rvalue) dop += "&&";
		if(!this->is_object) {
			switch(this->getsimpletype()) {
			case SimpleDataType::_int:
				return "`int" + dop + "`";
			case SimpleDataType::ptr:
				return "`ptr" + dop + "`";
			case SimpleDataType::_char:
				return "`char" + dop + "`";
			case SimpleDataType::_void:
				return "`void`";
			case SimpleDataType::any:
				return "`any`";
			case SimpleDataType::_constexpr:
				return "`constexpr`";
			case SimpleDataType::proc_ptr:
				return "ProcPtr";
			default:
				break;
			}
			assert(false);
		} else {
			return "`" + this->getobjectname() + dop + "`";
		}
		assert(false);
		return "unkown";
	}
	GString get_postfix() const {
		GString dop(this->link ? "&" : "");
		for(size_t i = 0;i < ptrlvl;++i) {
			dop += '*';
		}
		if(rvalue) dop += "&&";
		return dop;
	}
	GString to_string_wt() const {
		GString dop(get_postfix());
		if(!this->is_object) {
			switch(this->getsimpletype()) {
			case SimpleDataType::_int:
				return "int" + dop;
			case SimpleDataType::ptr:
				return "ptr" + dop;
			case SimpleDataType::_char:
				return "char" + dop;
			case SimpleDataType::_void:
				return "void";
			case SimpleDataType::any:
				return "any";
			case SimpleDataType::_constexpr:
				return "constexpr";
			case SimpleDataType::proc_ptr:
				return "ProcPtr";
			default:
				break;
			}
			assert(false);
		} else {
			return this->getobjectname() + dop;
		}
		assert(false);
		return "unkown";
	}
	GString to_string_d() const {
		if(!this->is_object) {
			switch(this->getsimpletype()) {
			case SimpleDataType::_int:
				return "int";
			case SimpleDataType::ptr:
				return "ptr";
			case SimpleDataType::_void:
				return "void";
			case SimpleDataType::any:
				return "any";
			case SimpleDataType::_char:
				return "char";
			case SimpleDataType::_constexpr:
				return "constexpr";
			case SimpleDataType::proc_ptr:
				return "ProcPtr";
			default:
				break;
			}
			assert(false);
		} else {
			return this->getobjectname();
		}
		assert(false);
		return "";
	}
	bool arg_eq(const BaseDataType& two) const {
		if(two.is_simple() && two.getsimpletype() == SimpleDataType::ptr && this->ptrlvl != 0ULL && !this->rvalue && !this->link) return true;
		if(this->link && !two.link) return false;
		if(this->ptrlvl != two.ptrlvl) return false;
		if(this->rvalue != two.rvalue) return false;
		if(two.is_simple() && two.getsimpletype() == SimpleDataType::any)
			return true;
		if((this->is_object && !two.is_object) || (!this->is_object && two.is_object))
			return false;
		if(this->is_object && two.is_object)
			return this->getobjectname() == two.getobjectname();
		else if(!this->is_object && !two.is_object)
			return this->getsimpletype() == two.getsimpletype();
		else
			assert(false); // maybe bug
	}
	bool eq(const BaseDataType& two) const {
		if(two.is_simple() && two.getsimpletype() == SimpleDataType::ptr && this->ptrlvl != 0ULL && !this->rvalue && !this->link) return true;
		if(two.link && !this->link)
			return false;
		if(!two.link && this->link)
			return false;
		if(this->ptrlvl != two.ptrlvl) return false;
		if(this->rvalue != two.rvalue) return false;
		if(two.is_simple() && two.getsimpletype() == SimpleDataType::any)
			return true;
		if((this->is_object && !two.is_object) || (!this->is_object && two.is_object))
			return false;
		if(this->is_object && two.is_object)
			return this->getobjectname() == two.getobjectname();
		else if(!this->is_object && !two.is_object)
			return this->getsimpletype() == two.getsimpletype();
		else
			assert(false); // maybe bug
	}
	bool operator==(const BaseDataType& two) const {
		return this->eq(two);
	}
	bool operator!=(const BaseDataType& two) const {
		return !this->eq(two);
	}
	BaseDataType() {}
	BaseDataType(SimpleDataType other) {
		this->type = other;
		this->is_object = false;
		this->link = false;
	}
	BaseDataType(const BaseDataType& other) {
		this->type = other.type;
		this->is_object = other.is_object;
		this->link = other.link;
		this->rvalue = other.rvalue;
		this->ptrlvl = other.ptrlvl;
	}
	BaseDataType(const GString& objname) {
		this->type = objname;
		this->is_object = true;
	}
	void operator=(SimpleDataType other) {
		this->type = other;
		this->is_object = false;
	}
	void operator=(const BaseDataType& other) {
		this->type = other.type;
		this->is_object = other.is_object;
		this->link = other.link;
		this->rvalue = other.rvalue;
		this->ptrlvl = other.ptrlvl;
	}
	void operator=(const GString& objname) {
		this->type = objname;
		this->is_object = true;
	}
	friend std::ostream& operator<<(std::ostream& out, const BaseDataType& type) {
        std::cout << type.to_string() << std::endl;
        return out;
    }
};

struct DataType;

struct TypeNode {
    BaseDataType data;
    GVector<DataType> generics;
};

struct DataType {
    std::shared_ptr<TypeNode> node;

    DataType() {
        node = std::make_shared<TypeNode>();
    }

    DataType(const BaseDataType& bs) {
        node = std::make_shared<TypeNode>();
        node->data = bs;
    }

    BaseDataType& root() const { return node->data; }
    
    bool is_object() const { return node->data.is_object; }
    bool is_simple() const { return node->data.is_simple(); }
    GString getobjectname() const { return node->data.getobjectname(); }
    
    bool operator==(const DataType& other) const {
        if (this == &other) return true;
        if (node->data != other.node->data) return false;
        if (node->generics.size() != other.node->generics.size()) return false;
        
        for(size_t i = 0; i < node->generics.size(); ++i) {
            if (node->generics[i] != other.node->generics[i]) return false;
        }
        return true;
    }
    
    bool operator!=(const DataType& other) const { return !(*this == other); }

    GString sign() const {
        GString res = node->data.sign();
        if(!node->generics.empty()) {
        	res += "_";
        	for(size_t i = 0;i < node->generics.size();i++) {
        		res += node->generics[i].sign();
        		if(i != node->generics.size() - 1) {
        			res += "$";
        		}
        	}
        	res += "_";
        }
        return res;
    }

    GString to_string() const {
        GString res = node->data.to_string_d();
        if (!node->generics.empty()) {
            res += "<";
            for(size_t i = 0; i < node->generics.size(); ++i) {
                res += node->generics[i].to_string();
                if (i != node->generics.size() - 1) res += ",";
            }
            res += ">";
        }
        res += node->data.get_postfix();
        return res;
    }

	bool is_compatible_with(const DataType& other) const {
	    if (!this->root().arg_eq(other.root())) return false;
	    
	    if (this->node->generics.size() != other.node->generics.size()) return false;
	    
	    for(size_t i = 0; i < this->node->generics.size(); ++i) {
	        if (this->node->generics[i] != other.node->generics[i]) return false;
	    }
	    
	    return true;
	}
};

BaseDataType make_int_type() {
	BaseDataType tp = SimpleDataType::_int;
	return tp;
}

BaseDataType make_ptr_type() {
	BaseDataType tp = SimpleDataType::ptr;
	return tp;
}

BaseDataType make_void_type() {
	BaseDataType tp = SimpleDataType::_void;
	return tp;
}

BaseDataType make_any_type() {
	BaseDataType tp = SimpleDataType::any;
	return tp;
}

BaseDataType make_char_type() {
	BaseDataType tp = SimpleDataType::_char;
	return tp;
}

BaseDataType BaseDataTypeInt = make_int_type();
BaseDataType BaseDataTypePtr = make_ptr_type();
BaseDataType BaseDataTypeVoid = make_void_type();
BaseDataType BaseDataTypeAny = make_any_type();
BaseDataType BaseDataTypeChar = make_char_type();
BaseDataType BaseDataTypeConst = SimpleDataType::_constexpr;
BaseDataType BaseDataTypeProcPtr = SimpleDataType::proc_ptr;

GString dt_to_string(DataType& dt) {
	return dt.root().to_string();
}

BaseDataType token_to_dt(TokenType_t tt) {
	switch(tt) {
	case TokenType_t::int_type:
		return BaseDataTypeInt;
	case TokenType_t::ptr_type:
		return BaseDataTypePtr;
	case TokenType_t::void_type:
		return BaseDataTypeVoid;
	case TokenType_t::any_type:
		return BaseDataTypeAny;
	case TokenType_t::char_type:
		return BaseDataTypeChar;
	case TokenType_t::_constexpr:
		return BaseDataTypeConst;
	case TokenType_t::proc_ptr:
		return BaseDataTypeProcPtr;
	default:
		break;
	}
	assert(false); // unreacheable
}

BaseDataType uni_token_to_dt(const Token& tok) {
	if(tok.type == TokenType_t::ident) {
		BaseDataType dt;
		dt = tok.value.value();
		return dt;
	}
	return token_to_dt(tok.type);
}

bool is_type_token(TokenType_t tp) {
	return (tp == TokenType_t::int_type || tp == TokenType_t::ptr_type || tp == TokenType_t::void_type || tp == TokenType_t::proc_ptr || tp == TokenType_t::any_type || tp == TokenType_t::char_type || tp == TokenType_t::_constexpr || tp == TokenType_t::ident);
}

std::ostream& operator<<(std::ostream& out, DataType& dt) {
	std::cout << dt_to_string(dt); 
	return out;
}