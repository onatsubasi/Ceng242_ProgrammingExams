#include "expr.h"

namespace sym 
{
	Expr::Expr(const Const& c) : expr_(new Const(c)) { }
	Expr::Expr(const Var& v) : expr_(new Var(v)){ }
	Expr::Expr(const __expr_t* e) : expr_(e){ }
	Expr::Expr(const __expr_t& e) : expr_(&e) { }
	Expr::Expr(const Expr& e) : expr_(e.expr_){ }
		
	Expr::~Expr() { delete expr_;}

	__expr_t* Expr::eval(const var_map_t& var_map) const {
		expr_->eval(var_map);
	}
	__expr_t* Expr::diff(const std::string& v) const {
		expr_->diff(v);
	}
	std::ostream& Expr::operator<< (std::ostream &out) const {
		expr_->operator<<(out);
	}
	bool Expr::operator==(const Expr& other) const {
		return expr_ == other.expr_;
	}
	bool Expr::operator==(const __expr_t& other) const {
		return expr_ == &other;
	}
}                                                                  	



	    // if (const Const* c = dynamic_cast<const Const*>(expr_))
	    //     return new Const(*c);
		// if (const Var* v = dynamic_cast<const Var*>(expr_))
        // 	return new Var(*v);
		// if (const __unary_op_t* u = dynamic_cast<const __unary_op_t*>(expr_)){
		// 	u->eval(var_map);
		// }
		// if (const __binary_op_t* b = dynamic_cast<const __binary_op_t*>(expr_)){
		// 	b->eval(var_map);
		// }
			