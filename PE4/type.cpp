#include "type.h"

namespace sym 
{
	__nullary_op_t::~__nullary_op_t() { }

	bool __nullary_op_t::is_nullary() const { return true; }

	bool __nullary_op_t::is_con() const {
	}

	bool __nullary_op_t::is_var() const { }

}

namespace sym 
{
	__unary_op_t::~__unary_op_t() { delete operand;}

	bool __unary_op_t::is_unary() const { return true; }

	bool __unary_op_t::is_neg() const { 	}
	bool __unary_op_t::is_exp() const { }

}

namespace sym 
{
	__binary_op_t::~__binary_op_t() {}

	bool __binary_op_t::is_binary() const {return true; }

	bool __binary_op_t::is_add() const { }
	bool __binary_op_t::is_mul() const { }

}
