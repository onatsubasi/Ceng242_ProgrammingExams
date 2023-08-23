#include "nullary.h"
#include "unary.h"
#include "binary.h"

namespace sym {
	__expr_t& operator-(const __expr_t &op) { __expr_t* result = new NegOp(&op); return *result; }
	__expr_t& exp(const __expr_t &op) { __expr_t* result = new ExpOp(&op); return *result; }

	__expr_t& operator+(const __expr_t &lhs, const __expr_t &rhs) {
		__expr_t* result = new AddOp(&lhs, &rhs);
		 return *result;
	 }
	__expr_t& operator+(double lhs, const __expr_t &rhs) {
		__expr_t* nlhs = new Const(lhs);
		__expr_t* result = new AddOp(nlhs, &rhs);
		 return *result;
	}
	__expr_t& operator+(const __expr_t &lhs, double rhs) {
		__expr_t* nrhs = new Const(rhs);
		__expr_t* result = new AddOp(&lhs, nrhs);
		 return *result;
	}

	__expr_t& operator*(const __expr_t &lhs, const __expr_t &rhs) {
		__expr_t* result = new MulOp(&lhs, &rhs);
		return *result;
	}
	__expr_t& operator*(double lhs, const __expr_t &rhs) {
		__expr_t* nlhs = new Const(lhs);
		__expr_t* result = new MulOp(nlhs, &rhs);
		return *result;
	}
	__expr_t& operator*(const __expr_t &lhs, double rhs) {
		__expr_t* nrhs = new Const(rhs);
		__expr_t* result = new MulOp(&lhs, nrhs);
		return *result;
	}
}
