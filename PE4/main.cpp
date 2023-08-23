#include <iostream>
#include "expr.h"

int main() {
	sym::Const c = 2;
	sym::Var x="x", y="y";

	sym::Expr f =  sym::exp(-y);
	sym::Expr df = f.diff(y);

	std::cout << "expression: " << f << std::endl;
	std::cout << "derivation: " << df << std::endl;

	return 0;
}
