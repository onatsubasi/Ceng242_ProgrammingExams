#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>

namespace sym 
{
	bool NegOp::is_neg() const {return true; }

	/*(overridden method) returns a pointer to a new sym::Const
	with a negative “variable” if its’ operand is evaluated as constant with “variable”.
	Otherwise, it returns a pointer to a new negation instance whose operand points to a
	sub-tree that is equal to one pointed to by operand.*/

	__expr_t* NegOp::eval(const var_map_t& vars) const {
		if (const auto* newconst = dynamic_cast<const Const*>(operand)){
			return new Const(-(newconst->get_value()));
		}
		else{
			return new NegOp(operand); 
		}
	 }
	 
	 /*  (overridden method) returns a pointer to a new expression tree
that is equal to the differentiation of the tree pointed to by operand. */


	__expr_t* NegOp::diff(const std::string& v) const { 
	    return new NegOp(operand->diff(v)); 
	    
	}


    /*  (overridden method) writes “-operand” to its input
“output stream” and returns it in top-to-down fashion. Additionally, if it’s operand
is not nullary, encloses it with parenthesis i.e. “-(operand)”, please check the advice */
	std::ostream& NegOp::operator<<(std::ostream& out) const {
	    if (const auto* newconst = dynamic_cast<const Const*>(operand)){
	        if (newconst->get_value() == 0.0){
	            out << "-";
	            operand->operator<<(out);
	        }
	       else{
	           out << "-";
	           operand->operator<<(out);
	       }
	    }
	    else{
    		out << "-";
    	    if(const Var* other = dynamic_cast<const Var*>(operand))
    	        operand->operator<<(out);
    	    else{
    	        out << "(";
    	        operand->operator<<(out);
    	        out << ")";
    	    }
	    }
		
    }
    
    /*  (overridden method) compares if its input argument
(say other) is a sym::NegOp instance with a sub-tree that is (recursively) equal to
the one pointed to by operand. */

	bool NegOp::operator==(const __expr_t& other_) const {
      if (const NegOp* other = dynamic_cast<const NegOp*>(&other_)) {
        return *operand == *other->operand;
      }
      return false;
    }

	
	
}

namespace sym 
{
	bool ExpOp::is_exp() const {return true; }

	__expr_t* ExpOp::eval(const var_map_t& vars) const {
	    if(const auto* newexp = dynamic_cast<const Const*>(operand)){
	        double newval = std::exp(newexp->get_value());
	        return new Const(newval);
	    }
	    else return new ExpOp(operand);
	}

	__expr_t* ExpOp::diff(const std::string& v) const {
	    if(const auto* newexp = dynamic_cast<const Const*>(operand)){
	        return new Const(0);
	    }
	    
	    if(const auto* newexp = dynamic_cast<const Var*>(operand)){
	        return new ExpOp(operand);
	    }
	    return new MulOp(operand->diff(v), new ExpOp(operand));
	}

	std::ostream& ExpOp::operator<< (std::ostream &out) const {
		out << "e^";
		if (const Const* other = dynamic_cast<const Const*>(operand)){
			operand->operator<<(out);
		}
	    else if(const Var* other = dynamic_cast<const Var*>(operand))
	        operand->operator<<(out);
	    else{
	        out << "(";
	        operand->operator<<(out);
	        out << ")";
	    }
	}

	bool ExpOp::operator==(const __expr_t& other_) const {
	    if (const ExpOp* other = dynamic_cast<const ExpOp*>(&other_)){
	        return *operand == *other -> operand;
	    }
	    return false;
	}
}
