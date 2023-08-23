#include "binary.h"
#include "nullary.h"
#include <math.h>

namespace sym 
{
	bool AddOp::is_add() const { return true; }

    /* – returns a pointer to a new expression tree (recursively) equal to the one-hand side
if the other side is evaluated as zero-constant.
– returns a pointer to a constant with a variable summation of “value1” and “value2”
if both attributes are evaluated as value1-constant and value2-constant respectively.
– returns a pointer to a new addition instance whose attributes are pointing to two
sub-trees which are equal to ones pointed by left-hand side and right-hand
side respectively, otherwise.*/

	__expr_t* AddOp::eval(const var_map_t& vars) const {
	    if (const auto* newexp1 = dynamic_cast<const Const*>(rhs_->eval(vars))){
			if (const auto* newexp2 = dynamic_cast<const Const*>(lhs_->eval(vars)))
	        	return new Const(newexp2->get_value() + newexp1-> get_value());
	    }
	    
	   	if (const auto* a = dynamic_cast<const Const*>(rhs_)){
			if (a->get_value() == 0.0){
	        	return lhs_->eval(vars);
			}
	   	}
	   	if (const auto* b = dynamic_cast<const Const*>(lhs_)){
			if (b->get_value() == 0.0)
	        	return rhs_->eval(vars);
	   	}
	   	
	   return new AddOp(lhs_->eval(vars),rhs_->eval(vars));
	}

	__expr_t* AddOp::diff(const std::string& v) const { return new AddOp((lhs_->diff(v))->eval(), rhs_->diff(v)->eval()); }

	std::ostream& AddOp::operator<< (std::ostream &out) const {
	    if (const auto* a = dynamic_cast<const Const*>(rhs_)){
	        if (const auto* b = dynamic_cast<const Const*>(lhs_)){
	            if (a->get_value() == 0.0 && b->get_value() == 0.0)
	                lhs_->operator<<(out);
				else if (a->get_value() == 0.0)
	                lhs_->operator<<(out);
				else if (b->get_value() == 0.0)
	                rhs_->operator<<(out);
	           else {
	               lhs_->operator<<(out);
	               out << " + ";
	               rhs_->operator<<(out);
	           }
	        }
			else if (a-> get_value() == 0.0){
                lhs_->operator<<(out);
			}
			else { 
				if (const auto* b = dynamic_cast<const Var*>(lhs_)){
					lhs_->operator<<(out);
				}
				else {
					out << "(";
					lhs_->eval()->operator<<(out);
					out << ")";
				}
				out << " + ";
				rhs_->operator<<(out);
			}
	    }
		else {
			if (const auto* b = dynamic_cast<const Const*>(lhs_)){
	            if (b->get_value() == 0.0){
					rhs_->operator<<(out);
	        	}
				else {
					lhs_->operator<<(out);
					out <<" + ";
					if (const auto* c = dynamic_cast<const Var*>(rhs_)) {
						rhs_->operator<<(out);
					}
					else {
						out << "(";
						rhs_->eval()->operator<<(out);
						out << ")";
					}
				}
			}
			else if (const auto* b = dynamic_cast<const Var*>(lhs_)){
				lhs_->operator<<(out);
				out << " + ";
				if (const auto* c = dynamic_cast<const Var*>(rhs_)){
				    rhs_->operator<<(out);
			    }
			    else {
				    out << "(";
				    rhs_->eval()->operator<<(out);
				    out << ")";
			    }
			}
			else {
				out << "(";
				lhs_->eval()->operator<<(out);
				out << ")";
			    out << " + ";
			     if (const auto* b = dynamic_cast<const Var*>(rhs_)){
				    rhs_->eval()->operator<<(out);
			     }
			    else {
				     out << "(";
				   rhs_->eval()->operator<<(out);
				    out << ")";
			    }
			}
		}	
	}

	bool AddOp::operator==(const __expr_t& other_) const {
	    if (const AddOp* other = dynamic_cast<const AddOp*>(&other_)){
	        return ((*rhs_ == *other->rhs_ && *lhs_ == *other->lhs_) || (*rhs_ == *other->lhs_ && *lhs_ == *other->rhs_));
	    }
	    return false;
	}
}

namespace sym 
{
	bool MulOp::is_mul() const {return true; }

	__expr_t* MulOp::eval(const var_map_t& vars) const {
	    if (const auto* a = dynamic_cast<const Const*>(rhs_)){
			if (a->get_value() == 0.0)
	        	return new Const(0.0);
	        if(a->get_value() == 1.0){
	            if (const auto* b = dynamic_cast<const Const*>(lhs_)){
	                return new Const(b->get_value());
	            }
	            else if (const auto* b = dynamic_cast<const Var*>(lhs_)){
	                return new Var(b->get_variable());
	            }
	            else return lhs_->eval(vars);
	        }
	   	}
	   	if (const auto* b = dynamic_cast<const Const*>(lhs_)){
			if (b->get_value() == 0.0)
	        	return new Const(0.0);
	        if(b->get_value() == 1.0){
				if (const auto* b = dynamic_cast<const Const*>(rhs_)){
	                return new Const(b->get_value());
	            }
	            else if (const auto* b = dynamic_cast<const Var*>(rhs_)){
	                return new Var(b->get_variable());
	            }
	            else return rhs_->eval(vars);
			}
	   	}
	   	if (const auto* newexp1 = dynamic_cast<const Const*>(rhs_->eval(vars))){
			if (const auto* newexp2 = dynamic_cast<const Const*>(lhs_->eval(vars)))
	        	return new Const(newexp2->get_value() * newexp1-> get_value());
	    }
	    
	    return new MulOp(lhs_->eval(vars), rhs_->eval(vars));
	   	
	}

	__expr_t* MulOp::diff(const std::string& v) const {
	    return new AddOp(new MulOp(lhs_->diff(v), rhs_),new MulOp(lhs_, rhs_->diff(v)->eval()));
	}

	std::ostream& MulOp::operator<< (std::ostream &out) const {
	    if (const auto* a = dynamic_cast<const Const*>(rhs_->eval())){
	        if (const auto* b = dynamic_cast<const Const*>(lhs_->eval())){
	            if (a->get_value() == 0.0 && b->get_value() == 0.0)
	                lhs_->operator<<(out);
				else if (a->get_value() == 0.0)
	                rhs_->operator<<(out);
				else if (b->get_value() == 0.0)
	                lhs_->operator<<(out);
	           else if (a->get_value() == 1.0)
	                lhs_->operator<<(out);
	           else if (b->get_value() == 1.0)
	                rhs_->operator<<(out);
	           else {
	               lhs_->operator<<(out);
	               out << " * ";
	               rhs_->operator<<(out);
	           }
	        }
			else if (a-> get_value() == 0.0){
                rhs_->operator<<(out);
			}
			else if (a->get_value() == 1.0){
			    lhs_->operator<<(out);
			}
			else { 
				if (const auto* b = dynamic_cast<const Var*>(lhs_->eval())){
					lhs_->operator<<(out);
				}
				else {
					out << "(";
					lhs_->eval()->operator<<(out);
					out << ")";
				}
				out << " * ";
				rhs_->operator<<(out);
			}
	    }
		else {
			if (const auto* b = dynamic_cast<const Const*>(lhs_->eval())){
	            if (b->get_value() == 0.0){
					lhs_->operator<<(out);
	        	}
	        	else if (b-> get_value() == 1.0){
	        	    rhs_->operator<<(out);
	        	}
				else {
					lhs_->operator<<(out);
					out <<" * ";
					if (const auto* c = dynamic_cast<const Var*>(rhs_->eval())) {
						rhs_->operator<<(out);
					}
					else {
						out << "(";
						rhs_->eval()->operator<<(out);
						out << ")";
					}
				}
			}
			else if (const auto* b = dynamic_cast<const Var*>(lhs_->eval())){
				lhs_->operator<<(out);
				out << " * ";
				if (const auto* c = dynamic_cast<const Var*>(rhs_->eval())){
				    rhs_->operator<<(out);
			    }
			    else {
				    out << "(";
				    rhs_->eval()->operator<<(out);
				    out << ")";
			    }
			}
			else {
				out << "(";
				lhs_->eval()->operator<<(out);
				out << ")";
				out << " * " ;
				if (const auto* b = dynamic_cast<const Var*>(rhs_->eval())){
    				rhs_->eval()->operator<<(out);
    			}
    			else {
    				out << "(";
    				rhs_->eval()->operator<<(out);
    				out << ")";
    			}
			}
		}	
	}

	bool MulOp::operator==(const __expr_t& other_) const {
	    if (const MulOp* other = dynamic_cast<const MulOp*>(&other_)){
	        return ((*rhs_ == *other->rhs_ && *lhs_ == *other->lhs_) || (*rhs_ == *other->lhs_ && *lhs_ == *other->rhs_));
	    }
	    return false;
	}
}
