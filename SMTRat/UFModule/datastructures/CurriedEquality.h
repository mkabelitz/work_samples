#ifndef CURRIEDEQUALITY_H
#define CURRIEDEQUALITY_H

#include "../../../solver/Module.h"
#include "CurriedEntity.h"

// represents an equality a = b where a and b are variables
// or equality f(a1,a2) = f(b1,b2) where a1, a2, b1, b2
// are arguments of the curry-function f
class CurriedEquality
{
public:
    CurriedEntity lhs;
    CurriedEntity rhs;

    CurriedEquality() :
        lhs(),
        rhs()
    {
    }
    
    // for equality a = b
    CurriedEquality(int variableA, int variableB) :
        lhs(variableA),
        rhs(variableB)
    {
    }

    // for functions f(a1, a2) = f(b1, b2)
    CurriedEquality(int a1, int a2, int b1, int b2) :
        CurriedEquality(CurriedEntity(a1, a2), CurriedEntity(b1, b2))
    {
    }

    CurriedEquality(const CurriedEntity &lhs, const CurriedEntity &rhs) :
        lhs(lhs),
        rhs(rhs)
    {
    }
    
    CurriedEquality(const CurriedEquality &ceq) :
        CurriedEquality(ceq.lhs, ceq.rhs)
    {
    }
};



inline bool operator==(const CurriedEquality &a, const CurriedEquality &b) {
    bool lhsIsPresent = a.lhs == b.lhs || a.lhs == b.rhs;
    bool rhsIsPresent = a.rhs == b.lhs || a.rhs == b.rhs;
    return lhsIsPresent && rhsIsPresent;
}

inline std::ostream& operator<<(std::ostream& os, const CurriedEquality& ceq)
{
    os << "CurriedEq[ " << ceq.lhs << " == " << ceq.rhs << " ]";
    return os;
}



struct HashForCurriedEquality {
public:
    std::size_t operator()(const CurriedEquality &x) const
    {
        size_t result = std::hash<int>()(x.lhs.arg1)
                ^ std::hash<int>()(x.lhs.arg2)
                ^ std::hash<int>()(x.rhs.arg1)
                ^ std::hash<int>()(x.rhs.arg2);
        return result;
    }
};

#endif /* CURRIEDEQUALITY_H */
