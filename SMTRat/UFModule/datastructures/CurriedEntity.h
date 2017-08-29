#ifndef CURRIEDENTITY_H
#define CURRIEDENTITY_H

#include <iostream>

// represents a variable (arg1)
// or a function with two arguments arg1 and arg2 i.e. f(arg1, arg2)
// where f is the curry-function
class CurriedEntity {
public:
    int arg1;
    int arg2;

    CurriedEntity() :
        arg1(-1),
        arg2(-1)
    {
    }
    
    // for a variable
    CurriedEntity(int variable) :
        arg1(variable),
        arg2(-1)
    {
    }

    // for a function f(arg1, arg2)
    CurriedEntity(int arg1, int arg2) :
        arg1(arg1),
        arg2(arg2)
    {
    }
    
    CurriedEntity(const CurriedEntity &entity) :
        arg1(entity.arg1),
        arg2(entity.arg2)
    {
    }
    
    bool isVariable() {
        return arg2 == -1;
    }
    
    bool isFunction() {
        return arg1 != -1 && arg2 != -1;
    }
};



inline bool operator==(const CurriedEntity &a, const CurriedEntity &b) {
    return a.arg1 == b.arg1 && a.arg2 == b.arg2;
}

inline std::ostream& operator<<(std::ostream& os, const CurriedEntity& entity)
{
    os << "(" << entity.arg1 << "," << entity.arg2 << ")";
    return os;
}

#endif /* CURRIEDENTITY_H */
