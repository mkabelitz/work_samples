/**
 * @file ProofForest.cpp
 * @author Alexander Ryndin <>
 * @author Niklas Rieken <niklas.rieken@rwth-aachen.de>
 *
 * @version 2016-05-13
 * Created on 2016-04-28.
 */

#include <assert.h>

#include "ProofForest.h"

namespace ufmodule {

    ProofForest::ProofForest(size_t initialCapacity):
        forest(initialCapacity, -1),
        edgeToEqualityInfo(),
        descendantsLists(initialCapacity, NULL),
        isVertexProcessed(initialCapacity, false),
        highestNode(initialCapacity),
        isEqualityAlreadyAdded(),
        pending(),
        useList(initialCapacity, NULL),
        lookup(),
        classList(initialCapacity, NULL)
    {
        CreateDescendantsLists();
        CreateUseLists();
        CreateClassLists(initialCapacity);
    }

    ProofForest::~ProofForest() {
        forest.clear();
        isVertexProcessed.clear();
        ClearDescendantsLists();
        DeleteDescendantsLists();
        descendantsLists.clear();
        ClearUseLists();
        DeleteUseLists();
        useList.clear();
        ClearClassLists();
        DeleteClassLists();
        classList.clear();
        for (auto it = lookup.begin(); it != lookup.end(); it++) {
            delete ((*it).second);
            (*it).second = NULL;
        }
        lookup.clear();
        while (!pending.empty()) {
            auto element = pending.front();
            pending.pop();
            if (element.first != NULL) {
                delete (element.first);
                element.first = NULL;
            }
            if (element.second != NULL) {
                delete (element.second);
                element.second = NULL;
            }
        }
        for (auto it = edgeToEqualityInfo.begin(); it != edgeToEqualityInfo.end(); it++) {
            auto pairOfEqualities = it -> second;
            if (pairOfEqualities.first != NULL) {
                delete pairOfEqualities.first;
                pairOfEqualities.first = NULL;
            }
            if (pairOfEqualities.second != NULL) {
                delete pairOfEqualities.second;
                pairOfEqualities.second = NULL;
            }
        }
        edgeToEqualityInfo.clear();
    }
    
    void ProofForest::Merge(CurriedEquality eq) {
        // TODO Delete
        // cout << "ProofForest.cpp: Merge, eq = " << eq << endl;
        CurriedEntity s = eq.lhs;
        CurriedEntity t = eq.rhs;
        if (s.isVariable() && t.isVariable()) {
            CurriedEquality* variableEqualilty = new CurriedEquality(eq.lhs, eq.rhs);
            // add a=b to Pending
            pending.push(std::make_pair(variableEqualilty, static_cast<CurriedEquality*>(NULL)));
            Propagate();
        } else if (s.isFunction() && t.isVariable()) {
            vertex_t a = t.arg1;
            vertex_t a1 = s.arg1;
            vertex_t a2 = s.arg2;
            vertex_t repr_a1 = Find(a1);
            vertex_t repr_a2 = Find(a2);
            // cout << "ProofForest.cpp: Merge, " << "repr_a1 = " << repr_a1 << ", repr_a2 = " << repr_a2 << endl;
            std::pair<vertex_t, vertex_t> key = std::make_pair(repr_a1, repr_a2);
            if (lookup.find(key) != lookup.end()) {
                CurriedEquality lu = *(lookup[key]);
                if (!(lu.lhs.isFunction() && lu.rhs.isVariable())) {
                    assert(false);
                }
                vertex_t b = lu.rhs.arg1;
                vertex_t b1 = lu.lhs.arg1;
                vertex_t b2 = lu.lhs.arg2;
                CurriedEquality* functionalEq1 = new CurriedEquality(CurriedEntity(a1, a2), CurriedEntity(a));
                CurriedEquality* functionalEq2 = new CurriedEquality(CurriedEntity(b1, b2), CurriedEntity(b));
                // TODO Delete
                // cout << "ProofForest.cpp: Merge, adding "
                //         << (*functionalEq1)
                //         << " and " << (*functionalEq2)
                //         << " to the pending list" << endl;
                // add f(a1,a2)=a and f(b1,b2)=b to Pending
                pending.push(std::make_pair(functionalEq1, functionalEq2));
                Propagate();
            } else {
                // set Lookup(a1',a2') to f(a1,a2) = a
                lookup[key] = new CurriedEquality(CurriedEntity(a1, a2), CurriedEntity(a));
                // TODO Delete
                // cout << "ProofForest.cpp: Merge, updating UseLists, a1 = " << a1
                        // << ", a2 = " << a2 << ", a = " << a << endl;
                // add f(a1,a2)=a to UseList(a1') and to UseList(a2')
                CurriedEquality useListsEquality = CurriedEquality(CurriedEntity(a1, a2), CurriedEntity(a));
                // TODO Delete
                // cout << "ProofForest.cpp: Merge, size of useList: " << useList.size() << endl;
                (*(useList[repr_a1])).push_back(useListsEquality);
                (*(useList[repr_a2])).push_back(useListsEquality);
            }
        } else {
            assert(false);
        }
    }
    
    void ProofForest::Split(CurriedEquality ceq) {
    }

    void ProofForest::Propagate() {
        // TODO Delete
        // cout << "ProofForest.cpp: Propagate" << endl;
        while (!pending.empty()) {
            auto pairOfEqualities = pending.front();
            pending.pop();
            vertex_t a = -1;
            vertex_t b = -1;
            if (IsEqualityOfTwoVariables(pairOfEqualities)) {
                CurriedEquality ceq = *(pairOfEqualities.first);
                a = ceq.lhs.arg1;
                b = ceq.rhs.arg1;
            }
            else if (IsTwoFunctionEqualities(pairOfEqualities)) {
                CurriedEquality ceq1 = *(pairOfEqualities.first);
                CurriedEquality ceq2 = *(pairOfEqualities.second);
                if (ceq1.lhs.isFunction() && ceq1.rhs.isVariable()) {
                    a = ceq1.rhs.arg1;
                }
                if (ceq2.lhs.isFunction() && ceq2.rhs.isVariable()) {
                    b = ceq2.rhs.arg1;
                }
            }
            else {
                assert(false);
            }
            if (a == -1 || b == -1) {
                assert(false);
            }
            vertex_t repr_a = Find(a);
            vertex_t repr_b = Find(b);
            if (repr_a == repr_b) {
                continue;
            }
            if ((*classList[repr_a]).size() <= (*classList[repr_b]).size()) {
                InsertEdgeAndPropagate(a, b, repr_a, repr_b, pairOfEqualities);
            } else {
                InsertEdgeAndPropagate(b, a, repr_b, repr_a, pairOfEqualities);
            }
        }
        // TODO Delete
        // cout << "ProofForest.cpp: end of Propagate" << endl;
    }

    bool ProofForest::Union(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq) {
        // TODO Delete
        // cout << "ProofForest.cpp: Union, v1 = " << v1 << ", v2 = " << v2
        //         << ", pairOfEq = ( ";
        if (pairOfEq.first == NULL) {
            // cout << "NULL";
        } else {
            // cout << *(pairOfEq.first);
        }
        // cout << ", ";
        if (pairOfEq.second == NULL) {
            // cout << "NULL";
        } else {
            // cout << *(pairOfEq.second);
        }
        // cout << " )" << endl;
        assert(IsVertexInBounds(v1));
        assert(IsVertexInBounds(v2));
        vertex_t r1 = Find(v1);
        vertex_t r2 = Find(v2);
        // TODO Delete
        // cout << "ProofForest.cpp: Union, roots are r1 = " << r1 << " and r2 = " << r2 << endl;
        if (r1 == r2) {
            return false;
        }
        const size_t h1 = classList[r1] -> size();
        const size_t h2 = classList[r2] -> size();
        if (h1 <= h2) {
            AddDirectedEdge(v1, v2, pairOfEq);
        } else {
            AddDirectedEdge(v2, v1, pairOfEq);
        }
        return true;
    }

    // TODO: add separate compression (only for Unions)
    vertex_t ProofForest::Find(vertex_t v) {
        assert(IsVertexInBounds(v));
        vertex_t root = v;
        while (forest[root] >= 0) {
            root = forest[root];
        }
        return root;
    }

    std::vector<CurriedEquality> ProofForest::Explain(vertex_t v1, vertex_t v2) {
        // TODO Delete
        // cout << "ProofForest.cpp: Explain, v1 = " << v1 << ", v2 = " << v2 << endl;
        assert(IsVertexInBounds(v1));
        assert(IsVertexInBounds(v2));
        std::vector<CurriedEquality> explanations;
        if (v1 == v2) {
            return explanations;
        }
        InitializeDatastructuresForExplain();
        // Set PendingProofs to { c_1 = c_2 }
        std::queue<CurriedEquality> pendingProofs;
        pendingProofs.push(CurriedEquality(v1, v2));
        while (!pendingProofs.empty())
        {
            // Remove an equation a == b from PendingProofs
            CurriedEquality eq = pendingProofs.front();
            pendingProofs.pop();
            assert(eq.lhs.isVariable());
            assert(eq.rhs.isVariable());
            vertex_t a = eq.lhs.arg1;
            vertex_t b = eq.rhs.arg1;
            // c := NearestCommonAncestor(a, b)
            vertex_t c = NearestCommonAncestor(a, b);
            // no common ancestor -- no explanations
            if (c == -1) {
                // TODO: raise exception
                assert(false);
                continue;
            }
            // ExplainAlongPath(a, c)
            ExplainAlongPath(a, c, explanations, pendingProofs);
            // ExplainAlongPath(b, c)
            ExplainAlongPath(b, c, explanations, pendingProofs);
        }
        return explanations;
    }

    void ProofForest::AddExplanation(CurriedEquality &ceq, std::vector<CurriedEquality> &explanations) {
        // TODO Delete
        // cout << "ProofForest.cpp: AddExplanation, ceq = " << ceq << endl;
        bool equalityHasAlreadyBeenAdded = isEqualityAlreadyAdded.find(ceq) != isEqualityAlreadyAdded.end();
        if (equalityHasAlreadyBeenAdded) {
            // TODO Delete
            // cout << "ProofForest.cpp: AddExplanation, the explanation has already been added before" << endl;
            return;
        }
        explanations.push_back(ceq);
        isEqualityAlreadyAdded[ceq] = true;
    }
    
    void ProofForest::InsertEdgeAndPropagate(vertex_t a, vertex_t b,
            vertex_t repr_a, vertex_t repr_b,
            tuple_of_curried_eq_t &pairOfEqualities) {
        assert (repr_a != repr_b);
        // TODO Delete
        // cout << "ProofForest.cpp: InsertEdgeAndPropagate, a = " << a << ", b = " << b
        //         << ", repr_a = " << repr_a << ", repr_b = " << repr_b << endl;
        vertex_t old_repr_a = repr_a;
        // insert edge a -> b labeled with E into ProofForest
        Union(a, b, pairOfEqualities);
        std::list<vertex_t> class_list_for_old_repr_a = *(classList[old_repr_a]);
        std::list<vertex_t> class_list_for_repr_b = *(classList[repr_b]);
        for (auto c = class_list_for_old_repr_a.begin(); c != class_list_for_old_repr_a.end(); ++c) {
            class_list_for_repr_b.push_back(*c);
        }
        class_list_for_old_repr_a.clear();
        std::list<CurriedEquality> use_list_for_old_repr_a = *(useList[old_repr_a]);
        for (auto fun = use_list_for_old_repr_a.begin(); fun != use_list_for_old_repr_a.end(); ++fun) {
            assert (fun -> lhs.isFunction());
            assert (fun -> rhs.isVariable());
            vertex_t c1 = fun -> lhs.arg1;
            vertex_t c2 = fun -> lhs.arg2;
            vertex_t c = fun -> rhs.arg1;
            vertex_t repr_c1 = Find(c1);
            vertex_t repr_c2 = Find(c2);
            std::pair<vertex_t, vertex_t> key = std::make_pair(repr_c1, repr_c2);
            if (lookup.find(key) != lookup.end()) {
                CurriedEquality eq = *(lookup[key]);
                assert (eq.lhs.isFunction());
                assert (eq.rhs.isVariable());
                vertex_t d1 = eq.lhs.arg1;
                vertex_t d2 = eq.lhs.arg2;
                vertex_t d = eq.rhs.arg1;
                CurriedEquality *eq1 = new CurriedEquality(CurriedEntity(c1,c2), CurriedEntity(c));
                CurriedEquality *eq2 = new CurriedEquality(CurriedEntity(d1,d2), CurriedEntity(d));
                tuple_of_curried_eq_t new_pen = std::make_pair(eq1, eq2);
                pending.push(new_pen);
            } else {
                lookup[key] = new CurriedEquality(fun->lhs, fun->rhs);
                (*(useList[repr_b])).push_back(*fun);
            }
        }
        (*(useList[old_repr_a])).clear();
    }

    void ProofForest::AddDirectedEdge(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq) {
        // TODO Delete
        // cout << "ProofForest.cpp: AddDirectedEdge, v1 = " << v1 << ", v2 = " << v2
        //         << ", forest size = " << forest.size() << endl;
        // flip the edges first
        vertex_t p1 = v1;
        vertex_t p2 = forest[p1];
        while (p2 >= 0) {
            vertex_t p3 = forest[p2];
            forest[p2] = p1;
            p1 = p2;
            p2 = p3;
        }
        // and then add the edge
        forest[v1] = v2;
        // save additional edge information
        // about the associated equalities
        SetEdgeEqualityInfo(v1, v2, pairOfEq);
    }

    void ProofForest::SetEdgeEqualityInfo(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq)
    {
        std::pair<vertex_t, vertex_t> p = GetVertexPair(v1, v2);
        edgeToEqualityInfo[p] = pairOfEq;
    }

    tuple_of_curried_eq_t ProofForest::GetEdgeEqualityInfo(vertex_t v1, vertex_t v2)
    {
        std::pair<vertex_t, vertex_t> p = GetVertexPair(v1, v2);
        return edgeToEqualityInfo[p];
    }

    bool ProofForest::HasEdgeEqualityInfo(vertex_t v1, vertex_t v2)
    {
        std::pair<vertex_t, vertex_t> p = GetVertexPair(v1, v2);
        bool result = edgeToEqualityInfo.find(p) != edgeToEqualityInfo.end();
        return result;
    }

    std::pair<vertex_t, vertex_t> ProofForest::GetVertexPair(vertex_t v1, vertex_t v2)
    {
        vertex_t first = v1;
        vertex_t second = v2;
        if (v2 > v1)
        {
            first = v2;
            second = v1;
        }
        return std::make_pair(first, second);
    }

    bool ProofForest::IsVertexInBounds(vertex_t v) {
        return 0 <= v && v < forest.size();
    }

    void ProofForest::ExplainAlongPath(vertex_t a, vertex_t c, std::vector<CurriedEquality> &explanations, std::queue<CurriedEquality> &pendingProofs) {
        // TODO Delete
        // cout << "ProofForest.cpp: ExplainAlongPath, a = " << a << ", c = " << c << endl;
        assert(IsVertexInBounds(a));
        assert(IsVertexInBounds(c));
        // a := HighestNode(a)
        a = highestNode[a];
        // While a!=c Do
        while (a != c) {
            // b := parent(a)
            vertex_t b = forest[a];
            tuple_of_curried_eq_t pairOfEq = GetEdgeEqualityInfo(a, b);
            // If edge is labeled with a single input merge a=b
            bool variableEqualsVariable = IsEqualityOfTwoVariables(pairOfEq);
            bool twoFunctionEqualities = IsTwoFunctionEqualities(pairOfEq);
            if (variableEqualsVariable) {
                AddExplanation(*(pairOfEq.first), explanations);
            }
            // edge labeled with f(a1,a2) = f(b1,b2)
            else if (twoFunctionEqualities) {
                AddExplanation(*(pairOfEq.first), explanations);
                AddExplanation(*(pairOfEq.second), explanations);
                int a1 = pairOfEq.first -> lhs.arg1;
                int a2 = pairOfEq.first -> lhs.arg2;
                int b1 = pairOfEq.second -> lhs.arg1;
                int b2 = pairOfEq.second -> lhs.arg2;
                if (a1 != b1) {
                    // TODO Delete
                    // cout << "ProofForest.cpp: ExplainAlongPath, also need to explain why " << a1 << " == " << b1 << endl;
                    pendingProofs.push(CurriedEquality(a1, b1));
                }
                if (a2 != b2) {
                    // TODO Delete
                    // cout << "ProofForest.cpp: ExplainAlongPath, also need to explain why " << a2 << " == " << b2 << endl;
                    pendingProofs.push(CurriedEquality(a2, b2));
                }
            }
            else {
                // TODO: raise exception
                exit(77);
                assert(false);
            }
            // we do not do Union(a,b) here
            // because we have only one data structure
            // for both UnionFind and ProofForest
            // but we need to update the HighestNode
            // a := HighestNode(b)
            a = highestNode[b];
        }
    }
    
    bool ProofForest::IsEqualityOfTwoVariables(tuple_of_curried_eq_t &pairOfEq) {
        bool variableEqualsVariable = pairOfEq.first != NULL && pairOfEq.second == NULL
                && (pairOfEq.first -> lhs).isVariable() && (pairOfEq.first -> rhs).isVariable();
        return variableEqualsVariable;
    }
    
    bool ProofForest::IsTwoFunctionEqualities(tuple_of_curried_eq_t &pairOfEq) {
        bool twoFunctionEqualities = pairOfEq.first != NULL && pairOfEq.second != NULL
                && (pairOfEq.first -> lhs).isFunction() && (pairOfEq.first -> rhs).isVariable()
                && (pairOfEq.second -> lhs).isFunction() && (pairOfEq.second -> rhs).isVariable();
        return twoFunctionEqualities;
    }

    void ProofForest::InitializeDatastructuresForExplain() {
        ClearDescendantsLists();
        std::fill(isVertexProcessed.begin(), isVertexProcessed.end(), false);
        int nodeIndex = 0;
        for(auto it = highestNode.begin(); it != highestNode.end(); ++it) {
            *it = nodeIndex;
            nodeIndex++;
        }
        isEqualityAlreadyAdded.clear();
    }

    vertex_t ProofForest::NearestCommonAncestor(vertex_t v1, vertex_t v2) {
        // TODO Delete
        // cout << "ProofForest.cpp: NearestCommonAncestor, v1 = " << v1 << ", v2 = " << v2 << endl;
        assert(IsVertexInBounds(v1));
        assert(IsVertexInBounds(v2));
        CheckVertexAndAddItIntoDescendantsListsIfNecessary(v1);
        CheckVertexAndAddItIntoDescendantsListsIfNecessary(v2);
        vertex_t node = v1;
        do {
            if (node == v2) {
                return node;
            }
            auto list = *(descendantsLists[node]);
            for(auto it = list.begin(); it != list.end(); ++it) {
                if (*it == v2) {
                    return node;
                }
            }
            node = forest[node];
        } while (node >= 0);
        return -1;
    }

    void ProofForest::ClearDescendantsLists() {
        for(auto it = descendantsLists.begin(); it != descendantsLists.end(); ++it) {
            (**it).clear();
        }
    }

    void ProofForest::DeleteDescendantsLists() {
        for(auto it = descendantsLists.begin(); it != descendantsLists.end(); ++it) {
            delete (*it);
            *it = NULL;
        }
    }

    void ProofForest::CreateDescendantsLists() {
        for(auto it = descendantsLists.begin(); it != descendantsLists.end(); ++it) {
            *it = new std::list<vertex_t>();
        }
    }
    
    void ProofForest::ClearUseLists() {
        for(auto it = useList.begin(); it != useList.end(); ++it) {
            (**it).clear();
        }
    }

    void ProofForest::DeleteUseLists() {
        for(auto it = useList.begin(); it != useList.end(); ++it) {
            delete (*it);
            *it = NULL;
        }
    }
    
    void ProofForest::CreateUseLists() {
        for(auto it = useList.begin(); it != useList.end(); ++it) {
            *it = new std::list<CurriedEquality>();
        }
    }
    
    void ProofForest::ClearClassLists() {
        for(auto it = classList.begin(); it != classList.end(); ++it) {
            (**it).clear();
        }
    }

    void ProofForest::DeleteClassLists() {
        for(auto it = classList.begin(); it != classList.end(); ++it) {
            delete (*it);
            *it = NULL;
        }
    }
    
    void ProofForest::CreateClassLists(size_t initialCapacity) {
        for (vertex_t i = 0; i < initialCapacity; i++) {
            std::list<vertex_t>* newList = new std::list<vertex_t>();
            newList -> push_back(i);
            classList[i] = newList;
        }
    }

    void ProofForest::CheckVertexAndAddItIntoDescendantsListsIfNecessary(vertex_t v) {
        assert(IsVertexInBounds(v));
        if (isVertexProcessed[v]) {
            return;
        }
        vertex_t node = forest[v];
        while (node >= 0) {
            (*descendantsLists[node]).push_back(v);
            node = forest[node];
        }
        isVertexProcessed[v] = true;
    }
}
