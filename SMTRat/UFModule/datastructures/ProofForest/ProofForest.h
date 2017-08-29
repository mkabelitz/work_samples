/**
 * @file ProofForest.h
 * @author Alexander Ryndin <>
 * @author Niklas Rieken <niklas.rieken@rwth-aachen.de>
 *
 * @version 2016-05-13
 * Created on 2016-04-28.
 */

#ifndef PROOFFOREST_H
#define PROOFFOREST_H

#include "../CurriedEntity.h"
#include "../CurriedEquality.h"
#include "../../../../solver/Module.h"

namespace ufmodule {

    // This struct is for hashing pairs<T, U>
    template <typename T, typename U> struct pairhash {
    public:
        std::size_t operator()(const std::pair<T, U> &x) const
        {
            return std::hash<T>()(x.first) ^ std::hash<U>()(x.second);
        }
    };

    typedef int vertex_t;
    typedef int forest_t;
    typedef std::pair<CurriedEquality*, CurriedEquality*> tuple_of_curried_eq_t;

    // default implementation, non-optimal, first approximation:
    // TODO: optimize this implementation
    // or write a new one, implementing the same interface
    // in the module use this data structure through the interface only
    // this way it'll be easier to add a new optimal implementation
    class ProofForest
    {
    private:
        std::vector<forest_t> forest;
        std::unordered_map<pair<vertex_t, vertex_t>, tuple_of_curried_eq_t, pairhash<vertex_t, vertex_t>> edgeToEqualityInfo;
        // data structures for explain operation
        std::vector<std::list<vertex_t>*> descendantsLists;
        std::vector<bool> isVertexProcessed;
        std::vector<vertex_t> highestNode;
        std::unordered_map<CurriedEquality, bool, HashForCurriedEquality> isEqualityAlreadyAdded;

        // merge + propagate
        std::queue<tuple_of_curried_eq_t> pending;
        std::vector<   std::list<CurriedEquality>*   > useList;
        std::unordered_map<   std::pair<vertex_t, vertex_t>, CurriedEquality*, pairhash<vertex_t, vertex_t>   > lookup;
        std::vector<   std::list<vertex_t>*   > classList;

        void AddDirectedEdge(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq);
        void SetEdgeEqualityInfo(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq);
        tuple_of_curried_eq_t GetEdgeEqualityInfo(vertex_t v1, vertex_t v2);
        bool HasEdgeEqualityInfo(vertex_t v1, vertex_t v2);
        std::pair<vertex_t, vertex_t> GetVertexPair(vertex_t v1, vertex_t v2);

        void ExplainAlongPath(vertex_t v1, vertex_t v2, std::vector<CurriedEquality> &explanations, std::queue<CurriedEquality> &pendingProofs);
        void InitializeDatastructuresForExplain();
        void ClearDescendantsLists();
        void DeleteDescendantsLists();
        void CreateDescendantsLists();
        void ClearUseLists();
        void DeleteUseLists();
        void CreateUseLists();
        void ClearClassLists();
        void DeleteClassLists();
        void CreateClassLists(size_t initialCapacity);
        void CheckVertexAndAddItIntoDescendantsListsIfNecessary(vertex_t v);
        void AddExplanation(CurriedEquality &ceq, std::vector<CurriedEquality> &explanations);
        vertex_t NearestCommonAncestor(vertex_t v1, vertex_t v2);

        bool IsVertexInBounds(vertex_t v);

        void Propagate();
        void InsertEdgeAndPropagate(vertex_t a, vertex_t b,
            vertex_t repr_a, vertex_t repr_b,
            tuple_of_curried_eq_t &pairOfEqualities);
        bool Union(vertex_t v1, vertex_t v2, tuple_of_curried_eq_t &pairOfEq);
        bool IsEqualityOfTwoVariables(tuple_of_curried_eq_t &pairOfEq);
        bool IsTwoFunctionEqualities(tuple_of_curried_eq_t &pairOfEq);

    public:
        ProofForest(size_t initialCapacity);

        virtual ~ProofForest();

        void Merge(CurriedEquality ceq);
        void Split(CurriedEquality ceq);
        virtual vertex_t Find(vertex_t v);
        virtual std::vector<CurriedEquality> Explain(vertex_t v1, vertex_t v2);
    };
}

#endif /* PROOFFOREST_H */
