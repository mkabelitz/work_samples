/**
 * @file UFModule.cpp
 * @author Marco Kabelitz <marco.kabelitz@rwth-aachen.de>
 *
 * @version 2016-04-28
 * Created on 2016-04-28.
 */

#include "UFModule.h"
namespace smtrat
{
	template<class Settings>
		UFModule<Settings>::UFModule(const ModuleInput* _formula, RuntimeSettings*, Conditionals& _conditionals, Manager* _manager):
			Module( _formula, _conditionals, _manager )
#ifdef SMTRAT_DEVOPTION_Statistics
			, mStatistics(Settings::moduleName)
#endif
			{
				proofForest = NULL;
			}

	template<class Settings>
		UFModule<Settings>::~UFModule()
		{
			if (proofForest != NULL)
			{
				delete proofForest;
			}
		}

	template<class Settings>
		bool UFModule<Settings>::intIDIsUVariable(int x) 
		{
			return (mIntIDToPair.find(x) != mIntIDToPair.end() && mIntIDToPair[x] == mUVariablePair);
		}

	template<class Settings>
		bool UFModule<Settings>::intIDIsUF(int x) 
		{
			return (mIntIDToPair.find(x) != mIntIDToPair.end() && mIntIDToPair[x] == mUFPair);
		}

	template<class Settings>
		int UFModule<Settings>::mapUV(UVariable uv) 
		{
			// Check if UV is already mapped to some intID
			if (mUVToIntID.find(uv) == mUVToIntID.end()) {
				// cout << "UFModule.cpp: \"" << uv << "\" is given the intID " << mNextIntID << endl;

				// Associate UV with new intID
				mUVToIntID[uv] = mNextIntID;
				mIntIDToUV[mNextIntID] = uv;
				mIntIDToPair[mNextIntID] = mUVariablePair;
				return mNextIntID++;
			} else { // UV is already mapped, return ID
				return mUVToIntID[uv];
			}
		}

	template<class Settings>
		int UFModule<Settings>::mapUF(UninterpretedFunction uf) 
		{
			// Check if UF is already mapped
			if (mUFToIntID.find(uf) == mUFToIntID.end()) {
				// cout << "UFModule.cpp: UF \"" << uf << "\" is given the intID " << mNextIntID << endl;

				mUFToIntID[uf] = mNextIntID;
				mIntIDToUF[mNextIntID] = uf;
				mIntIDToPair[mNextIntID] = mUFPair;
				return mNextIntID++;
			} else { // UF is already mapped, return ID
				return mUFToIntID[uf];
			}
		}

	template<class Settings>
		int UFModule<Settings>::mapPair(int x, int y) 
		{
			pair<int, int> p = make_pair(x, y);

			// Check if pair is already mapped
			if (mPairToIntID.find(p) == mPairToIntID.end()) {

				// cout << "UFModule.cpp: Pair <" << x << "," << y << "> is given the intID " << mNextIntID << endl;

				mPairToIntID[p] = mNextIntID;
				mIntIDToPair[mNextIntID] = p;
				return mNextIntID++;
			} else { // pair is already mapped, return ID
				return mPairToIntID[p];
			}
		}

	template<class Settings>
		int UFModule<Settings>::getIdForLhs(const UEquality& ueq) 
		{
			if (ueq.lhsIsUV()) {
				return getId(ueq.lhsAsUV());
			}
			if (ueq.lhsIsUF()) {
				return getId(ueq.lhsAsUF().uninterpretedFunction());
			}
			return -1;
		}

	template<class Settings>
		int UFModule<Settings>::getIdForRhs(const UEquality& ueq) 
		{
			if (ueq.rhsIsUV()) {
				return getId(ueq.rhsAsUV());
			}
			if (ueq.rhsIsUF()) {
				return getId(ueq.rhsAsUF().uninterpretedFunction());
			}
			return -1;
		}

	template<class Settings>
		int UFModule<Settings>::getId(UVariable uv) 
		{
			return mUVToIntID[uv];
		}

	template<class Settings>
		int UFModule<Settings>::getId(UninterpretedFunction uf) 
		{
			return mUFToIntID[uf];
		}

	template<class Settings>
		int UFModule<Settings>::processHandside(UEquality ueq, bool lhs) 
		{

			if (lhs ? ueq.lhsIsUV() : ueq.rhsIsUV()) { // handside is UVariable

				// cout << "UFModule.cpp: " << (lhs ? "lhs" : "rhs") << " is UVariable" << endl;

				const UVariable &uv = (lhs ? ueq.lhsAsUV() : ueq.rhsAsUV());
				return mapUV(uv);

			} else if (lhs ? ueq.lhsIsUF() : ueq.rhsIsUF()) { // handside is UFInstance

				// cout << "UFModule.cpp: " << (lhs ? "lhs" : "rhs") << " is UFInstance" << endl;

				const UFInstance &uf = (lhs ? ueq.lhsAsUF() : ueq.rhsAsUF());
				const vector<UVariable> args = uf.args();

				// Internal identifier of the current intermediate function (starts with function itself, is updated traversing function call)
				int curFunID;
				mapUF(uf.uninterpretedFunction());
				curFunID = mUFToIntID[uf.uninterpretedFunction()];

				// Currying, handling all arguments in args
				for(vector<int>::size_type i = 0; i != args.size(); i++) {
					mapUV(args[i]);
					mapPair(curFunID, mUVToIntID[args[i]]);
					curFunID = mPairToIntID[make_pair(curFunID, mUVToIntID[args[i]])];
				}
				return curFunID;

			} else { // handside is neither UVariable nor UFInstance, don't know if this case exists (at least for our theory)
				return -1;
			}
		}

    template<class Settings>
        void UFModule<Settings>::initMergingForCurried(int x, vector<bool> &alreadyMerged) 
        {
            // cout << "UFModule.cpp: Starting InitMerging for x=" << x << endl;
            int cur = x;
            while (!alreadyMerged[cur] && !intIDIsUVariable(cur) && !intIDIsUF(cur)) {
                pair<int,int> p = mIntIDToPair[cur];
                // cout << "UFModule.cpp: Merging selfmade CurriedEquality (" << p.first << "," << p.second << ")=" << cur << endl;
                proofForest->Merge(CurriedEquality(CurriedEntity(p.first,p.second),CurriedEntity(cur)));
                alreadyMerged[cur] = true;
                cur = p.first;
            }
    	}

	template<class Settings>
		void UFModule<Settings>::buildInfeasibleSubset(pair<int,int> p)
		{
            // cout << "UFModule.cpp: Found conflict, calling Explain()" << endl;
            vector<CurriedEquality> v = proofForest->Explain(p.first,p.second);
            FormulaSetT formulas;
            for (CurriedEquality& eq : v) {
                // cout << "UFModule.cpp: Processing CurriedEquality " << eq << endl;
                int lhs;
                if (eq.lhs.isVariable()) {
                    lhs = eq.lhs.arg1;
                } else {
                    pair<int, int> p1 = make_pair(eq.lhs.arg1, eq.lhs.arg2);
                    pair<int, int> p2 = make_pair(eq.lhs.arg2, eq.lhs.arg1);
                    if (mPairToIntID.find(p1) != mPairToIntID.end()) {
                        lhs = mPairToIntID[p1];
                    } else if (mPairToIntID.find(p2) != mPairToIntID.end()) {
                        lhs = mPairToIntID[p2];
                    } else {
                        assert(false);
                    }
                }
                int rhs;
                if (eq.rhs.isVariable()) {
                    rhs = eq.rhs.arg1;
                } else {
                    pair<int, int> p1 = make_pair(eq.rhs.arg1, eq.rhs.arg2);
                    pair<int, int> p2 = make_pair(eq.rhs.arg2, eq.rhs.arg1);
                    if (mPairToIntID.find(p1) != mPairToIntID.end()) {
                        rhs = mPairToIntID[p1];
                    } else if (mPairToIntID.find(p2) != mPairToIntID.end()) {
                        rhs = mPairToIntID[p2];
                    } else {
                        assert(false);
                    }
                }
                // cout << "UFModule.cpp: lhs is " << lhs << ", rhs is " << rhs << endl;
                FormulaT formula;
                pair<int, int> p1 = make_pair(lhs,rhs);
                pair<int, int> p2 = make_pair(rhs,lhs);
                if (mPairToPosFormulaT.find(p1) != mPairToPosFormulaT.end()) {
                    formula = mPairToPosFormulaT[p1];
                } else if (mPairToPosFormulaT.find(p2) != mPairToPosFormulaT.end()) {
                    formula = mPairToPosFormulaT[p2];
                } else {
                    // cout << "UFModule.cpp: Continuing..." << endl;
                    continue;
                }
                // cout << "UFModule.cpp: Adding formula " << formula << " to infeasible subset" << endl;
                formulas.insert(formula);
            }
            // cout << "UFModule.cpp: Adding fomula " << constraint << " to infeasible subset" << endl;
            formulas.insert(mPairToNegFormulaT[p]);
            mInfeasibleSubsets.push_back(formulas);
		}

	template<class Settings>
		bool UFModule<Settings>::checkIsThereConflict()
		{
            for (auto it = rReceivedFormula().begin(); it != rReceivedFormula().end(); ++it) {
                FormulaT constraint = (*it).formula();
                if (constraint.getType() == carl::UEQ && constraint.uequality().negated()) {
                    pair<int, int> p = mNegFormulaTToPair[constraint];
                    if (proofForest->Find(p.first) == proofForest->Find(p.second)) {
                        buildInfeasibleSubset(p);
                        return true;
                    }
                }
            }
			return false;
		}

	template<class Settings>
		void UFModule<Settings>::createNewProofForest(size_t capacity)
		{
			if (proofForest != NULL)
			{
				delete proofForest;
			}
			proofForest = new ufmodule::ProofForest(capacity);
		}

	template<class Settings>
		bool UFModule<Settings>::informCore( const FormulaT& _constraint )
		{
			// cout << "UFModule.cpp:   --- INFORM_CORE --- " << endl;

			if (_constraint.getType() == carl::UEQ) {

				const UEquality &ueq = _constraint.uequality();
				const UEquality::Arg &lhsArg = ueq.lhs();
				const UEquality::Arg &rhsArg = ueq.rhs();

				// cout << "UFModule.cpp: Found UEQ" << endl;
				// cout << "UFMOdule.cpp: UEQ is \"" << lhsArg << (ueq.negated() ? " != " : " = ") << rhsArg << "\"" << endl;

				// Copied this check from existing module, seems sane
				if (ueq.negated()) {
					if(lhsArg == rhsArg) {
						return false;
					}
				}

				int lhsIntID = processHandside(ueq, true);
				int rhsIntID = processHandside(ueq, false);

				if (!ueq.negated()) {
					// cout << "UFModule.cpp: Associating pos constraint " << _constraint << " with (" << lhsIntID << "," << rhsIntID << ")" << endl; 

					pair<int, int> p = make_pair(lhsIntID, rhsIntID);
					mPosFormulaTToPair[_constraint] = p;
					mPairToPosFormulaT[p] = _constraint;
				} else {
					// cout << "UFModule.cpp: Associating neg constraint " << _constraint << " with (" << lhsIntID << "," << rhsIntID << ")" << endl; 

					pair<int, int> p = make_pair(lhsIntID, rhsIntID);
					mNegFormulaTToPair[_constraint] = p;
					mPairToNegFormulaT[p] = _constraint;
                }
			}

			// Your code.
			return true; // This should be adapted according to your implementation.
		}

	template<class Settings>
		void UFModule<Settings>::init()
		{
			// cout << "UFModule.cpp:   --- INIT --- " << endl;
			createNewProofForest(mNextIntID);
            // Do all the merges resulting from currying
            vector<bool> alreadyMerged = vector<bool>(mNextIntID);
            for (int i = 0; i < mNextIntID; i++) {
                initMergingForCurried(i, alreadyMerged);
            }
		}

	template<class Settings>
		bool UFModule<Settings>::addCore( ModuleInput::const_iterator _subformula )
		{
			// cout << "UFModule.cpp:   --- ADD_CORE --- " << endl;
			FormulaT constraint = (*_subformula).formula();
            // cout << "UFModule.cpp: Starting Merge process for constraint " << constraint << endl;
            if (constraint.getType() == carl::UEQ) {
                const UEquality &ueq = constraint.uequality();
                if (!ueq.negated()) { // Only add constraint per Merge if constraint is positive equality
                    pair<int,int> p = mPosFormulaTToPair[constraint];
                    proofForest->Merge(CurriedEquality(p.first,p.second));
                } else { // If cosntraint is negative, check if it is valid in current ProofForest
                    pair<int,int> p = mNegFormulaTToPair[constraint];
                    if (proofForest->Find(p.first) == proofForest->Find(p.second)) {
                        buildInfeasibleSubset(p);
                        return false;
                    }
                }
            }
            return true;
		}

	template<class Settings>
		void UFModule<Settings>::removeCore( ModuleInput::const_iterator _subformula )
		{
			// cout << "UFModule.cpp:   --- REMOVE_CORE --- " << endl;
            FormulaT constraint = (*_subformula).formula();
            // cout << "UFModule.cpp: Starting Split process for constraint " << constraint << endl;
            if (constraint.getType() == carl::UEQ) {
                const UEquality &ueq = constraint.uequality();
                if (!ueq.negated()) { // Only split by constraint per Split if constraint is positive equality
                    pair<int,int> p = mPosFormulaTToPair[constraint];
                    proofForest->Split(CurriedEquality(p.first,p.second));
                }
            }
		}

	template<class Settings>
		void UFModule<Settings>::updateModel() const
		{
			// cout << "UFModule.cpp:   --- UPDATE_MODEL --- " << endl;
			mModel.clear();
			if( solverState() == Answer::SAT )
			{
				// Your code.
			}
		}

	template<class Settings>
		Answer UFModule<Settings>::checkCore()
		{
			// cout << "UFModule.cpp:   --- CHECK_CORE --- " << endl;
            return checkIsThereConflict() ? Answer::UNSAT : Answer::SAT;
		}
}

#include "Instantiation.h"
