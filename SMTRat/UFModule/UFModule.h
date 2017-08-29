/**
 * @file UFModule.h
 * @author Marco Kabelitz <marco.kabelitz@rwth-aachen.de> 
 *
 * @version 2016-04-28
 * Created on 2016-04-28.
 */

#pragma once

#include "../../solver/Module.h"
#include "UFStatistics.h"
#include "UFSettings.h"
#include "datastructures/ProofForest/ProofForest.h"

namespace smtrat
{
	template<typename Settings>
	class UFModule : public Module
	{
		private:
#ifdef SMTRAT_DEVOPTION_Statistics
			UFStatistics mStatistics;
#endif
			// Members.

            // Copied this from EQModule.h
            typedef carl::UEquality::Arg term_type;
            typedef carl::UEquality UEquality;
            typedef carl::UVariable UVariable;
            typedef carl::UninterpretedFunction UninterpretedFunction;
            typedef carl::UFInstance UFInstance;
            typedef carl::Sort Sort;

            // Counter for internal identifiers
            int mNextIntID = 0;

            // Constant pairs representing UVariables and Uninterpreted Functions in maps
            const std::pair<int, int> mUVariablePair = std::make_pair(-1,-1);
            const std::pair<int, int> mUFPair = std::make_pair(-2,-2);

            // Mappings between substituting internal identifier and substituted pair of internal identifiers
            std::unordered_map<int, pair<int, int>> mIntIDToPair; // TODO change to vector
            std::unordered_map<pair<int, int>, int, ufmodule::pairhash<int, int>> mPairToIntID; 

            // Mappings between UVariables and internal identifiers
            std::unordered_map<UVariable, int> mUVToIntID;
            std::unordered_map<int, UVariable> mIntIDToUV; // TODO change to vector

            // Mappings between functions and internal identifiers
            std::unordered_map<int, UninterpretedFunction> mIntIDToUF; // TODO change to vector
            std::unordered_map<UninterpretedFunction, int> mUFToIntID;

            // Mappings between pairs of internal identifiers and identifiers of the corresponding positive FormulaT
            std::map<FormulaT, pair<int, int>> mPosFormulaTToPair;
            std::unordered_map<pair<int, int>, FormulaT, ufmodule::pairhash<int, int>> mPairToPosFormulaT;
 
            // Mappings between pairs of internal identifiers and identifiers of the corresponding negative FormulaT
            std::map<FormulaT, pair<int, int>> mNegFormulaTToPair;
            std::unordered_map<pair<int, int>, FormulaT, ufmodule::pairhash<int, int>> mPairToNegFormulaT;

            // ProofForest is initialized in init()
            ufmodule::ProofForest* proofForest;
                        
            // Private function processing a handside of an UEquality during Currying (will be called with both true and false)
            int processHandside(UEquality ueq, bool lhs);

            // Other helper functions
            bool intIDIsUVariable(int x);
            bool intIDIsUF(int x);
            int mapUV(UVariable uv);
            int mapUF(UninterpretedFunction uf);
            int mapPair(int x,int y);
            void initMergingForCurried(int x, vector<bool> &alreadyMerged);
            void buildInfeasibleSubset(std::pair<int,int> p);

            bool checkIsThereConflict();
            int getIdForLhs(const UEquality& ueq);
            int getIdForRhs(const UEquality& ueq);
            int getId(UVariable uv);
            int getId(UninterpretedFunction uf);

            void createNewProofForest(size_t capacity);

		public:
			typedef Settings SettingsType;
			std::string moduleName() const {
				return SettingsType::moduleName;
			}
			UFModule(const ModuleInput* _formula, RuntimeSettings* _settings, Conditionals& _conditionals, Manager* _manager = nullptr);

			~UFModule();
			
			// Main interfaces.
			/**
			 * Informs the module about the given constraint. It should be tried to inform this
			 * module about any constraint it could receive eventually before assertSubformula
			 * is called (preferably for the first time, but at least before adding a formula
			 * containing that constraint).
			 * @param _constraint The constraint to inform about.
			 * @return false, if it can be easily decided whether the given constraint is inconsistent;
			 *		  true, otherwise.
			 */
			bool informCore( const FormulaT& _constraint );

			/**
			 * Informs all backends about the so far encountered constraints, which have not yet been communicated.
			 * This method must not and will not be called more than once and only before the first runBackends call.
			 */
			void init();

			/**
			 * The module has to take the given sub-formula of the received formula into account.
			 *
			 * @param _subformula The sub-formula to take additionally into account.
			 * @return false, if it can be easily decided that this sub-formula causes a conflict with
			 *		  the already considered sub-formulas;
			 *		  true, otherwise.
			 */
			bool addCore( ModuleInput::const_iterator _subformula );

			/**
			 * Removes the subformula of the received formula at the given position to the considered ones of this module.
			 * Note that this includes every stored calculation which depended on this subformula, but should keep the other
			 * stored calculation, if possible, untouched.
			 *
			 * @param _subformula The position of the subformula to remove.
			 */
			void removeCore( ModuleInput::const_iterator _subformula );

			/**
			 * Updates the current assignment into the model.
			 * Note, that this is a unique but possibly symbolic assignment maybe containing newly introduced variables.
			 */
			void updateModel() const;

			/**
			 * Checks the received formula for consistency.
			 * @param _full false, if this module should avoid too expensive procedures and rather return unknown instead.
                         * @param _minimize true, if the module should find an assignment minimizing its objective variable; otherwise any assignment is good.
			 * @return True,	if the received formula is satisfiable;
			 *		 False,   if the received formula is not satisfiable;
			 *		 Unknown, otherwise.
			 */
			Answer checkCore();
	};
}
