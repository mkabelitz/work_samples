package de.uni_luebeck.isp.rltlconv.automata

import algorithms.Minimization
import automata.FiniteAutomaton
import automata.FAState
import java.util.HashMap
import scala.collection.JavaConversions._
import scala.collection.generic.MutableMapFactory
import de.uni_luebeck.isp.buchi._
import scala.collection.immutable.HashSet

/**
  * Represents (2)NBAs.
  * 
  * @constructor creates a new NBA with an alphabet, states, start states, transitions and accepting states.
  * @param alphabet the alphabet of the NBA
  * @param states a list of the states of the NBA
  * @param start a list of states containing the initial states of the NBA
  * @param transitions maps an actual states and an element of the alphabet onto a list with the next states
  * @param accepting a list of states containing the accepting states of the NBA
  */
case class Nba(alphabet: Set[String],
  states: List[State],
  start: List[State],
  transitions: Map[(State, Sign), List[DirectedState]],
  accepting: List[State]) extends Automaton {
  
  /**
    * Returns a new Nba where all used States appear
    * in the states list.
    *
    * To achieve this goal the members start, transitions and accepting
    * are scanned and every appearing State is tried to find
    * in the states list by comparing the names. If it's not found,
    * an exception is thrown.
    *
    * Simultaneously for every symbol it is checked if the symbol is
    * element of the alphabet. If it's not found an exception is thrown.
    *
    * @return a new Nba with same states list but modified start,
    *          transactions and accepting member.
    */
  def syncStatesByName: Nba = {
    def findState(name: String) = {
      this.states.find(state => state.name == name) match {
        case Some(state) => state
        case None => throw StateNotFoundException("State with the name \"%s\" not found." format name)
      }
    }

    val start = this.start.map {
      case state => findState(state.name)
    }

    val accepting = this.accepting.map {
      case state => findState(state.name)
    }

    val transitions = this.transitions.collect {
      // collect is intended -- transitions to empty lists are removed
      case ((state, symbol), list) if !list.isEmpty => {
        symbol match {
          case Text(value) if !alphabet.contains(value) => throw new SignNotFoundException("Symbol with the value \"%s\" not found in the alphabet." format value)
          case _ => // do nothing, everything is fine
        }
        ((findState(state.name), symbol), list.map {
          case DirectedState(direction, state) => DirectedState(direction, findState(state.name))
        })
      }
    }

    Nba(this.alphabet, this.states, start, transitions, accepting)
  }
  
  /**
    * @return true if the NBA is a 2NBA else false.
    */
  def isTwoWay: Boolean = {
    transitions.exists {
      case ((_, sign), list) if sign != Epsilon => list.exists {
        case DirectedState(direction, _) => direction != Forward
      }
      case _ => false
    }
  }
  
  /**
    * @return true if the NBA is an epsilon-NBA else false.
    */
  def usesEpsilon: Boolean = {
    transitions.exists {
      case ((_, Epsilon), _) => true
      case _ => false
    }
  }
  
  /**
    * Converts the NBA into dot format.
    * 
    * @return the NBA in dot format.
    */
  def toDot(aDot: ADotOption = SingleBool): String = toNamedNba.toDotHandler

  private def toDotHandler: String = {
    val twoWay = isTwoWay
    
    val transLabel = if (twoWay) {
      (symbol: Sign, direction: Direction) =>
        symbol.toString + " / " + direction.toNumber
    } else {
      (symbol: Sign, direction: Direction) =>
        symbol.toString
    }
    
    def nodeShape(state: State): String = {
      state match {
        case state if accepting contains state => "doublecircle"
        case _ => "circle"
      }
    }

    "digraph G {\n" +
      "  rankdir=LR;\n" +
      states.map {
        state => "  " + state + " [shape=" + nodeShape(state) + ", margin=0];\n"
      }.mkString("") +
      start.view.zipWithIndex.map {
        case (state, index) => "  start" + index + " [shape=none, style=invis];\n" +
          "  start" + index + " -> " + state + " [label=\"START\"];\n"
      }.mkString("") +
      transitions.map {
        case ((from, symbol), list) => list.map {
          case DirectedState(direction, to) => "  " + from + " -> " + to + " [label=\"" + transLabel(symbol, direction).replaceAll("\"", "\\\\\"") + "\"];\n"
        }.mkString("")
      }.mkString("") +
      "}"
  }
    
  /**
    * Converts the NBA into AFF format.
    * 
    * @return the NBA in AFF format.
    */
  override def toString: String = toNamedNba.toStringHandler

  private def toStringHandler: String = {
    def acceptingState(state: State): String = state match {
      case state if accepting.contains(state) => state + ": ACCEPTING"
      case state => state.toString()
    }
    
    val twoWay = isTwoWay

    val statesToString = if (twoWay)
      (term: List[DirectedState]) => "[" + term.map {
        ds => ds.toString
      }.mkString(", ") + "]"
    else
      (term: List[DirectedState]) => "[" + term.map {
        ds => ds.state.toString
      }.mkString(", ") + "]"
    
    val typeName = (if (isTwoWay) "2" else "") + "NBA"
    
    typeName + " {\n" +
      "  ALPHABET = [" + alphabet.map("\"" + _ + "\"").mkString(", ") + "]\n" +
      "  STATES = [" + states.map(acceptingState(_)).mkString(", ") + "]\n" +
      "  START = [" + start.mkString(", ") + "]\n" +
      transitions.map {
        case ((state, symbol), list) => "  DELTA(" + state + ", " + symbol + ") = " + statesToString(list) + "\n"
      }.mkString("") +
      "}"
  }
  
  private[automata] def toNamedNba: Nba = toNamedNba(false)
  
  private[automata] def toNamedNba(forceNewNames: Boolean): Nba = {
    def newName(names: Set[String]) = {
      def compute(i: Int): String = {
        if (names contains "q" + i.toString) {
          compute(i+1)
        } else {
          "q" + i.toString
        }
      }
      compute(0)
    }
    
    def computeStatesMap(states: List[State], map: Map[State, String]): Map[State, State] = {
      if (states.isEmpty) {
        map.map{ case (k,v) => (k,State(v)) }
      } else {
        val state = states.head
        val name = if (state.name == "" || map.exists{ case (k,v) => k != state && v == state.name }) {
          newName(map.values.toSet)
        } else {
          state.name
        }
        computeStatesMap(states.tail, map + (state -> name))
      }
    }
    
    val map = if (forceNewNames) {
      Map(this.states.zipWithIndex.map{ case (state, index) => (state -> State("q" + index)) }: _*)
    } else {
      computeStatesMap(this.states, Map(this.states.map(s => (s -> s.name)): _*))
    }

    val states = map.values.toList
    val start = this.start.map {
      state => map(state)
    }
    val accepting = this.accepting.map {
      state => map(state)
    }
    val transitions = this.transitions.map {
      case ((state, symbol), list) => ((map(state), symbol), list.map(ds =>
        map(ds.state) :: ds.direction))
    }

    Nba(this.alphabet, states, start, transitions, accepting)
  }
  
  /**
    * Converts the NBA into an NFA.
    * 
    * @return the NBA with all states marked as accepting interpreted as an NFA.
    */
  def toNfa = Nfa(this)
  
  /**
    * Minimizes the NBA into an minimal NBA.
    * 
    * @return the minimized NBA
    */  
  def toMinimizedNba: Nba = {
    if (this.isTwoWay) {
      throw new RuntimeException("Minimization does not (yet) support two-way automata.")
    }
    if (this.usesEpsilon) {
      throw new RuntimeException("Minimization does not (yet) support epsilon transitions.")
    }
    val fa = toFiniteAutomaton
    val x: Minimization = new Minimization
    val fa2: FiniteAutomaton = x.Minimize_Buchi(fa, 12)
    Nba(fa2, this.alphabet)
  }
  
  private def toFiniteAutomaton = {
    // Create new result automaton
    val fa = new FiniteAutomaton()
    // Create new initial state
    val start = fa.createState()
    fa.setInitialState(start)
    // Create all states
    val map = Map(this.states.map(s => (s,fa.createState())):_*)
    // Set accepting states
    this.accepting.foreach{ s =>
      fa.F.add(map(s))
    }
    // Create transitions
    this.transitions.foreach{
      case ((actual,sym),next) => {
        val from = map(actual)
        val to = next.map(ds => map(ds.state))
        sym match {
          case Text(s) => to.foreach{ next =>
            fa.addTransition(from, next, s)
            if (this.start.contains(actual)) {
              fa.addTransition(start, next, s)
            }
          }
          case All => {
            this.alphabet.foreach{
              s => to.foreach{next =>  
                fa.addTransition(from, next, s)
                if (this.start.contains(actual)) {
                  fa.addTransition(start, next, s)
                }
              }
            }
          }
          case Epsilon => throw new RuntimeException("Epsilon transitions are not (yet) supported")
        }
      }
    }
    fa
  }
  
  /**
    * Converts the NBA into a DFA.
    * 
    * @return the DFA that results from the transformation of the result of toNfa into a DFA.
    */
  def toDeterministicNfa = toNfa.toDeterministicNfa
  
  /**
   	*	Converts a 2NBA into a 1ABA.
   	* 
   	* @return the 1ABA that results from the transformation of the 2NBA.
   	*/
  def twoWayToAba : Apa = {
    
    // ------------------------------------------
    // build alphabet
    // ------------------------------------------
  
    // alphabet can simply be copied from 2NBA
    val alphabet = this.alphabet
    
    // ------------------------------------------
    // build states
    // ------------------------------------------
    
    // build a list of signs from the alphabet
    val signs = this.alphabet.map((s : String) => new Text(s)).toList
    
    // a function finding all reachable states by dfs
    def dfsForReachableStates(states : List[State], startStates : List[State], transitions : Map[(State, Sign), List[DirectedState]], signs : List[Sign]) : List[State] = {
      def helper(toVisit : List[State], visited : HashSet[State]) : List[State] = {
        if (toVisit.isEmpty) {
          visited.toList
        } else {
          val s = toVisit.head
          if (!visited.contains(s)) {
            val nextStates = signs.filter((a : Sign) => transitions.keySet.exists(_ == (s,a))).flatMap((a : Sign) => {
              transitions.get(s,a).get.map((ds : DirectedState) => ds.state)
            })
            helper(toVisit.tail ::: nextStates, visited + s)
          } else {
            helper(toVisit.tail, visited)
          }
        }
      }
      helper(startStates, new HashSet)      
    }
    
    // holds all reachable states
    val reachableStates = dfsForReachableStates(this.states, this.start, this.transitions, signs)
    
    // we need a function for building a cartesian product
    // source: http://stackoverflow.com/questions/8217764/cartesian-product-of-two-lists
    def cartesianProduct[T](xss: List[List[T]]) : List[List[T]] = xss match {
      case Nil => List(Nil)
      case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
    }
  
    // holds all state names of the 2NBA
    val nbaStateStrings = reachableStates.map((state : State) => state.name)
    
    // holds the cartesian product: (2NBA state names x 2NBA state names)
    val pairStateStringsCross = cartesianProduct(List(nbaStateStrings, nbaStateStrings))

    // build all singleton and pair states with bottom and top
    val singletonStatesBottom = nbaStateStrings.map((s : String) => s + "_bottom").map(s => new State(s))
    val singletonStatesTop = nbaStateStrings.map((s : String) => s + "_top").map(s => new State(s))
    val pairStatesBottom = pairStateStringsCross.map((l : List[String]) => l(0) + "_" + l(1) + "_bottom").map(s => new State(s))
    val pairStatesTop = pairStateStringsCross.map((l : List[String]) => l(0) + "_" + l(1) + "_top").map(s => new State(s))
    
    // holds all singleton states
    val singletonStates = singletonStatesBottom ::: singletonStatesTop
    
    // holds all pair states
    val pairStates = pairStatesBottom ::: pairStatesTop 
  
    // holds whole state set
    val states = singletonStates ::: pairStates
    
    // build a mapping from a string to the corresponding state
    val stringToStateMap = states.map((state : State) => state.name -> state).toMap
  
    // ------------------------------------------
    // build start states
    // ------------------------------------------
  
    // all start states
    // (this is a list of possibly multiple start states now, while Vardi only has a single start state)
    val startStates = this.start.map((state : State) => stringToStateMap.get(state.name + "_bottom").get)
    
    // start states as positive boolean formula
    val start = BoolOr(startStates.map((state : State) => BoolElement(state)))
    
    // ------------------------------------------
    // build colors
    // ------------------------------------------
    
    // all bottom singleton states corresponding to the accepting set of the 2NBA
    val singletonStatesBottomAndAcceptingInNba = this.accepting.map((state : State) => stringToStateMap.get(state.name + "_bottom").get)
  
    // all accepting states
    val accepting = singletonStatesTop ::: singletonStatesBottomAndAcceptingInNba
  
    // the coloring of the APA
    val colors = states.map((state : State) => state match {
      case state if accepting.contains(state) => (state -> 2)
      case state => (state -> 1)
    }).toMap
    
    // ------------------------------------------
    // build singleton state sequences
    // ------------------------------------------
    
    // combine every sign with every state of the 2NBA
    val singletonStateSignCross = cartesianProduct(List(reachableStates,signs)).map((l : List[Object]) => (l(0).asInstanceOf[State],l(1).asInstanceOf[Sign]))
  
    // a function finding all the R(t,a) sequences recursively
    def allSingletonSeqFinder(t : State, a : Sign) : List[List[State]] = {
      def helper(path : List[State], possibleOnOddPosition : List[State], possibleOnEvenPosition : List[State], level : Int, t : State, a : Sign) : List[List[State]] = {
        if (level == 0) {
          val t0Candidates = possibleOnEvenPosition.filter(s => this.transitions.keySet.exists(_ == (t,a)) && this.transitions.get((t,a)).get.contains(DirectedState(Forward,s)))
          if (t0Candidates.isEmpty) {
            List(path.reverse)
          } else {
            t0Candidates.flatMap((t0 : State) => {
              val t0Position = possibleOnEvenPosition.indexOf(t0)
              helper(List(t0), new State("dummy for non maximum length sequences") :: possibleOnOddPosition, 
                  possibleOnEvenPosition.dropRight(possibleOnEvenPosition.length - t0Position) ::: possibleOnEvenPosition.drop(t0Position+1), 1, t, a)
            })
          }
        } else if (level % 2 == 1) {
          val sCandidates = possibleOnOddPosition
          if (sCandidates.isEmpty) {
            List(path.reverse)
          } else {
            sCandidates.flatMap((s : State) => {
              val sPosition = possibleOnOddPosition.indexOf(s)
              helper(s :: path, possibleOnOddPosition.dropRight(possibleOnOddPosition.length - sPosition) ::: possibleOnOddPosition.drop(sPosition+1),
                new State("dummy for non maximum length sequences") :: possibleOnEvenPosition, level + 1, t, a)
            })
          }
        } else {
          val tCandidates = possibleOnEvenPosition.filter(s => this.transitions.keySet.exists(_ == (path(0),a)) 
              && this.transitions.get((path(0),a)).get.contains(DirectedState(Forward,s)))
          if (tCandidates.isEmpty) {
            List(path.tail.reverse)
          } else {
            tCandidates.flatMap((t : State) => {
              val tPosition = possibleOnEvenPosition.indexOf(t)
              helper(t :: path, new State("dummy for non maximum length sequences") :: possibleOnOddPosition,
                possibleOnEvenPosition.dropRight(possibleOnEvenPosition.length - tPosition) ::: possibleOnEvenPosition.drop(tPosition+1), level + 1, t, a)
            })
          }
        }
      }
      helper(Nil, reachableStates, reachableStates, 0, t, a)
    }
    
    // build the R(t,a) described by Vardi
    val rtaMap = singletonStateSignCross.map((ta : (State, Sign)) => ta -> allSingletonSeqFinder(ta._1, ta._2).toSet.toList).toMap
    
    // decides whether a list contains a certain element at an odd position
    def containsOnOddPosition[T](l : List[T], x : T) : Boolean = {
      def helper(l : List[T], x : T, length : Int, level : Int) : Boolean = (l, x, length, level) match {
        case _ if level == length => false
        case _ if level % 2 == 1 && l(level) == x => true
        case _ => helper(l, x, length, level + 1)
      }
      helper(l, x, l.length, 0)
    }
    
    // a function finding all the L(t,a) loops
    def allSingletonLoopFinder(t : State, a : Sign) : List[(List[State], List[State])] = {
      def helper(seq : List[State]) : List[(List[State], List[State])] = {
        val sOneKPlusOneCandidates = reachableStates.filter((s : State) => !containsOnOddPosition(seq,s))
        val rawLoops = sOneKPlusOneCandidates.map((s : State) => {
          (seq :+ s) -> rtaMap.get(s,a).get.filter((l : List[State]) => !containsOnOddPosition(l,s)).map(l => l :+ s)
        })
        rawLoops.flatMap((tuple : (List[State], List[List[State]])) => {
          tuple._2.map((l : List[State]) => tuple._1 -> l)
        }).filter((tuple : (List[State], List[State])) => tuple._1.length % 2 == 0 && tuple._2.length % 2 == 0)
      }
      val sequencesSoFar = rtaMap.get((t,a)).get
      sequencesSoFar.flatMap((seq : List[State]) => helper(seq))
    }
    
    // build the L(t,a) described by Vardi
    val ltaMap = singletonStateSignCross.map((ta : (State, Sign)) => ta -> allSingletonLoopFinder(ta._1, ta._2)).toMap
    
    // ------------------------------------------
    // build pair state sequences
    // ------------------------------------------
    
    // combine every sign with every state pair of the 2NBA
    val pairStateSignCross = cartesianProduct(List(reachableStates,reachableStates,signs)).map((x : List[Object]) => 
      ((x(0).asInstanceOf[State],x(1).asInstanceOf[State]),x(2).asInstanceOf[Sign]))
  
    // a function finding all the R((t,s),a) sequences
    def allPairSeqFinder(t : State, s : State, a : Sign) : List[List[State]] = {
      def helper(seq : List[State]) : List[List[State]] = {
        val sOneKPlusOneCandidates = reachableStates.filter((x : State) => {
          !seq.isEmpty && !containsOnOddPosition(seq,x) && this.transitions.keySet.contains(x,a) && this.transitions.get(x,a).get.contains(DirectedState(Back,s))
        })
        sOneKPlusOneCandidates.map((x : State) => (x :: seq.reverse).reverse)
      }
      val sequencesSoFar = rtaMap.get(t,a).get
      sequencesSoFar.flatMap((seq : List[State]) => helper(seq))
    }
  
    // build the R((t,s),a) described by Vardi
    val rtsaMap = pairStateSignCross.map((tsa : ((State, State), Sign)) => tsa -> allPairSeqFinder(tsa._1._1, tsa._1._2, tsa._2)).toMap
    
    // ------------------------------------------
    // build singleton state alpha sequences
    // ------------------------------------------
    
    // a function building all alpha sequences for a specific R(t,a) sequence recursively
    def allSingletonSeqAlphaFinder(seq : List[State]) : List[List[State]] = {
      def helper(path : List[State], seq : List[State], containsTop : Boolean) : List[List[State]] = {
        if (seq.length == 1) {
          if (containsTop) {
            val nextElementTop = stringToStateMap.get(seq(0).name + "_top").get
            List((nextElementTop :: path).reverse)
          } else {
            val nextElementBottom = stringToStateMap.get(seq(0).name + "_bottom").get
            List((nextElementBottom :: path).reverse)
          }
        } else if (containsTop) {
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          helper(nextElementBottom :: path, seq.tail.tail, true)        
        } else {
          val nextElementTop = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_top").get
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          List(helper(nextElementTop :: path, seq.tail.tail, true), helper(nextElementBottom :: path, seq.tail.tail, false)).flatten
        }
      }
      helper(Nil, seq, false)
    }
  
    // build all R(t,a) alpha sequences
    val singletonSeqAlphaMap = singletonStateSignCross.map((ta : (State, Sign)) => {
      ta -> rtaMap.get(ta).get.filter((l : List[State]) => !l.isEmpty).flatMap((l : List[State]) => allSingletonSeqAlphaFinder(l))
    }).toMap
    
    // a function building a bottom only alpha sequence for a specific sequence
    def onlyBottomAlphaSequenceFinder(seq : List[State]) : List[State] = {
      def helper(path : List[State], seq : List[State]) : List[State] = {
        val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
        if (seq.length == 2) {
          (nextElementBottom :: path).reverse
        } else {
          helper(nextElementBottom :: path, seq.tail.tail)
        }
      }
      helper(Nil, seq)
    }
  
    // a function building all alpha sequences for a specific L(t,a) sequence recursively
    def allSingletonLoopAlphaFinder(tuple : (List[State],List[State])) : List[List[State]] = {
      def helper(path : List[State], seq : List[State], containsTop : Boolean) : List[List[State]] = {
        if (seq.length == 2) {
          if (containsTop) {
            val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
            List((nextElementBottom :: path).reverse)
          } else {
            val nextElementTop = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_top").get
            List((nextElementTop :: path).reverse)
          }
        } else if (containsTop) {
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          helper(nextElementBottom :: path, seq.tail.tail, true)        
        } else {
          val nextElementTop = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_top").get
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          List(helper(nextElementTop :: path, seq.tail.tail, true), helper(nextElementBottom :: path, seq.tail.tail, false)).flatten
        }
      }
      val onlyBottomAlphaSequenceOne = onlyBottomAlphaSequenceFinder(tuple._1)
      val allAlphaSequencesTwo = helper(Nil, tuple._2, false)
      allAlphaSequencesTwo.map((l : List[State]) => onlyBottomAlphaSequenceOne ::: l)
    }
  
    // build all L(t,a) alpha sequences
    val singletonLoopAlphaMap = singletonStateSignCross.map((ta : (State, Sign)) => {
      ta -> ltaMap.get(ta).get.flatMap((tuple : (List[State],List[State])) => allSingletonLoopAlphaFinder(tuple))
    }).toMap
    
    // ------------------------------------------
    // build pair state alpha sequences
    // ------------------------------------------
    
    // a function building all alpha sequences for a specific R((t,s),a) sequence recursively
    def allPairSeqAlphaFinder(seq : List[State], s : State, t : State) : List[List[State]] = {
      def helper(path : List[State], seq : List[State], containsTop : Boolean) : List[List[State]] = {
        if (seq.length == 2) {
          if (containsTop) {
            val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
            List((nextElementBottom :: path).reverse)
          } else {
            val nextElementTop = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_top").get
            List((nextElementTop :: path).reverse)
          }
        } else if (containsTop) {
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          helper(nextElementBottom :: path, seq.tail.tail, true)        
        } else {
          val nextElementTop = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_top").get
          val nextElementBottom = stringToStateMap.get(seq(0).name + "_" + seq(1).name + "_bottom").get
          List(helper(nextElementTop :: path, seq.tail.tail, true), helper(nextElementBottom :: path, seq.tail.tail, false)).flatten
        }
      }
      if (!this.accepting.contains(s) && !this.accepting.contains(t)) {
        helper(Nil, seq, false)
      } else {
        List(onlyBottomAlphaSequenceFinder(seq))
      }
    }
  
    // build all R((t,s),a) alpha sequences
    val pairSeqAlphaMap = pairStateSignCross.map((tsa : ((State,State), Sign)) => {
      tsa -> rtsaMap.get(tsa).get.filter((l : List[State]) => !l.isEmpty).flatMap((l : List[State]) => allPairSeqAlphaFinder(l, tsa._1._2, tsa._1._1))
    }).toMap
      
    // ------------------------------------------
    // build singleton state transitions
    // ------------------------------------------
    
    // holds a map for all singleton state sign pairs to corresponding boolean formulas
    val singletonTransitionsMap = singletonStateSignCross.map((ta : (State,Sign)) => {
     ta -> BoolOr(
         BoolOr(singletonSeqAlphaMap.get(ta).get.map((l : List[State]) => BoolAnd(l.map((x : State) => BoolElement(DirectedState(Forward,x)))))),
         BoolOr(singletonLoopAlphaMap.get(ta).get.map((l : List[State]) => BoolAnd(l.map((x : State) => BoolElement(DirectedState(Forward,x)))))))
    }).toMap
    
    // build the singleton state transitions
    val singletonTransitions = singletonStateSignCross.flatMap((ta : (State,Sign)) => {
      List((stringToStateMap.get(ta._1.name + "_top").get,ta._2) -> singletonTransitionsMap.get(ta).get,
          (stringToStateMap.get(ta._1.name + "_bottom").get,ta._2) -> singletonTransitionsMap.get(ta).get)
    })
    
    // ------------------------------------------
    // build pair state transitions
    // ------------------------------------------
    
    // build the pair state transitions
    val pairTransitions = pairStateSignCross.flatMap((tsa : ((State,State),Sign)) => {
      val t = tsa._1._1
      val s = tsa._1._2
      val a = tsa._2
      List(
        (stringToStateMap.get(t.name + "_" + s.name + "_top").get,a) -> {
          if (this.transitions.keySet.contains(t,a) && this.transitions.get(t,a).get.contains(DirectedState(Back,s)) && (this.accepting.contains(s) || this.accepting.contains(t))) {
            BoolTrue
          } else {
            BoolOr(pairSeqAlphaMap.get((t,s),a).get.map({
              (l : List[State]) => BoolAnd(l.map((x : State) => BoolElement(DirectedState(Forward,x))))
            }))
          }
        },
        (stringToStateMap.get(t.name + "_" + s.name + "_bottom").get,a) -> {
          if (this.transitions.keySet.contains(t,a) && this.transitions.get(t,a).get.contains(DirectedState(Back,s))) {
            BoolTrue
          } else {
            BoolOr(rtsaMap.get((t,s),a).get.map((l : List[State]) => onlyBottomAlphaSequenceFinder(l)).map({
              (l : List[State]) => BoolAnd(l.map((x : State) => BoolElement(DirectedState(Forward,x))))
            }))
          }
        }
      )
    })
    
    // ------------------------------------------
    // build ABA
    // ------------------------------------------
    
    new Apa(
      alphabet,
      states,
      start,
      (singletonTransitions ::: pairTransitions).toMap,
      colors
    ).toReducedApa
  }
  
  /**
   	*	Converts a 2NBA into a 1NBA.
   	* 
   	* @return the 1NBA that results from the transformation of the 2NBA.
   	*/  
  def twoWayToOneWay : Nba = {
    this.twoWayToAba.toNba
  }
  
  /**
   	*	Converts an 2NBA into an ABA accepting the complement of the original language. 
   	* (The included transformation ABA -> cABA behaves for most parts like Apa.toComplementAba
   	* but uses only quadratic state blowup instead of biquadratic)
   	*  
   	*  @return the ABA that accepts the complement of the language of the original 2NBA.
   	*/  
  def twoWayToComplementAba : Apa = {
    
    val aba = this.twoWayToAba
    
    // ------------------------------------------
    // build coABA
    // ------------------------------------------
    
    // we need a function for building a cartesian product
    // source: http://stackoverflow.com/questions/8217764/cartesian-product-of-two-lists
    def cartesianProduct[T](xss: List[List[T]]) : List[List[T]] = xss match {
      case Nil => List(Nil)
      case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
    }
    
    // takes a list of two state strings and builds a single string
    def cartProdToString(x : List[List[String]]) = {
      x.map((l : List[String]) => l(0) + "_" + l(1))
    }
    
    // holds a mapping from all coABA state names to the actual coABA state objects
    val coAbaStateMap = aba.states.map((s : State) => s.name -> s).toMap
  
    // holds all state names of the coABA
    val coAbaStateStrings = coAbaStateMap.toList.map((tuple : (String,State)) => tuple._1)
    
    // cartesian product of coAba state names and alphabet
    val coAbaStateSignCross = cartesianProduct(List(coAbaStateStrings, aba.alphabet.toList))

    // function for dualization of a positive boolean formula
    def dualizePosBool(phi : PosBool[DirectedState]) : PosBool[DirectedState] = phi match {
      case phi : BoolOr[DirectedState] => BoolAnd(dualizePosBool(phi._1),dualizePosBool(phi._2))
      case phi : BoolAnd[DirectedState] => BoolOr(dualizePosBool(phi._1),dualizePosBool(phi._2))
      case phi : BoolElement[DirectedState] => phi
      case phi if phi == BoolTrue => BoolFalse
      case phi if phi == BoolFalse => BoolTrue
      case _ => null
    }
    
    // holds the dualized transitions
    val dualizedTransitions = aba.transitions.toList.map((tuple : ((State,Sign),PosBool[DirectedState])) => tuple._1 -> dualizePosBool(tuple._2)).toMap
    
    // holds the coABA transitions
    val coAbaTransitions = coAbaStateSignCross.map((l : List[String]) => {
      val s = coAbaStateMap.get(l(0)).get
      val a = new Text(l(1)).asInstanceOf[Sign]
      if (dualizedTransitions.keySet.exists(_ == (s,All))) {
        (s,a) -> dualizedTransitions.get(s,All).get
      } else if (dualizedTransitions.keySet.exists(_ == (s,a))) {
        (s,a) -> dualizedTransitions.get(s,a).get
      } else {
        (s,a) -> BoolTrue
      }
    }).toSet.toMap
    
    // the coABA for building the cABA
    val coAba = new Apa(
      aba.alphabet,
      aba.states,
      aba.start,
      coAbaTransitions,
      aba.colors
    )
    
    // ------------------------------------------
    // build alphabet
    // ------------------------------------------
  
    // alphabet can simply be copied from coABA
    val alphabet = coAba.alphabet
    
    // ------------------------------------------
    // build states
    // ------------------------------------------
    
    // all coABA singleton state strings
    val coAbaSingletonStateStrings = coAbaStateStrings.filter((s : String) => s.indexOf("_",0) != -1 && s.indexOf("_", s.indexOf("_",0)+1) == -1)
    
    // all other coABA state strings
    val coAbaOtherStateStrings = coAbaStateStrings.filter((s : String) => !coAbaSingletonStateStrings.contains(s))
    
    // number of singleton states of the coABA
    val n = coAbaSingletonStateStrings.length

    // holds all ranks from 0 to 2n inclusively as strings
    val ranks = List.range(0, 2*n+1).map((x : Int) => x.toString)
    
    // build cABA singleton state strings
    val cAbaSingletonStateStrings = cartProdToString(cartesianProduct(List(coAbaSingletonStateStrings, ranks)))
    
    // build cABA singleton states
    val cAbaSingletonStates = cAbaSingletonStateStrings.map((s : String) => new State(s))
    
    // build all other cABA states 
    val cAbaOtherStates = coAbaOtherStateStrings.map((s : String) => coAbaStateMap.get(s).get)
  
    // build cABA states
    val states = cAbaSingletonStates ::: cAbaOtherStates
    
    // holds a mapping from a string to the corresponding state
    val stringToStateMap = (states.map((s : State) => s.name -> s)).toMap
  
    // ------------------------------------------
    // build start states
    // ------------------------------------------

    // function for finding the coABA start states in the positive boolean formula
    def findStartStates(phi : PosBool[State]) : List[State] = phi match {
      case phi : BoolElement[State] => List(phi.value)
      case phi : BoolOr[State] => List(findStartStates(phi._1),findStartStates(phi._2)).flatten
      case phi : BoolAnd[State] => List(findStartStates(phi._1),findStartStates(phi._2)).flatten
      case _ => null
    }
    
    // start states indicated by upper bound on rank 2n
    val start = BoolOr(findStartStates(coAba.start).map((s : State) => s match {
      case s : State if s.name.indexOf("_") != -1 => BoolElement(stringToStateMap.get(s.name + "_" + (2*n)).get)
      case s : State => BoolElement(stringToStateMap.get(s.name).get)
    }))

    // ------------------------------------------
    // build colors
    // ------------------------------------------
    
    // the coloring of the cABA
    val colors = states.map((s : State) => {
      if ((cAbaSingletonStates.contains(s) && s.name.substring(s.name.lastIndexOf("_")+1).toInt % 2 == 1) || (!cAbaSingletonStates.contains(s) && s.name.lastIndexOf("_") != -1)) {
        s -> 2
      } else if (s.name.indexOf("_") == -1) {
        if (coAba.colors.get(s).get == 2) 
          s -> 1
        else 
          s -> 2
      } else {
        s -> 1
      }
    }).toMap

    // ------------------------------------------
    // build transitions
    // ------------------------------------------
    
    // function 'release' described by Vardi
    def release(phi : PosBool[DirectedState], r : List[Int]) : PosBool[DirectedState] = phi match {
      case phi : BoolOr[DirectedState] => BoolOr(release(phi._1,r),release(phi._2,r))
      case phi : BoolAnd[DirectedState] => BoolAnd(release(phi._1,r),release(phi._2,r))
      case phi : BoolElement[DirectedState] => {
        if(phi.value.state.name.indexOf("_",0) != -1 && phi.value.state.name.indexOf("_", phi.value.state.name.indexOf("_",0)+1) == -1) {
        	BoolOr(r.map((i : Int) => BoolElement(DirectedState(phi.value.direction,stringToStateMap.get(phi.value.state.name + "_" + i).get))))
        } else {
          phi
        }
      }
      case phi if phi == BoolTrue || phi == BoolFalse => phi
      case _ => null
    }
    
    // function returning a singleton transition
    def getTransition(s : State, a : Sign) : PosBool[DirectedState] = {
      if (coAba.colors.get(coAbaStateMap.get(s.name.substring(0, s.name.lastIndexOf("_"))).get).get == 1 || s.name.substring(s.name.lastIndexOf("_")+1).toInt % 2 == 0) {
        release(coAba.transitions.get(coAbaStateMap.get(s.name.substring(0, s.name.lastIndexOf("_"))).get,a).get, List.range(0, s.name.substring(s.name.lastIndexOf("_")+1).toInt+1))
      } else {
        BoolFalse
      }
    }
    
    // cartesian product of cABA  singleton states and signs
    val singletonStateSignCross = cartesianProduct(List(cAbaSingletonStateStrings, alphabet.toList)).map((l : List[String]) => (stringToStateMap.get(l(0)).get,new Text(l(1))))

    // build the singleton transitions
    val singletonTransitions = singletonStateSignCross.map((tuple : (State,Sign)) => tuple -> getTransition(tuple._1, tuple._2))
    
    // cartesian product of the other cABA states and signs
    val otherStateSignCross = cartesianProduct(List(coAbaOtherStateStrings, alphabet.toList)).map((l : List[String]) => (stringToStateMap.get(l(0)).get,new Text(l(1))))
    
    // build the singleton transitions
    val otherTransitions = otherStateSignCross.map((tuple : (State,Sign)) => tuple -> coAba.transitions.get(coAbaStateMap.get(tuple._1.name).get, tuple._2).get)
    
    // build transitions
    val transitions = (singletonTransitions ::: otherTransitions).toMap
    
    // ------------------------------------------
    // build cABA
    // ------------------------------------------

    new Apa(
      alphabet,
      states,
      start,
      transitions,
      colors
    ).toReducedApa
  }
  
  /** 
    *	Checks if this NBA is 2NBA.
    * 
    * @return one way version of this NBA.
    */
  def eliminateTwoWay : Nba = {
    if (this.isTwoWay) {
      this.twoWayToAba.toNba
    } else {
      this
    }
  }
}

/** Factory for [[de.uni_luebeck.isp.rltlconv.automata.Nba]] instances. */
object Nba {
  
  /**
    * Creates an NBA from a given APA.
    * 
    * @param unnamedApa the APA that is transformed into a language-equivalent NBA.
    * @return the NBA created from the APA.
    */
  def apply(unnamedApa: Apa): Nba = {
    
    val apa = unnamedApa.toNamedApa
    val colors = apa.colors.values.toSet
    
    if (colors(3) || (colors(0) && colors(1))) {
      // Use translation implemented in BuchiAutomata
      val pa = apa.toParityAutomata
      val ba = de.uni_luebeck.isp.buchi.Transformation.transform(pa)
      apply(ba)
    } else {
      // Compute accepting states from the colors of the Apa
      val accepting: Set[State] = if (!colors(0)) {
          apa.colors.collect{
            case (state, color) if color == 2 => state
          }.toSet
        } else if (!colors(1)) {
          apa.states.toSet
        } else {
          Set()
        }
      Nba(apa, accepting)
    }
  }
  
  /**
    * Creates an NBA from a given Buchi-automaton. This is just a transformation from one data structure into another
    * while both represent the same automaton.
    * 
    * @param ba the Buchi-automaton that is transformed into an NBA.
    * @return the NBA created from the Buchi-automaton.
    */
  def apply(ba: BuchiAutomaton): Nba = {
    val alphabet = ba.getAlphabet().toSet
    val map = Map(ba.getStates().map(s => (s, State())): _*)
    val states = map.values.toList
    val start = ba.getStartStates().map(map(_)).toList
    val accepting = ba.getAcceptingStates().map(map(_)).toList
    val transitions: Map[(State, Sign), List[DirectedState]] = Map(ba.getTransitions().map(t => {
      val state = map(t.getActualState())
      val sign = Text(t.getInput())
      val list = t.getNextState().map(map(_) :: Forward).toList
      ((state, sign), list)
    }): _*)
    Nba(alphabet, states, start, transitions, accepting)
  }
  
  /**
    * Creates an NBA from a given finite automaton. This is just a transformation from one data structure into another
    * while both represent the same automaton. This is needed to use the reduce.jar for minimizing the NBA.
    * 
    * @param fa the finite automaton that is transformed into an NBA.
    * @param alphabet the alphabet for the NBA because the finite automaton has none.
    * @return the NBA created from the finite automaton.
    */
  private def apply(fa: FiniteAutomaton, alphabet: Set[String]): Nba = {
    val statesList = fa.states.toList
    val map = Map(statesList.map(s => (s, State())): _*)
    val states = map.values.toList
    val accepting = fa.F.map(map(_)).toList
    val start = map(fa.getInitialState())
    val transitions = Map(statesList.flatMap(state => alphabet.map(a => {
      val next = state.getNext(a)
      val sign: Sign = Text(a)
      val list: List[DirectedState] = if (next != null) {
        next.toList.map(map(_) :: Forward)
      } else {
        List()
      }
      ((map(state), sign), list)
    })): _*)
    Nba(alphabet, states, List(start), transitions.toMap, accepting)
  }

  private def apply(apa: Apa, accepting: Set[State]): Nba = {
    // Create start states
    val startStatesTemp = apa.start.minimalModels.map(l => (l.toSet,Set[State]()))
    
    implicit class Crossable[X](xs: Traversable[X]) {
      def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }
    
    val transitionsWithDefault = apa.transitions.withDefaultValue(BoolFalse)
    
    // depths first search
    def dfs(state: (Set[State],Set[State]), transitions: Map[((Set[State],Set[State]), Sign), List[(Set[State],Set[State])]], map: Map[(Set[State],Set[State]),(Set[State],Set[State])]):
        (Map[((Set[State],Set[State]), Sign), List[(Set[State],Set[State])]], Map[(Set[State],Set[State]),(Set[State],Set[State])]) = {
      
      def minModels(ss: Set[State], sym: Sign) = BoolAnd(ss.toList.map(s => BoolOr(transitionsWithDefault(s,sym), transitionsWithDefault(s,All)))).minimalModels.map(l => (l.map{
          case DirectedState(Forward,b) => b
          case _ => throw new RuntimeException("Two-way automata are not (yet) supported.")
        }).toSet)
        
      def mergeEQStates(state: (Set[State],Set[State]), transitions: Map[((Set[State],Set[State]), Sign), List[(Set[State],Set[State])]], map: Map[(Set[State],Set[State]),(Set[State],Set[State])]) = {
        val transitionsWithDefault = transitions.withDefaultValue(List((Set(State("junk1")),Set(State("junk2")))))
        val eqState = map.values.toSet.find(s => s != state && state._1.isEmpty == s._1.isEmpty && apa.alphabet.forall{ a =>
          val listOfNextStatesOfS = transitionsWithDefault((s,Text(a)))
          val listOfNextStatesOfState = transitionsWithDefault((state,Text(a)))
          (listOfNextStatesOfS == listOfNextStatesOfState) ||
            ((listOfNextStatesOfS.contains(s) && listOfNextStatesOfState.contains(state)) &&
              ((listOfNextStatesOfS diff List(s)) == (listOfNextStatesOfState diff List(state))))
        })
        eqState match {
          case Some(eqS) => {
            val newMap = map + (state -> map(eqS))
            // eliminate transitions starting in state and redirect incoming transitions to eqs 
            val newTrans = transitions.collect{
              case ((s,sign),next) if s != state => ((s,sign) -> next.map(n => newMap(n)))
            }
            (newTrans, newMap)
          }
          case None => (transitions, map)
        }
      }
      
      if ((state._1 ++ state._2).exists(s => apa.transitions contains ((s, Epsilon)))) {
        throw new RuntimeException("Epsilon transitions are not (yet) supported")
      }
      val newTransitions = Map((apa.alphabet.map(Text(_)): Set[Sign]).toSeq.flatMap ( sym => {
        // Compute all minimal models for all states of the second part of the current state and an input symbol
        val min_2 = minModels(state._2,sym)
        val target = (if (state._1.isEmpty) {
            min_2.map(n_2 => (n_2 -- accepting, n_2.intersect(accepting)))
          } else {
            val min_1 = minModels(state._1,sym)
            min_1.cross(min_2).map { case (n_1, n_2) => {
              val target_1 = n_1 -- accepting
              val target_2 = n_1.intersect(accepting).union(n_2 -- target_1)
              (target_1, target_2)
            } }
          }).toSet
          if (target.nonEmpty) {
            Map((state,sym) -> target.toList.map(s => if (map.contains(s)) map(s) else s))
          } else {
            Map[((Set[State],Set[State]), Sign), List[(Set[State],Set[State])]]()
          }
        }): _*)

      val newStates = newTransitions.values.flatten.toSet -- map.keys
      val tt = transitions ++ newTransitions
        
      // minimize
      val (minTransitions,minMap) = mergeEQStates(state,tt,map)
      //val (minTransitions,minMap) = (tt,map)
      val mm = minMap ++ newStates.map(s => (s -> s))      
        
      newStates.foldLeft((minTransitions, mm)) {
        case ((transitions, map), s) => dfs(s,transitions,map)
      }
 
    }
    
    // Find states and transitions
    val (transitions,map) = startStatesTemp.foldLeft((Map[((Set[State],Set[State]), Sign), List[(Set[State],Set[State])]](),Map[(Set[State],Set[State]),(Set[State],Set[State])]())){
      case ((transitions, map),s) => {
        dfs(s, transitions,map + (s -> s))
      }
    }
    val states = map.values.toSet
    
    // convert states into new states
    val newStatesMap = Map(states.map (s =>  s -> State(s._1.mkString + "x" + s._2.mkString)).toSeq: _*)
    val mappedStates = newStatesMap.values.toList
    val mappedStart = startStatesTemp.map(s => newStatesMap(map(s)))
    val mappedAccepting = states.toList.collect{
      case (s1, s2) if s1.isEmpty => newStatesMap((s1, s2))
    }
    val mappedTransitions = Map(transitions.map {
      case ((from, symbol), to) => (newStatesMap(from),symbol) -> to.map(newStatesMap(_) :: Forward).toList
    }.toSeq: _*)
    
    Nba(apa.alphabet, mappedStates, mappedStart, mappedTransitions, mappedAccepting)
  }
}

/**
  * Represents a pair of NBAs
  * 
  * @constructor creates a new pair of NBAs from two NBAs.
  * @param positive the first NBA.
  * @param negative the second NBA.
  */
case class NbaSpecular(positive: Nba, negative: Nba) extends AutomatonSpecular {
  
  /**
    * Converts the two NBAs into AFF format.
    * 
    * @return a string containing the two NBAs in AFF format.
    */
  override def toString: String =
    positive.toString + "\n\n" + negative.toString

  /**
    * Converts the two NBAs into dot format.
    * 
    * @return a string containing the two NBAs in dot format.
    */  
  def toDot(aDot: ADotOption = SingleBool): String =
    positive.toDot(aDot) + "\n\n" + negative.toDot(aDot)
  
  /**
    * Applies a function to both NBAs.
    * 
    * @param f the function to apply on both NBAs.
    * @return a new NBA specular containing the two transformed NBAs.
    */
  def map(f: Nba => Nba): NbaSpecular =
    NbaSpecular(f(positive), f(negative))
  
  /**
    * Minimizes the two NBAs of the specular.
    * 
    * @return a specular containing the minimized NBAs 
    */
  def toMinimizedNbas = map(_.toMinimizedNba)
    
  /**
    * Converts the NBA specular into an NFA specular by transforming both NBAs.
    * 
    * @return the NFA specular. 
    */
  def toNfas = NfaSpecular(positive.toNfa, negative.toNfa)
  
  /**
    * Converts the NBA specular into a DFA specular by transforming both NBAs.
    * 
    * @return the DFA specular.
    */
  def toDeterministicNfas = toNfas.toDeterministicNfas
  
  /**
    * Converts the two NBA of the specular to a single FSM by transforming them into NFAs and merging these two NFAs.
    * 
    * @return the FSM.
    */
  def toMoore = toNfas.toMoore
}

/** Factory for [[de.uni_luebeck.isp.rltlconv.automata.NbaSpecular]] instances. */
object NbaSpecular {
  
  /**
    * Creates an NBA specular from an APA specular by transforming both APAs into language-equivalent NBAs.
    * 
    * @param the APA specular
    * @return the NBA specular which is created from the APA specular.
    */
  def apply(apas: ApaSpecular): NbaSpecular = {
    NbaSpecular(Nba(apas.positive), Nba(apas.negative))
  }
}