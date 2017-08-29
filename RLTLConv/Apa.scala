package de.uni_luebeck.isp.rltlconv.automata

import java.util.LinkedList
import de.uni_luebeck.isp.rltlconv.formula

/**
  * Represents (2)APAs.
  * 
  * @constructor creates a new APA with an alphabet, states, start states, transitions and a color map.
  * @param alphabet the alphabet of the APA
  * @param states a list of the states of the APA
  * @param start a positive boolean formula describing the initial states of the APA
  * @param transitions maps an actual states and an element of the alphabet onto a positive boolean combination describing the next states
  * @param colors maps a state onto its color.
  */
case class Apa(alphabet: Set[String],
  states: List[State],
  start: PosBool[State],
  transitions: Map[Pair[State, Sign], PosBool[DirectedState]],
  colors: Map[State, Int]) extends Automaton {

  /**
    * Converts the APA into AFF format.
    * 
    * @return the APA in AFF format.
    */
  override def toString(): String = toNamedApa.toStringHandler

  private def toStringHandler: String = {
    val twoWay = isTwoWay

    val termToString = if (twoWay)
      (term: PosBool[DirectedState]) => term.toString
    else
      (term: PosBool[DirectedState]) => term.map { ds: DirectedState => ds.state }.toString

    val typeName = if (twoWay) "2APA" else "APA"

    typeName + " {\n" +
      "  ALPHABET = [" + alphabet.map("\"" + _ + "\"").mkString(", ") + "]\n" +
      "  STATES = [" + states.map { state => state.toString + ":" + colors(state) }.mkString(", ") + "]\n" +
      "  START = " + start.toString + "\n" +
      transitions.map {
        case ((state, symbol), term) => "  DELTA(" + state + ", " + symbol + ") = " + termToString(term) + "\n"
      }.mkString("") +
      "}"
  }

  /**
    * Returns a new Apa class where all used State instances appear
    * in the states list.
    *
    * To achieve this goal the members start, transitions and accepting
    * are scanned and every appearing State instance is tried to find
    * in the states list by comparing the names. If it's not found,
    * an exception is thrown.
    *
    * Simultaneously for every symbol it is checked if the symbol is
    * element of the alphabet. If it's not found an exception is thrown.
    *
    * @return a new Apa with same states list but modified start,
    * transactions and accepting member.
    */
  def syncStatesByName: Apa = {
    def findState(name: String) = {
      this.states.find(state => state.name == name) match {
        case Some(state) => state
        case None => throw StateNotFoundException("State with the name \"%s\" not found." format name)
      }
    }

    def handleTerm(term: PosBool[DirectedState]): PosBool[DirectedState] = term match {
      case BoolElement(DirectedState(direction, state)) => BoolElement(DirectedState(direction, findState(state.name)))
      case BoolAnd(_1, _2) => BoolAnd(handleTerm(_1), handleTerm(_2))
      case BoolOr(_1, _2) => BoolOr(handleTerm(_1), handleTerm(_2))
      case x => x
    }

    val start = handleTerm(this.start.map { state: State => state :: Forward })

    val colors = this.colors.map {
      case (state, color) => (findState(state.name), color)
    }

    val transitions = this.transitions.map {
      case ((state, symbol), term) => {
        symbol match {
          case Text(value) if !alphabet.contains(value) => SignNotFoundException("Symbol with the value \"%s\" not found in the alphabet." format value)
          case _ => // do nothing, everything is fine
        }
        ((findState(state.name), symbol), handleTerm(term))
      }
    }

    Apa(this.alphabet, this.states, start.map { ds: DirectedState => ds.state }, transitions, colors)
  }

  /**
    * Looks for a transition that has not the direction "Forward".
    * 
    * @return true if the APA is a 2APA, else false.
    */
  def isTwoWay: Boolean = {
    def handleTerm(term: PosBool[DirectedState]): Boolean = term match {
      case BoolAnd(_1, _2) => handleTerm(_1) | handleTerm(_2)
      case BoolOr(_1, _2) => handleTerm(_1) | handleTerm(_2)
      case BoolElement(DirectedState(Forward, _)) => false
      case BoolElement(DirectedState(_, _)) => true
      case _ => false
    }

    transitions.exists {
      case ((_, sign), term) if sign != Epsilon => handleTerm(term)
      case _ => false
    }
  }

  /**
   * Looks if there exists an epsilon transition in the APA.
   * 
   * @return true if there exists an epsilon transitions, else false.
   */
  def usesEpsilon: Boolean = {
    transitions.exists {
      case ((_, Epsilon), _) => true
      case _ => false
    }
  }

  /**
    * Converts the APA into dot format.
    * 
    * @return the APA in dot format.
    */
  def toDot(adot: ADotOption = SingleBool): String = toNamedApa.toDotHandler(adot)

  private def toDotHandler(adot: ADotOption): String = {
    case class ExportedTerm(val dot: String, // created automaton in dot
      val id: String, // id of created start element for the term
      val label: Option[String], // label for the transition into the start element
      val index: Int // incremented index
      )

    val twoWay = isTwoWay

    val label = if (twoWay) {
      (direction: Direction) =>
        Some("/ " + direction.toNumber.toString)
    } else {
      (direction: Direction) =>
        None
    }

    def exportTerm(term: PosBool[DirectedState], index: Int, label: (Direction) => Option[String]): ExportedTerm = {
      def labelTag(label: Option[String]) = label match {
        case Some(string) => " [label=\"" + string + "\"]"
        case None => ""
      }

      def andOr(first: PosBool[DirectedState], second: PosBool[DirectedState], op: String): ExportedTerm = {
        val a = exportTerm(first, index, label)
        val b = exportTerm(second, a.index, label)
        val commonLabel = if (a.label == b.label) a.label else None 
        ExportedTerm(a.dot + b.dot +
          "  " + op.toLowerCase() + b.index + " [shape=diamond, label=\"" + op.toUpperCase() + "\", margin=0];\n" +
          "  " + op.toLowerCase() + b.index + " -> " + a.id + labelTag(a.label) + ";\n" +
          "  " + op.toLowerCase() + b.index + " -> " + b.id + labelTag(b.label) + ";\n",
          op.toLowerCase() + b.index, commonLabel, b.index + 1)
      }

      term match {
        case BoolTrue => adot match {
          case SingleBool => ExportedTerm("  true [shape=triangle, label=\"TRUE\", margin=0];\n", "true", None, index)
          case MultipleBool => ExportedTerm("  true" + index + " [shape=triangle, label=\"TRUE\", margin=0];\n",
            "true" + index, None, index + 1)
        }
          
        case BoolFalse => adot match {
          case SingleBool => ExportedTerm("  false [shape=invtriangle, label=\"FALSE\", margin=0];\n", "false", None, index)
          case MultipleBool => ExportedTerm("  false" + index + " [shape=invtriangle, label=\"FALSE\", margin=0];\n",
            "false" + index, None, index + 1) 
        }
          
        case BoolElement(DirectedState(direction, state)) => ExportedTerm("", state.toString, label(direction), index)
        case BoolAnd(first, second) => andOr(first, second, "and")
        case BoolOr(first, second) => andOr(first, second, "or")
      }
    }

    val startDot = exportTerm(start.map { state: State => state :: Forward }, 1, _ => None)
    val transitionsDot = transitions.foldLeft(("", startDot.index)) {
      case ((dot, index), ((from, symbol), term)) => {
        val a = exportTerm(term, index, label)
        val caption = "" + symbol.toString.replaceAll("\"", "\\\\\"") +
          (a.label match {
            case Some(string) => " " + string
            case None => ""
          })
        (dot +
          a.dot +
          "  " + from + " -> " + a.id + " [label=\"" + caption + "\"];\n",
          a.index)
      }
    }

    "digraph G {\n" +
      "  rankdir=LR;\n" +
      states.map { state => "  " + state + " [shape=circle, label=\"" + state + ":" + colors(state) + "\", margin=0];\n" }.mkString("") +
      "  start [shape=none, label=\"\"];\n" +
      startDot.dot +
      "  start -> " + startDot.id + " [label=\"START\"];\n" +
      transitionsDot._1 +
      "}"
  }

  private[automata] def toNamedApa: Apa = toNamedApa(false)
  
  private[automata] def toNamedApa(forceNewNames: Boolean): Apa = {
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
      state: State => map(state)
    }
    val transitions = this.transitions.map {
      case ((state, symbol), term) => ((map(state), symbol), term.map {
        ds: DirectedState => map(ds.state) :: ds.direction
      })
    }
    val colors = this.colors.map {
      case (state, color) => (map(state), color)
    }

    Apa(this.alphabet, states, start, transitions, colors)
  }

  /**
    * Merges the APA with another one by starting in both simultaneously.
    * 
    * @param the other APA which shall be merged with this one.
    * @return a new APA created by merging the other two.
    */
  def &(other: Apa): Apa = {
    Apa(
      this.alphabet ++ other.alphabet,
      this.states ++ other.states,
      BoolAnd(this.start, other.start),
      this.transitions ++ other.transitions,
      this.colors ++ other.colors)
  }

  /**
    * Merges the APA with another one by starting in nondeterministically in one of the two.
    * 
    * @param the other APA which shall be merged with this one.
    * @return a new APA created by merging the other two.
    */
  def |(other: Apa): Apa = {
    Apa(
      this.alphabet ++ other.alphabet,
      this.states ++ other.states,
      BoolOr(this.start, other.start),
      this.transitions ++ other.transitions,
      this.colors ++ other.colors)
  }

  /**
    * Removed all states which are not at all reachable by any transition.
    * This does not check if there are transitions which are never reached.
    * Only states with no incoming transition are removed. Such states
    * are created by the other reduction methods.
    */
  private[automata] def toApaWithoutUnreachableStates: Apa = {
    def findReachables(term: PosBool[DirectedState], reachables: Set[State]): Set[State] = {
      def handleState(state: State, reachables: Set[State]): Set[State] = {
        if (reachables.contains(state)) {
          reachables
        } else {
          val alph = (alphabet.map(symbol => Text(symbol)): Set[Sign]) + All + Epsilon
          alph.foldLeft(reachables + state) {
            case (base, sign) if this.transitions.contains((state, sign)) => findReachables(this.transitions((state, sign)), base)
            case (base, _) => base; // do nothing
          }
        }
      }

      term match {
        case BoolAnd(_1, _2) => {
          val r = findReachables(_1, reachables)
          findReachables(_2, r)
        }
        case BoolOr(_1, _2) => {
          val r = findReachables(_1, reachables)
          findReachables(_2, r)
        }
        case BoolElement(DirectedState(_, state)) => handleState(state, reachables)
        case _ => reachables; // do nothing
      }
    }

    val reachables = findReachables(start.map { state: State => state :: Forward }, Set())

    Apa(alphabet, reachables.toList, start, transitions.filter {
      case ((state, _), _) if reachables.contains(state) => true
      case _ => false
    }, colors.filter {
      case (state, _) if reachables.contains(state) => true
      case _ => false
    })
  }

  /**
    * Tries to remove as much states and transitions as possible
    * while keeping the Apa as much readable to human as possible.
    * Different from toHeavyReducedApa all epsilon-transitions
    * and transitions to true are kept. The job is done by calling
    * different reduction methods on the Apa.
    * 
    * @return the slimed APA.
    */
  def toSlimApa =
    // remove transitions getting always true or false
    toSmallerApa.
      // remove now unreachable states
      toApaWithoutUnreachableStates.
      // remove false
      toApaWithoutFalse

  /**
    * Tries to remove as much states and transitions as possible
    * by calling different reduction methods on the Apa.
    * In most cases one new state is added as junk state to replace
    * transitions to true. This is needed for further processing
    * to an Nba.
    * 
    * @return the reduced APA.
    */
  def toReducedApa =
    // remove transitions getting always true or false
    toSmallerApa.
      // remove epsilon-transitions
      toApaWithoutEpsilon.
      // remove now unreachable states
      toApaWithoutUnreachableStates.
      // remove false
      toApaWithoutFalse.
      // replace true with a junk state
      toApaWithoutTrue

  /**
    * Returns a new Apa where transitions which result only in false
    * or only in true are replaced by their
    * boolean equivalent. This method is not able to detect cycles for which
    * this condition holds.
    */
  private[automata] def toSmallerApa: Apa = {
    // Returns true if the set of transitions specifies a target for
    // every sign of the alphabet. Else false.
    def fullyQualified(transitions: Map[(State, Sign), PosBool[DirectedState]]): Boolean = {
      transitions.exists { // if a transition for All or Epsilon is present, everything is fine
        case ((_, All), _) => true
        case ((_, Epsilon), _) => true
        case _ => false
      } || alphabet.forall { sym => // or if a transition for every symbol is present
        transitions.exists {
          case ((_, Text(s)), _) => s == sym
        }
      }
    }

    // Returns true iff all transitions return where they started.
    def loopState(transitions: Map[Pair[State, Sign], PosBool[DirectedState]]): Boolean = {
      transitions.forall {
        case ((start, _), term) if (term match {
          case BoolElement(DirectedState(_, target)) if start == target => true
          case _ => false
        }) => true
        case _ => false
      }
    }

    def traverseState(directedState: DirectedState, transitions: Map[(State, Sign), PosBool[DirectedState]], cache: Map[DirectedState, BoolPiece[DirectedState]]): (PosBool[DirectedState], Map[(State, Sign), PosBool[DirectedState]], Map[DirectedState, BoolPiece[DirectedState]]) = {
      if (cache.contains(directedState)) {
        (cache(directedState), transitions, cache)
      } else {
        val DirectedState(_, state) = directedState
        // save this "dummy" result before starting the recursion as visited flag
        val cache1 = cache + (directedState -> BoolElement(directedState))

        val alph = (this.alphabet.map(Text(_)): Set[Sign]) + All + Epsilon
        val (newTransitions, trans2, cache2) = alph.foldLeft((Map.empty[(State, Sign), PosBool[DirectedState]], transitions, cache1)) {
          case ((newTransitions, transitions, cache), sign) if transitions.contains((state, sign)) => {
            val (res2, trans2, cache2) = traverseExpression(transitions((state, sign)), transitions, cache)
            (newTransitions + ((state, sign) -> res2), trans2, cache2)
          }
          case (base, _) => base // do nothing 
        }

        // always keep transitions moving back
        if (directedState.direction == Back) {
          (BoolElement(directedState), trans2 ++ newTransitions, cache2)
        } else {
          val fulQual = fullyQualified(newTransitions)

          // check if all outgoing transitions reaches only true or only false
          val result: BoolPiece[DirectedState] = newTransitions match {
            case t if fulQual && t.forall { // if fully qualified AND all true
              case ((_, _), term) if term == BoolTrue => true
              case _ => false
            } => BoolTrue
            case t if t.forall { // if all false
              // not needed to be fully qualified because not qualified transitions
              // can be seen as transition to false, too
              case ((_, _), term) if term == BoolFalse => true
              case _ => false
            } => BoolFalse
            case t if fulQual && loopState(t) => this.colors(state) match {
              case 1 => BoolFalse
              case _ => BoolTrue // 0 or 2
            }
            case _ => BoolElement(directedState)
          }
  
          (result, trans2 ++ newTransitions, cache2 + (directedState -> result))
        }
      }
    }

    def traverseExpression(expression: PosBool[DirectedState], transitions: Map[(State, Sign), PosBool[DirectedState]], cache: Map[DirectedState, BoolPiece[DirectedState]]): (PosBool[DirectedState], Map[(State, Sign), PosBool[DirectedState]], Map[DirectedState, BoolPiece[DirectedState]]) = expression match {
      case BoolAnd(_1, _2) => {
        val (res1, trans1, cache1) = traverseExpression(_1, transitions, cache) 
        val (res2, trans2, cache2) = traverseExpression(_2, trans1, cache1)
        val result = (res1, res2) match {
          case (BoolTrue, BoolTrue) => BoolTrue
          case (BoolFalse, _) => BoolFalse
          case (_, BoolFalse) => BoolFalse
          case (BoolTrue, term) => term
          case (term, BoolTrue) => term
          case (_1, _2) => BoolAnd(_1, _2)
        }
        (result, trans2, cache2)
      }
      case BoolOr(_1, _2) => {
        val (res1, trans1, cache1) = traverseExpression(_1, transitions, cache) 
        val (res2, trans2, cache2) = traverseExpression(_2, trans1, cache1)
        val result = (res1, res2) match {
          case (BoolFalse, BoolFalse) => BoolFalse
          case (BoolTrue, _) => BoolTrue
          case (_, BoolTrue) => BoolTrue
          case (BoolFalse, term) => term
          case (term, BoolFalse) => term
          case (_1, _2) => BoolOr(_1, _2)
        }
        (result, trans2, cache2)
      }
      case BoolElement(directedState) => traverseState(directedState, transitions, cache)
      case bool => (bool, transitions, cache)
    }

    val startExpression = this.start.map { state: State => state :: Forward }
    val (result, transitions, _) = traverseExpression(startExpression, this.transitions, Map())
    result match {
      case BoolTrue => Apa.Everything(formula.Alphabet(this.alphabet))
      case BoolFalse => Apa.Nothing(formula.Alphabet(this.alphabet))
      case start => Apa(this.alphabet, this.states, start.map { ds: DirectedState => ds.state }, transitions, this.colors)
    }
  }

  /**
    * Removes every transitions to false. Transitions containing
    * more than just false are not touched. Such are handled
    * by toSmallerApa.
    */
  private[automata] def toApaWithoutFalse = this.start match {
    case BoolFalse => Apa.Nothing(formula.Alphabet(this.alphabet))
    case _ => {
      // collect all transitions not mapping to false
      val transitions = this.transitions.collect {
        case ((state, symbol), term) if term != BoolFalse => ((state, symbol), term)
      }

      Apa(this.alphabet, this.states, this.start, transitions, this.colors)
    }
  }

  /**
    * Replaces every transition to true with a transition to the junk state.
    * This junk state is created if nothing suitable is found. Transitions
    * containing more than just true are not touched. Such are handled
    * by toSmallerApa.
    */
  private[automata] def toApaWithoutTrue: Apa = this.start match {
    case BoolTrue => Apa.Everything(formula.Alphabet(this.alphabet))
    case _ => {
      // create a new junk state if there is none
      val (junk: State, addState: List[State], addTransition: Map[Pair[State, Sign], PosBool[DirectedState]], addColor: Map[State, Int]) =
          findJunkState(2) match {
        case Some(state) => (state, List.empty, Map.empty[Pair[State, Sign], PosBool[DirectedState]], Map.empty)
        case None => {
          // create a new junk state
          val state = State()
          // create a new transitions to make it a junk state
          (state, List(state), Map((state, All) -> BoolElement(state :: Forward)), Map(state -> 2))
        }
      }

      val transitions = this.transitions.map {
        case ((state, symbol), BoolTrue) => ((state, symbol), BoolElement(junk :: Forward))
        case x => x
      }

      // is the (possibly) newly created junk state really used?
      if (transitions.exists {
        case (_, BoolElement(DirectedState(Forward, state))) if state == junk => true
        case _ => false
      })
        Apa(this.alphabet, states ++ addState, this.start, transitions ++ addTransition, colors ++ addColor)
      else
        Apa(this.alphabet, states, this.start, transitions, colors)
    }
  }

  /**
    * Removes all epsilon transitions.
    */
  private[automata] def toApaWithoutEpsilon: Apa = {
    // Create a table mapping states where epsilon transitions start in to
    // the target list of the epsilon transitions. This table maps from a state
    // to a list of states annotated with an acceptance condition and a number
    // of incoming edges. The acceptance condition is the one the new target
    // state at least needs and the number is the maximum number of incoming
    // edges of all target states skipped in the computation of the closure below.
    val table1 = this.transitions.collect {
      case ((start, Epsilon), term) => start -> term.map {
        ds: DirectedState => {
          val state = ds.state
          val color = this.colors(start)
          val count = this.transitions.count {
            case (_, term) if term.map { ds: DirectedState => ds.state }.contains(state) => true
            case _ => false
          }
          (state, color, count)
        }
      }
    }

    // Create closure of the translation table by replacing every replaceable states
    // in the target list. The acceptance flag has to be the maximum of the current
    // flag and the flag of the state in the target list. The number of incoming
    // edges has to be the maximum of the current number and the number of the state
    // in the target list.
    def step(table: Map[State, PosBool[(State, Int, Int)]]): Map[State, PosBool[(State, Int, Int)]] = table.map {
      case (start, term) => start -> term.flatMap { x: (State, Int, Int) =>
        x match {
          case (state, color, count) if table.contains(state) => table(state).map { x: (State, Int, Int) =>
            x match {
              case (s, c, n) => (s, math.max(color, c), math.max(count, n))
            }
          }
          case x => BoolElement(x)
        }
      }
    }
    def closure(table: Map[State, PosBool[(State, Int, Int)]]): Map[State, PosBool[(State, Int, Int)]] = {
      val newTable = step(table)
      if (newTable != table)
        closure(newTable)
      else
        table
    }
    val table2 = closure(table1)

    // find target states in table with lower acceptance condition than needed
    val (table, apa) = table2.foldLeft((table2, this)) {
      case ((table, apa), (start, targets)) => {
        targets.toList.foldLeft((table, apa)) {
          case ((table, apa), (targetState, targetColor, edgeCount)) if targetColor > this.colors(targetState) => {
            // If target has only one incoming transition, this has to be the epsilon
            // transitions which is going to be removed. In this case just change the
            // acceptance condition of target.
            if (edgeCount == 1) {
              // simple solution in this case: just increase targets color
              (table, Apa(apa.alphabet, apa.states, apa.start, apa.transitions, apa.colors ++ Map(targetState -> targetColor)))
            } else {
              // Target has more than one incoming transition.
              // We have to clone target.
              val clone = State()
              // Clone the transitions, too.
              val clonedTransitions = apa.transitions.collect {
                case ((state, sign), term) if state == targetState =>
                  ((clone, sign), term)
              }
              // Create new table entry where target is replaced with the clone
              val newTableEntry = Map(start -> table(start).map { x: (State, Int, Int) =>
                x match {
                  case (s, c, n) if s == targetState => (clone, c, n)
                  case x => x
                }
              })
              (table ++ newTableEntry, Apa(apa.alphabet, apa.states :+ clone, apa.start, apa.transitions ++ clonedTransitions, apa.colors ++ Map(clone -> targetColor)))
            }
          }
          case (base, _) => base
        }
      }
    }

    // apply table on transitions and keep original transitions if the
    // target state has other outgoing transitions than epsilon transitions
    def applyTableAndKeep(term: PosBool[DirectedState]): PosBool[DirectedState] = term.flatMap { x: DirectedState =>
      x match {
        case DirectedState(direction, state) if table.contains(state) => {
          val result = table(state).map { p: (State, Int, Int) => p._1 :: direction }
          // if the given state has outgoing transitions
          // which aren't epsilon transitions
          // the original target needs to be kept
          val needToKeep = apa.transitions.exists {
            case ((s, sgn), _) if (s == state) && (sgn != Epsilon) => true
            case _ => false
          }
          if (needToKeep)
            BoolOr(result, BoolElement(state :: direction))
          else
            result
        }
        case directedState => BoolElement(directedState)
      }
    }

    // skip epsilon transitions which are not affected by the table
    val transitions = apa.transitions.collect {
      case ((state, sign), term) if (sign != Epsilon) => (state, sign) -> applyTableAndKeep(term)
    }

    // apply table on start
    val start = applyTableAndKeep(apa.start.map { state: State => state :: Forward }).map { ds: DirectedState => ds.state }

    Apa(apa.alphabet, apa.states, start, transitions, apa.colors)
  }

  /**
    * Finds a junk state in the given color, if there is one.
    * A junk state is a state which has only one outgoing transitions for
    * all elements of the alphabet (moving the head forward) and returning into
    * the junk state.
    */
  private def findJunkState(color: Int) = this.states.find { state =>
    if (this.transitions.contains((state, All))) {
      val outgoing = this.transitions.count {
        case ((from, _), _) => from == state
      }
      this.colors(state) == color && outgoing == 1 && this.transitions((state, All)) == BoolElement(state :: Forward)
    } else {
      false
    }
  }

  /**
    * Converts the APA to a parity automaton. This is just a transformation from one data structure into another
    * while both represent the same automaton. This is needed to transform an APA with in which both colors 0 and 1 exist.
    * 
    * @return the created parity automaton.
    */
  def toParityAutomata = {
    // states
    val states = new LinkedList[de.uni_luebeck.isp.buchi.State]
    // create new states and keep a map from old states to new states
    val indices = this.states.zipWithIndex.toMap
    val stateMap = indices.map {
      case (state, index) => {
        val newState = new de.uni_luebeck.isp.buchi.ParityState(index + 1, this.colors(state))
        states.add(newState)
        (state, newState)
      }
    }

    // create minimal models for start formula
    val start = new LinkedList[LinkedList[de.uni_luebeck.isp.buchi.State]]
    this.start.minimalModels.foreach { m =>
      {
        val newModel = new LinkedList[de.uni_luebeck.isp.buchi.State]
        start.add(newModel)
        m.foreach {
          state => newModel.add(stateMap(state))
        }
      }
    }

    // translate alphabet
    val alphabet = new LinkedList[String]
    this.alphabet.foreach {
      value => alphabet.add(value)
    }

    // compute transitions
    val transitions = new LinkedList[de.uni_luebeck.isp.buchi.Transition]
    this.transitions.foreach {
      case ((_, Epsilon), _) => throw new RuntimeException("Translation to NBW cannot handle epsilon transitions")
      case ((state, input), formula) => {
        formula.minimalModels.foreach { m =>
          {
            val newModel = new LinkedList[de.uni_luebeck.isp.buchi.State]
            m.foreach {
              state =>
                {
                  if (state.direction != Forward) {
                    throw new RuntimeException("Translation to NBW cannot handle two-way automata")
                  }
                  newModel.add(stateMap(state.state))
                }
            }
            input match {
              case All => {
                val it = alphabet.iterator()
                while (it.hasNext()) {
                  val s = it.next()
                  transitions.add(new de.uni_luebeck.isp.buchi.Transition(s, stateMap(state), newModel))
                }
              }
              case Text(value) => transitions.add(new de.uni_luebeck.isp.buchi.Transition(value, stateMap(state), newModel))
            }
          }
        }
      }
    }

    // put everything together
    new de.uni_luebeck.isp.buchi.ParityAutomaton(states, start, transitions, 2, alphabet)
  }
  
  /**
    * Converts the APA into a language-equivalent NBA.
    * 
    * @return the NBA.
    */
  def toNba = Nba(this)
  
  /**
    * Converts the APA into a DFA.
    * 
    * @return the DFA that results from the transformation of the result of toNba.toMinimizedNba.toNfa into a DFA.
    */
  def toDeterministicNfa = toNba.toMinimizedNba.toDeterministicNfa
  
  /**
    * Converts the APA into an NFA.
    * 
    * @return the NFA that results from the transformation of the result of toNba.toMinimizedNba into a NFA.
    */
  def toNfa = toNba.toMinimizedNba.toNfa
  
  /**
   	*	Converts an 1ABA into an 1ABA accepting the complement of the original language. 
   	*  
   	*  @return the 1ABA that accepts the complement of the language of the original 1ABA.
   	*/
  def toComplementAba : Apa = {
    
    // check if this APA is ABA (which means checking if parities are 2 or 1)
    if (this.colors.exists((tuple : (State,Int)) => tuple._2 != 2 && tuple._2 != 1)) throw new RuntimeException("Automaton needs to be ABA (parities 2 and 1 only).")
    
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
    val coAbaStateMap = this.states.map((s : State) => s.name -> s).toMap
  
    // holds all state names of the coABA
    val coAbaStateStrings = coAbaStateMap.toList.map((tuple : (String,State)) => tuple._1)
    
    // cartesian product of coAba state names and alphabet
    val coAbaStateSignCross = cartesianProduct(List(coAbaStateStrings, this.alphabet.toList))

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
    val dualizedTransitions = this.transitions.toList.map((tuple : ((State,Sign),PosBool[DirectedState])) => tuple._1 -> dualizePosBool(tuple._2)).toMap
    
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
      this.alphabet,
      this.states,
      this.start,
      coAbaTransitions,
      this.colors
    )
    
    // ------------------------------------------
    // build alphabet
    // ------------------------------------------
  
    // alphabet can simply be copied from coABA
    val alphabet = coAba.alphabet
    
    // ------------------------------------------
    // build states
    // ------------------------------------------
    
    // number of states of the coABA
    val n = coAba.states.length

    // holds all ranks from 0 to 2n inclusively as strings
    val ranks = List.range(0, 2*n+1).map((x : Int) => x.toString)
    
    // build cABA state strings
    val cAbaStateStrings = cartProdToString(cartesianProduct(List(coAbaStateStrings, ranks)))
  
    // build cABA states
    val states = cAbaStateStrings.map((s : String) => new State(s))
    
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
    val start = BoolOr(findStartStates(coAba.start).map((s : State) => BoolElement(stringToStateMap.get(s.name + "_" + (2*n)).get)))

    // ------------------------------------------
    // build colors
    // ------------------------------------------
    
    // the coloring of the cABA
    val colors = states.map((s : State) => s match {
      case s : State if s.name.substring(s.name.lastIndexOf("_")+1).toInt % 2 == 1 => (s -> 2)
      case s : State => (s -> 1)
      }
    ).toMap

    // ------------------------------------------
    // build transitions
    // ------------------------------------------
    
    // function 'release' described by Vardi
    def release(phi : PosBool[DirectedState], r : List[Int]) : PosBool[DirectedState] = phi match {
      case phi : BoolOr[DirectedState] => BoolOr(release(phi._1,r),release(phi._2,r))
      case phi : BoolAnd[DirectedState] => BoolAnd(release(phi._1,r),release(phi._2,r))
      case phi : BoolElement[DirectedState] => BoolOr(r.map((i : Int) => BoolElement(DirectedState(phi.value.direction,stringToStateMap.get(phi.value.state.name + "_" + i).get))))
      case phi if phi == BoolTrue || phi == BoolFalse => phi
      case _ => null
    }
    
    // function returning a transition
    def getTransition(s : State, a : Sign) : PosBool[DirectedState] = {
      if (coAba.colors.get(coAbaStateMap.get(s.name.substring(0, s.name.lastIndexOf("_"))).get).get == 1 || s.name.substring(s.name.lastIndexOf("_")+1).toInt % 2 == 0) {
        release(coAba.transitions.get(coAbaStateMap.get(s.name.substring(0, s.name.lastIndexOf("_"))).get,a).get, List.range(0, s.name.substring(s.name.lastIndexOf("_")+1).toInt+1))
      } else {
        BoolFalse
      }
    }
    
    // cartesian product of cABA states and signs
    val stateSignCross = cartesianProduct(List(cAbaStateStrings, alphabet.toList)).map((l : List[String]) => (stringToStateMap.get(l(0)).get,new Text(l(1))))
    
    // build the transitions
    val transitions = stateSignCross.map((tuple : (State,Sign)) => tuple -> getTransition(tuple._1, tuple._2)).toMap
        
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
   	*	Converts an 1ABA into an 1NBA accepting the complement of the original language. 
   	*  
   	*  @return the 1NBA that accepts the complement of the language of the original 1ABA.
   	*/
  def toComplementNba : Nba = {
    this.toComplementAba.toNba
  }
}

/**
  * Represents a pair of APAs
  * 
  * @constructor creates a new pair of APAs from two APAs.
  * @param _1 the first APA.
  * @param _2 the second APA.
  */
case class ApaSpecular(positive: Apa, negative: Apa) extends AutomatonSpecular {
  
  /**
    * Merges two APA speculars by merging their first and their second components accordingly.
    * 
    * @param the APA specular which shall be merged with this one.
    * @return a new APA specular containing the merged APAs. 
    */
  def |(other: ApaSpecular): ApaSpecular = {
    ApaSpecular(this.positive | other.positive, this.negative & other.negative)
  }

  /**
    * Merges two APA speculars by merging their first and their second components accordingly.
    * 
    * @param the APA specular whic shall be merged with this one.
    * @return a new APA specular containing the merged APAs. 
    */
  def &(other: ApaSpecular): ApaSpecular = {
    ApaSpecular(this.positive & other.positive, this.negative | other.negative)
  }

  private def sequentialBuilder(nfa: Nfa, apa: Apa, operator: BoolOperator)(implicit options: formula.Options) = {
    val states = nfa.states ++ apa.states
    val start = operator(nfa.start.map { state => BoolElement(state) })
    val transitions = apa.transitions ++ nfa.transitions.map {
      case ((state, symbol), list) =>
        (state, symbol) -> operator(list.map {
          case DirectedState(direction, state) if nfa.accepting.contains(state) =>
            operator(BoolElement(state :: direction), apa.start.map { state: State => state :: direction })
          case directedState => BoolElement(directedState)
        })
    }
    val color = operator match {
      case BoolAnd => 2
      case BoolOr => 1
    }
    val colors = Map(nfa.states.map { state => state -> color }: _*) ++ apa.colors
    Apa(options.alphabet, states, start, transitions, colors)
  }

  // TODO was tut das?
  def ::(other: Nfa)(implicit options: formula.Options): ApaSpecular = {
    val prepared = other.toPreparedNfa
    ApaSpecular(sequentialBuilder(prepared, positive, BoolOr),
      sequentialBuilder(prepared, negative, BoolAnd))
  }

  // TODO was tut das?
  def ~::(other: Nfa)(implicit options: formula.Options): ApaSpecular = {
    val prepared = other.toPreparedNfa
    ApaSpecular(sequentialBuilder(prepared, positive, BoolAnd),
      sequentialBuilder(prepared, negative, BoolOr))
  }

  /**
    * Swaps the position of the two elements of the APA specular.
    * 
    * @return a new APA specular with the second as first and the first as second component.
    */
  def swap: ApaSpecular = {
    ApaSpecular(this.negative, this.positive)
  }

  private def powerBuilder(obligation: Apa, delay: Nfa, attempt: Apa, operator: BoolOperator, startColor: Int)(implicit options: formula.Options): Apa = {
    val start = State()
    val states = obligation.states ++ delay.states ++ attempt.states :+ start
    val transitions =
      Map((start, Epsilon) -> operator(attempt.start.map { state: State => state :: Pause }, operator.other(obligation.start.map { state: State => state :: Pause }, operator(delay.start.map { state => BoolElement(state :: Pause) })))) ++
        delay.transitions.map {
          case (key, list) => key -> operator(list.flatMap {
            case directedState if delay.accepting.contains(directedState.state) => List(BoolElement(directedState), BoolElement(start :: directedState.direction))
            case directedState => List(BoolElement(directedState))
          })
        } ++
        obligation.transitions ++
        attempt.transitions
    // compute colors for the delay
    val cs = if (operator == BoolAnd && startColor == 1 && !delay.hasCycle) {
      Map(delay.states.map {
        // junk states get color 0
        case state if delay.transitions.forall {
            case ((s, sign), list) if s == state => list.forall { case DirectedState(_, s) => s == state }
            case _ => true
          } => state -> 0
        // all other states are not part of a cycle and therefore get same color like startColor 
        case state => state -> 1
      }: _*)
    } else {
      val c = operator match {
        // if startColor is 2 then use 2 instead of 0 as color for the delay NFA
        case BoolAnd if startColor == 2 => 2
        case BoolAnd => 0
        case BoolOr => 1
      }
      Map(delay.states.map { state => state -> c }: _*)
    }
    // compute colors for the new APW
    val colors =
      Map(start -> startColor) ++
        cs ++
        obligation.colors ++
        attempt.colors
    // put together the new APW
    Apa(options.alphabet, states, BoolElement(start), transitions, colors)
  }

  // TODO was tut das?
  def power(delay: Nfa, attempt: ApaSpecular)(implicit options: formula.Options): ApaSpecular = {
    val prepared = delay.toPreparedNfa
    ApaSpecular(powerBuilder(positive, prepared, attempt.positive, BoolOr, 1),
      powerBuilder(negative, prepared, attempt.negative, BoolAnd, 0))
  }

  // TODO was tut das?
  def uniPower(delay: Nfa, attempt: ApaSpecular)(implicit options: formula.Options): ApaSpecular = {
    val prepared = delay.toPreparedNfa
    ApaSpecular(powerBuilder(positive, prepared, attempt.positive, BoolAnd, 1),
      powerBuilder(negative, prepared, attempt.negative, BoolOr, 2))
  }

  // TODO was tut das?
  def weakPower(delay: Nfa, attempt: ApaSpecular)(implicit options: formula.Options): ApaSpecular = {
    val prepared = delay.toPreparedNfa
    ApaSpecular(powerBuilder(positive, prepared, attempt.positive, BoolOr, 2),
      powerBuilder(negative, prepared, attempt.negative, BoolAnd, 1))
  }

  // TODO was tut das?
  def uniWeakPower(delay: Nfa, attempt: ApaSpecular)(implicit options: formula.Options): ApaSpecular = {
    val prepared = delay.toPreparedNfa
    ApaSpecular(powerBuilder(positive, prepared, attempt.positive, BoolAnd, 0),
      powerBuilder(negative, prepared, attempt.negative, BoolOr, 1))
  }

  /**
    * Converts the two APAs into AFF format.
    * 
    * @return a string containing the two APAs in AFF format.
    */
  override def toString: String =
    positive.toString + "\n\n" + negative.toString

  /**
    * Converts the two APAs into dot format.
    * 
    * @return a string containing the two APAs in dot format.
    */
  def toDot(adot: ADotOption = SingleBool): String =
    positive.toDot(adot) + "\n\n" + negative.toDot(adot)

  /**
    * Applies a function to both APAs.
    * 
    * @param f the function to apply on both APAs.
    * @return a new APA specular containing the two transformed APAs.
    */
  def map(f: Apa => Apa): ApaSpecular =
    ApaSpecular(f(positive), f(negative))
    
  /**
    * Reduces both APAs by applying toReducedApa on both.
    * 
    * @return a new APA specular containing the reduced APAs 
    */
  def toReducedApas = map(_.toReducedApa)
  
  /**
    * Slims both APAs by applying toSlimApa on both.
    * 
    * @return a new APA specular containing the slimed APAs 
    */
  def toSlimApas = map(_.toSlimApa)
  
  /**
    * Transforms the APA specular into an NBA specular by applying toNba on the APAs.
    * 
    * @return a NBA specular containing the created NBAs.
    */
  def toNbas = NbaSpecular(positive.toNba, negative.toNba)
  
  /**
    * Transforms the APA specular into a DFA specular by transforming the NBAs of the NBA
    * specular resulting from toNbas.toMinimizedNbas into DFAs.
    * 
    * @return a DFA specular containing the created DFAs.
    */
  def toDeterministicNfas = toNbas.toMinimizedNbas.toDeterministicNfas
  
  /**
    * Transforms the APA specular into a NFA specular by transforming the NBAs of the NBA
    * specular resulting from toNbas.toMinimizedNbas into NFAs.
    * 
    * @return a NFA specular containing the created NFAs.
    */
  def toNfas = toNbas.toMinimizedNbas.toNfas
  
  /**
    * Transforms the APA specular into a single FSM by transforming the NBAs of the NBA
    * specular resulting from toNbas.toMinimizedNbas into a FSM.
    * 
    * @return a FSM.
    */
  def toMoore = toNbas.toMinimizedNbas.toMoore
}

/** Factory for [[de.uni_luebeck.isp.rltlconv.automata.Apa]] instances. */
object Apa {
  
  // TODO was tut das, wofür ist das gut?
  def Everything(implicit options: formula.Options) = {
    val state = State()
    Apa(options.alphabet, List(state), BoolElement(state), Map((state, All) -> BoolElement(state :: Forward)), Map(state -> 2))
  }
  
  //TODO was tut das, wofür ist das gut?
  def Nothing(implicit options: formula.Options) = {
    val state = State()
    Apa(options.alphabet, List(state), BoolElement(state), Map((state, All) -> BoolElement(state :: Forward)), Map(state -> 1))
  }
}

