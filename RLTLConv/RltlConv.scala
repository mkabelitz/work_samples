package de.uni_luebeck.isp.rltlconv.cli

import de.uni_luebeck.isp.rltlconv.automata._
import de.uni_luebeck.isp.rltlconv.profiling.Profiling
import de.uni_luebeck.isp.buchi.BuchiAutomaton
import de.uni_luebeck.isp.rltlconv.{formula, ltl, rltl, regex, omegaregex}
import de.uni_luebeck.isp.rltlconv.ltl.LtlFormula
import de.uni_luebeck.isp.rltlconv.rltl.RltlFormula
import de.uni_luebeck.isp.rltlconv.omegaregex.OmegaregexFormula
import de.uni_luebeck.isp.rltlconv.regex.RegexFormula

object RltlConv {
  private val aDot = if (sys.env.withDefaultValue("")("ALTERNATING_DOT").toLowerCase() == "multiple") MultipleBool else SingleBool
  
  private val converter: Map[Conversion, PartialFunction[AnyRef, AnyRef]] = Map(
    Conversion.AUTOMATON -> ((input) => input match {
      case automata: Automata => automata  
      case s: String => Automata(s)
    }),
    Conversion.DOT -> ((input) => input match {
      case automata: Automata => automata.toDot(aDot)
    }),
    Conversion.PDF -> ((input) => input match {
      case automata: Automata => automata.toPdf(aDot)
    }),
    Conversion.PNG -> ((input) => input match {
      case automaton: Automaton => automaton.toPng(aDot)
    }),
    Conversion.SVG -> ((input) => input match {
      case automaton: Automaton => automaton.toSvg(aDot)
    }),
    //Conversion.PRRED -> ((input) => input match {
    //  case nba: BuchiAutomata => nba.toReduce
    //  case nbas: BuchiAutomataSpecular => nbas.toReduce
    //}),
    //Conversion.ATNT -> ((input) => input match {
    //  case nba: BuchiAutomata => nba.toAtnt
    //  case nbas: BuchiAutomataSpecular => nbas.toAtnt
    //}),
    Conversion.SLIM -> ((input) => input match {
      case apa: Apa => apa.toSlimApa
      case apas: ApaSpecular => apas.toSlimApas
    }),
    Conversion.DFA -> ((input) => input match {
      case nfa: Nfa => nfa.toDeterministicNfa
      case nba: Nba => nba.toDeterministicNfa
      case apa: Apa => apa.toDeterministicNfa
      case f: LtlFormula => f.toDeterministicNfa
      case f: RltlFormula => f.toDeterministicNfa
      case f: OmegaregexFormula => f.toDeterministicNfa
    }),
    Conversion.NEGDFA -> ((input) => input match {
      case f: LtlFormula => f.toNegDeterministicNfa
      case f: RltlFormula => f.toNegDeterministicNfa
      case f: OmegaregexFormula => f.toNegDeterministicNfa
    }),
    Conversion.DFAS -> ((input) => input match {
      case nfas: NfaSpecular => nfas.toDeterministicNfas
      case nbas: NbaSpecular => nbas.toDeterministicNfas
      case apas: ApaSpecular => apas.toDeterministicNfas
      case f: LtlFormula => f.toDeterministicNfas
      case f: RltlFormula => f.toDeterministicNfas
      case f: OmegaregexFormula => f.toDeterministicNfas
    }),
    Conversion.REDUCE -> ((input) => input match {
      case apa: Apa => apa.toReducedApa
      case apas: ApaSpecular => apas.toReducedApas
      case nfa: Nfa => nfa.toReducedNfa
      case nfas: NfaSpecular => nfas.toReducedNfaSpecular
    }),
    Conversion.MIN -> ((input) => input match {
      case apa: Apa => apa.toReducedApa
      case apas: ApaSpecular => apas.toReducedApas
      case nba: Nba => nba.toMinimizedNba
      case nbas: NbaSpecular => nbas.toMinimizedNbas
      case nfa: Nfa => nfa.toMinimizedNfa
      case nfas: NfaSpecular => nfas.toMinimizedNfas
      case moore: Moore => moore.toMinimizedMoore
      case mealy: Mealy => mealy.toMinimizedMealy
    }),
    Conversion.COMPLETE -> ((input) => input match {
      case nfa: Nfa => nfa.toCompletedNfa
    }),
    Conversion.FORMULA -> ((input) => input match {
      case f: formula.Formula => f  
      case s: String => formula.Formula(s)
    }),
    Conversion.PROPS -> ((input) => input match {
      case f: formula.Formula => f.toFormulaWithPropositions
    }),
    Conversion.FUTURE -> ((input) => input match {
      case f: RegexFormula => f.removePast
    }),
    Conversion.POSITIVE -> ((input) => input match {
      case f: LtlFormula => f.removeNegation
      case f: RltlFormula => f.removeNegation
    }),
    Conversion.NFA -> ((input) => input match {
      case nfa: Nfa => nfa
      case nba: Nba => nba.toNfa
      case apa: Apa => apa.toNfa
      case f: LtlFormula => f.toNfa
      case f: RltlFormula => f.toNfa
      case f: OmegaregexFormula => f.toNfa
      case f: RegexFormula => f.toNfa
    }),
    Conversion.NEGNFA -> ((input) => input match {
      case f: LtlFormula => f.toNegNfa
      case f: RltlFormula => f.toNegNfa
      case f: OmegaregexFormula => f.toNegNfa
    }),
    Conversion.NFAS -> ((input) => input match {
      case nfas: NfaSpecular => nfas
      case nbas: NbaSpecular => nbas.toNfas
      case apas: ApaSpecular => apas.toNfas
      case f: LtlFormula => f.toNfas
      case f: RltlFormula => f.toNfas
      case f: OmegaregexFormula => f.toNfas
    }),
    Conversion.NBA -> ((input) => input match {
      case nba: Nba => nba.eliminateTwoWay  
      case apa: Apa => apa.toNba
      case f: LtlFormula => f.toNba
      case f: RltlFormula => f.toNba
      case f: OmegaregexFormula => f.toNba
    }),
    Conversion.NEGNBA -> ((input) => input match {
      case f: LtlFormula => f.toNegNba
      case f: RltlFormula => f.toNegNba
      case f: OmegaregexFormula => f.toNegNba
    }),
    Conversion.NBAS -> ((input) => input match {
      case nbas: NbaSpecular => nbas  
      case apas: ApaSpecular => apas.toNbas
      case f: LtlFormula => f.toNbas
      case f: RltlFormula => f.toNbas
      case f: OmegaregexFormula => f.toNbas
    }),
    Conversion.APA -> ((input) => input match {
      case nba: Nba => nba.twoWayToAba
      case apa: Apa => apa
      case f: LtlFormula => f.toApa
      case f: RltlFormula => f.toApa
      case f: OmegaregexFormula => f.toApa
    }),
    Conversion.NEGAPA -> ((input) => input match {
      case f: LtlFormula => f.toNegApa
      case f: RltlFormula => f.toNegApa
      case f: OmegaregexFormula => f.toNegApa
    }),
    Conversion.APAS -> ((input) => input match {
      case apas: ApaSpecular => apas
      case f: LtlFormula => f.toApas
      case f: RltlFormula => f.toApas
      case f: OmegaregexFormula => f.toApas
    }),
    Conversion.COMPABA -> ((input) => input match {
      case nba: Nba => nba.twoWayToComplementAba
      case apa : Apa => apa.toComplementAba
      case f: LtlFormula => f.toApa.toComplementAba
    }),
    Conversion.RLTL -> ((input) => input match {
    case f: LtlFormula => f.toRltl  
    case f: OmegaregexFormula => f.toRltl
    }),
    Conversion.MOORE -> ((input) => input match {
      case moore: Moore => moore
      case nfas: NfaSpecular => nfas.toMoore
      case nbas: NbaSpecular => nbas.toMoore
      case apas: ApaSpecular => apas.toMoore
      case f: LtlFormula => f.toMoore
      case f: RltlFormula => f.toMoore
      case f: OmegaregexFormula => f.toMoore
    }),
    Conversion.AMEALY -> ((input) => input match {
      case amealy: AMealy => amealy
      case f: LtlFormula => f.toAMealy
    }),
    Conversion.MEALY -> ((input) => input match {
      case f: LtlFormula => f.toMealy
      case mealy: Mealy => mealy
      case amealy: AMealy => amealy.toMealy
    }),
    Conversion.PROFILE -> ((input) => input match {
      case data => {
        Profiling.start
        data
      }
    })) 
    
  def convert(input: AnyRef, commands: Array[String]): AnyRef =
    convert(input, commands: _*)
  
  def convert(input: AnyRef, commands: String*): AnyRef = commands.toList match {
    case head :: tail => convert(convert(input, head), tail: _*)
    case Nil => input
  }
  
  def convert(input: AnyRef, commands: Array[Conversion]): AnyRef =
    convert(input, commands: _*)
    
  def convert(input: AnyRef, conversions: Conversion*)(implicit i1:DummyImplicit): AnyRef = conversions.toList match {
    case head :: tail => convert(convert(input, head), tail: _*)
    case Nil => input
  }
    
  def convert(input: AnyRef, command: String): AnyRef = {
    val conversion = Conversion.fromCommand(command)
    if (conversion == null) {
      throw new ConverterNotFound("Conversion %s does not exists" format command)
    }
    convert(input, conversion)
  }
  
  def convert(input: AnyRef, conversion: Conversion): AnyRef = {
    val f = converter(conversion)
    if (!f.isDefinedAt(input)) {
      // try implicit conversion
      if (conversion.hasImplicit) {
        val i = converter(conversion.getImplicit)
        if (i.isDefinedAt(input)) {
          val converted = i(input)
          if (f.isDefinedAt(converted)) {
            return f(converted)
          }
        }
      }
      // no implicit conversion available
      throw ConverterNotFound("Cannot convert %s using %s." format (input.getClass.getSimpleName, conversion.getCommand))
    }
    f(input)
  }
}

case class ConverterNotFound(val message: String) extends Exception {
  override def toString = message
}
