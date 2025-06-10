package hkmc2
package semantics
package ups

import mlscript.utils.shorthands.*
import syntax.Tree
import Tree.Ident
import Pattern.*
import codegen.{Case, Block}

import Compiler.*

import Message.MessageContext

import collection.mutable.Map
import hkmc2.codegen.Value.Rcd


class Compiler(using Raise):

  var labels: Map[Pattern, Label] = Map()
  var Matchers: Map[Label, (BlockLocalSymbol, Term)] = Map()
  var MultiMatchers: Map[Set[Label], (BlockLocalSymbol, Term)] = Map()

  var subScrutineeData: Map[VarSymbol, Map[(Ident | Int), VarSymbol]] = Map()

  def buildMatcher(pattern: Pattern): BlockLocalSymbol = Matchers.get(pattern.label) match
    case Some((f, _)) => f
    case None => buildMatchFunction(Set(pattern), MatcherType.Single)

  // TODO : it refuses this definition for some reason
  // def buildMultiMatcher(patterns: Set[Pattern]): BlockLocalSymbol =
  //   buildMultiMatcher(patterns.map(_.label))

  def buildMatchFunction(patterns: Set[Pattern], matcherType: MatcherType): BlockLocalSymbol =
    MultiMatchers.get(patterns.map(_.label)) match
    case Some((f, _)) => f
    case None => ???
        
  // it is probably possible to eliminate the parameters scrut and subScrutineeData
  def evalPattern(pattern: Pattern, scrut: VarSymbol): Term = pattern match
      case _: (Lit | ClassLike) => ??? // should not appear
      case Tuple(size, inf) => ???
      case Record(entries) => ???
      case And(patterns) => ???
      case Or(patterns) => ???
      case Rename(pattern, name) => ???
      case Extract(pattern, term) => ???
      case Var(sym) => ???
      case NonTerminal(sym, params) => ???

  extension (pattern: Pattern)

    def expanded: Pattern =
      def checkedExpand(pattern: Pattern, nonTerminalSeen: Set[PatternSymbol]): Pattern = pattern match
        case _: (Lit | ClassLike | Tuple | Record | And | Or | Rename | Extract | Var) => pattern
        case NonTerminal(sym, params) if nonTerminalSeen contains sym =>
          raise(ErrorReport(msg"pattern symbol loop" -> pattern.toLoc :: Nil))
          pattern
        case NonTerminal(sym, params) => params match
          case Some(_) =>
            raise(ErrorReport(msg"higher order patterns not yet supported" -> pattern.toLoc :: Nil))
            pattern
          case None => sym.defn match
            case None =>
              raise(ErrorReport(msg"no definition found for this pattern synonym" -> pattern.toLoc :: Nil))
              pattern
            case Some(defn) => ???
              // TODO : transform the definition's body in a proper pattern
              // checkedExpand(defn.body, nonTerminalSeen + sym)
        checkedExpand(pattern, Set())

    def label: Label =
      labels.getOrElseUpdate(pattern, labels.size)

    def SpecializeField = ???
  

object Compiler:

  type Label = Int

  enum MatcherType:
    case Single
    case Multi

