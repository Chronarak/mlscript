package hkmc2
package semantics
package ups

import mlscript.utils.shorthands.*
import mlscript.utils.*
import syntax.{Keyword, Tree}
import Tree.{Under, InfixApp, Ident, SynthSel, Sel, App, Tup, DecLit, IntLit}
import Pattern.{Lit, ClassLike, Record, And, Or}
import semantics.Elaborator.State
import hkmc2.syntax.Tree.OpApp


object Elaborator:
  extension (op: Keyword.Infix)
    infix def unnaply(tree: Tree): Option[(Tree, Tree)] = tree match
      case InfixApp(lhs, `op`, rhs) => Some((lhs, rhs))
      case _ => None


class Elaborator(elaborator: hkmc2.semantics.Elaborator)(using State):

  def elaborate(patternParams: List[Param] = Nil, body: Tree): Pattern =
    def elaborateConstructorLike(ctor: Ident | Sel, parameters: List[Tree]): Pattern = ctor match
      // case Ident(ctorName) => patternParams.find(_.sym.name == ctorName) match
      //   case S(Param(sym = symbol)) =>
      //     NonTerminal(sym, parameters.map(elaborate(patternParams, _)))
      //   case N => resolve(ctor, params).getOrElse:
      //     error(msg"Name not found: ${ctorName}" -> ctor.toLoc)
      //     Or(Nil)
      // case ctor: Sel => resolve(ctor, params).getOrElse:
      //   error(msg"Name not found: ${ctor.showDbg}" -> ctor.toLoc)
      //   Or(Nil)
      case _ => ???


    body.deparenthesized match
    case pattern @ App(Ident("|"), Tup(lhs :: rhs :: Nil)) =>
      val patterns = flattenAppInfix(Ident("|"), pattern).map(elaborate(patternParams, _))
      Or(patterns)
    case pattern @ App(Ident("&"), Tup(lhs :: rhs :: Nil)) =>
      val patterns = flattenAppInfix(Ident("&"), pattern).map(elaborate(patternParams, _))
      And(patterns)
    // case lhs ~ rhs => ???
      // (scrutinee, innermost, alternative) =>
      //   Branch(scrutinee, ClassLike(ConstructorLike.StringJoin), innermost.increment(2), alternative)
      //   alternative
    case Under() => Or(Nil)
    case ctor: (Ident | Sel) => elaborateConstructorLike(ctor, Nil)
    case App(ctor: (Ident | Sel), Tup(params)) => elaborateConstructorLike(ctor, params)
    case OpApp(lhs, op: Ident, rhs :: Nil) => ???
      // elaborateConstructorLike(op, Ls(lhs, rhs))
    case App(Ident("-"), Tup(IntLit(n) :: Nil)) => Lit(IntLit(-n))
    case App(Ident("-"), Tup(DecLit(n) :: Nil)) => Lit(DecLit(-n))
    case literal: syntax.Literal => Lit(literal)
    // BEGIN TODO: Support range patterns. This is just to suppress the errors.
    // case (lo: StrLit) to (incl, hi: StrLit) => ???
    // case (lo: IntLit) to (incl, hi: IntLit) => ???
    // case (lo: DecLit) to (incl, hi: DecLit) => ???
    // case (lo: syntax.Literal) to (_, hi: syntax.Literal) => ???
    // END TODO: Support range patterns
    case Tree.TypeDef(syntax.Pat, body, N, N) => ???

 

def flattenAppInfix(ident: Ident, term: Tree): List[Tree] = term match
  case App(visibleIdent, Tup(lhs :: rhs :: Nil)) if visibleIdent === ident =>
    flattenAppInfix(ident, lhs) ++ flattenAppInfix(ident, rhs)
  case _ => term :: Nil 