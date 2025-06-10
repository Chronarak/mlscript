package hkmc2
package semantics

import mlscript.utils.*, shorthands.*
import syntax.*, Tree.Ident
import Elaborator.{Ctx, ctx}
import ucs.DeBrujinSplit

import Pattern.*

/** Flat patterns for pattern matching */
enum Pattern extends AutoLocated:
  
  case Lit(literal: Literal)
  
  /** An individual argument is None when it is not matched, i.e. when an underscore is used there.
    * The whole argument list is None when no argument list is being matched at all, as in `x is Some then ...`. */
  case ClassLike(
      val constructor: Term,
      val arguments: Opt[Ls[Argument]],
      val mode: MatchMode,
      var refined: Bool
  )(val tree: Tree)
  
  case Tuple(size: Int, inf: Bool)
  
  case Record(entries: List[Ident -> SubPattern])

  /** Corresponds to p1 & ... & pn.
    * @param patterns is the list of patterns of which we take the conjunction.
    * Technically, nothing forbids them to be themselves a conjunction
    * but it is advised to avoid this situation.
    * If @param patterns is Nil, then we have the neutral element of conjunction,
    * i.e. true, which is really the wildcard pattern in this case
    */
  case And(patterns: List[Pattern])

  /** Corresponds to p1 | ... | pn.
    * @param patterns is the list of patterns of which we take the disjunction.
    * Technically, nothing forbids them to be themselves a disjunction
    * but it is advised to avoid this situation.
    * If @param patterns is Nil, then we have the neutral element of disjunction,
    * i.e. false, which is really the never pattern in this case,
    * a pattern that never matches anything.
    */
  case Or(patterns: List[Pattern])

  /** Corresponds to @param pattern as @param name .
    * @param pattern is the concrete pattern.
    * @param name is the variable binding @param pattern's output.
    */
  case Rename(pattern: Pattern, name: VarSymbol)

  /** Corresponds to @param pattern => @term .
    * @param pattern is the concrete pattern.
    * @param term is the term to be executed.
    */
  case Extract(pattern: Pattern, term: Term)

  /** Corresponds to a pattern variable @param sym .
    * @param sym the variable symbol corresponding to the variable being bound.
    */
  case Var(sym: VarSymbol)

  /** Represents the call to a named pattern,
    * @param sym is the symbol of this occurence of the named pattern
    * @param params corresponds to the arguments of the non terminal.
    *        It is Some if a parameter list is provided and None otherwise.
    *        e.g. ``Foo`` has params=None, while ``Foo()`` has params=Some(Nil)
    */
  case NonTerminal(sym: PatternSymbol, params: Opt[Ls[Pattern]])

  def subTerms: Ls[Term] = this match
    case p: ClassLike => p.constructor :: (p.mode match
      case MatchMode.Default | _: MatchMode.StringPrefix => Nil
      case MatchMode.Annotated(annotation) => annotation :: Nil)
    case _: (Lit | Tuple | Record | And | Or | Rename | Var | NonTerminal) => Nil
    case Extract(_, term) => term :: Nil 
  
  def children: Ls[Located] = this match
    case Lit(literal) => literal :: Nil
    case ClassLike(ctor, scruts, _, _) => ctor :: scruts.fold(Nil)(_.map(_.scrutinee))
    case Tuple(fields, _) => Nil
    case Record(entries) => entries.flatMap:
      case (nme, _) => nme :: Nil
    case And(patterns) => patterns
    case Or(patterns) => patterns
    case Rename(pattern, name) => pattern :: name :: Nil
    case Extract(pattern, term) => pattern :: term :: Nil
    case Var(sym) => Nil
    case NonTerminal(sym, params) => params.getOrElse(Nil)
  
  def showDbg: Str = this match
    case Lit(literal) => literal.idStr
    case ClassLike(ctor, args, _, rfd) =>
      def showCtor(ctor: Term): Str = ctor match
        // This prints the symbol name without `refNum` and "member:" prefix.
        case Term.Ref(sym: BlockMemberSymbol) => sym.nme
        // This prints the symbol without `refNum`.
        case Term.Ref(sym) => sym.toString
        case Term.Sel(p, i) => s"${showCtor(p)}.${i.name}"
        case Term.SynthSel(p, i) => s"${showCtor(p)}.${i.name}"
        case _ => ctor.showDbg
      (if rfd then "refined " else "") + showCtor(ctor) +
        args.fold("")(_.iterator.map(_.scrutinee.nme).mkString("(", ", ", ")"))
    case Tuple(size, inf) => "[]" + (if inf then ">=" else "=") + size
    case Record(entries) =>
      entries.iterator.map(_.name + ": " + _).mkString("{ ", ", ", " }")
    case Wildcard => "_"
    case And(patterns) => patterns.mkString("(", ") & (", ")")
    case Never => "never"
    case Or(patterns) => patterns.mkString("(", ") | (", ")")
    case Rename(pattern, name) => pattern.showDbg + " as " + name.nme
    case Extract(pattern, term) => pattern.showDbg + " => " + term.showDbg
    case Var(sym) => sym.nme
    case NonTerminal(sym, params) => sym.nme + params.mkString("(", "), (", ")")

  /** @return the set of constructors that this patterns looks
    *         at with the scrutinee
    * It only returns those that are directly visible
    * examples:
    * {foo: C}.visibleConstructors = {}
    * (C | D).visibleConstructors = {C, D}
   */
  def visibleConstructors: Set[Term] = this match
    case _: (Lit | Record | Var) => Set()
    case ClassLike(constructor = constructor) => Set(constructor)
    case Tuple(size, inf) => ???
    case And(patterns) => patterns.toSet.flatMap(_.visibleConstructors)
    case Or(patterns) => patterns.toSet.flatMap(_.visibleConstructors)
    case Rename(pattern, _) => pattern.visibleConstructors
    case Extract(pattern, _) => pattern.visibleConstructors
    case NonTerminal(sym, params) => ???

  def visibleLiterals: Set[Literal] = this match
    case Lit(lit) => Set(lit)
    case _: (ClassLike| Record | Var) => Set()
    case Tuple(size, inf) => ???
    case And(patterns) => patterns.toSet.flatMap(_.visibleLiterals)
    case Or(patterns) => patterns.toSet.flatMap(_.visibleLiterals)
    case Rename(pattern, _) => pattern.visibleLiterals
    case Extract(pattern, _) => pattern.visibleLiterals
    case NonTerminal(sym, params) => ???

  /** @return the set of field name this pattern is directly interseted in
    * It only returns thos that are directly visible
    * examples:
    * {foo: {bar: _}}.fields = {foo}
    * ({foo: _} | {bar: _}) = {foo, bar}
    */
  def fields: Set[Ident] = this match
    case _: (Lit | ClassLike | Var) =>  Set()
    case Tuple(size, inf) => ???
    case Record(entries) => entries.toSet.map((id, _) => id)
    case And(patterns) => patterns.toSet.flatMap(_.fields)
    case Or(patterns) => patterns.toSet.flatMap(_.fields)
    case Rename(pattern, _) => pattern.fields
    case Extract(pattern, _) => pattern.fields
    case NonTerminal(sym, params) => ???
  
  /** @param id the field name for which we want subpatterns
    * @return all subpatterns that are about the given field
    */
  def collect(id: Ident): Set[Pattern] = this match
    case _: (Lit | ClassLike | Var) => Set()
    case Tuple(size, inf) => ???
    case Record(entries) => ???
      // entries.toSet.collect:
      //   case (id1, p) if id1 === id => p
    case And(patterns) => patterns.toSet.flatMap(_.collect(id))
    case Or(patterns) => patterns.toSet.flatMap(_.collect(id))
    case Rename(pattern, _) => pattern.collect(id)
    case Extract(pattern, _) => pattern.collect(id)
    case NonTerminal(smy, params) => ???
  
  def simplified: Pattern = this match
    case _: (Lit | Tuple | Var) => this 
    case pat @ ClassLike(term, args, matchMode, refined) => ???
      // val simplifiedArgs = args.map(_.map{ case (sym, pat) => (sym, pat.simplified) })
      // simplifiedArgs match
      //   case Some(args) if args.exists{ case (_, p) => p === Never } =>
      //     // if a component cannot match, then neither can the whole pattern
      //     Never
      //   case _ /* None and Some(args) if there are no never patterns */ =>
      //     Pattern.ClassLike(term, simplifiedArgs, matchMode, refined)(pat.tree)
    case Record(entries) => ???
      // val simplifiedEntries = entries.map((id, pat) => (id, pat.simplified))
      // if simplifiedEntries.exists((_, p) => p === Never) then
      //   // if a field cannot match, then neither can the whole pattern
      //   Never
      // else
      //   Record(simplifiedEntries)
    case And(patterns) =>
      // we cannot simplify wildcard (And(Nil)) in a conjunction
      // as we still have to return the value from this
      // side of the conjunciton
      val simplifiedPatterns = patterns.map(_.simplified)
      if simplifiedPatterns.contains(Never) then
        // we have a never pattern
        Never
      else
        And(simplifiedPatterns)
    case Or(patterns) =>
      def simplifyDisjunction(patterns: List[Pattern]): List[Pattern] = patterns match
          case Nil => Nil
          case Wildcard :: tl => Wildcard :: Nil
          case Never :: tl => simplifyDisjunction(tl)
          case hd :: tl => hd :: simplifyDisjunction(tl)
      simplifyDisjunction(patterns.map(_.simplified)) match
        case pat :: Nil => pat
        case patterns => Or(patterns)
    case Rename(pattern, name) => pattern.simplified match
      case Never => Never
      case pat => Rename(pat, name)
    case Extract(pattern, term) => pattern.simplified match
      case Never => Never
      case pat => Extract(pat, term)
    case NonTerminal(sym, params) => ???
  
  def specializeCons(cons: ClassSymbol /* TODO: use right type */): Pattern = this match
    case _: (Lit | Tuple | Record | Var) => this
    case ClassLike(constructor, arguments, _, _) => constructor.symbol match
      case Some(c) if c === cons => arguments match
        case None | Some(Nil) => Wildcard
        case Some(argList) => ???
          // TODO: recover the names of this constructor's arguments
      case _ => Never
    case And(patterns) => And(patterns.map(_.specializeCons(cons)))
    case Or(patterns) => Or(patterns.map(_.specializeCons(cons)))
    case Rename(pattern, name) => Rename(pattern.specializeCons(cons), name)
    case Extract(pattern, term) => Extract(pattern.specializeCons(cons), term)
    case NonTerminal(sym, params) => ???
  
object Pattern:
  /** Represent the type of arguments in `ClassLike` patterns. This type alias
   *  is used to reduce repetition in the code.
   * 
   *  - Field `pattern` is for error messages.
   *  - Field `split` is for pattern compilation.
   *    **TODO(ucs/rp)**: Replace with suitable representation when implement
   *    the new pattern compilation.
   */
  type Argument = (scrutinee: BlockLocalSymbol, pattern: Tree, split: Opt[DeBrujinSplit])
  // todo replace Argument by subPattern
  // todo : change pattern's type to Pattern
  type SubPattern = (scrutinee: BlockLocalSymbol, pattern: Tree)
  // TODO : remove scrutinee for the above

  /** Wildcard always matches.
    * It is the neutral element of conjunction
    */
  val Wildcard = And(Nil)

  /** The Never patterns never matches.
    * It is the neutral element of disjunction 
    */
  val Never = Or(Nil)

  /** A class-like pattern whose symbol is resolved to a class. */
  object Class:
    def unapply(p: Pattern): Opt[ClassSymbol] = p match
      case p: Pattern.ClassLike => p.constructor.symbol.flatMap(_.asCls)
      case _ => N
  
  /** A class-like pattern whose symbol is resolved to a module. */
  object Module:
    def unapply(p: Pattern): Opt[ModuleSymbol] = p match
      case p: Pattern.ClassLike => p.constructor.symbol.flatMap(_.asModOrObj)
      case _ => N
  
  enum MatchMode:
    /** The default mode. If the constructor resolves to:
     *  - a `ClassSymbol`, then check if the scrutinee is an instance;
     *  - a `ModuleSymbol`, then check if the scrutinee is the object;
     *  - a `PatternSymbol`, then call `unapply` on the pattern.
     */
    case Default
    /** Call `unapplyStringPrefix` instead of `unapply`. */
    case StringPrefix(prefix: TempSymbol, postfix: TempSymbol)
    /** The pattern is annotated. The normalization will intepret the pattern
     *  matching behavior based on the resolved symbol
     */
    case Annotated(annotation: Term)
    
  object ClassLike:
    def apply(constructor: Term, arguments: Opt[Ls[BlockLocalSymbol]]): ClassLike =
      ClassLike(constructor, arguments.map(_.map(s => (s, Tree.Dummy, N))), MatchMode.Default, false)(Tree.Dummy)
