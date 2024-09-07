package fr.istic.cal.interpreter

import scala.annotation.tailrec

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 * 
 * ETUDIANT 1 : BURLOT Mathis
 *
 * ETUDIANT 2 : LE STUM Nil
 *
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  @tailrec
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil => NlValue
      case (v0,d) :: tail => if (v0==v) d else lookUp(v, tail)
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case Nil => (v,d) :: Nil
      case (v0,d0) :: tail => if(v0==v) (v,d) :: tail else (v0, d0) :: assign(v,d,tail)
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => lookUp(Var(name), mem)
      case Cons(arg1, arg2) => ConsValue(interpreterExpr(arg1, mem), interpreterExpr(arg2, mem))
      case Hd(arg) => hdAndTlInterpreter(arg, mem, _.arg1)
      case Tl(arg) => hdAndTlInterpreter(arg, mem, _.arg2)
      case Eq(arg1, arg2) => if (interpreterExpr(arg1, mem) == interpreterExpr(arg2, mem)) CstValue("") else NlValue
    }
  }

  private def hdAndTlInterpreter(arg: Expression, memory: Memory, function: ConsValue => Value): Value = {
    val interpretArg = interpreterExpr(arg, memory)
    interpretArg match {
      case CstValue(_) => NlValue
      case value: ConsValue => function(value)
      case _ => interpreterExpr(arg, memory)
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1), valueToExpression(arg2))
    }
  }


  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  @tailrec
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop => memory
      case Set(variable, expression) => assign(variable, interpreterExpr(expression, memory), memory)
      case While(condition, body) =>
        if (interpreterExpr(condition, memory) == NlValue) memory
        else interpreterCommand(command, interpreterCommands(body, memory))
      case For(count, body) =>
        if (interpreterExpr(count, memory) == NlValue) memory
        else {
          val newCount = Tl(valueToExpression(interpreterExpr(count, memory)))
          interpreterCommand(For(newCount, body), interpreterCommands(body, memory))
        }
      case If(condition, then_commands, else_commands) =>
        if (interpreterExpr(condition, memory) != NlValue)
          interpreterCommands(then_commands, memory)
        else interpreterCommands(else_commands, memory)
    }
  }


  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  @tailrec
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil => throw ExceptionListeVide
      case command :: Nil => interpreterCommand(command, memory)
      case command :: tail => interpreterCommands(tail, interpreterCommand(command, memory))
    }
  }


  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    throwIfNil(vars)
    throwIfNil(vals)
    if (vars.length != vals.length)
      throw ExceptionListesDeLongueursDifferentes
    vars.indices.foldLeft[Memory](Nil)((memory, i) => (vars(i), vals(i)) :: memory)
  }


  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    throwIfNil(vars)
    vars.map(lookUp(_, memory))
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
    program match {
      case Progr(in, body, out) =>
        interpreterMemoryGet(out, interpreterCommands(body, interpreterMemorySet(in, vals)))
    }
  }


  private def throwIfNil(list: List[_]): Unit = if (list == Nil) throw ExceptionListeVide

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}