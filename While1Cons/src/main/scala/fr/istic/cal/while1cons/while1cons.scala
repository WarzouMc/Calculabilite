package fr.istic.cal.while1cons

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 *
 * ETUDIANT 1 : Burlot Mathis
 *
 * ETUDIANT 2 : Bourdelles Marc
 *
 */
//Burlot Mathis
//Bourdelles Marc

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception
object While1cons {



  /**
   * UN ELIMINATEUR D'EXPRESSIONS COMPLEXES POUR LE LANGAGE WHILE
   *
   */

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */
  def getName(vari: Variable): String = vari match {
      case Var(name) => name
    }

  def getVarExp(vari: Variable): VarExp = VarExp(getName(vari))

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et de la variable qui contient le résultat
   */
  // TODO TP4
  def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
    expression match{
      case Nl | Cst(_) =>
        val variable = NewVar.make()
        (List(Set(variable, expression)), variable)
      case VarExp(name) => (Nil, Var(name))
      case Cons(arg1, arg2) =>
        val arg1list = while1ConsExprV(arg1)
        val arg2list = while1ConsExprV(arg2)
        val variable = NewVar.make()
        val rendu = arg1list._1 ::: arg2list._1 ::: List(Set(variable, Cons(getVarExp(arg1list._2), getVarExp(arg2list._2))))
        (rendu, variable)
      case Hd(arg) =>
        val (cmd1,val1) = while1ConsExprV(arg)
        val variable = NewVar.make()
        val list= cmd1 ::: List(Set(variable, Hd(getVarExp(val1))))
        (list,variable)
      case Tl(arg) =>
        val (cmd1, val1) = while1ConsExprV(arg)
        val variable = NewVar.make()
        val list= cmd1 ::: List(Set(variable, Tl(getVarExp(val1))))
        (list,variable)
      case Eq(expr1, expr2) =>
        val (cmd1,val1) = while1ConsExprV(expr1)
        val (cmd2,val2) = while1ConsExprV(expr2)
        val variable = NewVar.make()
        val list = cmd1 ::: cmd2 ::: List(Set(variable, Eq(getVarExp(val1), getVarExp(val2))))
        (list, variable)
    }
  }


  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations et une expression simple
   * qui, combinées, ont le même effet que l'expression initiale
   */
  // TODO TP4
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
    expression match{
      case Nl | VarExp(_) | Cst(_) => (Nil, expression)
      case Cons(expr1, expr2) =>
        val(cmd1, val1) = while1ConsExprV(expr1)
        val(cmd2, val2) = while1ConsExprV(expr2)
        val list = cmd1:::cmd2
        (list, Cons(getVarExp(val1), getVarExp(val2)))
      case Hd(expr) =>
        val(cmd1, val1) = while1ConsExprV(expr)
        (cmd1, Hd(getVarExp(val1)))
      case Tl(expr) =>
        val(cmd1, val1) = while1ConsExprV(expr)
        (cmd1, Tl(getVarExp(val1)))
      case Eq(expr1, expr2) =>
        val(cmd1, val1) = while1ConsExprV(expr1)
        val(cmd2, val2) = while1ConsExprV(expr2)
        val list = cmd1 ::: cmd2
        (list, Eq(getVarExp(val1), getVarExp(val2)))

    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que la commande initiale
   */
  // TODO TP4
  def while1ConsCommand(command: Command): List[Command] = {
    command match {
      case Nop => List(Nop)
      case Set(variable, expression) =>
        val (cmd1, val1): (List[Command], Expression) = while1ConsExprSE(expression)
        cmd1 ::: List(Set(variable, val1))
      case While(condition, body) =>
        condition match {
          case VarExp(nom) =>
            val newBody = while1ConsCommands(body)
            List(While(condition, newBody))
          case _ =>
            val (cmd1, val1) = while1ConsExprSE(condition)
            val variable = NewVar.make()
            val newCondition = getVarExp(variable)
            val setBefore = Set(variable, val1)
            val newBody = while1ConsCommands(body) ::: cmd1 ::: List(Set(Var(getName(variable)), val1))
            cmd1 ::: List(setBefore, While(newCondition, newBody))
        }
      case For(count, body) =>
        count match {
          case VarExp(nom) =>{
            val newBody = while1ConsCommands(body)
            List(For(count, newBody))
          }
          case _ =>
            val (cmd1, val1) = while1ConsExprSE(count)
            val variable = NewVar.make()
            val newCount = getVarExp(variable)
            val setBefore = Set(variable, val1)
            val newBody = while1ConsCommands(body)
            cmd1 ::: List(setBefore, For(newCount, newBody))
        }
      case If(condition, thenCommands, elseCommands) =>
        condition match {
          case VarExp(nom) =>
            val newThenCommands = while1ConsCommands(thenCommands)
            val newElseCommands = while1ConsCommands(elseCommands)
            List(If(condition, newThenCommands, newElseCommands))
          case _ =>
            val (cmd1, val1) = while1ConsExprSE(condition)
            val variable = NewVar.make()
            val newCondition = VarExp(getName(variable))
            val newThenCommands = while1ConsCommands(thenCommands)
            val newElseCommands = while1ConsCommands(elseCommands)
            val setBefore = Set(variable, val1)
            cmd1 ::: List(setBefore, If(newCondition, newThenCommands, newElseCommands))
        }
      case _ => throw ExceptionListeVide
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que les commandes initiales
   */
  // TODO TP4
  def while1ConsCommands(commands: List[Command]): List[Command] = {
    commands match {
      case Nil => throw ExceptionListeVide
      case a :: Nil => while1ConsCommand(a)
      case a :: b => while1ConsCommand(a) ::: while1ConsCommands(b)
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @return un AST décrivant un programme du langage WHILE
   * de même sémantique que le programme initial mais ne contenant que des expressions simples
   */
  // TODO TP4
  def while1ConsProgr(program: Program): Program = {
    program match {
      case Progr(in, body, out) => {
        if (in == Nil || out == Nil) throw ExceptionListeVide
        Progr(in, while1ConsCommands(body), out)
      }
    }
  }

  def main(args: Array[String]): Unit = {

    // vous pouvez ici tester manuellement vos fonctions par des print

  }


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
