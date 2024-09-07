package fr.istic.cal.prettyprinter

/**
 * définition d'une exception pour le cas des listes vides de commandes
 */
case object ExceptionListeVide extends Exception

object Prettyprinter {

  /**
   * UN PRETTY-PRINTER POUR LE LANGAGE WHILE
   *
   */

  /**
   * définition d'un type pour les spécifications d'indentation
   */
  type IndentSpec = List[(String, Int)]

  /**
   * définition d'une valeur d'indentation par défaut
   */
  val indentDefault: Int = 1

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une chaîne représentant la syntaxe concrète de l'expression
   */
  // TODO TP2
  def prettyPrintExpr(expression: Expression): String = {
    expression match {
      case Nl => "nil"
      case Cst(name) => name
      case VarExp(name) => name
      case Cons(arg1, arg2) => s"(cons ${prettyPrintExpr(arg1)} ${prettyPrintExpr(arg2)})"
      case Hd(arg) => s"(hd ${prettyPrintExpr(arg)})"
      case Tl(arg) => s"(tl ${prettyPrintExpr(arg)})"
      case Eq(arg1, arg2) => s"${prettyPrintExpr(arg1)} =? ${prettyPrintExpr(arg2)}"
    }
  }



  /**
   *  FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES COMMANDES
   *  OU LA PRESENTATION DU PROGRAMME
   */

  /**
   * recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
   *
   * @param context une chaîne de caractères décrivant un contexte d'indentation
   * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (un contexte,une indentation)
   * les contextes possibles seront, en majuscules, "WHILE", "FOR", "IF", ou "PROGR".
   * @return l'indentation correspondant à context
   */

  // TODO TP2
  def indentSearch(context: String, is: IndentSpec): Int =
    is.find(tuple => tuple._1.equals(context)).map(tuple => tuple._2).getOrElse(indentDefault)

  /**
   * création d'une indentation
   *
   * @param n un nombre d'espaces
   * @return une chaîne de n espaces
   */
  // TODO TP2
  def makeIndent(n: Int): String = " " * n

  /**
   * ajout d'une chaîne devant chaque élément d'une liste non vide de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
   */

  //TODO TP2
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] = {
    throwIfNil(strings)
    strings.map(s => pref + s)
  }

  /**
   * ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
   */

  //TODO TP2
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] = {
    throwIfNil(strings)
    strings.map(s => s + suff)
  }

  /**
   * ajout d'une chaîne après le dernier élément d'une liste non vide de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après le dernier élément de strings
   */
  
  //TODO TP2
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] = {
    throwIfNil(strings)
    val last = strings.last
    strings.take(strings.size - 1).appended(last + suff)
  }
  

  /**
   * ajout d'une chaîne après chaque élément d'une liste non vide de chaînes sauf le dernier
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings sauf le dernier
   */
  
  //TODO TP2
  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] = {
    throwIfNil(strings)
    val last = strings.last
    strings.take(strings.size - 1).map(string => string + suff).appended(last)
  }


  def throwIfNil(list: List[_]): Unit = {
    if (list.isEmpty)
      throw ExceptionListeVide
  }
  
  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la commande
   */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
    command match {
      case Nop => "nop" :: Nil
      case Set(variable, expression) => {
        variable match {
          case Var(name) => s"$name := ${prettyPrintExpr(expression)}" :: Nil
          case _ => throw new IllegalArgumentException()
        }
      }
      case While(expression, body) => s"while ${prettyPrintExpr(expression)} do" ::
        appendStringBeforeAll(makeIndent(indentSearch("WHILE", is)), prettyPrintCommands(body, is)) :::
        "od" ::
        Nil
      case For(expression, body) => s"for ${prettyPrintExpr(expression)} do" ::
        appendStringBeforeAll(makeIndent(indentSearch("FOR", is)), prettyPrintCommands(body, is)) :::
        "od" ::
        Nil
      case If(condition, alors, sinon) => s"if ${prettyPrintExpr(condition)} then" ::
        appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(alors, is)) :::
        "else" ::
        appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(sinon, is)) :::
        "fi" ::
        Nil
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la liste de commandes
   */
  // TODO TP2
  def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
    commands match {
      case element0 :: element1 :: tail => appendStringAfterLast(" ;", prettyPrintCommand(element0, is)) :::
        prettyPrintCommands(element1 :: tail, is)
      case element :: Nil => prettyPrintCommand(element, is)
      case Nil => Nil
    }
  }

  
  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les paramètres d'entrée d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
   */
  // TODO TP2
  def prettyPrintIn(vars: List[Variable]): String = {
    vars match {
      case Var(name) :: variable :: tail => f"$name, ${prettyPrintIn(variable :: tail)}"
      case Var(name) :: Nil => name
      case Nil => throw ExceptionListeVide
    }
  }

  /**
   * @param vars : une liste non vide décrivant les paramètres de sortie d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
   */
  // TODO TP2
  def prettyPrintOut(vars: List[Variable]): String = {
    vars match {
      case Var(name) :: variable :: tail => f"$name, ${prettyPrintIn(variable :: tail)}"
      case Var(name) :: Nil => name
      case Nil => throw ExceptionListeVide
    }
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = {
    program match {
      case Progr(in, body, out) => (s"read ${prettyPrintIn(in)}" :: "%" :: Nil) :::
        appendStringBeforeAll(makeIndent(indentSearch("PROGR", is)), prettyPrintCommands(body, is)) :::
        ("%" :: s"write ${prettyPrintOut(out)}":: Nil)
    }
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une chaîne représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrint(program: Program, is: IndentSpec): String = {
    val programList = prettyPrintProgram(program, is)
    programList.take(programList.size - 1).
      foldRight("")((a, b) => a + "\n" + b) + programList.last
  }

  val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      List(Var("Y")));
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = WhileParser.analyserexpression(s)

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = WhileParser.analysercommand(s)

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = WhileParser.analyserprogram(s)

}