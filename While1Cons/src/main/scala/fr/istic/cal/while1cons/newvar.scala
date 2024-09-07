package fr.istic.cal.while1cons

trait TraitNewVar {

  /**
   * mise à zéro de l'indice des variables
   */
  def reset(): Unit

  /**
   * @return une nouvelle variable temporaire
   */
  def make(): Variable
}

object NewVar extends TraitNewVar {
  /**
   * définition d'une variable globale représentant l'indice des variables créées
   */
  private var index: Int = 0

  /**
   * mise à zéro de l'indice des variables
   */
  def reset(): Unit = { index = 0 }

  /**
   * @return une nouvelle variable temporaire
   */
  def make(): Variable = {
    val newindex = index;
    index = index + 1;
    Var("A" + newindex)
  }
}

object Main extends App {
  NewVar.reset();
  println(NewVar.make());
  println(NewVar.make())
}


