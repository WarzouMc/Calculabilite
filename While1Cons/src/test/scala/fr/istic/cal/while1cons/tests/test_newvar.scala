package fr.istic.cal.while1cons.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.while1cons._

class TestsNewVarSet {

  @Test
  def Test_newVarReset(): Unit = {
    assertEquals(
      (),
      NewVar.reset())
  }
  
  @Test
  def Test_newVarMake(): Unit = {
    NewVar.reset();
    NewVar.make();
    assertEquals(
      Var("A1"),
      NewVar.make())
  }
 
   @Test
  def Test_newVarMakeReset(): Unit = {
    NewVar.reset();
    NewVar.make();
    NewVar.reset();
    assertEquals(
      Var("A0"),
      NewVar.make())
  }
}

