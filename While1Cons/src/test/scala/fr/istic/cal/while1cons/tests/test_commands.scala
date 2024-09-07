package fr.istic.cal.while1cons.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.while1cons.While1cons._
import fr.istic.cal.while1cons._

class TestsCommands {

  @Test
  def Test_while1ConsNop(): Unit = {
    NewVar.reset();
    assertEquals(
      List(Nop),
      while1ConsCommand(Nop))
  }

  @Test
  def Test_while1ConsSet_1(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("X"), Nl)),
      while1ConsCommand(Set(Var("X"), Nl)))
  }

  @Test
  def Test_while1ConsSet_2(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("X"), VarExp("Y"))),
      while1ConsCommand(Set(Var("X"), VarExp("Y"))))
  }

  @Test
  def Test_while1ConsSet_3(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("X"), Cons(VarExp("A0"), VarExp("A1")))),
      while1ConsCommand(Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X"))))))
  }

  @Test
  def Test_while1ConsCommands_Nil(): Unit = {
    try {
      while1ConsCommands(Nil);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }

  @Test
  def Test_while1ConsCommands_Nop(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Nop),
      while1ConsCommands(
        List(Nop)))
  }

  @Test
  def Test_while1ConsCommands_NopNop(): Unit = {
    NewVar.reset();
    assertEquals(
      List(Nop, Nop),
      while1ConsCommands(
        List(Nop, Nop)))
  }



  @Test
  def Test_while1ConsCommands_SetSet_1(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("X"), Cons(VarExp("A0"), VarExp("A1"))),
        Set(Var("X"), VarExp("Y"))),
      while1ConsCommands(
        List(
          Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X")))),
          Set(Var("X"), VarExp("Y")))))
  }

  @Test
  def Test_while1ConsCommands_SetSet_2(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("X"), Cons(VarExp("A0"), VarExp("A1"))),
        Set(Var("A2"), Tl(VarExp("Y"))),
        Set(Var("A3"), Hd(VarExp("X"))),
        Set(Var("X"), Cons(VarExp("A2"), VarExp("A3")))),
      while1ConsCommands(
        List(
          Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X")))),
          Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X")))))))
  }

  @Test
  def Test_while1ConsCommands_SetSet_3(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("Z"), Nl),
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("X"), Cons(VarExp("A0"), VarExp("A1"))),
        Set(Var("X"), VarExp("Y"))),
      while1ConsCommands(
        List(
          Set(Var("Z"), Nl),
          Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X")))),
          Set(Var("X"), VarExp("Y")))))
  }

  @Test
  def Test_while1ConsWhile_1(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Eq(VarExp("X"), VarExp("A0"))),
        While(
          VarExp("A1"),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("A2"), Tl(VarExp("Y"))),
            Set(Var("A3"), Hd(VarExp("X"))),
            Set(Var("X"), Cons(VarExp("A2"), VarExp("A3"))),
            Set(Var("A0"), Tl(VarExp("Y"))),
            Set(Var("A1"), Eq(VarExp("X"), VarExp("A0")))))),
      while1ConsCommand(While(
        Eq(VarExp("X"), Tl(VarExp("Y"))),
        List(
          Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
          Set(Var("X"), Cons(Tl(VarExp("Y")), Hd(VarExp("X"))))))))
  }

  @Test
  def Test_while1ConsWhile_2(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        While(
          VarExp("X"),
          List(
            Set(Var("A0"), Hd(VarExp("X"))),
            Set(Var("Y"), Cons(VarExp("A0"), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      while1ConsCommand(While(
        VarExp("X"),
        List(
          Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
          Set(Var("X"), Tl(VarExp("X")))))))
  }

  @Test
  def Test_while1ConsFor_1(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl),
        Set(Var("A2"), Nl),
        Set(Var("A3"), Nl),
        Set(Var("A4"), Cons(VarExp("A2"), VarExp("A3"))),
        Set(Var("A5"), Cons(VarExp("A1"), VarExp("A4"))),
        Set(Var("A6"), Cons(VarExp("A0"), VarExp("A5"))),
        For(
          VarExp("A6"),
          List(
            Set(Var("A7"), Nl),
            Set(Var("X"), Cons(VarExp("A7"), VarExp("X"))),
            Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))))),
      while1ConsCommand(For(
        Cons(Nl, Cons(Nl, Cons(Nl, Nl))),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X"))),
          Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))))))
  }

  @Test
  def Test_while1ConsFor_2(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        For(
          VarExp("X"),
          List(
            Set(Var("A0"), Nl),
            Set(Var("X"), Cons(VarExp("A0"), VarExp("X"))),
            Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))))),
      while1ConsCommand(For(
        VarExp("X"),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X"))),
          Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))))))
  }

  @Test
  def Test_while1ConsIf_1(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl),
        Set(Var("A2"), Nl),
        Set(Var("A3"), Nl),
        Set(Var("A4"), Cons(VarExp("A2"), VarExp("A3"))),
        Set(Var("A5"), Cons(VarExp("A1"), VarExp("A4"))),
        Set(Var("A6"), Cons(VarExp("A0"), VarExp("A5"))),
        If(
          VarExp("A6"),
          List(
            Set(Var("A7"), Nl),
            Set(Var("X"), Cons(VarExp("A7"), VarExp("X"))),
            Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))),
          List(
            Set(Var("A8"), Nl),
            Set(Var("X"), Cons(VarExp("A8"), VarExp("X")))))),
      while1ConsCommand(If(
        Cons(Nl, Cons(Nl, Cons(Nl, Nl))),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X"))),
          Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X")))))))
  }

  @Test
  def Test_while1ConsIf_2(): Unit = {
    NewVar.reset();
    assertEquals(
      List(
        If(
          VarExp("X"),
          List(
            Set(Var("A0"), Nl),
            Set(Var("X"), Cons(VarExp("A0"), VarExp("X"))),
            Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))),
          List(
            Set(Var("A1"), Nl),
            Set(Var("X"), Cons(VarExp("A1"), VarExp("X")))))),
      while1ConsCommand(If(
        VarExp("X"),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X"))),
          Set(Var("Y"), Cons(VarExp("X"), VarExp("X")))),
        List(
          Set(Var("X"), Cons(Nl, VarExp("X")))))))
  }
}