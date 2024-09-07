package fr.istic.cal.while1cons.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.while1cons.While1cons._
import fr.istic.cal.while1cons._

class TestsExpressions {

  @Test
  def Test_while1ConsExprV_Nil(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(Set(Var("A0"), Nl)), Var("A0")),
      while1ConsExprV(Nl))
  }

  @Test
  def Test_while1ConsExprV_Cst(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(Set(Var("A0"), Cst("aaa"))), Var("A0")),
      while1ConsExprV(Cst("aaa")))
  }

  @Test
  def Test_while1ConsExprV_VarExp(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Var("X")),
      while1ConsExprV(VarExp("X")))
  }

  @Test
  def Test_while1ConsExprV_Cons1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl),
        Set(Var("A2"), Cons(VarExp("A0"), VarExp("A1")))),
        Var("A2")),
      while1ConsExprV(Cons(Nl, Nl)))
  }
  
  @Test
  def Test_while1ConsExprV_Cons2(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Cons(VarExp("X"), VarExp("Y")))),
        Var("A0")),
      while1ConsExprV(Cons(VarExp("X"), VarExp("Y"))))
  }
  
  @Test
  def Test_while1ConsExprV_Hd1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Hd(VarExp("X")))),
        Var("A0")),
      while1ConsExprV(Hd(VarExp("X"))))
  }
  
  @Test
  def Test_while1ConsExprV_Hd_2(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Hd(VarExp("A0")))),
        Var("A1")),
      while1ConsExprV(Hd(Nl)))
  }
  
  @Test
  def Test_while1ConsExprV_Tl(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Tl(VarExp("X")))),
        Var("A0")),
      while1ConsExprV(Tl(VarExp("X"))))
  }
  
  @Test
  def Test_while1ConsExprV_Eq1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl),
        Set(Var("A2"), Eq(VarExp("A0"), VarExp("A1")))),
        Var("A2")),
      while1ConsExprV(Eq(Nl, Nl)))
  }
  
  @Test
  def Test_while1ConsExprV_Eq2(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Eq(VarExp("X"), VarExp("Y")))),
        Var("A0")),
      while1ConsExprV(Eq(VarExp("X"), VarExp("Y"))))
  }
  
  @Test
  def Test_while1ConsExprV_Expr1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Hd(VarExp("A0"))),
        Set(Var("A2"), Tl(VarExp("A1"))),
        Set(Var("A3"), Hd(VarExp("A2")))),
        Var("A3")),
      while1ConsExprV(Hd(Tl(Hd(Nl)))))
  }
  
  @Test
  def Test_while1ConsExprV_Expr2(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("A2"), Cons(VarExp("A0"),VarExp("A1")))),
        Var("A2")),
      while1ConsExprV(Cons(Tl(VarExp("Y")),Hd(VarExp("X")))))
  }
  
  @Test
  def Test_while1ConsExprV_Expr3(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("A2"), Cons(VarExp("A0"),VarExp("A1"))),
        Set(Var("A3"), Nl),
        Set(Var("A4"), Hd(VarExp("A3"))),
        Set(Var("A5"), Tl(VarExp("A4"))),
        Set(Var("A6"), Hd(VarExp("A5"))),
        Set(Var("A7"), Eq(VarExp("A2"),VarExp("A6")))),
        Var("A7")),
      while1ConsExprV(Eq(Cons(Tl(VarExp("Y")),Hd(VarExp("X"))),Hd(Tl(Hd(Nl))))))
  }
  
  @Test
  def Test_while1ConsExprSE_Nil(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Nl),
      while1ConsExprSE(Nl))
  }

  @Test
  def Test_while1ConsExprSE_Cst(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Cst("aaa")),
      while1ConsExprSE(Cst("aaa")))
  }

  @Test
  def Test_while1ConsExprSE_VarExp(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, VarExp("X")),
      while1ConsExprSE(VarExp("X")))
  }

  @Test
  def Test_while1ConsExprSE_Cons1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl)),
        Cons(VarExp("A0"), VarExp("A1"))),
      while1ConsExprSE(Cons(Nl, Nl)))
  }
  
  @Test
  def Test_while1ConsExprSE_Cons2(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Cons(VarExp("X"), VarExp("Y"))),
      while1ConsExprSE(Cons(VarExp("X"), VarExp("Y"))))
  }
  
  @Test
  def Test_while1ConsExprSE_Hd(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Hd(VarExp("X"))),
      while1ConsExprSE(Hd(VarExp("X"))))
  }
  
  @Test
  def Test_while1ConsExprSE_Tl(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Tl(VarExp("X"))),
      while1ConsExprSE(Tl(VarExp("X"))))
  }
  
  @Test
  def Test_while1ConsExprSE_Eq1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Nl)),
        Eq(VarExp("A0"), VarExp("A1"))),
      while1ConsExprSE(Eq(Nl, Nl)))
  }
  
  @Test
  def Test_while1ConsExprSE_Eq2(): Unit = {
    NewVar.reset();
    assertEquals(
      (Nil, Eq(VarExp("X"), VarExp("Y"))),
      while1ConsExprSE(Eq(VarExp("X"), VarExp("Y"))))
  }
  
  @Test
  def Test_while1ConsExprSE_Expr1(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Nl),
        Set(Var("A1"), Hd(VarExp("A0"))),
        Set(Var("A2"), Tl(VarExp("A1")))),
        Hd(VarExp("A2"))),
      while1ConsExprSE(Hd(Tl(Hd(Nl)))))
  }
  
  @Test
  def Test_while1ConsExprSE_Expr2(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X")))),
        Cons(VarExp("A0"),VarExp("A1"))),
      while1ConsExprSE(Cons(Tl(VarExp("Y")),Hd(VarExp("X")))))
  }
  
  @Test
  def Test_while1ConsExprSE_Expr3(): Unit = {
    NewVar.reset();
    assertEquals(
      (List(
        Set(Var("A0"), Tl(VarExp("Y"))),
        Set(Var("A1"), Hd(VarExp("X"))),
        Set(Var("A2"), Cons(VarExp("A0"),VarExp("A1"))),
        Set(Var("A3"), Nl),
        Set(Var("A4"), Hd(VarExp("A3"))),
        Set(Var("A5"), Tl(VarExp("A4"))),
        Set(Var("A6"), Hd(VarExp("A5")))),
        Eq(VarExp("A2"),VarExp("A6"))),
      while1ConsExprSE(Eq(Cons(Tl(VarExp("Y")),Hd(VarExp("X"))),Hd(Tl(Hd(Nl))))))
  }
}