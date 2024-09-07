package fr.istic.cal.prettyprinter.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.prettyprinter.Prettyprinter._
import fr.istic.cal.prettyprinter._

class TestsStrings {

  @Test
  def Test_indentDefault(): Unit = {
    assertEquals(
      1,
      indentDefault)
  }

  @Test
  def Test_indentSearch_1(): Unit = {
    assertEquals(
      2,
      indentSearch("IF", List(("IF", 2), ("WHILE", 4))))
  }

  @Test
  def Test_indentSearch_2(): Unit = {
    assertEquals(
      4,
      indentSearch("WHILE", List(("IF", 2), ("WHILE", 4))))
  }

  @Test
  def Test_indentSearch_3(): Unit = {
    assertEquals(
      indentDefault,
      indentSearch("FOR", List(("IF", 2), ("WHILE", 4))))
  }

  @Test
  def Test_makeIndent_1(): Unit = {
    assertEquals(
      "",
      makeIndent(0))
  }

  @Test
  def Test_makeIndent_2(): Unit = {
    assertEquals(
      "        ",
      makeIndent(8))
  }

  @Test
  def Test_appendStringBeforeAll_0(): Unit = {
    try {
      appendStringBeforeAll("pref",Nil);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def Test_appendStringBeforeAll_1(): Unit = {
    assertEquals(
      List("prefsuff1", "prefsuff2", "prefsuff3"),
      appendStringBeforeAll("pref", List("suff1", "suff2", "suff3")))
  }

  @Test
  def Test_appendStringBeforeAll_2(): Unit = {
    assertEquals(
      List("prefsuff1"),
      appendStringBeforeAll("pref", List("suff1")))
  }

  @Test
  def Test_appendStringAfterAll_0(): Unit = {
    try {
      appendStringBeforeAll("suff",Nil);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def Test_appendStringAfterAll_1(): Unit = {
    assertEquals(
      List("pref1suff", "pref2suff", "pref3suff"),
      appendStringAfterAll("suff", List("pref1", "pref2", "pref3")))
  }

   @Test
  def Test_appendStringAfterAll_2(): Unit = {
    assertEquals(
      List("pref1suff"),
      appendStringAfterAll("suff", List("pref1")))
  }
   
  @Test
  def Test_appendStringAfterAllButLast_0(): Unit = {
    try {
      appendStringAfterAllButLast("suff",Nil);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def Test_appendStringAfterAllButLast_1(): Unit = {
    assertEquals(
      List("pref1suff", "pref2suff", "pref3"),
      appendStringAfterAllButLast("suff", List("pref1", "pref2", "pref3")))
  }

  @Test
  def Test_appendStringAfterAllButLast_2(): Unit = {
    assertEquals(
      List("pref1"),
      appendStringAfterAllButLast("suff", List("pref1")))
  }
  
  @Test
  def Test_appendStringAfterLast_0(): Unit = {
    try {
      appendStringAfterLast("suff",Nil);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }
  @Test
  def Test_appendStringAfterLast_1(): Unit = {
    assertEquals(
      List("pref1", "pref2", "pref3suff"),
      appendStringAfterLast("suff", List("pref1", "pref2", "pref3")))
  }

  @Test
  def Test_appendStringAfterLast_2(): Unit = {
    assertEquals(
      List("pref1suff"),
      appendStringAfterLast("suff", List("pref1")))
  }

}