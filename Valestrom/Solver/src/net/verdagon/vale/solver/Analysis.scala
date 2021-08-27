package net.verdagon.vale.solver

//start here, lets see if we can get rid of rules?
case class Analysis(
  // For each rule, what are all the runes involved in it
  ruleToRunes: Array[Array[Int]],

  // For example, if rule 7 says:
  //   1 = Ref(2, 3, 4, 5)
  // then 2, 3, 4, 5 together could solve the rule, or 1 could solve the rule.
  // In other words, the two sets of runes that could solve the rule are:
  // - [1]
  // - [2, 3, 4, 5]
  // Here we have two "puzzles". The runes in a puzzle are called "pieces".
  // Puzzles are identified up-front by Astronomer.

  // This tracks, for each puzzle, what rule does it refer to
  puzzleToRule: Array[Int],
  // This tracks, for each puzzle, what rules does it have
  puzzleToRunes: Array[Array[Int]],

  // For every rule, this is which puzzles can solve it.
  ruleToPuzzles: Array[Array[Int]],

  // For every rune, this is which puzzles it participates in.
  runeToPuzzles: Array[Array[Int]],

  // Rules that we don't need to execute (e.g. Equals rules)
  noopRules: Array[Int])
//
//  // For every rune, which other rune might describe the interface that it must
//  // inherit from.
//  kindRuneToBoundingInterfaceRune: Array[Int])
