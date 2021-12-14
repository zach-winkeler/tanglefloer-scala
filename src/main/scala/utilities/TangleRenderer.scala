package utilities

object TangleRenderer {
  private val CupStrings = IndexedSeq(" | ", "` ,", "- -", "- -")
  private val CapStrings = IndexedSeq("- -", "- -", "\' .", " | ")
  private val OverStrings = IndexedSeq("- -", "- -", "- -", "- -", "\' .", " / ", "` ,", "- -")
  private val UnderStrings = IndexedSeq("- -", "\' .", " \\ ", "` ,", "- -", "- -", "- -", "- -")
}
