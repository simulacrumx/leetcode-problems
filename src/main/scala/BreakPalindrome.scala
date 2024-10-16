object BreakPalindrome extends App {

  def breakPalindrome(palindrome: String): String = {

    val a = 'a'
    val z = 'z'

    val length = palindrome.length

    if (palindrome.length <= 1) ""
    else {

      lazy val res = (0 until palindrome.length / 2).foldLeft(Option.empty[String]) {
        case (None, j) => {
          val first = palindrome.charAt(j)

          if (first == a) {
            None
          } else {
            Some(
              palindrome.substring(0, j) + 'a' + palindrome.substring(
                j + 1
              )
            )
          }
        }

        case (s @ Some(res), _) => s
      }

      // if forward sweep has no result => string has only 'a' letters

      res.getOrElse(palindrome.substring(0, length - 1) + 'b')
    }
  }

  println(breakPalindrome("abccba"))
  println(breakPalindrome("aaaaaaaaa"))
  println(breakPalindrome("zzzz"))
}
