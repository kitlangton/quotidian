package quotidian

import scala.quoted.*

object StringUtils:
  extension (self: String)
    def blue    = Console.BLUE + self + Console.RESET
    def red     = Console.RED + self + Console.RESET
    def cyan    = Console.CYAN + self + Console.RESET
    def green   = Console.GREEN + self + Console.RESET
    def yellow  = Console.YELLOW + self + Console.RESET
    def magenta = Console.MAGENTA + self + Console.RESET

    def dim        = s"\u001b[2m$self\u001b[0m"
    def bold       = Console.BOLD + self + Console.RESET
    def underlined = Console.UNDERLINED + self + Console.RESET
