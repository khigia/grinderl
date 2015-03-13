# Documentation #

  * Task definition, with argument's usage rules
  * Scenario definition, and repetition of task/test
  * Statistics of task results
  * Complete [Example](Example.md) (goal, test deployment and results)

# Implementation #

  * See if Mnesia should be used for statistic gathering.
  * Build system: [sinan](http://code.google.com/p/sinan) or [erlware](http://www.erlware.org)?

# Features #

  * Disambiguate data: function/test/task/scenario. Have helpers to create/manipulate.
  * API for statistic results: per node, and global one
  * Run external tests: python port to run tests written in Python, or run external process...
  * Command-line interface
  * More node manipulation API (display, remove, remote start...).
  * Adding scenario description (load evolution in time).
  * Use the R library to plot statistic results.
  * Embed yaws as a graphical API
