# SystemStudio
A backtester for trading systems, written in F#.

- [turtle.json](turtle.json) - A trading system definition based on the Turtle trading system created by Richard Dennis and William Eckhardt. It is a trend-following system that uses volatility-based entry, volatility-based position sizing, and pyramiding.
- [jy_sf_trema_20_21.json](jy_sf_trema_20_21.json) - Configuration file that controls template variables and system permutation variables. Templates are used to duplicate parts of the system definition with different values determined by template variables, which reduces boilerplate code. System permutations let the user test the system over a range of possible settings.
- [long_entry.txt](long_entry.txt) - Code file that determines when to enter long positions.
- [long_exit.txt](long_exit.txt) - Code file that determines when to exit long positions.
- [short_entry.txt](short_entry.txt) - Code file that determines when to enter short positions.
- [short_exit.txt](short_exit.txt) - Code file that determines when to exit short positions.
- [money_management.txt](money_management.txt) - Code file that determines position sizing.
- [output_20240204.txt](output_20240204.txt) - The simulated results of trading the Turtle system with the Japanese Yen and Swiss Franc futures from February 1975 to December 1999, using the above configuration files.
