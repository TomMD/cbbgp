 * Re-work the benchmark execution from it's current hack: The call to
   `readProcessMemory` needs a binary and arguments but the INI file claims to
   take an arbitrary (shell) command.  This difference is reconcipled by parsing
   the command by whitespace then considering the first token the executable and
   following tokens as the arguments.  What we want is a new primitive of
   `runCommandMemory`, but the method by which the memory is recorded makes that
   trickier than one might think.
 * Record an error log, push to the result repo.
