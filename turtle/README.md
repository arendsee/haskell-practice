# Haskell for scripting

I script. I script a lot.

To learn a new language I need immersion, I need to solve problems. Perhaps
I can replace my normal Bash scripts with Haskell using the turtle library.
This will provide plenty of varied experience in Haskell and also allow me to
replace the often shady Bash scripts I so often write.

## Notes on speed

 * Hello World is about 50% faster than Bash in the compiled Haskell script.
   I tested this in a loop of 100 calls.

## Notes on types

These cases are all pretty straightforward

 * cp,mv :: MonadIO io => Turtle.FilePath -> Turtle.FilePath -> io ()

 * rm,mkdir,touch,pwd :: MonadIO io => Turtle.FilePath -> io ()

 * testfile :: MonadIO io => Turtle.FilePath -> io Bool

The shell command is a bit different

shell :: MonadIO io => Text -> Shell Text -> io ExitCode
                        ^          ^
                        |          |
   shell command -------'          |
   STDIN --------------------------' 
