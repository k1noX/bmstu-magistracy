toPart recipient = "Hello " ++ recipient ++ "!\n"

bodyPart reportTitle =
  "Thanks for your report on \""
    ++ reportTitle
    ++ "\"!\n"

fromPart lecturer = "Sincerly,\n" ++ lecturer

createEmail recipient reportTitle author =
  toPart recipient
    ++ bodyPart reportTitle
    ++ fromPart author

main = do
  putStrLn "Who are replying to?"
  recipient <- getLine
  putStrLn "Enter work name regarding to report:"
  title <- getLine
  putStrLn "Who is the lecturer of this subject?"
  author <- getLine
  putStrLn (createEmail recipient title author)
