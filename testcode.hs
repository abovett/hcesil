-- Test function for lineSplit
testLineSplit :: IO ()
testLineSplit = do
  print $ lineSplit ""
  print $ lineSplit "label"
  print $ lineSplit "label instr"
  print $ lineSplit "      instr"
  print $ lineSplit "label instr op1 op2"
  print $ lineSplit "      instr op1 op2  "
  print $ lineSplit "      instr op1 op2 \n"
