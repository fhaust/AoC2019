

import            IntcodeComputer
















prog1 = compile "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
test1 = run2results [] prog1 == unIC prog1

prog2 = compile "1102,34915192,34915192,7,4,7,99,0"
test2 = (length . show $ run2result [] prog2) == 16

prog3 = compile "104,1125899906842624,99"
test3 = run2result [] prog3 == 1125899906842624


tests = all (==True) [test1, test2, test3]
