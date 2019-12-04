# Star 3
opcode_input = c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,6,23,27,1,5,27,31,2,31,9,35,1,35,5,39,1,39,5,43,1,43,10,47,2,6,47,51,1,51,5,55,2,55,6,59,1,5,59,63,2,63,6,67,1,5,67,71,1,71,6,75,2,75,10,79,1,79,5,83,2,83,6,87,1,87,5,91,2,9,91,95,1,95,6,99,2,9,99,103,2,9,103,107,1,5,107,111,1,111,5,115,1,115,13,119,1,13,119,123,2,6,123,127,1,5,127,131,1,9,131,135,1,135,9,139,2,139,6,143,1,143,5,147,2,147,6,151,1,5,151,155,2,6,155,159,1,159,2,163,1,9,163,0,99,2,0,14,0)

process_opcodes <- function(opcodes) {
  i = 1
  repeat {
    if (opcodes[i] == 1) {
      opcodes[opcodes[i+3] + 1] = sum(opcodes[opcodes[c(i+1, i+2)] + 1])
    } else if (opcodes[i] == 2) {
      opcodes[opcodes[i+3] + 1] = prod(opcodes[opcodes[c(i+1, i+2)] + 1])
    } else if (opcodes[i] == 99) {
      break
    }
    i = i + 4
  }
  return(opcodes)
}

process_opcodes(c(1,9,10,3,2,3,11,0,99,30,40,50))
process_opcodes(c(1,0,0,0,99))
process_opcodes(c(2,3,0,3,99))
process_opcodes(c(2,4,4,5,99,0))
process_opcodes(c(1,1,1,4,99,5,6,0,99))

opcode_input[2] = 12
opcode_input[3] = 2

process_opcodes(opcode_input)[1]

# Star 4
use_intcode <- function (noun_verb) {
  noun = noun_verb[1]
  verb = noun_verb[2]
  opcode_input = c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,6,23,27,1,5,27,31,2,31,9,35,1,35,5,39,1,39,5,43,1,43,10,47,2,6,47,51,1,51,5,55,2,55,6,59,1,5,59,63,2,63,6,67,1,5,67,71,1,71,6,75,2,75,10,79,1,79,5,83,2,83,6,87,1,87,5,91,2,9,91,95,1,95,6,99,2,9,99,103,2,9,103,107,1,5,107,111,1,111,5,115,1,115,13,119,1,13,119,123,2,6,123,127,1,5,127,131,1,9,131,135,1,135,9,139,2,139,6,143,1,143,5,147,2,147,6,151,1,5,151,155,2,6,155,159,1,159,2,163,1,9,163,0,99,2,0,14,0)
  opcode_input[2] = noun
  opcode_input[3] = verb
  return(process_opcodes(opcode_input)[1])
}

use_intcode(c(12, 2))

brute_force_combos = combn(1:99, 2, list)
brute_force_results = sapply(brute_force_combos, use_intcode)

star_2_answer = brute_force_combos[[which(brute_force_results == 19690720)]]
100 * star_2_answer[1] + star_2_answer[2]
