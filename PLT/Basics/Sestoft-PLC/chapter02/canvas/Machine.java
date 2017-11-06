// javac Machine.java && java Machine expression.svm && rm Machine.class

import java.io.*;
import java.util.*;


class Machine {

    final static int CST = 0;
    final static int VAR = 1;
    
    final static int ADD = 2;
    final static int SUB = 3;
    final static int MUL = 4;
    final static int DIV = 5;
    final static int MOD = 6;
    final static int EQU = 7;
    final static int NEQ = 8;
    final static int MAX = 9;
    final static int MIN = 10;

    final static int POP = 11;
    final static int SWP = 12;

    final static int SIZE_BOUND = 1000;

    static int eval(int[] code) {
        int[] stack = new int[1000];            // evaluation and env stack
        int sp = -1;                            // pointer to current stack top

        int pc = 0;                             // program counter
        int instr = 0;                          // current instruction

        while (pc < code.length) {
            try {
                switch (instr = code[pc++]) {
                    case CST:
                        stack[sp + 1] = code[pc++];
                        sp++;
                        break;
                    case VAR:
                        stack[sp + 1] = stack[sp - code[pc++]];
                        sp++;
                        break;
                    case ADD:
                        stack[sp - 1] = stack[sp - 1] + stack[sp];
                        sp--;
                        break;
                    case SUB:
                        stack[sp - 1] = stack[sp - 1] - stack[sp];
                        sp--;
                        break;
                    case MUL:
                        stack[sp - 1] = stack[sp - 1] * stack[sp];
                        sp--;
                        break;
                    case DIV:
                        stack[sp - 1] = stack[sp - 1] / stack[sp];
                        sp--;
                        break;
                    case MOD:
                        stack[sp - 1] = stack[sp - 1] % stack[sp];
                        sp--;
                        break;
                    case EQU:
                        stack[sp - 1] = (stack[sp - 1] == stack[sp]) ? 1 : 0;
                        sp--;
                        break;
                    case NEQ:
                        stack[sp - 1] = (stack[sp - 1] != stack[sp]) ? 1 : 0;
                        sp--;
                        break;
                    case MAX:
                          stack[sp - 1] = Math.max(stack[sp - 1], stack[sp]);
                        sp--;
                        break;
                    case MIN:
                        stack[sp - 1] = Math.min(stack[sp - 1], stack[sp]);
                        sp--;
                        break;
                    case POP:
                        sp--;
                        break;
                    case SWP:
                        {
                            int tmp = stack[sp];
                            stack[sp] = stack[sp - 1];
                            stack[sp - 1] = tmp;
                            break;
                        }
                    default:
                        throw new RuntimeException("Illegal instruction " + instr +
                            " at address " + (pc - 1));
                }
            } catch(Exception e) {
                System.out.println("Error while reading instruction " + instr +
                        " at address " + (pc - 1));
                break;
            }
        }

        return stack[sp];
    }

    static void test() {
        int[] rpn1 = { CST, 17, VAR, 0, VAR, 1, ADD, SWP, POP };
        System.out.println(eval(rpn1));

        int[] rpn2 = { CST, 17, CST, 22, CST, 100, VAR, 1, MUL, SWP, POP, VAR, 1, ADD, SWP, POP };
        System.out.println(eval(rpn2));

        int[] rpn3 = { CST, 8, VAR, 0, CST, 9, ADD, SWP, POP };
        System.out.println(eval(rpn3));
    }

    static int[] loadProgram(String filename) {
        BufferedReader br = null;
        int[] program = null;

        try {

            String line;
            List<String> instructions = new ArrayList<String>();
            br = new BufferedReader(new FileReader(filename));

            while ((line = br.readLine()) != null)
               instructions.add(line);

            program = new int[instructions.size()];
            int i = 0;

            for(String instruction : instructions)
                program[i++] = Integer.parseInt(instruction);

        } catch (IOException e) { e.printStackTrace(); }

        return program;
    }

    public static void main(String[] args) {
        if(args.length == 1) {
            String filename = args[0];
            int[] program = loadProgram(filename);
            System.out.println(eval(program));
        }
        else
            test();
        
    }
}