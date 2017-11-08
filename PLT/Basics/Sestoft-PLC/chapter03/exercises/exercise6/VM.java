// javac VM.java && java VM program.expvm && rm VM.class

import java.io.*;
import java.util.*;

class VM {

    final static int CST = 0;
    final static int VAR = 1;

    final static int ADD = 2;
    final static int SUB = 3;
    final static int MUL = 4;

    final static int POP = 5;
    final static int SWP = 6;

    final static int SIZE_BOUND = 1000;

    static int eval(int[] code) {
        int[] stack = new int[SIZE_BOUND]; // evaluation and env stack
        int sp = -1; // pointer to current stack top

        int pc = 0; // program counter
        int instr = 0; // current instruction

        while (pc < code.length) {
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
        }

        return stack[sp];
    }

    public static void main(String[] args) {
        String filename = args[0];
        BufferedReader br = null;

        try {

            String line;
            List<String> instructions = new ArrayList<String>();
            br = new BufferedReader(new FileReader(filename));

            while ((line = br.readLine()) != null)
               instructions.add(line);

            int[] program = new int[instructions.size()];
            int i = 0;

            for(String instruction : instructions)
                program[i++] = Integer.parseInt(instruction);

            System.out.println(eval(program));

        } catch (IOException e) { e.printStackTrace(); }

    }
}