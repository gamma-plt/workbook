// make exercise4 && ./exercise4 < expression.svm && rm exercise4

#include <stdio.h>
#include <stdlib.h>

#define SIZE_BOUND 1000
#define SIZE_OF(a) (sizeof(a)) / (sizeof(a[0]))

#define CST 0
#define VAR 1

#define ADD 2
#define SUB 3
#define MUL 4

#define POP 5
#define SWP 6

int eval(int size, int* program) {
	int i = 0;
	int stack[SIZE_BOUND];

	int stack_top = -1;
	int program_counter = 0;
	int current_instruction;

	while(program_counter < size) {
		switch(current_instruction = program[program_counter++]) {
			case CST:
				stack[stack_top + 1] = program[program_counter++];
				stack_top++;
				break;
			case VAR:
				stack[stack_top + 1] = stack[stack_top - program[program_counter++]];
				stack_top++;
				break;
			case ADD:
				stack[stack_top - 1] = stack[stack_top - 1] + stack[stack_top];
				stack_top--;
				break;
			case SUB:
				stack[stack_top - 1] = stack[stack_top - 1] - stack[stack_top];
				stack_top--;
				break;
			case MUL:
				stack[stack_top - 1] = stack[stack_top - 1] * stack[stack_top];
				stack_top--;
				break;
			case POP:
				stack_top--;
				break;
			case SWP: {
				int temp = stack[stack_top];
				stack[stack_top] = stack[stack_top - 1];
				stack[stack_top - 1] = temp;
				break;
			}
			default:
				printf("ERROR: unknown instruction %d at address %d.\n", 
					current_instruction, program_counter - 1);
				return 1;
				break;
		}
	}

	return stack[stack_top];
}

int main(int argc, char const *argv[]) {

	// [SCstI 17; SVar 0; SVar 1; SAdd; SSwap; SPop]
	int e1[] = { CST, 17, VAR, 0, VAR, 1, ADD, SWP, POP };
	printf("%d\n", eval(SIZE_OF(e1), e1));

	// [SCstI 17; SCstI 22; SCstI 100; SVar 1; SMul; SSwap; SPop; SVar 1; SAdd; SSwap; SPop]
	int e2[] = { CST, 17, CST, 22, CST, 100, VAR, 1, MUL, SWP, POP, VAR, 1, ADD, SWP, POP };
	printf("%d\n", eval(SIZE_OF(e2), e2));

	// [SCstI 5; SCstI 4; SSub; SCstI 100; SVar 1; SMul; SSwap; SPop]
	int e3[] = { CST, 5, CST, 4, SUB, CST, 100, VAR, 1, MUL, SWP, POP };
	printf("%d\n", eval(SIZE_OF(e3), e3));

	// [SCstI 20; SCstI 17; SVar 0; SCstI 2; SAdd; SSwap; SPop; SAdd; SCstI 30; SAdd]
	int e4[] = { CST, 20, CST, 17, VAR, 0, CST, 2, ADD, SWP, POP, ADD, CST, 30, ADD };
	printf("%d\n", eval(SIZE_OF(e4), e4));

	// [SCstI 2; SCstI 3; SVar 0; SCstI 4; SAdd; SSwap; SPop; SMul]
	int e5[] = { CST, 2, CST, 3, VAR, 0, CST, 4, ADD, SWP, POP, MUL };
	printf("%d\n", eval(SIZE_OF(e5), e5));

	return 0;
}