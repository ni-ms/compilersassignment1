#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/***
 *    Nikhil Sharma
 *    2020A7PS1303H
 */
// We are converting the regular expression to postfix notation in order to make it easier to convert to NFA using Thompson's algorithm.
char * regexToPostfix(char * regexp) {
    // Variables to keep track of the number of alternations and atoms
    // Atom is the smallest unit of a pattern that can be matched
    // Alternation refers to the | operator where multiple patterns can be matched
    int numberOfAlts, numberOfAtoms;

    static char buf[8000];
    char * end_ptr;

    // Parenthesis are used to group subexpressions
    struct {
        int nalt;
        int natom;
    }
            exprStack[100], * p;

    //Point to the beginning of the expression stack
    p = exprStack;
    // Point to the beginning of the buffer
    end_ptr = buf;
    // Initialize the number of alternations and atoms to 0
    numberOfAlts = 0;
    numberOfAtoms = 0;

    // If the length of the regular expression is greater than the size of the buffer, return NULL
    if (strlen(regexp) >= sizeof buf / 2)
        return NULL;

    // loop through the regular expression
    for (;* regexp; regexp++) {
        // switch on the character
        switch ( * regexp) {

            case '(':
                if (numberOfAtoms > 1) {
                    --numberOfAtoms;
                    * end_ptr++ = '.';
                }
                if (p >= exprStack + 100)
                    return NULL;
                p -> nalt = numberOfAlts;
                p -> natom = numberOfAtoms;
                p++;
                numberOfAlts = 0;
                numberOfAtoms = 0;
                break;
            case '|':
                if (numberOfAtoms == 0)
                    return NULL;
                while (--numberOfAtoms > 0)
                    *
                            end_ptr++ = '.';
                numberOfAlts++;
                break;
            case ')':
                if (p == exprStack)
                    return NULL;
                if (numberOfAtoms == 0)
                    return NULL;
                while (--numberOfAtoms > 0)
                    *
                            end_ptr++ = '.';
                for (; numberOfAlts > 0; numberOfAlts--)
                    *
                            end_ptr++ = '|';
                --p;
                numberOfAlts = p -> nalt;
                numberOfAtoms = p -> natom;
                numberOfAtoms++;
                break;
            case '*':
            case '+':
            case '?':
                if (numberOfAtoms == 0)
                    return NULL;
                * end_ptr++ = * regexp;
                break;
            default:
                if (numberOfAtoms > 1) {
                    --numberOfAtoms;
                    * end_ptr++ = '.';
                }
                * end_ptr++ = * regexp;
                numberOfAtoms++;
                break;
        }
    }

    // If the expression stack is not empty, return NULL
    if (p != exprStack)
        return NULL;

    // while there are atoms, add a dot
    while (--numberOfAtoms > 0)
        * end_ptr++ = '.';
    // while there are alternations, add a pipe
    for (; numberOfAlts > 0; numberOfAlts--)
        *end_ptr++ = '|';

    // Add a null terminator
    * end_ptr = 0;

    return buf;
}

// Match indicates that the state matches the string. Branch indicates that the createState branches to two other states.
// Match and Branch are the only special characters in the NFA.
enum {
    Match = 256,
    Branch = 257
};

// Struct to represent a state in the NFA
typedef struct State State;
struct State {
    int specialChar;
    State * nextState;
    State * branchState;
    int lastlist;
};

// The state machine is represented by a list of states. The list is terminated by a createState with specialChar == Match.
State matchstate = {Match };

int numberOfStates;

// Create a state with the given specialChar, next and branch states.
State * createState(int character, State * nState, State * bState) {
    State * s;
    numberOfStates++;
    s = malloc(sizeof * s);
    s -> lastlist = 0;
    s -> specialChar = character;
    s -> nextState = nState;
    s -> branchState = bState;
    return s;
}

// Struct to represent a fragment of a string. A fragment is a list of states that can be reached from a single state.
typedef struct fragmentOfString fragmentOfString;

// Union to represent a list of pointers to states.
typedef union listOfPointer listOfPointer;

struct fragmentOfString {
    State * start;
    listOfPointer * out;
};

// Create a fragment of a string with the given start state and list of pointers to states.
fragmentOfString getFragment(State * start, listOfPointer * out) {
    fragmentOfString n = { start, out };
    return n;
}

// Union to represent a list of pointers to states.
union listOfPointer {
    listOfPointer * next;
    State * s;
};

// Create a list of pointers to states with the given state.
listOfPointer * list1(State ** pState) {
    listOfPointer * l;
    l = (listOfPointer * ) pState;
    l -> next = NULL;
    return l;
}


// Replace the list of pointers to states with the given state.
void replacePointer(listOfPointer * ptrlist, State * s) {
    listOfPointer * next;
    for (; ptrlist; ptrlist = next) {
        next = ptrlist -> next;
        ptrlist -> s = s;
    }
}


// Append the second list of pointers to states to the first list of pointers to states.
listOfPointer * append(listOfPointer * l1, listOfPointer * l2) {
    listOfPointer * oldl1;
    oldl1 = l1;
    while (l1 -> next)
        l1 = l1 -> next;
    l1 -> next = l2;
    return oldl1;
}

// Convert the postfix regular expression to an NFA.
State * postfixToNFA(char * postfix) {
    char * p;
    fragmentOfString stack[1000], * stackp, e1, e2, e;
    State * s;
    if (postfix == NULL)
        return NULL;

    // Define the push and pop macros
#define push(s) * stackp++ = s
#define pop() * --stackp
    // Initialize the stack pointer
    stackp = stack;
    // Loop through the postfix regular expression
    for (p = postfix;* p; p++) {
        switch ( * p) {
            default:
                s = createState( * p, NULL, NULL);
                push(getFragment(s, list1(&s->nextState)));
                break;
            case '.':

                e2 = pop();
                e1 = pop();
                replacePointer(e1.out, e2.start);
                push(getFragment(e1.start, e2.out));
                break;
            case '|':
                e2 = pop();
                e1 = pop();
                s = createState(Branch, e1.start, e2.start);
                push(getFragment(s, append(e1.out, e2.out)));
                break;
            case '?':
                e = pop();
                s = createState(Branch, e.start, NULL);
                push(getFragment(s, append(e.out, list1(&s->branchState))));
                break;
            case '*':
                e = pop();
                s = createState(Branch, e.start, NULL);
                replacePointer(e.out, s);
                push(getFragment(s, list1(&s->branchState)));
                break;
            case '+':
                e = pop();
                s = createState(Branch, e.start, NULL);
                replacePointer(e.out, s);
                push(getFragment(e.start, list1(&s->branchState)));
                break;
        }
    }

    e = pop();
    if (stackp != stack)
        return NULL;
    // Add a match state to the end of the NFA
    replacePointer(e.out, &matchstate);
    return e.start;
#undef pop
#undef push
}

// adjacency list representation of an NFA
typedef struct List List;
struct List {
    State ** s;
    int n;
};


// Variables and functions used to keep track of the list of states that can be reached from a single state.
List l1, l2;
static int listid;
void addstate(List * , State * );
void traverseList(List *clist, int c, List *nlist);

// Add state to start of list
List * startlist(State * start, List * l) {
    l -> n = 0;
    listid++;
    addstate(l, start);
    return l;
}

// Check if the given list of states contains the match state
int ismatch(List * l) {
    int i;
    for (i = 0; i < l -> n; i++)
        if (l -> s[i] == & matchstate) {
            return 1;
        }
    return 0;
}

// Add the given state to the given list of states.
void addstate(List * l, State * s) {
    if (s == NULL || s -> lastlist == listid)
        return;
    s -> lastlist = listid;
    if (s -> specialChar == Branch) {
        /* follow unlabeled arrows */
        addstate(l, s -> nextState);
        addstate(l, s -> branchState);
        return;
    }
    l -> s[l -> n++] = s;

}


// traverse NFA from state list after consuming character c
void traverseList(List * clist, int c, List * nlist) {
    int i;
    State * s;

    listid++;
    nlist -> n = 0;
    for (i = 0; i < clist -> n; i++) {
        s = clist -> s[i];
        if (s -> specialChar == c) {
            addstate(nlist, s -> nextState);
        }
    }

}

// Run NFA to determine whether it matches s
int match(State * start, char * s) {
    int c;
    List * clist, * nlist, * t;

    clist = startlist(start, & l1);
    nlist = & l2;
    for (;* s; s++) {
        // Convert the character to an integer
        c = *s & 0xFF;
        // Traverse the NFA
        traverseList(clist, c, nlist);
        t = clist;
        clist = nlist;
        nlist = t;
    }
    return ismatch(clist);
}

// Function to print the output string
char * printOutput(State * start, char * text) {
    // Create an output string to return
    char * outputString = (char * ) malloc(sizeof(char) * 1000);
    outputString[0] = '\0';

    while (text != NULL) {
        //If input string is empty, break
        if (strlen(text) == 0) {
            break;
        }

        char * matchList[strlen(text) + 1];
        memset(matchList, 0, sizeof(matchList));
        int indexList[strlen(text) + 1];
        memset(indexList, 0, sizeof(indexList));
        int i;
        int breakFlag = 0;

        for (i = 0; i < strlen(text) + 1; i++) {
            char * substring = (char * ) malloc(sizeof(char) * 1000);
            strncpy(substring, text, i);
            substring[i] = '\0';
            // Print the substring

            if (match(start, substring)) {
                matchList[i] = substring;
                indexList[i] = i;
                breakFlag = 1;

                printf("<-- The matched string is %s -->\n", substring);
            } else {
                matchList[i] = NULL;

            }

        }
        if (breakFlag == 1) {
            printf("The text is %s\n", text);
            //Get index of last element in array
            int indexofelem = 0;
            for (i = strlen(text); i >= 0; i--) {
                if (matchList[i] != NULL) {
                    indexofelem = i;
                    break;
                }
            }

            char * stringToAdd = matchList[indexofelem];
            int j = indexList[indexofelem];
            //  outputString = (char *)realloc(outputString, sizeof(char) * (strlen(outputString) + strlen(stringToAdd) + 2));
            strcat(outputString, "$");
            strcat(outputString, stringToAdd);
            text = text + j;
            stringToAdd = NULL;
        } else {
            char * temp = (char * ) malloc(sizeof(char) * 1000);
            temp[0] = text[0];
            //   outputString = (char *) realloc(outputString, sizeof(char) * (strlen(outputString) + strlen(temp) + 2));
            strcat(outputString, "@");
            strcat(outputString, temp);
            text = text + 1;

            temp = NULL;

        }

        //Free the memory
        //        for(i = 1; i < strlen(text); i++){
        //            if(matchList[i] != NULL){
        //                free(matchList[i]);
        //            }
        //        }
    }
    strcat(outputString, "#");
    return outputString;

}

int main(int argc, char ** argv) {

    int i;
    char * postfixExpression;
    State * start;

    // Read regex from file
    FILE * fp = fopen("input.txt", "r");
    char * regex = NULL;
    fscanf(fp, "%ms", & regex);

    char * inputString = NULL;
    fscanf(fp, "%ms", & inputString);

    printf("The input is: %s\n", inputString);

    if (regex == NULL) {
        fprintf(stderr, "no regexp\n");
        return 1;
    }

    printf("The regex is: %s\n", regex);

    postfixExpression = regexToPostfix(regex);

    if (postfixExpression == NULL) {
        fprintf(stderr, "bad regexp %s\n", argv[1]);
        return 1;
    }

    start = postfixToNFA(postfixExpression);
    if (start == NULL) {
        fprintf(stderr, "error in postfixToNFA %s\n", postfixExpression);
        return 1;
    }

    // Adjacency list for NFA states
    l1.s = malloc(numberOfStates * sizeof l1.s[0]);
    l2.s = malloc(numberOfStates * sizeof l2.s[0]);

    // CORE FUNCTIONALITY
    FILE * fp2 = fopen("output.txt", "w");
    fprintf(fp2, "%s", printOutput(start, inputString));

    return 0;
}