#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAX_LINE 1024
#define MAX_VARS 256
#define MAX_TABLE_ITEMS 64
#define MAX_BLOCK_DEPTH 64

// ---------------- Value Types ----------------
typedef enum
{
    VAL_NIL,
    VAL_BOOL,
    VAL_INT,
    VAL_FLOAT,
    VAL_STRING,
    VAL_TABLE
} ValueType;

struct Value; // forward declare

typedef struct Table
{
    struct Value *items;
    int count;
} Table;

typedef struct Value
{
    ValueType type;
    union
    {
        int b;
        int i;
        float f;
        char *s;
        Table table;
    } as;
} Value;

// ---------------- Variables ----------------
typedef struct
{
    char name[64];
    Value value;
} Variable;

Variable variables[MAX_VARS];
int var_count = 0;

// --- Memory Management for Values ---
void free_value(Value val)
{
    if (val.type == VAL_STRING)
    {
        free(val.as.s);
    }
    else if (val.type == VAL_TABLE)
    {
        for (int i = 0; i < val.as.table.count; i++)
        {
            free_value(val.as.table.items[i]);
        }
        free(val.as.table.items);
    }
}

void set_variable(const char *name, Value val)
{
    for (int i = 0; i < var_count; i++)
    {
        if (strcmp(variables[i].name, name) == 0)
        {
            // Free the old value to prevent memory leaks before overwriting
            free_value(variables[i].value);
            variables[i].value = val;
            return;
        }
    }
    if (var_count < MAX_VARS)
    {
        strncpy(variables[var_count].name, name, 63);
        variables[var_count].name[63] = '\0';
        variables[var_count].value = val;
        var_count++;
    }
}

Value get_variable(const char *name)
{
    for (int i = 0; i < var_count; i++)
    {
        if (strcmp(variables[i].name, name) == 0)
        {
            return variables[i].value;
        }
    }
    Value nilval = {.type = VAL_NIL};
    return nilval;
}

// ---------------- Helper Functions ----------------
int is_truthy(Value val)
{
    switch (val.type)
    {
    case VAL_NIL:
        return 0;
    case VAL_BOOL:
        return val.as.b;
    case VAL_INT:
        return val.as.i != 0;
    case VAL_FLOAT:
        return val.as.f != 0.0;
    case VAL_STRING:
        return val.as.s && strlen(val.as.s) > 0;
    case VAL_TABLE:
        return val.as.table.count > 0;
    default:
        return 0;
    }
}

void value_to_string(Value val, char *buf, size_t buf_size)
{
    switch (val.type)
    {
    case VAL_STRING:
        strncpy(buf, val.as.s, buf_size - 1);
        buf[buf_size - 1] = '\0';
        break;
    case VAL_INT:
        snprintf(buf, buf_size, "%d", val.as.i);
        break;
    case VAL_FLOAT:
        snprintf(buf, buf_size, "%g", val.as.f);
        break;
    case VAL_BOOL:
        strncpy(buf, val.as.b ? "true" : "false", buf_size - 1);
        buf[buf_size - 1] = '\0';
        break;
    case VAL_NIL:
        strncpy(buf, "nil", buf_size - 1);
        buf[buf_size - 1] = '\0';
        break;
    default:
        strncpy(buf, "[table]", buf_size - 1);
        buf[buf_size - 1] = '\0';
        break;
    }
}

int values_compare(Value a, Value b, const char *op)
{
    if (a.type == VAL_STRING && b.type == VAL_STRING)
    {
        int cmp = strcmp(a.as.s, b.as.s);
        if (strcmp(op, "==") == 0)
            return cmp == 0;
        if (strcmp(op, "!=") == 0)
            return cmp != 0;
        if (strcmp(op, "<") == 0)
            return cmp < 0;
        if (strcmp(op, "<=") == 0)
            return cmp <= 0;
        if (strcmp(op, ">") == 0)
            return cmp > 0;
        if (strcmp(op, ">=") == 0)
            return cmp >= 0;
    }
    else if ((a.type == VAL_FLOAT || a.type == VAL_INT) && (b.type == VAL_FLOAT || b.type == VAL_INT))
    {
        float af = (a.type == VAL_FLOAT) ? a.as.f : (float)a.as.i;
        float bf = (b.type == VAL_FLOAT) ? b.as.f : (float)b.as.i;
        if (strcmp(op, "==") == 0)
            return af == bf;
        if (strcmp(op, "!=") == 0)
            return af != bf;
        if (strcmp(op, "<") == 0)
            return af < bf;
        if (strcmp(op, "<=") == 0)
            return af <= bf;
        if (strcmp(op, ">") == 0)
            return af > bf;
        if (strcmp(op, ">=") == 0)
            return af >= bf;
    }
    return 0;
}

// ---------------- Parser ----------------
const char *src_ptr; // Use a distinct name for the parser's source pointer
char current_char;

void next_char()
{
    if (src_ptr && *src_ptr)
        current_char = *src_ptr++;
    else
        current_char = '\0';
}

void skip_whitespace()
{
    while (current_char == ' ' || current_char == '\t')
        next_char();
}

Value parse_expression_precedence(int min_precedence); // Fwd declare

Value parse_string()
{
    char buf[MAX_LINE];
    int i = 0;
    while (current_char && current_char != '"' && i < MAX_LINE - 1)
    {
        buf[i++] = current_char;
        next_char();
    }
    buf[i] = '\0';
    if (current_char == '"')
        next_char();
    return (Value){
        .type = VAL_STRING, .as.s = strdup(buf)};
}

Value parse_number()
{
    char buf[128];
    int i = 0;
    while ((isdigit(current_char) || current_char == '.' || (i == 0 && current_char == '-')) && i < 127)
    {
        buf[i++] = current_char;
        next_char();
    }
    buf[i] = '\0';
    if (strchr(buf, '.'))
        return (Value){
            .type = VAL_FLOAT, .as.f = atof(buf)};
    return (Value){
        .type = VAL_INT, .as.i = atoi(buf)};
}

Value parse_primary()
{
    skip_whitespace();
    if (current_char == '(')
    {
        next_char();
        Value val = parse_expression_precedence(0);
        if (current_char == ')')
            next_char();
        return val;
    }
    if (current_char == '"')
    {
        next_char();
        return parse_string();
    }
    if (isdigit(current_char) || (current_char == '-' && isdigit(*src_ptr)))
    {
        return parse_number();
    }
    if (isalpha(current_char))
    {
        char name[64];
        int i = 0;
        while (isalnum(current_char) || current_char == '_')
        {
            name[i++] = current_char;
            next_char();
        }
        name[i] = '\0';
        if (strcmp(name, "true") == 0)
            return (Value){
                .type = VAL_BOOL, .as.b = 1};
        if (strcmp(name, "false") == 0)
            return (Value){
                .type = VAL_BOOL, .as.b = 0};
        if (strcmp(name, "nil") == 0)
            return (Value){
                .type = VAL_NIL};
        return get_variable(name);
    }
    if (current_char == '{')
    {
        Value val;
        val.type = VAL_TABLE;
        val.as.table.count = 0;
        val.as.table.items = malloc(sizeof(Value) * MAX_TABLE_ITEMS);

        next_char(); // skip '{'
        skip_whitespace();
        while (current_char && current_char != '}')
        {
            if (val.as.table.count >= MAX_TABLE_ITEMS)
                break;
            val.as.table.items[val.as.table.count++] = parse_expression_precedence(0);
            skip_whitespace();
            if (current_char == ',')
            {
                next_char();
                skip_whitespace();
            }
        }
        if (current_char == '}')
            next_char(); // skip '}'
        return val;
    }
    return (Value){
        .type = VAL_NIL};
}

int get_precedence(char op)
{
    switch (op)
    {
    case '*':
    case '/':
    case '%':
        return 2;
    case '+':
    case '-':
        return 1;
    default:
        return 0;
    }
}

Value parse_math_operator(Value left, char op, Value right)
{
    if ((left.type != VAL_INT && left.type != VAL_FLOAT) || (right.type != VAL_INT && right.type != VAL_FLOAT))
    {
        fprintf(stderr, "Warning: arithmetic on non-number.\n");
        free_value(left);
        free_value(right);
        return (Value){
            .type = VAL_NIL};
    }
    Value result;
    if (left.type == VAL_FLOAT || right.type == VAL_FLOAT || op == '/')
    {
        float a = (left.type == VAL_INT) ? (float)left.as.i : left.as.f;
        float b = (right.type == VAL_INT) ? (float)right.as.i : right.as.f;
        result.type = VAL_FLOAT;
        switch (op)
        {
        case '+':
            result.as.f = a + b;
            break;
        case '-':
            result.as.f = a - b;
            break;
        case '*':
            result.as.f = a * b;
            break;
        case '/':
            result.as.f = b != 0.0f ? a / b : 0.0f;
            break;
        case '%':
            result.as.f = b != 0.0f ? fmod(a, b) : 0.0f;
            break;
        }
    }
    else
    {
        result.type = VAL_INT;
        int a = left.as.i;
        int b = right.as.i;
        switch (op)
        {
        case '+':
            result.as.i = a + b;
            break;
        case '-':
            result.as.i = a - b;
            break;
        case '*':
            result.as.i = a * b;
            break;
        case '%':
            result.as.i = b != 0 ? a % b : 0;
            break;
        }
    }
    free_value(left);
    free_value(right);
    return result;
}

Value parse_expression_precedence(int min_precedence)
{
    Value left = parse_primary();
    skip_whitespace();
    while (strchr("+-*/%", current_char))
    {
        char op = current_char;
        int prec = get_precedence(op);
        if (prec < min_precedence)
            break;
        next_char();
        skip_whitespace();
        Value right = parse_expression_precedence(prec + 1);
        left = parse_math_operator(left, op, right);
        skip_whitespace();
    }

    // Handle concatenation
    // Handle string concatenation (..)
    while (current_char == '.' && *src_ptr == '.')
    {
        next_char(); // skip first '.'
        next_char(); // skip second '.'
        skip_whitespace();

        Value right = parse_expression_precedence(3); // higher than math

        char buf1[256], buf2[256];
        value_to_string(left, buf1, sizeof(buf1));
        value_to_string(right, buf2, sizeof(buf2));

        char *joined = malloc(strlen(buf1) + strlen(buf2) + 1);
        strcpy(joined, buf1);
        strcat(joined, buf2);

        free_value(left);
        free_value(right);

        left.type = VAL_STRING;
        left.as.s = joined;
        skip_whitespace();
    }

    return left;
}

Value evaluate_expression(const char *expr_str)
{
    src_ptr = expr_str;
    next_char();
    return parse_expression_precedence(0);
}

void print_value(Value val)
{
    switch (val.type)
    {
    case VAL_NIL:
        printf("nil\n");
        break;
    case VAL_BOOL:
        printf(val.as.b ? "true\n" : "false\n");
        break;
    case VAL_INT:
        printf("%d\n", val.as.i);
        break;
    case VAL_FLOAT:
        printf("%g\n", val.as.f);
        break;
    case VAL_STRING:
        printf("%s\n", val.as.s ? val.as.s : "");
        break;
    case VAL_TABLE:
        printf("{");
        for (int i = 0; i < val.as.table.count; i++)
        {
            char buf[256];
            value_to_string(val.as.table.items[i], buf, sizeof(buf));
            printf("%s", buf);
            if (i < val.as.table.count - 1)
                printf(", ");
        }
        printf("}\n");
        break;
    }
}
// ---------------- Runner ----------------
// NEW: Structure to hold the script in memory
typedef struct
{
    char **lines;
    int count;
} Script;

// NEW: Helper to find the matching `else` or `end` for an `if` or `while`
int find_match(Script *script, int start_pc)
{
    int depth = 1;
    int pc = start_pc + 1;
    char keyword[10];
    sscanf(script->lines[start_pc], "%s", keyword);

    while (pc < script->count)
    {
        char line_keyword[10] = "";
        sscanf(script->lines[pc], "%s", line_keyword);
        if (strcmp(line_keyword, keyword) == 0)
        {
            // nested block of same type
            depth++;
        }
        else if (strcmp(line_keyword, "end") == 0)
        {
            depth--;
            if (depth == 0)
                return pc;
        }
        else if (strcmp(keyword, "if") == 0 && strcmp(line_keyword, "else") == 0)
        {
            if (depth == 1)
                return pc;
        }
        pc++;
    }
    return pc - 1; // Should not be reached with well-formed code
}

// NEW: The main execution engine for running scripts
void execute_script(Script *script)
{
    int pc = 0; // Program Counter
    int loop_starts[MAX_BLOCK_DEPTH];
    int loop_stack_top = -1;

    while (pc < script->count)
    {
        char *line = script->lines[pc];
        char keyword[64] = "";
        sscanf(line, " %s", keyword);

        if (strcmp(keyword, "print") == 0)
        {
            Value val = evaluate_expression(line + 5);
            print_value(val);
            free_value(val);
        }
        else if (strcmp(keyword, "var") == 0)
        {
            char name[64];
            sscanf(line, "var %s", name);
            char *value_part = strchr(line, '=');
            if (value_part)
            {
                Value val = evaluate_expression(value_part + 1);
                set_variable(name, val);
            }
        }
        else if (strcmp(keyword, "if") == 0)
        {
            char *condition_str = line + 2;
            Value cond_val = evaluate_expression(condition_str);
            if (!is_truthy(cond_val))
            {
                pc = find_match(script, pc); // Skip to else or end
            }
            free_value(cond_val);
        }
        else if (strcmp(keyword, "else") == 0)
        {
            // If we hit an else, it means the `if` was true, so we skip to end
            pc = find_match(script, pc);
        }
        else if (strcmp(keyword, "while") == 0)
        {
            if (loop_stack_top < MAX_BLOCK_DEPTH - 1)
            {
                loop_starts[++loop_stack_top] = pc;
            }
            char *condition_str = line + 5;
            Value cond_val = evaluate_expression(condition_str);
            if (!is_truthy(cond_val))
            {
                pc = find_match(script, pc); // Skip to end of while loop
                loop_stack_top--;            // Pop this loop from the stack
            }
            free_value(cond_val);
        }
        else if (strcmp(keyword, "end") == 0)
        {
            if (loop_stack_top >= 0)
            {
                // Check if the block we are ending was a while loop
                char loop_keyword[10];
                sscanf(script->lines[loop_starts[loop_stack_top]], "%s", loop_keyword);
                if (strcmp(loop_keyword, "while") == 0)
                {
                    pc = loop_starts[loop_stack_top] - 1; // Jump back to re-evaluate while
                }
                else
                {
                    loop_stack_top--; // It was an if, just pop it
                }
            }
        }
        pc++;
    }
}

void run_file(const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (!file)
    {
        printf("Error: Could not open %s\n", filename);
        return;
    }

    Script script = {.lines = NULL, .count = 0};
    int capacity = 10;
    script.lines = malloc(sizeof(char *) * capacity);

    char line_buf[MAX_LINE];
    while (fgets(line_buf, MAX_LINE, file))
    {
        if (script.count >= capacity)
        {
            capacity *= 2;
            script.lines = realloc(script.lines, sizeof(char *) * capacity);
        }
        script.lines[script.count++] = strdup(line_buf);
    }
    fclose(file);

    execute_script(&script);

    // --- Cleanup ---
    int i;
    for (i = 0; i < script.count; i++)
        free(script.lines[i]);

    free(script.lines);
    for (int i = 0; i < var_count; i++)
        free_value(variables[i].value);
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        printf("Usage: arlang <file.arl>\n");
        return 1;
    }
    run_file(argv[1]);
    return 0;
}

// send help, my coding language aint wroking
// i tried to run my file and it leaves blank :(