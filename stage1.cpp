// Adian Acosta
// CS 4301
// Stage 1

#include <stage1.h>
#include <iomanip>
#include <set>
#include <ctime>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

Compiler::Compiler(char **argv) // constructor
{
    sourceFile.open(argv[1]);
    listingFile.open(argv[2]);
    objectFile.open(argv[3]);
}

Compiler::~Compiler() // destructor
{
    if (sourceFile.is_open())
        sourceFile.close();
    if (listingFile.is_open())
        listingFile.close();
    if (objectFile.is_open())
        objectFile.close();
}

void Compiler::createListingHeader()
{
    time_t now = time (NULL);
    listingFile << "STAGE1: " << " Adian Acosta       " << ctime(&now) << endl;
    listingFile << left << setw(22) << "LINE NO." << "SOURCE STATEMENT\n\n";
}

void Compiler::parser()
{
    nextChar();
    // ch must be initilized to the first character of the source file
    if (nextToken() != "program")
        processError("keyword \"program\" expected");
    //a call to nextToken() has two effects
    // (1) the variable, token, is assigned the value of the next token
    // (2) the next token is read from the source file in order to make
    // the assignment. The value returned by nextToken() is also
    // the next token.
    prog();
    // parser implements the grammar rules, calling first rule
}

void Compiler::createListingTrailer()
{
    if (errorCount > 0)
        listingFile << endl << "COMPILATION TERMINATED      " << errorCount << " ERROR ENCOUNTERED" << endl;
    else
        listingFile << "COMPILATION TERMINATED      " << errorCount << " ERRORS ENCOUNTERED" << endl;
}

void Compiler::processError(string err)
{
    listingFile << endl << "Error: Line " << lineNo << ": " << err << endl;
    errorCount++;
    createListingTrailer();
    exit(EXIT_FAILURE);
}

void Compiler::freeTemp() {
	currentTempNo--;
	if (currentTempNo < -1) {
		processError("compiler error, currentTempNo should be >= -1");
	}
}

string Compiler::getTemp()
{
	string temp;
	currentTempNo++;
	temp = "T" + to_string(currentTempNo);
	if (currentTempNo > maxTempNo) {
		insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
		maxTempNo++;
	}
	return temp;
}

string Compiler::getLabel()
{
    static int lCount = 0;
    int counter = 0;
    counter = lCount;
    lCount++;
    return ".L" + to_string(counter);
}

bool Compiler::isTemporary(string s) const // determines if s represents a temporary
{
	return s[0] == 'T';
}

// GRAMMAR RULES FUNCTIONS **********************************************************************

void Compiler::prog()   // token should be "program"
{
    if (token != "program")
        processError("keyword \"program\" expected");
    progStmt();
    if (token == "const")
        consts();
    if (token == "var")
        vars();
    if (token != "begin")
        processError("keyword \"begin\" expected");
    beginEndStmt();
    if (token != "$")
        processError("no text may follow \"end\"");
}

void Compiler::progStmt() // token should be "program"
{
    string x;
    if (token != "program")
        processError("keyword \"program\" expected");
    
    x = nextToken();
    if (!isNonKeyId(x))
        processError("program name expected");
    
    if (nextToken() != ";")
        processError("semicolon expected");
    
    nextToken();
    code("program", x);
    insert(x, PROG_NAME, CONSTANT, x, NO, 0);
}

void Compiler::consts() //token should be "const"
{
    if (token != "const") 
        processError("Keyword \"const\" expected");
    
    if (!isNonKeyId(nextToken()))
        processError("non-keyword identifier must follow \"const\"");
    
    constStmts();
}

void Compiler::vars()   //token should be "var"
{
    if (token != "var")
        processError("Keyword \"var\" expected");
    
    if (!isNonKeyId(nextToken()))
        processError("non-keyword identifier must follow \"var\"");
    
    varStmts();
}

void Compiler::beginEndStmt()   //token should be "begin"
{
    if (token != "begin")
        processError("Keyword \"begin\" expected");
    
    nextToken();
    if (token == "read" || token == "write" || isNonKeyId(token))
        execStmts();
    
    if (token != "end")
        processError("Keyword \"end\" expected");
    
    if (nextToken() != ".")
        processError("period expected");
    
    nextToken();
    code("end", ".");
}

void Compiler::constStmts()     //token should be NON_KEY_ID
{
    string x, y, z, next;
    
    if (!isNonKeyId(token))
        processError("non-keyword identifier expected");
    
    x = token;

    if (nextToken() != "=")
        processError("\"=\" expected");
    
    y = nextToken();
    
    if (!(y == "+" || y == "-" || y == "not" || isInteger(y) || y == "true" || y == "false" || isNonKeyId(y)))
        processError("Token to right of \"=\" illegal");
    
    if (y == "+" || y == "-") {
        if (!isInteger(nextToken())){
            processError("integer expected after sign");
        }
        y += token;
    }
    
    if (y == "not") {
        next = nextToken();
        
        if (!isNonKeyId(next) && !isBoolean(next)){
            processError("boolean or non-keyword identifier expected after \"not\"");
        }
        
        y = (next == "true") ? "false" : "true";
    }
    
    if (nextToken() != ";")
        processError("semicolon expected");
    
    if (isBoolean(y))
        z = (y == "true" || y == "yes") ? "-1" : "0";                                                   // TEST AGAINST THIS 
    else
        z = y;
    
    insert(x, whichType(y), CONSTANT, whichValue(z), YES, 1);
    
    x = nextToken();
    
    if (!(x == "begin" || x == "var" || isNonKeyId(x)))
        processError("non-keyword identifier, \"begin\", or \"var\" expected");
    
    if (isNonKeyId(x))
        constStmts();
}

void Compiler::varStmts()   //token should be NON_KEY_ID
{
    string x, y;
    if (!isNonKeyId(token))
        processError("non-keyword identifier expected");
    
    x = ids();
    if (token != ":")
        processError("\":\" expected");
    
    y = nextToken();
    
    if (!(isInteger(y) || isBoolean(y)))
        processError("illegal type follows \":\"");
    
    if (nextToken() != ";")
        processError("semicolon expected");
    
    insert(x, whichType(y), VARIABLE, "", YES, 1);
    y = nextToken();
    
    if (!(y == "begin" || isNonKeyId(y)))
        processError("non-keyword identifier or \"begin\" expected");
    
    if (isNonKeyId(token))
        varStmts();
}

string Compiler::ids()      //token should be NON_KEY_ID
{
    string temp, tempString;
    if (!isNonKeyId(token))
        processError("non-keyword identifier expected");
    
    tempString = token;
    temp = token;
    if (nextToken() == ",") {
        if (!isNonKeyId(nextToken()))
            processError("non-keyword identifier expected");
        
        tempString = temp + "," + ids();
    }
    return tempString;
}

void Compiler::execStmts()
{
    while (token != "end") {
        execStmt();
    }
}

void Compiler::execStmt()
{
    if (token == "read") {
        readStmt();
    } else if (token == "write") {
        writeStmt();
    } else if (isNonKeyId(token)) {
        assignStmt();
    } else {
        processError("invalid statement");
    }
}

void Compiler::assignStmt()
{
    if (!isNonKeyId(token))
        processError("non-keyword identifier expected");
    
    pushOperand(token);
    
    if (nextToken() != ":=")
        processError("\":=\" expected");
    pushOperator(token);
    nextToken();
    express();
    if (token != ";"){
        processError("one of \"*\", \"and\", \"div\", \"mod\", \")\", \"+\", \"-\", \";\", \"<\", \"<=\", \"<>\", \"=\", \">\", \">=\", or \"or\" expected");
    }
    
    string rhs = popOperand();
    string lhs = popOperand();
    string op = popOperator();
    code(op, rhs, lhs);
    nextToken();
}

void Compiler::readStmt()
{
    if (token != "read")
        processError("read expected");
    
    if (nextToken() != "(")
        processError("\"(\" expected");
    
    nextToken();
    string x = ids();
    code("read", x);
    if (token != ")"){
        processError("\")\" expected");
    }
    
    if (nextToken() != ";")
        processError("semicolon expected");
    
    nextToken();
    
}

void Compiler::writeStmt()
{
    if (token != "write") {
        processError("write expected");
    }
    
    if (nextToken() != "(")
        processError("\"(\" expected");
    
    nextToken();
    string x = ids();
    code("write", x);
    if (token != ")"){
        processError("\")\" expected");
    }
    
    if (nextToken() != ";")
        processError("semicolon expected");
    
    nextToken();
    
}

void Compiler::express()
{
    term();
    expresses();
}

void Compiler::expresses() 
{
    // REL_OP includes <, <=, >, >=, =, and <>
    if (token == "<" || token == "<=" || token == ">" || token == ">=" || token == "=" || token == "<>") {
        pushOperator(token);
        nextToken();
        term();
        string rhs = popOperand();
        string lhs = popOperand();
        string op = popOperator();
        code(op, rhs, lhs);
        expresses();
    }
    // Else, epsilon (empty string) – do nothing for expresses
}

void Compiler::term()
{
    factor();
    terms();
}

void Compiler::terms() 
{
    // TERMS -> ADD_LEVEL_OP FACTOR TERMS | ε
    if (token == "+" || token == "-" || token == "or") {
        pushOperator(token);
        nextToken();
        factor();
        string rhs = popOperand();
        string lhs = popOperand();
        string op = popOperator();
        code(op, rhs, lhs);
        terms();
    }
    // Else, ε (empty string) – do nothing
}

void Compiler::factor()
{
    part();
    factors();
}

void Compiler::factors() 
{
    // FACTORS -> MULT_LEV_OP PART FACTORS | ε
    if (token == "*" || token == "div" || token == "mod" || token == "and") {
        pushOperator(token);
        nextToken();
        part();
        string rhs = popOperand();
        string lhs = popOperand();
        string op = popOperator();
        code(op, rhs, lhs);
        factors();
    }
    // Else, ε (empty string) – do nothing
}

void Compiler::part() 
{
    string y, next;
    y = token;
    
    if (!(y == "not" || y == "+" || y == "-" || y == "(" || isLiteral(y) || isNonKeyId(y))) {
        processError("error within part()");
    }
    
    if (y == "(") {
        nextToken();
        express();
        if (token != ")") {
            processError("\")\" expected");
        }
    } else if (y == "not") {
        next = nextToken();
        if (!(isNonKeyId(next) || isBoolean(next) || next == "(")) {
            processError("boolean or non-keyword identifier or \"(\" expected after \"not\"");
        }
        if (next == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("\")\" expected");
            }
            string operand = popOperand();
            code("not", operand);
        } else if (isBoolean(next)) {
            y = (next == "true") ? "false" : "true";
            if (symbolTable.count(y) == 0) {
                if (y == "true") {
                    SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
                    symbolTable.insert(std::make_pair("true", entry));
                }
                if (y == "false") {
                    SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
                    symbolTable.insert(std::make_pair("false", entry));
                }
            }
            pushOperand(y);
        } else {
            code("not", next);
        }
    } else if (y == "+") {
        next = nextToken();
        
        if (!isNonKeyId(next) && whichType(next) != INTEGER && next != "(" ) {
            processError("expected \'(\', integer, or non-keyword id; found " + next);  
        }
        if (next == "+" || next == "-" || next == "*")
            processError("expected \'(\', integer, or non-keyword id; found " + next); 

        if (next == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("\")\" expected");
            }
        } else {
            y = next;
            if (symbolTable.count(y) == 0) {
                insert(y, whichType(y), CONSTANT, whichValue(y), YES, 1);
            }
            pushOperand(y);
        }
    } else if (y == "-") {
        next = nextToken();
        if (!isNonKeyId(next) && whichType(next) != INTEGER && next != "(" ) {
            processError("expected \'(\', integer, or non-keyword id; found " + next);  
        }
        if (next == "(") {
            nextToken();
            express();
            if (token != ")") {
                processError("\")\" expected");
            }
            string operand = popOperand();
            code("neg", operand);
        } else if (isInteger(next)) {
            y += next;
            if (symbolTable.count(y) == 0) {
                insert(y, whichType(y), CONSTANT, whichValue(y), YES, 1);
            }
            pushOperand(y);
        } else {
            code("neg", next);
        }
    } else {
        if (isInteger(y)) {
            if (symbolTable.count(y) == 0) {
                insert(y, whichType(y), CONSTANT, whichValue(y), YES, 1);
            }
            pushOperand(y);
        } else {
            if (symbolTable.count(y) == 0) {
                if (y == "true") {
                    SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
                    symbolTable.insert(std::make_pair("true", entry));
                }
                if (y == "false") {
                    SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
                    symbolTable.insert(std::make_pair("false", entry));
                }
            }
            pushOperand(y);
        }
    }
    
    nextToken();
}

// ACTION ROUTINES FUNCTIONS ********************************************************************

void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits)
{
    replace(externalName.begin(), externalName.end(), ',', ' ');
    
    stringstream ss(externalName);
    string name;
    vector<string> names;
    
    while (ss >> name) {
        if (name.size() > 15) {
            name.resize(15);
        }
        names.push_back(name);
    }
    
    for (auto i : names) {
        if (symbolTable.count(i)) {
            processError("symbol " + i + " is multiply defined");
        } else if (isKeyword(i)) {
            processError("illegal use of keyword");
        } else {    // create table entry
            if (isupper(i[0])) {
                SymbolTableEntry entry(i, inType, inMode, inValue, inAlloc, inUnits);
                symbolTable.insert(std::make_pair(i, entry));
            } else {
                SymbolTableEntry entry(genInternalName(inType), inType, inMode, inValue, inAlloc, inUnits);
                symbolTable.insert(std::make_pair(i, entry));
            }
        }
    }
}

storeTypes Compiler::whichType(string name)     // tells which data type a name has
{
    storeTypes dataType;
    
    if (isLiteral(name)) {
        if (isBoolean(name)) {
            dataType = BOOLEAN;
        } else {
            dataType = INTEGER;
        }
    } else {    // name is an identifier and hopefully a constant
        if (symbolTable.count(name)) {
            dataType = symbolTable.at(name).getDataType();
        }
        else
            processError("reference to undefined symbol " + name);
    }
    return dataType;
}

string Compiler::whichValue(string name)       // tells which value a name has
{ 
    string value;
    
    if (isLiteral(name)) {
        value = name;
    } else {
        if (symbolTable.count(name) > 0 && !symbolTable.at(name).getValue().empty()) {
            value = symbolTable.at(name).getValue();
        } else {
            processError("reference to undefined symbol " + name);
        }
    }
    
    return value;
}
//                                       rhs               lhs
void Compiler::code(string op, string operand1, string operand2)
{
    if (op == "program")
        emitPrologue(operand1);
    else if (op == "end")
        emitEpilogue();
    else if (op == "read")
        emitReadCode(operand1);
    else if (op == "write")
        emitWriteCode(operand1);
    else if (op == "+")
        emitAdditionCode(operand1, operand2); // this must be binary
    else if (op == "-")
        emitSubtractionCode(operand1, operand2); // this must be binary
    else if (op == "neg")
        emitNegationCode(operand1);  // this must be unary
    else if (op == "not")
        emitNotCode(operand1);
    else if (op == "*")
        emitMultiplicationCode(operand1, operand2); 
    else if (op == "div")
        emitDivisionCode(operand1, operand2);
    else if (op == "mod")
        emitModuloCode(operand1, operand2);
    else if (op == "and")
        emitAndCode(operand1, operand2);
    else if (op == "or")
        emitOrCode(operand1, operand2);
    else if (op == "=")
        emitEqualityCode(operand1, operand2);
    else if (op == "<>")
        emitInequalityCode(operand1, operand2);
    else if (op == "<")
        emitLessThanCode(operand1, operand2);
    else if (op == "<=")
        emitLessThanOrEqualToCode(operand1, operand2);
    else if (op == ">")
        emitGreaterThanCode(operand1, operand2);
    else if (op == ">=")
        emitGreaterThanOrEqualToCode(operand1, operand2);
    else if (op == ":=")
        emitAssignCode(operand1, operand2);
    else
        processError("compiler error since function code should not be called with illegal arguments");
}

void Compiler::pushOperator(string name) // push name onto operatorStk
{
	operatorStk.push(name);
}

void Compiler::pushOperand(string name) // push name onto operandStk
{
	if (isLiteral(name) && symbolTable.count(name) == 0) {
		insert(name, whichType(name), VARIABLE, whichValue(name), YES, 1);
	}
	operandStk.push(name);
}

string Compiler::popOperator() // pop name from operatorStk
{
	string op;
	if (!operatorStk.empty()) {
		op = operatorStk.top();
		operatorStk.pop();
	} else {
		processError("compiler error; operator stack underflow");
	}
	return op;
}

string Compiler::popOperand() // pop name from operandStk
{
	string operand;
	if (!operandStk.empty()) {
		operand = operandStk.top();
		operandStk.pop();
	} else {
		processError("compiler error; operand stack underflow");
	}
	return operand;
}

// EMIT FUNCTIONS ********************************************************

void Compiler::emit(string label, string instruction, string operands, string comment)
{
    objectFile << left;
    objectFile << setw(8) << label;
    objectFile << setw(8) << instruction;
    objectFile << setw(24) << operands;
    objectFile << comment << endl;
}

void Compiler::emitPrologue(string progName, string operand2)
{
    if (progName.size() > 15)
        progName.resize(15);
    
    time_t now = time (NULL);
    objectFile << "; Adian Acosta       " << ctime(&now);
    objectFile << "%INCLUDE \"Along32.inc\"" << endl;
    objectFile << "%INCLUDE \"Macros_Along.inc\"" << endl << endl;
    
    emit("SECTION", ".text");
    emit("global", "_start", "", "; program " + progName + "\n");
    emit("_start:");
}

void Compiler::emitEpilogue(string operand1, string operand2)
{
    emit("", "Exit", "{0}", "\n");
    emitStorage();
}

void Compiler::emitStorage()
{
    string y;
    emit("SECTION", ".data");
    for (const auto& pair : symbolTable) {
        const SymbolTableEntry& entry = pair.second;

        if (entry.getMode() == CONSTANT && entry.getAlloc() == YES) {
            
            emit(entry.getInternalName(), "dd", entry.getValue(), "; " + pair.first);
        }
    }
    
    objectFile << endl;
    
    emit("SECTION", ".bss");
    for (const auto& pair : symbolTable) {
        const SymbolTableEntry& entry = pair.second;

        if (entry.getMode() == VARIABLE && entry.getAlloc() == YES) {    
            
            emit(entry.getInternalName(), "resd", "1", "; " + pair.first);
        }
    }
    
}

void Compiler::emitReadCode(string operand, string op)
{
    replace(operand.begin(), operand.end(), ',', ' ');
    
    stringstream ss(operand);
    string name;
    vector<string> names;
    
    while (ss >> name) {
        if (name.size() > 15) {
            name.resize(15);
        }
        names.push_back(name);
    }
    
    for (auto i : names) {
        if (symbolTable.count(i) == 0) {
            processError("reference to undefined symbol");
        }
        if (whichType(i) != INTEGER) {
            listingFile << "\n";
            processError("can't read variables of this type");
        }
        emit("", "call", "ReadInt", "; read int; value placed in eax");
        emit("", "mov", "[" + symbolTable.at(i).getInternalName() + "],eax", "; store eax at " + i);
        contentsOfAReg = i;
    }
}

void Compiler::emitWriteCode(string operand, string op)
{
    replace(operand.begin(), operand.end(), ',', ' ');
    
    stringstream ss(operand);
    string name;
    vector<string> names;
    
    while (ss >> name) {
        if (name.size() > 15) {
            name.resize(15);
        }
        names.push_back(name);
    }
    
    for (auto i : names) {
        if (symbolTable.count(i) == 0) {
            processError("reference to undefined symbol");
        }
        if (contentsOfAReg != i) {
            emit("", "mov", "eax,[" + symbolTable.at(i).getInternalName() + "]", "; load " + i + " in eax");
            contentsOfAReg = i;
        }
        if (symbolTable.at(i).getDataType() == INTEGER || symbolTable.at(i).getDataType() == BOOLEAN) {
            emit("", "call", "WriteInt", "; write int in eax to standard out");
        }
        emit("", "call", "Crlf", "; write \\r\\n to standard out");
    }
}

void Compiler::emitAssignCode(string operand1, string operand2) // assign the value of operand1 to operand2
{
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \':=\'");
    }
    if (symbolTable.at(operand2).getMode() != VARIABLE) {
        processError("symbol on left-hand side of assignment must have a storage mode of VARIABLE");
    }
    if (operand1 == operand2) {
        return;
    }
    if (contentsOfAReg != operand1) {
        emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
    }
    emit("", "mov", "[" + symbolTable.at(operand2).getInternalName() + "],eax", "; " + operand2 + " = AReg");
    contentsOfAReg = operand2;
    if (isTemporary(operand1)) {
        freeTemp();
    }
}

void Compiler::emitAdditionCode(string operand1, string operand2) // add operand1 to operand2
{
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary \'+\' requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "add", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " + " + operand2);
    } else {
        emit("", "add", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " + " + operand1);
    }
    
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitSubtractionCode(string operand1, string operand2)    // op2 -  op1
{
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary \'-\' requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 || contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 || contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    emit("", "sub", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " - " + operand1);
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitMultiplicationCode(string operand1, string operand2) // op2 *  op1
{
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary \'*\' requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }  

    if (contentsOfAReg == operand1) {
        emit("", "imul", "dword [" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " * " + operand2);
    } else {
        emit("", "imul", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " * " + operand1);
    }
    
    //emit("", "imul", "dword [" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2 + " * " + operand1);
    
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitDivisionCode(string operand1, string operand2)       // op2 /  op1
{
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary \'div\' requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    emit("", "cdq", "", "; sign extend dividend from eax to edx:eax");
    emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitModuloCode(string operand1, string operand2)         // op2 %  op1
{
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary \'mod\' requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    emit("", "cdq", "", "; sign extend dividend from eax to edx:eax");
    emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
    emit("", "xchg", "eax,edx", "; exchange quotient and remainder");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitNegationCode(string operand1, string operand2)           // -op1
{
    if (whichType(operand1) != INTEGER) {
        processError("unary \"-\" requires integer operands");
    }
    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
        contentsOfAReg = "";
    }
    if (operand1 != contentsOfAReg) {
        emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
    }
    
    emit("", "neg", "eax", "; AReg = -AReg");

    if (isTemporary(operand1))
        freeTemp();

    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}

void Compiler::emitNotCode(string operand1, string operand2)                // !op1
{
    if (whichType(operand1) != BOOLEAN) {
        processError("unary \"<>\" requires boolean operands");
    }
    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
        contentsOfAReg = "";
    }
    if (operand1 != contentsOfAReg) {
        emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
    }
    
    emit("", "not", "eax", "; AReg = !AReg");

    if (isTemporary(operand1))
        freeTemp();

    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitAndCode(string operand1, string operand2) // and operand1 to operand2
{
    if (whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN) {
        processError("binary \'and\' requires boolean operands");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "and", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " and " + operand2);
    } else {
        emit("", "and", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " and " + operand1);
    }
    
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitOrCode(string operand1, string operand2)             // op2 || op1
{
    if (whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN) {
        processError("binary \"or\" requires boolean operands");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "or", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " or " + operand2);
    } else {
        emit("", "or", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " or " + operand1);
    }
    
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitEqualityCode(string operand1, string operand2)       // op2 == op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'=\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "je", x, "; if " + operand2 + " = " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitInequalityCode(string operand1, string operand2)     // op2 != op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'<>\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "jne", x, "; if " + operand2 + " <> " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitLessThanCode(string operand1, string operand2)       // op2 <  op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'<\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "jl", x, "; if " + operand2 + " < " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitLessThanOrEqualToCode(string operand1, string operand2) // op2 <= op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'<=\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "jle", x, "; if " + operand2 + " <= " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitGreaterThanCode(string operand1, string operand2)    // op2 >  op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'>\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "jg", x, "; if " + operand2 + " > " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

void Compiler::emitGreaterThanOrEqualToCode(string operand1, string operand2) // op2 >= op1
{
    string x, y;
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator \'>=\'");
    }
    if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        emit("", "mov", "[" + symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }
    if (!isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
        contentsOfAReg = "";
    }
    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }
    
    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }
    
    x = getLabel();
    emit("", "jge", x, "; if " + operand2 + " >= " + operand1 + " then jump to set eax to TRUE");
    emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
    if (symbolTable.count("false") == 0) {
        SymbolTableEntry entry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1);
        symbolTable.insert(std::make_pair("false", entry));
    }
    y = getLabel();
    emit("", "jmp", y, "; unconditionally jump");
    emit(x + ":");
    emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
    if (symbolTable.count("true") == 0) {
        SymbolTableEntry entry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1);
        symbolTable.insert(std::make_pair("true", entry));
    }
    emit(y + ":");
    if (isTemporary(operand1))
        freeTemp();
    if (isTemporary(operand2))
        freeTemp();
    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}

// LEXICAL ROUTINES ******************************************************************

string Compiler::nextToken()    // returns the next token or end of file marker
{
    token = "";
    while (token == "")
    {
        if (isspace(ch)) { // Skip whitespace
            ch = nextChar();
            continue;
        }
        
        if (ch == '{') { // Process comment
            while ((ch = nextChar()) != END_OF_FILE && ch != '}') {
                // Empty body
            }
            if (ch == END_OF_FILE) {
                processError("unexpected end of file");
            } else {
                ch = nextChar(); // Move past closing '}'
            }
            continue;
        }

        if (ch == '}') {
            processError("'}' cannot begin token");
            ch = nextChar();
            continue;
        }

        if (isSpecialSymbol(ch)) {
            token = ch;
            ch = nextChar();
            
            if ((token == ":" && ch == '=') || (token == "<" && ch == '=') || (token == ">" && ch == '=') || (token == "<" && ch == '>')) {
                token += ch;
                ch = nextChar();
            }
            
            continue;
        }

        if (islower(ch)) { // Start of an identifier
            token = ch;
            ch = nextChar();
            while ((isalnum(ch) || ch == '_') && ch != END_OF_FILE) {
                if (isupper(ch)) 
                    processError("illegal symbol");
                
                token += ch;
                ch = nextChar();
            }
            if (ch == END_OF_FILE) {
                processError("unexpected end of file");
            }
            continue;
        }

        if (isdigit(ch)) { // Start of a number
            token = ch;
            ch = nextChar();
            while (isdigit(ch) && ch != END_OF_FILE) {
                token += ch;
                ch = nextChar();
            }
            if (ch == END_OF_FILE) {
                processError("unexpected end of file");
            }
            continue;
        }

        if (ch == '.') {
            token = ch;
            ch = nextChar();
            break;
        }

        if (ch == END_OF_FILE) {
            token = ch;
            return token;
        }

        // Handle illegal symbols
        processError("illegal symbol");
        ch = nextChar();
    }

    return token;
}

char Compiler::nextChar()       // returns the next character or end of file marker
{
    if (lineNo == 0) {
        lineNo++;
        listingFile << setw(5) << right << lineNo << "|";
    }
    static char prevCh;
    static bool endReached = false;
    static bool lastLine = false;
    
    sourceFile.get(ch);
    
    if (sourceFile.eof()) {
        listingFile << "\n";
        return END_OF_FILE;
    }
    
    listingFile << ch;
    if (ch == '\n') { // prevCh == '\n' while having something in ch
        if (endReached && lastLine) {
            // do nothing
        } else if (prevCh == '.') {
            // do nothing
        } else {
            lineNo++;
            listingFile << setw(5) << right << lineNo << "|";
        }
    }
    prevCh = ch;
    
    if (token == "end") {
        endReached = true;
    }
    if (endReached && token == ".") {
        lastLine = true;
    }

    return ch;
}

// HELPER FUNCTIONS *********************************************************************

bool Compiler::isKeyword(string s) const {
    static const set<string> keywords = {"program", "const", "var", "begin", "end", "mod", "div", "and", "or", "read", "write"};
    return keywords.find(s) != keywords.end();
}

bool Compiler::isSpecialSymbol(char c) const {
    static const set<char> specialSymbols = {';', ':', ',', '+', '-', '*', '(', ')', '=', '<', '>'};
    return specialSymbols.find(c) != specialSymbols.end();
}

bool Compiler::isNonKeyId(string s) const {
    return !isKeyword(s);
}

bool Compiler::isInteger(string s) const {
    if (s == "integer")
        return true;
    
    int count = 0;
    if (s[0] == '+' || s[0] == '-')
        count++;
    
    for (uint i = count; i < s.length(); i++) {
        if (!isdigit(s[i]))
            return false;
    }
    
    return true;
}

bool Compiler::isBoolean(string s) const {
    return s == "true" || s == "false" || s == "boolean" || s == "yes" || s == "no";
}

bool Compiler::isLiteral(string s) const {
    return isBoolean(s) || isInteger(s);
}

string Compiler::genInternalName(storeTypes stype) const {
    static int iCount = 0;
    static int bCount = 0;
    static int pCount = 0;
    static int uCount = 0;
    int counter = 0;
    string internalName;
    
    switch (stype) {
        case INTEGER:
            internalName = "I";
            counter = iCount;
            iCount++;
            break;
        case BOOLEAN:
            internalName = "B";
            counter = bCount;
            bCount++;
            break;
        case PROG_NAME:
            internalName = "P";
            counter = pCount;
            pCount++;
            break;
        default:
            internalName = "U"; // U for undefined
            counter = uCount;
            uCount++;
    }
    
    internalName += to_string(counter);
    return internalName;
}