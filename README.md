# -#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAX_LINE_LEN  1024
#define MAX_TOKEN_CNT 512
#define MAX_VAR_CNT   128


typedef enum {

TK_NUM,
TK_VAR,
TK_ASSIGN,
TK_LPAREN,
TK_RPAREN,
TK_OP_ADD,
TK_OP_SUB,
TK_OP_MUL,
TK_OP_DIV,
TK_UMINUS,
TK_UPLUS,
TK_ERROR
} TokenType;

typedef enum {
// Value类型
VT_INT,
VT_FLOAT,
VT_ERROR
} ValueType;


typedef struct {
TokenType type;
char      lex[64]; // 原始串
} Token;


typedef struct {
ValueType type;
union {
int    iVal;
double fVal;
} val;
} Value;


typedef struct {
char   name[64];
double val;
int    isFloat;
} VarInfo;


static Token   g_tokens[MAX_TOKEN_CNT];
static int     g_tokCount = 0;

static VarInfo g_vars[MAX_VAR_CNT];
static int     g_varCount = 0;


static int g_exprHasFloat = 0;


static int isInteger(const char *s);
static int isFloat(const char *s);
static int isNumber(const char *s);
static int isVariable(const char *s);
static double getVarValue(const char *name, int *varIsFloat);
static void   setVarValue(const char *name, double val, int isFloat);


static void tokenizeLine(char *line)
{
g_tokCount = 0;
g_exprHasFloat = 0;


TokenType prevT = TK_ERROR;
char *p = strtok(line, " ");
while(p && g_tokCount < MAX_TOKEN_CNT)
{
TokenType t = TK_ERROR;
strncpy(g_tokens[g_tokCount].lex, p, 63);
g_tokens[g_tokCount].lex[63] = '\0';

if(strcmp(p, "=")==0) {
t = TK_ASSIGN;
}
else if(strcmp(p, "(")==0) {
t = TK_LPAREN;
}
else if(strcmp(p, ")")==0) {
t = TK_RPAREN;
}
else if(strcmp(p, "+")==0 || strcmp(p, "-")==0 ||
strcmp(p, "*")==0 || strcmp(p, "/")==0) {

if(strcmp(p, "*")==0) {
t = TK_OP_MUL;
}
else if(strcmp(p, "/")==0) {
t = TK_OP_DIV;
}
else if(strcmp(p, "+")==0) {

if(g_tokCount==0 ||
prevT==TK_LPAREN || prevT==TK_ASSIGN ||
prevT==TK_OP_ADD|| prevT==TK_OP_SUB ||
prevT==TK_OP_MUL|| prevT==TK_OP_DIV ||
prevT==TK_UMINUS || prevT==TK_UPLUS) {
t = TK_UPLUS;
} else {
t = TK_OP_ADD;
}
}
else if(strcmp(p, "-")==0) {
if(g_tokCount==0 ||
prevT==TK_LPAREN || prevT==TK_ASSIGN ||
prevT==TK_OP_ADD|| prevT==TK_OP_SUB ||
prevT==TK_OP_MUL|| prevT==TK_OP_DIV ||
prevT==TK_UMINUS || prevT==TK_UPLUS) {
t = TK_UMINUS;
} else {
t = TK_OP_SUB;
}
}
}
else if(isNumber(p)) {
t = TK_NUM;
if(isFloat(p)) {
g_exprHasFloat=1;
}
}
else if(isVariable(p)) {
t = TK_VAR;
}
else {
t = TK_ERROR;
}

g_tokens[g_tokCount].type = t;
g_tokCount++;
prevT = t;

p = strtok(NULL," ");
}
}


static int isInteger(const char *s)
{

int n=(int)strlen(s);
if(n==0) return 0;
for(int i=0;i<n;i++){
if(!isdigit((unsigned char)s[i])) return 0;
}
return 1;
}
static int isFloat(const char *s)
{

int n=(int)strlen(s);
if(n<3) return 0;

int dotCount=0;
for(int i=0;i<n;i++){
char c=s[i];
if(c=='.'){
dotCount++;
if(dotCount>1) return 0;
}
else if(!isdigit((unsigned char)c)){
return 0;
}
}
return (dotCount==1);
}
static int isNumber(const char *s)
{
if(isInteger(s)) return 1;
if(isFloat(s))   return 1;
return 0;
}
static int isVariable(const char *s)
{

if(!isalpha((unsigned char)s[0]) && s[0] != '_') {
return 0;
}
for(int i=1; s[i]; i++){
if(!isalnum((unsigned char)s[i]) && s[i] != '_') {
return 0;
}
}
return 1;
}


static double getVarValue(const char *name,int *varIsFloat)
{
for(int i=0;i<g_varCount;i++){
if(strcmp(g_vars[i].name,name)==0){
*varIsFloat = g_vars[i].isFloat;
return g_vars[i].val;
}
}
return NAN;
}
static void setVarValue(const char *name,double val,int isFloat)
{
for(int i=0;i<g_varCount;i++){
if(strcmp(g_vars[i].name,name)==0){
g_vars[i].val=val;
if(isFloat) g_vars[i].isFloat=1;
return;
}
}
// 新增
if(g_varCount<MAX_VAR_CNT){
strncpy(g_vars[g_varCount].name, name,63);
g_vars[g_varCount].name[63]='\0';
g_vars[g_varCount].val=val;
g_vars[g_varCount].isFloat=isFloat;
g_varCount++;
}
}


static Value makeError(void)
{
Value v; v.type=VT_ERROR; v.val.fVal=0; return v;
}
static Value makeInt(int x)
{
Value v; v.type=VT_INT; v.val.iVal=x; return v;
}
static Value makeFloat(double d)
{
Value v; v.type=VT_FLOAT; v.val.fVal=d; return v;
}


static int isZero(double x)
{
return fabs(x)<1e-15;
}

/* ========== parse函数声明 ========== */
static Value parseAssignment(int *idx);
static Value parseExpr(int *idx);
static Value parseTerm(int *idx);
static Value parseFactor(int *idx);



static Value parseBase(int *idx)
{
if(*idx >= g_tokCount) return makeError();

TokenType t = g_tokens[*idx].type;
if(t == TK_LPAREN){
// consume '('
(*idx)++;
// parse expr
Value inner = parseExpr(idx);
// expect ')'
if(*idx>=g_tokCount || g_tokens[*idx].type!=TK_RPAREN) {
return makeError();
}
(*idx)++; // consume ')'
return inner;
}
else if(t == TK_NUM){
// 解析数字
double d = atof(g_tokens[*idx].lex);
(*idx)++;
// 判断是否近似int
double frac = d-(int)d;
if(fabs(frac)<1e-12){
// 近似整数
return makeInt((int)round(d));
} else {
return makeFloat(d);
}
}
else if(t == TK_VAR){

int varF=0;
double varv = getVarValue(g_tokens[*idx].lex,&varF);
if(isnan(varv)){

return makeError();
}
(*idx)++;

if(varF){
return makeFloat(varv);
} else {

double frac = varv-(int)varv;
if(fabs(frac)<1e-12) {
return makeInt((int)round(varv));
} else {

return makeFloat(varv);
}
}
}
return makeError();
}


static Value parseFactor(int *idx)
{
if(*idx >= g_tokCount) return makeError();


int sign=1;
int unaryCount=0;
while(*idx < g_tokCount) {
TokenType t = g_tokens[*idx].type;
if(t==TK_UPLUS) {

unaryCount++;
(*idx)++;
}
else if(t==TK_UMINUS) {
sign = -sign;
unaryCount++;
(*idx)++;
}
else {
break;
}
}

if(*idx >= g_tokCount) return makeError();

// parse base
Value base = parseBase(idx);
if(base.type==VT_ERROR) return base;

// 乘 sign
if(base.type==VT_INT) {
base.val.iVal *= sign;
}
else if(base.type==VT_FLOAT){
base.val.fVal *= sign;
}
return base;
}

/* ========== parseTerm ==========
  term := factor { (*|/) factor }
  左结合
 */
static Value parseTerm(int *idx)
{
Value lhs = parseFactor(idx);
if(lhs.type==VT_ERROR) return lhs;

while(*idx<g_tokCount) {
TokenType t = g_tokens[*idx].type;
if(t==TK_OP_MUL || t==TK_OP_DIV) {
// consume op
TokenType op = t;
(*idx)++;
// parse next factor
Value rhs = parseFactor(idx);
if(rhs.type==VT_ERROR) return rhs;

// 计算
double L = (lhs.type==VT_INT)? (double)lhs.val.iVal : lhs.val.fVal;
double R = (rhs.type==VT_INT)? (double)rhs.val.iVal : rhs.val.fVal;
if(op==TK_OP_MUL) {
double ret = L*R;
// 若 ret 近似int
double frac = ret-(int)ret;
if(fabs(frac)<1e-12){
lhs.type=VT_INT;
lhs.val.iVal=(int)round(ret);
} else {
lhs.type=VT_FLOAT;
lhs.val.fVal=ret;
}
}
else {
// TK_OP_DIV
if(isZero(R)) {
return makeError();
}
// 若lhs,rhs都是int => 整除
if(lhs.type==VT_INT && rhs.type==VT_INT) {
double ret=(double)(lhs.val.iVal / rhs.val.iVal);

lhs.type=VT_FLOAT;
lhs.val.fVal=ret;
} else {
double ret=L/R;
lhs.type=VT_FLOAT;
lhs.val.fVal=ret;
}
}
}
else {
break; // no more * or /
}
}
return lhs;
}

/* ========== parseExpr ==========
  expr := term { (+|-) term }
  左结合
 */
static Value parseExpr(int *idx)
{
Value lhs = parseTerm(idx);
if(lhs.type==VT_ERROR) return lhs;

while(*idx<g_tokCount) {
TokenType t = g_tokens[*idx].type;
if(t==TK_OP_ADD || t==TK_OP_SUB) {
TokenType op=t;
(*idx)++;
// parse next term
Value rhs = parseTerm(idx);
if(rhs.type==VT_ERROR) return rhs;

double L=(lhs.type==VT_INT)? lhs.val.iVal: lhs.val.fVal;
double R=(rhs.type==VT_INT)? rhs.val.iVal: rhs.val.fVal;
double ret=0.0;
if(op==TK_OP_ADD) ret=L+R;
else ret=L-R;

double frac=ret-(int)ret;
if(fabs(frac)<1e-12){
lhs.type=VT_INT;
lhs.val.iVal=(int)round(ret);
} else {
lhs.type=VT_FLOAT;
lhs.val.fVal=ret;
}
}
else {
break;
}
}
return lhs;
}

/* ========== parseAssignment ==========
  assignment := var = assignment | expr
  连续赋值: a = b = 10
 */
static Value parseAssignment(int *idx)
{
// 先看看是否形如  <var> = ...
if(*idx+1<g_tokCount &&
g_tokens[*idx].type==TK_VAR &&
g_tokens[*idx+1].type==TK_ASSIGN) {
// 记录变量名
char varName[64];
strcpy(varName, g_tokens[*idx].lex);

(*idx)+=2; // consume var & '='

// 递归 parseAssignment
Value rightVal=parseAssignment(idx);
if(rightVal.type==VT_ERROR) return rightVal;

// 赋值
double d= (rightVal.type==VT_INT)? rightVal.val.iVal : rightVal.val.fVal;
int varF = (rightVal.type==VT_FLOAT)? 1:0;
setVarValue(varName,d,varF);
return rightVal;
}
else {
// parseExpr
return parseExpr(idx);
}
}

/* ========== 主函数 ========== */
int main(void)
{
char line[MAX_LINE_LEN];
while(1) {
if(!fgets(line,sizeof(line),stdin)) {
break; // EOF
}
// 去掉换行
line[strcspn(line,"\n")]='\0';
if(line[0]=='\0') {
continue;
}

// 词法分析
tokenizeLine(line);
if(g_tokCount==0) continue;

// 检查词法错误
int lexErr=0;
for(int i=0;i<g_tokCount;i++){
if(g_tokens[i].type==TK_ERROR){
lexErr=1;
break;
}
}
if(lexErr){
printf("Error\n");
continue;
}

// 语法解析
int idx=0;
Value result = parseAssignment(&idx);

// 若还没消费完token，也算表达式异常
if(result.type==VT_ERROR || idx<g_tokCount) {
printf("Error\n");
continue;
}

// 若正确 => 输出
if(result.type==VT_ERROR) {
printf("Error\n");
continue;
}

// 根据是否出现过浮点token => 决定输出模式
if(g_exprHasFloat) {
// 强制 float 6位
double val = (result.type==VT_INT)? (double)result.val.iVal: result.val.fVal;
printf("%.6f\n", val);
} else {
// 若无浮点token => 全是整数
if(result.type==VT_INT) {
printf("%d\n", result.val.iVal);
} else {
double dv = result.val.fVal;
int iv = (int)round(dv);
printf("%d\n", iv);
}
}
}
return 0;
}

