//Group No. 7
//ANIRUDH GOYAL 2017A7PS0031P
//ANISHKUMAR SS 2017A7PS0069P
//ROHIT JAIN 2017A7PS0122P
//ADITYA SAXENA 2017A7PS0166P


#include "ast.h"
int label = 1;
SYMBOL_TABLE* curr_st, *save_st;
int locallabel=1;
int switchlabel=1;
int whilelabel = 1;
tree_node* child;
FTValue* lookup;

int sameprec(tree_node* ast, tree_node* child){
    TOKEN t1 = (ast->val).value.t_val;
    TOKEN t2 = (child->val).value.t_val;

    if(t1 == t2)
        return 1;
    if((t1 == PLUS && t2 == MINUS) || (t2 == PLUS && t1 == MINUS))
        return 1;
    if((t1 == MUL && t2 == DIV) || (t2 == MUL && t1 == DIV))
        return 1;
    if((t1 == AND && t2 == OR) || (t2 == AND && t1 == OR))
        return 1;

    return 0;
}
int checkassoc(tree_node* ast, FILE* fp){
    if(!sameprec(ast,ast->child[0])){
        generateAssembly(ast->child[0],fp);
        return 0;
    }
    generateAssembly(ast->child[0]->childLeft[0],fp);
    (ast->child[0]->childLeft[0]->val).value.t_val = EPSILON;
    return 1;
}
void generateAssembly(tree_node* ast, FILE* fp){
  STValue* find, *find2;
  int flag;
  int tillNow;
  int stsize;
  if((ast->val).tag==1){
    tree_node* iterator,* rangeStart,* rangeEnd, *index;
    char* buffer = malloc(sizeof(char)*100);
    switch((ast->val).value.t_val){
      case SWITCH:  
        save_st = curr_st;
        curr_st = ast->stptr;
        fprintf(fp, "switch%d:\n",switchlabel);
        generateAssembly(ast->child[0], fp);
        fprintf(fp, "    pop r10\n");
        fprintf(fp, "\n");
        for(int i=0;i<ast->child[1]->no_child;i+=2){
          fprintf(fp, ".case%d:\n",i/2+1);
          generateAssembly(ast->child[1]->child[i],fp);
          fprintf(fp, "    pop rax\n");
          fprintf(fp, "    cmp r10w, ax\n");
          fprintf(fp, "    jne .case%d",i/2+2);
          fprintf(fp, "\n");
          generateAssembly(ast->child[1]->child[i+1],fp);
          fprintf(fp, "    jmp .endswitch\n");
          fprintf(fp, "\n");
        }
        fprintf(fp, ".case%d:\n",ast->child[1]->no_child/2 +1);
        if((ast->child[2]->child[0]->val).value.t_val == EPSILON);
        else
          generateAssembly(ast->child[2]->child[0],fp);

        fprintf(fp, ".endswitch:\n");
        switchlabel++;
        curr_st = save_st;
        return;  break;

      case FOR: 
        save_st = curr_st;
        curr_st = ast->stptr;
        iterator = ast->child[0];
        rangeStart = ast->child[1]->child[0];
        rangeEnd = ast->child[1]->child[1];
        
        generateAssembly(rangeEnd,fp);
        generateAssembly(rangeStart,fp);
        
        find = returnIfPresentST(ast->child[0]->lexeme, curr_st,ast->child[0]);

        fprintf(fp,"    mov rbx, rbp\n");
        fprintf(fp,"    sub rbx, %d\n",find->offset+find->width);           
        fprintf(fp,"    push rbx\n");
        fprintf(fp,"\n");

        fprintf(fp, "    pop rdi\n");
        fprintf(fp, "    pop rax\n");
        fprintf(fp, "    mov word[rdi], ax\n");
        fprintf(fp, "    push rdi\n");
        fprintf(fp,"loopStart%d:\n", label);
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    cmp word[rax], bx\n");
        fprintf(fp,"    jg loopEnd%d\n",label);
        fprintf(fp,"    push rbx\n");
        fprintf(fp,"    push rax\n");
        generateAssembly(ast->child[2],fp);
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    add word[rax],1\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"    jmp loopStart%d\n",label);
        fprintf(fp,"loopEnd%d:\n",label);
        label++;
        
        curr_st = save_st;
        return; break;
      
      case WHILE:
        save_st = curr_st;
        curr_st = ast->stptr;
        fprintf(fp,"whileStart%d:\n", whilelabel);
        generateAssembly(ast->child[0], fp);
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    cmp al,0\n");
        fprintf(fp,"    je whileEnd%d\n",whilelabel);    
        generateAssembly(ast->child[1], fp);
        fprintf(fp,"    jmp whileStart%d\n",whilelabel);
        fprintf(fp,"whileEnd%d:\n",whilelabel);
        whilelabel++;
        curr_st = save_st;
        return; break;

      case NUM:
        fprintf(fp,"    mov rax, %d\n", (ast->value).num_value);
        fprintf(fp,"    push rax\n");
        return; break;

      case RNUM:
        // fprintf(fp,"    MOV AX, %f\n", (ast->value).rnum_value);  // change to register that is used for RNUM
        // fprintf(fp,"    PUSH AX\n");
        return; break;

      case ID:
        find = returnIfPresentST(ast->lexeme, curr_st, ast);
        fprintf(fp,"    mov ax, word[rbp - %d]\n",find->offset+find->width);           
        fprintf(fp,"    push rax\n");
        return; break;
      
      case TRUE:
        fprintf(fp,"    mov rax, 1\n");
        fprintf(fp,"    push rax\n");
        return; break;
      
      case FALSE:
        fprintf(fp,"    mov rax, 0\n");
        fprintf(fp,"    push rax\n");
        return; break;

      case PLUS:
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    add ax, bx\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;
                
      case ARRAY:
        generateAssembly(ast->child[1],fp);    
        find = returnIfPresentST(ast->child[0]->lexeme, curr_st, ast->child[0]);
        fprintf(fp,"    mov r8, rbp\n");    
        fprintf(fp,"    sub r8, %d\n",find->offset+8);          
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"\n");
        fprintf(fp,"    cmp ax, %d\n",(find->type).begin);
        fprintf(fp,"    jl outofbounds\n");
        fprintf(fp,"    cmp ax, %d\n",(find->type).end);
        fprintf(fp,"    jg outofbounds\n");
        fprintf(fp,"\n");
        fprintf(fp,"    sub rax, %d\n",(find->type).begin);
        fprintf(fp,"    add rax, 1\n");
        if((find->type).dtype == INTEGER){
          fprintf(fp,"    mov cx, 2\n");
          fprintf(fp,"    mul cx\n");
        }
        
        fprintf(fp,"    and rax, 0fh\n"); 
        fprintf(fp,"    sub r8, rax\n"); 
        fprintf(fp,"    mov ax, word[r8]\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        return; break;
                
      case MINUS:
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    sub ax, bx\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;
                
      case DIV: 
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    div bx\n");
        fprintf(fp,"    and ax, 0fh\n");       
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;
                
      case MUL: 
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    imul bx\n");     
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;
          
      case LT: 
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    jl label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
                
      case LE:
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    jle label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
            
      case GE: 
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    jge label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
            
      case GT: 
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    jg label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
      
      case EQ: 
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    je label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
      
      case NE: 
        generateAssembly(ast->childLeft[0],fp);    
        generateAssembly(ast->child[0],fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    mov rdx, 1\n");
        fprintf(fp,"    cmp ax,bx\n");
        fprintf(fp,"    jne label%d\n",label);
        fprintf(fp,"    mov rdx, 0\n");
        fprintf(fp,"    label%d:\n",label);
        fprintf(fp,"    push rdx\n");
        fprintf(fp,"\n");
        label++;
        return; break;
          
      case AND: 
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    and ax, bx\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;
      
      case OR: 
        generateAssembly(ast->childLeft[0],fp);    
        flag = checkassoc(ast,fp);
        fprintf(fp,"    pop rbx\n");
        fprintf(fp,"    pop rax\n");
        fprintf(fp,"    or ax, bx\n");
        fprintf(fp,"    push rax\n");
        fprintf(fp,"\n");
        if(flag == 1)
            generateAssembly(ast->child[0],fp);
        return; break;

      default: return;
    }
  }

  switch((ast->val).value.nt_val){

    case PROGRAM_NT:
        fprintf(fp, "SECTION .data\n");
        fprintf(fp, "inputi  db \"Input: Enter an integer value\",00h\n");
        fprintf(fp, "inputb  db \"Input: Enter a boolean value\",00h\n");
        fprintf(fp, "output db \"Output: \",00h\n");
        fprintf(fp, "formatstring db \"%%s\",00h\n");
        fprintf(fp, "newline db 0Ah,00h\n");
        fprintf(fp, "space db \" \",00h\n");
        fprintf(fp, "formatint db \"%%hi\",00h\n");
        fprintf(fp, "printint db \"%%hi \",00h\n");
        fprintf(fp, "formatbool db \"%%hi\",00h\n");
        fprintf(fp, "true db \"true \",00h\n");
        fprintf(fp, "false db \"false \",00h\n");
        fprintf(fp, "range1 dw 0\n");
        fprintf(fp, "range2 dw 0\n");
        fprintf(fp, "base dq 0\n");
        fprintf(fp, "formatarray db \"Input: Enter %%hi elements of %%s type for range %%hi to %%hi \",00h \n");
        fprintf(fp, "INTEGER db \"integer\",00h\n");
        fprintf(fp, "BOOLEAN db \"boolean\",00h\n");
        fprintf(fp, "booltemp dw 0\n");
        fprintf(fp, "runtimeerror db \"RUN TIME ERROR: Array index out of bounds\",00h\n");
        fprintf(fp, "darraysize dw 0\n");
        fprintf(fp, "\n");
        fprintf(fp, "SECTION .text\n");
        fprintf(fp, "global main\n");
        fprintf(fp, "extern printf\n");
        fprintf(fp, "extern scanf\n");   
        fprintf(fp, "\n");   
        fprintf(fp, "main:\n");
        fprintf(fp, "    call driver\n");
        fprintf(fp, "\n");
        
        fprintf(fp, "    mov rax, 60\n");
        fprintf(fp, "    xor rdi, rdi\n");
        fprintf(fp, "    syscall\n");
        fprintf(fp, "\n");
        
        for(int i=0;i<ast->no_child;i++)
          generateAssembly(ast->child[i], fp);

        fprintf(fp, "\n");
        fprintf(fp, "outofbounds:\n");
        fprintf(fp, "    mov rdi, formatstring\n");
        fprintf(fp, "    mov rsi, runtimeerror\n");
        fprintf(fp, "    xor rax, rax\n");
        fprintf(fp, "    call printf\n");
        fprintf(fp, "\n");
        fprintf(fp, "    mov rdi, formatstring\n");
        fprintf(fp, "    mov rsi, newline\n");
        fprintf(fp, "    xor rax, rax\n");
        fprintf(fp, "    call printf\n");
        fprintf(fp, "\n");
        fprintf(fp, "    mov rax, 60\n");
        fprintf(fp, "    xor rdi, rdi\n");
        fprintf(fp, "    syscall\n");
        fprintf(fp, "\n");

        return; break;                                                  //done

    // MODULE DECS
    case DRIVERMODULE:        
        save_st = curr_st;                                            //done
        curr_st = ast->stptr;
        fprintf(fp, "driver:\n");
        fprintf(fp, "    push rbp\n");
        fprintf(fp, "    mov rbp, rsp\n");
        fprintf(fp, "    sub rsp, %d\n", ((curr_st->curr_offset/16)+1)*16);
        fprintf(fp, "    push rbp\n");
        fprintf(fp, "\n");
        generateAssembly(ast->child[0]->child[0], fp);
        fprintf(fp, "    pop rax\n");
        fprintf(fp, "    mov rax, 60\n");
        fprintf(fp, "    xor rdi, rdi\n");
        fprintf(fp, "    syscall\n");
        fprintf(fp, "\n");
        
        fprintf(fp, "    add rsp, %d\n", ((curr_st->curr_offset/16)+1)*16);
        fprintf(fp, "    pop rbp\n");
        fprintf(fp, "    ret\n");
        fprintf(fp, "\n");

        curr_st = save_st;
        return; break;

    case MODULE_NT:

        save_st = curr_st;                                            //done
        curr_st = ast->stptr;
        fprintf(fp, "%s:\n", ast->child[0]->lexeme);
        child = ast->child[1];
        if((child->child[0]->val).value.t_val == EPSILON);
        else{
            for(int i=0;i<child->no_child;i+=2){
                find = returnIfPresentST(child->child[i]->lexeme, curr_st,child->child[i]);
                find->offset *= -1;
                find->offset -= 24;
                find->offset -= find->width;
            }
        }
        fprintf(fp, "    push rbp\n");
        fprintf(fp, "    push rbp\n");
        fprintf(fp, "    mov rbp, rsp\n");
        fprintf(fp, "    sub rsp, %d\n", ((curr_st->curr_offset/16)+1)*16);
        fprintf(fp, "    push rbp\n");
        fprintf(fp, "\n");
        generateAssembly(ast->child[3]->child[0], fp);
        fprintf(fp, "    pop rsp\n");
        fprintf(fp, "    pop rbp\n");
        fprintf(fp, "    pop rax\n");
        fprintf(fp, "    pop rax\n");
        child = ast->child[2];
        if((child->child[0]->val).value.t_val == EPSILON);
        else{
            for(int i=0;i<child->no_child;i+=2){
                find = returnIfPresentST(child->child[i]->lexeme, curr_st,child->child[i]);
                fprintf(fp, "    push qword[rbp-%d]\n",find->offset + find->width);
            }
        }
        fprintf(fp, "    push rax\n");
        fprintf(fp, "    ret\n");
        fprintf(fp, "\n");

        curr_st = save_st;
        return; break;
        // curr_st = ast->stptr;
        // fprintf(fp, "%s:\n",ast->child[0]->lexeme);
        // //input, output list
        // generateAssembly(ast->child[3]->child[0], fp);
        // fprintf(fp, "  RET\n");
        // fprintf(fp, ".end%s:\n",ast->child[0]->lexeme);
        // fprintf(fp, "\n");
        return; break;

    case ARMORBOOL:
    case EXPRESSION:
      generateAssembly(ast->child[0],fp);
      break;

    case MODULEDECLARATIONS:return; break;
    case MODULEREUSESTMT: 
        child = ast->child[ast->no_child-1];
        stsize = 0;
        if((child->child[0]->val).value.t_val == EPSILON);
        else{
            lookup = get_ftvalue(ft, ast->child[ast->no_child-2]->lexeme);
            stsize = lookup->inputSymbolTable->curr_offset;
            fprintf(fp, "    xor rbx, rbx\n");
            fprintf(fp, "    mov bx, %d\n",stsize);
            fprintf(fp, "    mov al, %d\n",stsize);
            fprintf(fp, "    mov cl, 16\n");
            fprintf(fp, "    div cl\n");
            fprintf(fp, "    cmp ah, 0\n");
            fprintf(fp, "    je locallabel%d\n",locallabel);
            fprintf(fp, "    add bl, 16\n");
            fprintf(fp, "    sub bl, ah\n");
            fprintf(fp, "    locallabel%d:\n",locallabel);
            fprintf(fp, "\n");
            locallabel++;
            fprintf(fp, "    add rsp, rbx\n");
            fprintf(fp, "\n");
            tillNow = 0;
            for(int i=0;i<child->no_child;i++){
                find = returnIfPresentST(child->child[i]->lexeme, curr_st,child->child[i]);
                if((find->type).dtype == INTEGER){
                    fprintf(fp, "    mov ax, word[rbp - %d]\n", find->offset+find->width);
                    fprintf(fp, "    mov word[rsp + %d], ax\n",tillNow);
                    tillNow += find->width;
                }
                else{
                    fprintf(fp, "    mov al, byte[rbp - %d]\n", find->offset+find->width);
                    fprintf(fp, "    mov byte[rsp + %d], al\n",tillNow);
                    tillNow += find->width;
                }
            }
        }

        fprintf(fp, "\n");
        fprintf(fp, "    mov r13, rbp\n");
        fprintf(fp, "    call %s\n",ast->child[ast->no_child-2]->lexeme);
        fprintf(fp, "    mov rbp, r13\n");
        fprintf(fp, "\n");

        child = ast->child[0];
        if((child->child[0]->val).value.t_val == EPSILON);
        else{
            child = child->child[0];
            for(int i=child->no_child-1;i>=0;i--){
                find = returnIfPresentST(child->child[i]->lexeme, curr_st,child->child[i]);
                fprintf(fp, "    pop rax\n");
                if((find->type).dtype == INTEGER){
                    fprintf(fp, "    mov word[rbp - %d], ax\n", find->offset+find->width);
                }
                else{
                    fprintf(fp, "    mov byte[rbp - %d], al\n", find->offset+find->width);
                }
            }
        }
        fprintf(fp, "    xor rbx, rbx\n");
        fprintf(fp, "    mov bx, %d\n",stsize);
        fprintf(fp, "    mov al, %d\n",stsize);
        fprintf(fp, "    mov cl, 16\n");
        fprintf(fp, "    div cl\n");
        fprintf(fp, "    cmp ah, 0\n");
        fprintf(fp, "    je locallabel%d\n",locallabel);
        fprintf(fp, "    add bl, 16\n");
        fprintf(fp, "    sub bl, ah\n");
        fprintf(fp, "    locallabel%d:\n",locallabel);
        fprintf(fp, "\n");
        locallabel++;
        fprintf(fp, "    sub rsp, rbx\n");
        fprintf(fp, "\n");

        return; break;
    case IOSTMT:
        if((ast->child[0]->val).value.t_val == GET_VALUE){

          find = returnIfPresentST(ast->child[1]->lexeme, curr_st,ast->child[1]);
          if((find->type).isArray == 1){
            if((find->type).begin != -1)
                fprintf(fp, "    mov word[range1], %d\n",(find->type).begin);
            else{
                find2 = returnIfPresentST((find->type).begins, curr_st,ast->child[1]);
                fprintf(fp, "    mov ax, word[rbp-%d]\n",find->offset+find->width);
                fprintf(fp, "    mov word[range1], ax\n");
            }

            if((find->type).end != -1)
                fprintf(fp, "    mov word[range2], %d\n",(find->type).end);
            else{
                find2 = returnIfPresentST((find->type).ends, curr_st,ast->child[1]);
                fprintf(fp, "    mov ax, word[rbp-%d]\n",find->offset+find->width);
                fprintf(fp, "    mov word[range2], ax\n");
            }
            fprintf(fp, "\n");
            fprintf(fp, "    mov qword[base], rbp\n");
            fprintf(fp, "    sub qword[base], %d\n",find->offset+8);
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatarray\n");
            fprintf(fp, "    xor rsi, rsi\n");
            fprintf(fp, "    mov si, word[range2]\n");
            fprintf(fp, "    sub si, word[range1]\n");
            fprintf(fp, "    add si, 1\n");
            fprintf(fp, "    mov rdx, %s\n",convert_token_lexer((find->type).dtype));
            fprintf(fp, "    xor rcx, rcx\n");
            fprintf(fp, "    xor r8, r8\n");
            fprintf(fp, "    mov cx, word[range1]\n");
            fprintf(fp, "    mov r8w, word[range2]\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, newline\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov r15w, word[range1]\n");
            fprintf(fp, "    loopStart%d:\n",label);
            fprintf(fp, "    cmp r15w, word[range2]\n");
            fprintf(fp, "    jg loopEnd%d\n",label);
            fprintf(fp, "\n");

            if((find->type).dtype == INTEGER){
                fprintf(fp, "    mov rdi, formatint\n");
                fprintf(fp, "    mov rsi, qword[base]\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    mov ax, r15w\n");
                fprintf(fp, "    sub ax, word[range1]\n");
                fprintf(fp, "    mov cx, 2\n");
                fprintf(fp, "    mul cx\n");
                fprintf(fp, "    sub rsi, rax\n");
                fprintf(fp, "    sub rsi, 2\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    call scanf\n");
                fprintf(fp, "\n");  
            }
            else if((find->type).dtype == BOOLEAN){
                fprintf(fp, "    mov rdi, formatbool\n");
                fprintf(fp, "    mov rsi, booltemp\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    call scanf\n");
                fprintf(fp, "\n");

                fprintf(fp, "    mov rsi, qword[base]\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    mov ax, r15w\n");
                fprintf(fp, "    sub ax, word[range1]\n");
                fprintf(fp, "    sub rsi, rax\n");
                fprintf(fp, "    sub rsi, 1\n");
                fprintf(fp, "    mov al, byte[booltemp]\n");
                fprintf(fp, "    mov byte[rsi], al\n");

                fprintf(fp, "\n");
            }

            fprintf(fp, "    inc r15w\n");
            fprintf(fp, "    jmp loopStart%d\n",label);
            fprintf(fp, "    loopEnd%d:\n",label);
            fprintf(fp, "\n");
            label++;
            return; break;
          }
          if((find->type).dtype == INTEGER){
            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, inputi\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, newline\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatint\n");
            fprintf(fp, "    mov rsi, rbp\n");
            fprintf(fp, "    sub rsi, %d\n", find->offset+find->width);
            fprintf(fp, "    mov rax, 0\n");
            fprintf(fp, "    call scanf\n");
            fprintf(fp, "\n");     
          }
          else if((find->type).dtype == BOOLEAN){

            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, inputb\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, newline\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            fprintf(fp, "    mov rdi, formatbool\n");
            fprintf(fp, "    mov rsi, rbp\n");
            fprintf(fp, "    sub rsi, %d\n", find->offset+find->width);
            fprintf(fp, "    mov rax, 0\n");
            fprintf(fp, "    call scanf\n");
            fprintf(fp, "\n");
          }
        }
        else{
          fprintf(fp, "    mov rdi, formatstring\n");
          fprintf(fp, "    mov rsi, output\n");
          fprintf(fp, "    xor rax, rax\n");
          fprintf(fp, "    call printf\n");
          fprintf(fp, "\n");

          find = returnIfPresentST(ast->child[1]->lexeme, curr_st,ast->child[1]);

          if((find->type).isArray == 1){
            if((find->type).begin != -1)
                fprintf(fp, "    mov word[range1], %d\n",(find->type).begin);
            else{
                find2 = returnIfPresentST((find->type).begins, curr_st,ast->child[1]);
                fprintf(fp, "    mov ax, word[rbp-%d]\n",find->offset+find->width);
                fprintf(fp, "    mov word[range1], ax\n");
            }

            if((find->type).end != -1)
                fprintf(fp, "    mov word[range2], %d\n",(find->type).end);
            else{
                find2 = returnIfPresentST((find->type).ends, curr_st,ast->child[1]);
                fprintf(fp, "    mov ax, word[rbp-%d]\n",find->offset+find->width);
                fprintf(fp, "    mov word[range2], ax\n");
            }
            fprintf(fp, "\n");
            fprintf(fp, "    mov qword[base], rbp\n");
            fprintf(fp, "    sub qword[base], %d\n",find->offset+8);
            fprintf(fp, "\n");

            fprintf(fp, "    mov r15w, word[range1]\n");
            fprintf(fp, "    loopStart%d:\n",label);
            fprintf(fp, "    cmp r15w, word[range2]\n");
            fprintf(fp, "    jg loopEnd%d\n",label);
            fprintf(fp, "\n");

            if((find->type).dtype == INTEGER){
                fprintf(fp, "    mov rdi, printint\n");
                fprintf(fp, "    mov r8, qword[base]\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    mov ax, r15w\n");
                fprintf(fp, "    sub ax, word[range1]\n");
                fprintf(fp, "    mov cx, 2\n");
                fprintf(fp, "    mul cx\n");
                fprintf(fp, "    sub r8, rax\n");
                fprintf(fp, "    sub r8, 2\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    xor rsi, rsi\n");
                fprintf(fp, "    mov si, word[r8]\n");
                fprintf(fp, "    call printf\n");
                fprintf(fp, "\n");  
            }
            else if((find->type).dtype == BOOLEAN){

                fprintf(fp, "    mov rdi, formatstring\n");
                fprintf(fp, "    xor rsi, rsi\n");
                fprintf(fp, "    mov r8, qword[base]\n");
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    mov ax, r15w\n");
                fprintf(fp, "    sub ax, word[range1]\n");
                fprintf(fp, "    sub r8, rax\n");
                fprintf(fp, "    sub r8, 1\n");
                fprintf(fp, "    mov dl, byte[r8]\n");
                fprintf(fp, "    mov rsi, true\n");
                fprintf(fp, "    cmp dl, 1\n");
                fprintf(fp, "    je locallabel%d\n",locallabel);
                fprintf(fp, "    mov rsi, false\n");
                fprintf(fp, "    locallabel%d:\n",locallabel);
                fprintf(fp, "    mov rax, 0\n");
                fprintf(fp, "    call printf\n");
                fprintf(fp, "\n");
                locallabel++;
            
            }

            fprintf(fp, "    inc r15w\n");
            fprintf(fp, "    jmp loopStart%d\n",label);
            fprintf(fp, "    loopEnd%d:\n",label);
            fprintf(fp, "\n");
            label++;

            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    mov rsi, newline\n");
            fprintf(fp, "    xor rax, rax\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");

            return; break;
          }

          if((find->type).dtype == INTEGER){
            fprintf(fp, "    mov rdi, printint\n");
            fprintf(fp, "    xor rsi, rsi\n");
            fprintf(fp, "    mov si, word[rbp-%d]\n", find->offset+find->width);
            fprintf(fp, "    mov rax, 0\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");
          }
          else{
            fprintf(fp, "    mov rdi, formatstring\n");
            fprintf(fp, "    xor rsi, rsi\n");
            fprintf(fp, "    mov dl, byte[rbp-%d]\n", find->offset+find->width);
            fprintf(fp, "    mov rsi, true\n");
            fprintf(fp, "    cmp dl, 1\n");
            fprintf(fp, "    je label%d\n",label);
            fprintf(fp, "    mov rsi, false\n");
            fprintf(fp, "    label%d:\n",label);
            fprintf(fp, "    mov rax, 0\n");
            fprintf(fp, "    call printf\n");
            fprintf(fp, "\n");
            label++;
          }
          fprintf(fp, "    mov rdi, formatstring\n");
          fprintf(fp, "    mov rsi, newline\n");
          fprintf(fp, "    xor rax, rax\n");
          fprintf(fp, "    call printf\n");
          fprintf(fp, "\n");

        }

        return; break;
    case DECLARESTMT: 
          if((ast->child[1]->type).isArray  && (ast->child[1]->type).isStatic == 0){
            generateAssembly(ast->child[1]->child[0], fp);
            generateAssembly(ast->child[1]->child[1], fp);
            fprintf(fp, "    pop rax\n");
            fprintf(fp, "    pop rbx\n");
            fprintf(fp, "    mov word[range1], ax\n");
            fprintf(fp, "    mov word[range2], bx\n");
            fprintf(fp, "    mov word[darraysize], bx\n");
            fprintf(fp, "    sub word[darraysize], ax\n");
            fprintf(fp, "    add word[darraysize], 1\n");
            if((ast->child[1]->type).dtype == INTEGER){
              fprintf(fp, "    mov ax, word[darraysize]\n");
              fprintf(fp, "    mov cx, 2\n");
              fprintf(fp, "    mul cx\n");
              fprintf(fp, "    mov word[darraysize], ax\n");
            }
            fprintf(fp, "    add word[darraysize], 4\n");
            fprintf(fp, "    mov ax, word[darraysize]\n");
            fprintf(fp, "    div 16\n");
            fprintf(fp, "    cmp ah, 0\n");
            fprintf(fp, "    je locallabel%d\n",locallabel);
            fprintf(fp, "    add byte[darraysize], 16\n");
            fprintf(fp, "    sub byte[darraysize], ah\n");
            fprintf(fp, "    locallabel%d:\n",locallabel);
            fprintf(fp, "\n");
            locallabel++;
            for(int i=0;i<ast->child[0]->no_child;i++){
              find = returnIfPresentST(ast->child[0]->child[i]->lexeme, curr_st,ast->child[0]->child[i]);
              fprintf(fp, "    mov qword[rbp-%d], rsp\n",find->offset + 8);
              fprintf(fp, "    mov rdi, rsp\n");
              fprintf(fp, "    add sp, word[darraysize]\n");
              fprintf(fp, "    mov ax, word[range1]\n");
              fprintf(fp, "    mov word[rdi-2], ax\n");
              fprintf(fp, "    mov ax, word[range2]\n");
              fprintf(fp, "    mov word[rdi-4], ax\n");
              fprintf(fp, "\n");
            }
            
          }
          return; break;
    case ASSIGNMENTSTMT:  
        generateAssembly(ast->child[ast->no_child-1], fp);
        
        
        find = returnIfPresentST(ast->child[0]->lexeme, curr_st,ast->child[0]);
        if((find->type).isArray){
            generateAssembly(ast->child[1], fp);  
            fprintf(fp,"    mov r8, rbp\n");    
            fprintf(fp,"    sub r8, %d\n",find->offset+8);          
            fprintf(fp,"    pop rax\n");
            fprintf(fp,"\n");
            fprintf(fp,"    cmp ax, %d\n",(find->type).begin);
            fprintf(fp,"    jl outofbounds\n");
            fprintf(fp,"    cmp ax, %d\n",(find->type).end);
            fprintf(fp,"    jg outofbounds\n");
            fprintf(fp,"\n");
            fprintf(fp,"    sub rax, %d\n",(find->type).begin);
            fprintf(fp,"    add rax, 1\n");
            if((find->type).dtype == INTEGER){
              fprintf(fp,"    mov cx, 2\n");
              fprintf(fp,"    mul cx\n");
            }
            
            fprintf(fp,"    and rax, 0fh\n"); 
            fprintf(fp,"    sub r8, rax\n"); 
            fprintf(fp,"    pop rax\n");

            if((find->type).dtype == INTEGER)
              fprintf(fp,"    mov word[r8], ax\n");
            else 
              fprintf(fp,"    mov byte[r8], al\n");
            fprintf(fp,"\n");
            return; break;
        }

        fprintf(fp,"    mov rbx, rbp\n");
        fprintf(fp,"    sub rbx, %d\n",find->offset+find->width);           
        fprintf(fp,"    pop rax\n");
        if((find->type).dtype == INTEGER)
          fprintf(fp,"    mov word[rbx], ax\n");
        else 
          fprintf(fp,"    mov byte[rbx], al\n");
        fprintf(fp,"\n");
        return; break;
    default:
        if((ast->child[0]->val).value.t_val==EPSILON)
          return;

        for(int i=0;i<ast->no_child;i++)
          generateAssembly(ast->child[i], fp);

        return; break;    
  }
}

