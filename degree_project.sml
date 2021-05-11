(*pasring 模块的signature*)
signature REGEXP =
sig
    (*正则表达式的类型声明*)
    datatype regexp = None
                 | Empty
                 | Char of char
                 | AnyChar
                 | Or of regexp * regexp
                 | Concat of regexp * regexp
                 | Star of regexp
    
    exception SyntaxError of string
    (*parse 函数 从 string 类型映射到regexp 类型*)
    val parse : string -> regexp
    val token_print: string -> string
end


(*parsing 模块的实现*)
structure RegExp :> REGEXP =
struct
    datatype regexp = None
                 | Empty
                 | Char of char
                 | AnyChar
                 | Or of regexp * regexp
                 | Concat of regexp * regexp
                 | Star of regexp
    
    (*raised when there is a syntax error*)
    exception SyntaxError of string

    (*tokens 类型声明*)
    datatype token = AtSign 
                    | Literal of char 
                    | Pipe 
                    | Percent 
                    | Asterisk 
                    | LParen 
                    | RParen 
                    | Period

    (*raised when there is a token that we can not recognize*)
    exception LexialError

    (*Lexial Analysis*)
    (*通过模式匹配进行词法分析 *)
    fun tokenize (inp : char list) =
        (
            case inp of 
                nil => nil 
                | (#"+"::cs) =>(Pipe ::tokenize cs)
                | (#"%":: cs)=> (Percent :: tokenize cs)
                | (#".":: cs)=> (Period :: tokenize cs)
                | (#"*":: cs) => (Asterisk :: tokenize cs)
                | (#"[":: cs) => (LParen :: tokenize cs)
                | (#"]"::cs)=> (RParen :: tokenize cs)
                | (#"@":: cs) => (AtSign :: tokenize cs)
                | (#" " :: cs) => tokenize cs
                | (C :: cs) => (Literal C :: tokenize cs)
        )



    (*paring functions *)
    (*正则表达式的上下文无关文法
        rexp ::= rtrm | rtrm+rexp
        rtrm ::= rfac | rfac.rtrm
        rfac ::= ratm | ratm*
        ratm ::= @ | % | a | (rexp)
        其中表示或的“|”和上下文无关文法里面的
        “|”冲突，所以用+符号来替代
    *)

    
    fun parse_exp ts =
        let val (r, ts') = parse_term ts
            in 
                case ts' of 
                    (Pipe :: ts'') =>
                    let val (r', ts''') = parse_exp ts''
                    in
                        (Or(r, r'), ts''')
                    end
                | _ => (r, ts')
        end
        
    and parse_term ts =
        let val (r, ts')= parse_factor ts
        in 
            case ts' of 
                ((AtSign | Percent | Period | Literal _| LParen):: ts) =>
                    let val(r',ts'') = parse_term ts'
                    in
                        (Concat (r, r'), ts'')
                    end
                | _ => (r, ts')
        end


    and parse_factor ts =
        let val (r,ts') = parse_atom ts 
        in 
            case ts' of 
                (Asterisk :: ts'') => (Star r, ts'')
                | _ => (r, ts')
        end

    and parse_atom nil = raise SyntaxError ("Factor expected\n")
        | parse_atom (AtSign:: ts) = (None, ts)
        | parse_atom (Period :: ts) = (AnyChar,ts)
        | parse_atom (Percent:: ts)=(Empty,ts)
        | parse_atom ((Literal c):: ts)=(Char c, ts)
        | parse_atom (LParen :: ts) =
            let 
                val(r, ts')= parse_exp ts
            in 
                case ts' of 
                    (RParen :: ts'')=> (r, ts'')
                    | _ => raise SyntaxError ("Right-parenthesis expected\n")
            end
        | parse_atom _ =  raise SyntaxError "Not an atom \n"

    
    fun parse s =
        let
            val (r , ts) = parse_exp (tokenize(String.explode s))
        in
            case ts of
                nil => r
            | _ => raise SyntaxError " Unexpected input. \n"
        end
            handle LexialError => raise SyntaxError "Illegal input. \n"


    (*Das ist Scheiße!!!*)
    fun preso_token (inp: char list)=
         (
            case inp of 
                nil => nil 
                | (#"+"::cs) =>(" Pipe " ::preso_token cs)
                | (#"%":: cs)=> (" Percent " :: preso_token cs)
                | (#".":: cs)=> (" Period" :: preso_token cs)
                | (#"*":: cs) => (" Asterisk" :: preso_token cs)
                | (#"[":: cs) => (" LParen " :: preso_token cs)
                | (#"]"::cs)=> (" RParen " :: preso_token cs)
                | (#"@":: cs) => (" AtSign " :: preso_token cs)
                | (#" " :: cs) => preso_token cs
                | (C :: cs) => ( Char.toString(C) :: preso_token cs)
        )

    fun token_print(inp : string) = 
        String.concat(preso_token (String.explode inp))
        
end

(***==============================just base on Robert Harper's book==========================================*)
(*匹配正则表达式模块的signature*)
signature MATCHER =
sig
   structure RegExp: REGEXP
   val match: string ->string -> bool
end

structure MATCHER :> MATCHER =
struct
    structure RegExp = RegExp
    open RegExp

    (*cominator组合子， 用于组合match,和类型声明相对应*)
    fun FAIL cs k = false
    fun NULL cs k = k cs
    fun LITERALLY c cs k = case cs of
       nil => false
     | c'::cs' => (c=c') andalso (k cs')
    
    fun OR (m1,m2) cs k = 
        m1 cs k orelse m2 cs k
    infix 8 OR
    fun THEN (m1,m2) cs k = 
        m1 cs (fn cs' => m2 cs' k)
    infix 9 THEN
    fun REPEATED m cs k =
        (NULL OR (m THEN(REPEATED m))) cs k

    (*AST matching，Robert Harper书上的算法*)
    fun match' None = FAIL
        | match' Empty = NULL
        | match' (Char c)= LITERALLY c 
        | match' (Or(r1,r2)) = match' r1 OR match' r2
        | match' (Concat(r1,r2))=match' r1 THEN match' r2
        | match' (Star r)= REPEATED(match' r)
    
    fun match (regexp: string) (string: string) =
        match'  (parse regexp) (String.explode string) (fn nil => true| _ =>false)
end

fun match (regex: string)(string: string): bool=
    MATCHER.match regex string

fun parse (regex : string) =
    RegExp.parse regex

fun tokenize (regex: string)=
    RegExp.token_print regex

(*输入输出处理*)
(*
val args = CommandLine.arguments() 
val str1 = hd args
val str2 = hd (tl args)
val result = match str1 str2
val _ = print ("The regular expression is  "^str1^" and the string is "^str2^"\n")
val _ = print ("The token series is "^ tokenize str1^"\n")
val _ = print ("The match result is: "^Bool.toString result^"\n")
val _ = OS.Process.exit(OS.Process.success) 
*)
