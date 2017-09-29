module WHILE-UB-SYNTAX
     syntax Exp ::= Int | Id
               | "(" Exp ")"             [bracket]
               | "-" Exp                 [strict]
               > left:
                    Exp "*" Exp             [strict, left]
               | Exp "/" Exp             [strict, left]
               | Exp "%" Exp             [strict, left]
               > left:
                    Exp "+" Exp             [strict, left]
               | Exp "-" Exp             [strict, left]
               > non-assoc:
                    Exp "<" Exp             [strict, non-assoc]
               | Exp "<=" Exp            [strict, non-assoc]
               | Exp ">" Exp             [strict, non-assoc]
               | Exp ">=" Exp            [strict, non-assoc]
               | Exp "==" Exp            [strict, non-assoc]
               | Exp "!=" Exp            [strict, non-assoc]
               > "!" Exp                 [strict]
               > left:
                    Exp "&&" Exp            [strict(1), left]
               | Exp "||" Exp            [strict(1), left]
               | Exp "+|+" Exp           [left]

     syntax Ids  ::= List{Id,","}
     syntax Exps ::= List{Exp,","}          [strict]

     syntax Stmt ::= "if" "(" Exp ")" "{" Stmts "}" [avoid, strict(1)]
                   | "if" "(" Exp ")" Stmt
                   | "while" "(" Exp ")" "{" Stmts "}"
                   | "obs" Exp ";"              [strict]
                   | Id ":=" Exp ";"            [strict(2)]
                   | "undef" ";"
                   | "nop" ";"

     syntax Stmts ::= Stmt
                    | Stmts Stmts      [right]

     rule if ( E ) S => if ( E ) { S } [macro]
endmodule

module WHILE-UB
     imports WHILE-UB-SYNTAX

     syntax KResult ::= Int

     configuration
          <T>
               <k> $PGM:Stmts </k>
               <id> 0 </id>
               <store> .Map </store>
               <in> .List </in>
               <out> .List </out>
          </T>

     rule I1 + I2 => I1 +Int I2
     rule I1 - I2 => I1 -Int I2
     rule I1 * I2 => I1 *Int I2
     rule I1 / I2 => I1 /Int I2 requires I2 =/=Int 0
     rule I1 % I2 => I1 %Int I2 requires I2 =/=Int 0
     rule - I => 0 -Int I
     rule I1 < I2 => I1 <Int I2
     rule I1 <= I2 => I1 <=Int I2
     rule I1 > I2 => I1 >Int I2
     rule I1 >= I2 => I1 >=Int I2

     rule V1:Int == V2:Int => V1 ==K V2
     rule V1:Int != V2:Int => V1 =/=K V2

     rule ! 0 => 1
     rule ! N => 0
          requires N =/=Int 0
     rule N && E => E
          requires N =/=Int 0
     rule 0 && _ => 0
     rule N || _ => 1
          requires N =/=Int 0
     rule 0 || E => E

     rule E +|+ _ => E
          [transition]
     rule _ +|+ E => E
          [transition]

     rule <k> X:Id => V ...</k>
          <store>... X |-> V:Int ...</store>

     rule <k> X:Id := V:Int ; => . ...</k>
          <store> Store:Map => Store[X <- V] </store>

     rule S1:Stmts S2:Stmts => S1 ~> S2

     rule if ( N ) { S } => S
          requires N =/=Int 0
     rule if ( 0 ) { _ } => .

     rule while ( E ) { S } => if (E) { S while(E) { S } }

     rule <k> obs V:Int ; => . ...</k>
          <out>... .List => ListItem(V) </out>

endmodule
