module ParserTest where
import Parser.NlasParser
import Expression
import UnificationProblem
import UnificationContext
import DAG

-- Expected value on right
parse upFile "" "(a b) (c d) a = (c d)  (e f)b=(a b)c;"
@?=
Right [[AtomSuspension [(AtVar (Name "a"),AtVar (Name "b")),(AtVar (Name "c"),AtVar (Name "d"))] (AtVar (Name "a")),
AtomSuspension [(AtVar (Name "c"),AtVar (Name "d")),(AtVar (Name "e"),AtVar (Name "f"))] (AtVar (Name "b")),
AtomSuspension [(AtVar (Name "a"),AtVar (Name "b"))] (AtVar (Name "c"))]]


parse upFile "" "S = $app (\\ (a b) x -> x) S2 = $+ (a b) S3 (a b) a;"
@?=
Right [[ExpressionSuspension [] (ExVar (Name "S")),
        Fn "$app" [Lam ([(AtVar (Name "a"),AtVar (Name "b"))],AtVar (Name "x")) (AtomSuspension [] (AtVar (Name "x"))),
                   ExpressionSuspension [] (ExVar (Name "S2"))],
        Fn "$+" [ExpressionSuspension [(AtVar (Name "a"),AtVar (Name "b"))] (ExVar (Name "S3")),
                 AtomSuspension [(AtVar (Name "a"),AtVar (Name "b"))] (AtVar (Name "a"))]]]
