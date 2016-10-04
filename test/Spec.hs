import Test.HUnit
import Text.Parsec

import VCMangler
import VCMangler.Types
import VCMangler.Parsers

{-
    The mangled names for these tests come directly from Visual Studio 2015
-}

parseType :: String -> Either String Type
parseType s = case runParser typeParser (MangledState [] []) "(source)" s of
                (Left e) -> Left (show e)
                (Right t) -> Right t

parseName :: String -> Either String QualifiedName
parseName s = case runParser qualifiedNameParser (MangledState [] []) "(source)" s of
                        (Left e) -> Left (show e)
                        (Right t) -> Right t

liftTypes r ts = Right (Function (Name "Test") (Global Near CDecl) r ts)
liftName n = Right (Function n (Global Near CDecl) Void_ [Void_])
liftTemplateArgs ts = Right (Function (TName "Test" ts) (Global Near CDecl) Void_ [Void_])

-- Test double qualified name with two replacements

nameTests = test
    [ "X::Test" ~: parseMangledName "?Test@X@@YAXXZ" ~?= liftName (Qualifier "X" (Name "Test"))
    , "Y::X::Test" ~: parseMangledName "?Test@X@Y@@YAXXZ" ~?= liftName (Qualifier "Y" (Qualifier "X" (Name "Test")))
    , "<>::Test" ~: parseMangledName "?Test@?A0x15b4d841@@YAXXZ" ~?= liftName (Qualifier "0x15b4d841" (Name "Test"))
    , "$"        ~: parseName "$@" ~?= Right (Name "$")
    ,  "Test<'void Wrapper(void)'::'2'::X>'" ~: parseName "?$Test@UX@?1??Wrapper@@YAXXZ@@@" ~?= Right (TName "Test" [TType (Composite Struct (FQualifier (Function (Name "Wrapper") (Global Near CDecl) Void_ [Void_]) (Qualifier "2" (Name "X"))))])
    ]

typeTests = test 
    [ "void -> void" ~: parseMangledName "?Test@@YAXXZ" ~?= liftTypes Void_ [Void_]
    , "void -> bool" ~: parseMangledName "?Test@@YA_NXZ" ~?= liftTypes Bool_ [Void_]
    , "enum X -> void" ~: parseMangledName "?Test@@YAXW4X@@@Z" ~?= liftTypes Void_ [Composite Enum (Name "X")]
    , "(void -> void) -> void" ~: parseMangledName "?Test@@YAXP8X@@AEXXZ@Z" ~?= liftTypes Void_ [FunctionPointer ThisCall Void_ [Void_]]
    , "(int -> void) -> void" ~: parseMangledName "?Test@@YAXP8X@@AEXH@Z@Z" ~?= liftTypes Void_ [FunctionPointer ThisCall Void_ [Int_]]
    ] 

referenceTests = test
    [ "int &"                           ~: parseType "AAH"  ~?= Right (Reference [] [] Int_)
    , "int & restrict"                  ~: parseType "AIAH" ~?= Right (Reference [] [Restrict] Int_)
    , "int const &"                     ~: parseType "ABH"  ~?= Right (Reference [Const] [] Int_)
    , "int const & restrict"            ~: parseType "AIBH" ~?= Right (Reference [Const] [Restrict] Int_)
    , "int volatile &"                  ~: parseType "ACH"  ~?= Right (Reference [Volatile] [] Int_)
    , "int volatile & restrict"         ~: parseType "AICH" ~?= Right (Reference [Volatile] [Restrict] Int_)
    , "int const volatile &"            ~: parseType "ADH"  ~?= Right (Reference [Const, Volatile] [] Int_)
    , "int const volatile & restrict"   ~: parseType "AIDH" ~?= Right (Reference [Const, Volatile] [Restrict] Int_)
    ]

rValueReferenceTests = test
    [ "int &&"                          ~: parseType "$$QAH"  ~?= Right (RValueReference [] [] Int_)
    , "int && restrict"                 ~: parseType "$$QIAH" ~?= Right (RValueReference [] [Restrict] Int_)
    , "int const &&"                    ~: parseType "$$QBH"  ~?= Right (RValueReference [Const] [] Int_)
    , "int const && restrict"           ~: parseType "$$QIBH" ~?= Right (RValueReference [Const] [Restrict] Int_)
    , "int volatile &&"                 ~: parseType "$$QCH"  ~?= Right (RValueReference [Volatile] [] Int_)
    , "int volatile && restrict"        ~: parseType "$$QICH" ~?= Right (RValueReference [Volatile] [Restrict] Int_)
    , "int const volatile &&"           ~: parseType "$$QDH"  ~?= Right (RValueReference [Const, Volatile] [] Int_)
    , "int const volatile && restrict"  ~: parseType "$$QIDH" ~?= Right (RValueReference [Const, Volatile] [Restrict] Int_)
    ]

pointerTests = test
    [ "int *"                                         ~: parseType "PAH"  ~?= Right (Pointer [] [] Int_)
    , "int * const"                                   ~: parseType "QAH"  ~?= Right (Pointer [] [Const] Int_)
    , "int * volatile"                                ~: parseType "RAH"  ~?= Right (Pointer [] [Volatile] Int_)
    , "int * restrict"                                ~: parseType "PIAH" ~?= Right (Pointer [] [Restrict] Int_)
    , "int * const volatile"                          ~: parseType "SAH"  ~?= Right (Pointer [] [Const, Volatile] Int_)
    , "int * const restrict"                          ~: parseType "QIAH" ~?= Right (Pointer [] [Const, Restrict] Int_)
    , "int * volatile restrict"                       ~: parseType "RIAH" ~?= Right (Pointer [] [Volatile, Restrict] Int_)
    , "int * const volatile restrict"                 ~: parseType "SIAH" ~?= Right (Pointer [] [Const, Volatile, Restrict] Int_)
    , "int const *"                                   ~: parseType "PBH"  ~?= Right (Pointer [Const] [] Int_)
    , "int const * const"                             ~: parseType "QBH"  ~?= Right (Pointer [Const] [Const] Int_)
    , "int const * volatile"                          ~: parseType "RBH"  ~?= Right (Pointer [Const] [Volatile] Int_)
    , "int const * restrict"                          ~: parseType "PIBH" ~?= Right (Pointer [Const] [Restrict] Int_)
    , "int const * const volatile"                    ~: parseType "SBH"  ~?= Right (Pointer [Const] [Const, Volatile] Int_)
    , "int const * const restrict"                    ~: parseType "QIBH" ~?= Right (Pointer [Const] [Const, Restrict] Int_)
    , "int const * restrict volatile"                 ~: parseType "RIBH" ~?= Right (Pointer [Const] [Volatile, Restrict] Int_)
    , "int const * const volatile restrict"           ~: parseType "SIBH" ~?= Right (Pointer [Const] [Const, Volatile, Restrict] Int_)
    , "int volatile *"                                ~: parseType "PCH"  ~?= Right (Pointer [Volatile] [] Int_)
    , "int volatile * const"                          ~: parseType "QCH"  ~?= Right (Pointer [Volatile] [Const] Int_)
    , "int volatile * volatile"                       ~: parseType "RCH"  ~?= Right (Pointer [Volatile] [Volatile] Int_)
    , "int volatile * restrict"                       ~: parseType "PICH" ~?= Right (Pointer [Volatile] [Restrict] Int_)
    , "int volatile * const volatile"                 ~: parseType "SCH"  ~?= Right (Pointer [Volatile] [Const, Volatile] Int_)
    , "int volatile * const restrict"                 ~: parseType "QICH" ~?= Right (Pointer [Volatile] [Const, Restrict] Int_)
    , "int volatile * restrict volatile"              ~: parseType "RICH" ~?= Right (Pointer [Volatile] [Volatile, Restrict] Int_)
    , "int volatile * const volatile restrict"        ~: parseType "SICH" ~?= Right (Pointer [Volatile] [Const, Volatile, Restrict] Int_)
    , "int const volatile *"                          ~: parseType "PDH"  ~?= Right (Pointer [Const, Volatile] [] Int_)
    , "int const volatile * const"                    ~: parseType "QDH"  ~?= Right (Pointer [Const, Volatile] [Const] Int_)
    , "int const volatile * volatile"                 ~: parseType "RDH"  ~?= Right (Pointer [Const, Volatile] [Volatile] Int_)
    , "int const volatile * restrict"                 ~: parseType "PIDH" ~?= Right (Pointer [Const, Volatile] [Restrict] Int_)
    , "int const volatile * const volatile"           ~: parseType "SDH"  ~?= Right (Pointer [Const, Volatile] [Const, Volatile] Int_)
    , "int const volatile * const restrict"           ~: parseType "QIDH" ~?= Right (Pointer [Const, Volatile] [Const, Restrict] Int_)
    , "int const volatile * restrict volatile"        ~: parseType "RIDH" ~?= Right (Pointer [Const, Volatile] [Volatile, Restrict] Int_)
    , "int const volatile * const volatile restrict"  ~: parseType "SIDH" ~?= Right (Pointer [Const, Volatile] [Const, Volatile, Restrict] Int_)
    ]

operatorTests = test
    [ "X::X()" ~: parseMangledName "??0X@@QAE@XZ" ~?= Right (Function (Qualifier "X" (OpName OpConstructor)) (Member Near ThisCall Method Public) (Composite Class (Name "X")) [Void_])
    ]

templateTests = test
    [ "Test<int>"                   ~: parseName "?$Test@H@"        ~?= Right (TName "Test" [TType Int_])
    , "Test<int,int>"               ~: parseName "?$Test@HH@"       ~?= Right (TName "Test" [TType Int_, TType Int_])
    , "Test<X<int>>"                ~: parseName "?$Test@V?$X@H@@@" ~?= Right (TName "Test" [TType (Composite Class (TName "X" [TType Int_] ))])
    , "Test<-100>"                  ~: parseName "?$Test@$0?GE@@"   ~?= Right (TName "Test" [TValue (-100)])
    , "Test<-10>"                   ~: parseName "?$Test@$0?9@"     ~?= Right (TName "Test" [TValue (-10)])
    , "Test<-1>"                    ~: parseName "?$Test@$0?0@"     ~?= Right (TName "Test" [TValue (-1)])
    , "Test<0>"                     ~: parseName "?$Test@$0A@@"     ~?= Right (TName "Test" [TValue 0])
    , "Test<1>"                     ~: parseName "?$Test@$00@"      ~?= Right (TName "Test" [TValue 1])
    , "Test<10>"                    ~: parseName "?$Test@$09@"      ~?= Right (TName "Test" [TValue 10])
    , "Test<100>"                   ~: parseName "?$Test@$0GE@@"    ~?= Right (TName "Test" [TValue 100])
    , "Test<int const>"             ~: parseName "?$Test@$$CBH@@@"  ~?= Right (TName "Test" [TType (Qualified [Const] Int_)])
    , "Test<int volatile>"          ~: parseName "?$Test@$$CCH@@@"  ~?= Right (TName "Test" [TType (Qualified [Volatile] Int_)])
    , "Test<int const volatile>"    ~: parseName "?$Test@$$CDH@@@"  ~?= Right (TName "Test" [TType (Qualified [Const, Volatile] Int_)])
    ]

templateNameTests = test
    [ "Test<void X(void)>"          ~: parseName "?$Test@$1?X@@YAXXZ@" ~?= Right (TName "Test" [TFunc (Function (Name "X") (Global Near CDecl) Void_ [Void_])])
    ]

typeReplacementTests = test
    [ "X -> X -> void" ~: parseMangledName "?Test@@YAXVX@@0@Z" ~?= liftTypes Void_ [Composite Class (Name "X"), Composite Class (Name "X")]
    , "int -> X -> X -> void" ~: parseMangledName "?Test@@YAXHVX@@0@Z" ~?= liftTypes Void_ [Int_, Composite Class (Name "X"), Composite Class (Name "X")]
    , "wchar_t -> wchar_t -> void" ~: parseMangledName "?Test@@YAX_W0@Z" ~?= liftTypes Void_ [WCharT_, WCharT_]
    , "bool -> bool -> void" ~: parseMangledName "?Test@@YAX_N0@Z" ~?= liftTypes Void_ [Bool_, Bool_]
    , "bool -> (bool -> void) -> void" ~: parseMangledName "?Test@@YAX_NP6AX0@Z@Z" ~?= liftTypes Void_ [Bool_, FunctionPointer CDecl Void_ [Bool_]]
    , "(bool -> void) -> bool -> void" ~: parseMangledName "?Test@@YAXP6AX_N@Z0@Z" ~?= liftTypes Void_ [FunctionPointer CDecl Void_ [Bool_], Bool_]
    ]

nameReplacementTests = test
    [ "X* -> X" ~: parseMangledName "?Test@@YA?AVX@@PAV1@@Z" ~?= liftTypes (Composite Class (Name "X")) [Pointer [] [] (Composite Class (Name "X"))]
    , "X -> X" ~: parseMangledName "?Test@@YA?AVX@@V1@@Z" ~?= liftTypes (Composite Class (Name "X")) [Composite Class (Name "X")]
    , "void Z::Test(X<Y,Y>)" ~: parseMangledName "?Test@Z@@YAXV?$X@VY@@V1@@@@Z" ~?= Right (Function (Qualifier "Z" (Name "Test")) (Global Near CDecl) Void_ [Composite Class (TName "X" [TType (Composite Class (Name "Y")), TType (Composite Class (Name "Y"))])])
    , "void X::Test(X,int)" ~: parseMangledName "?Test@X@@QAEXV1@H@Z" ~?= Right (Function (Qualifier "X" (Name "Test")) (Member Near ThisCall Method Public) Void_ [Composite Class (Name "X"), Int_])
    , "X<int>::Test(X<int>)" ~: parseMangledName "?Test@?$X@H@@SAXAAV1@@Z" ~?= Right (Function (TQualifier "X" [TType Int_] (Name "Test")) (Member Near CDecl Static Public) Void_ [Reference [] [] (Composite Class (TName "X" [TType Int_]))])
    , "<anon>::X::Test(<anon>::X)" ~: parseMangledName "?Test@X@?A0x15b4d841@@SAXV12@@Z" ~?= Right (Function (Qualifier "0x15b4d841" (Qualifier "X" (Name "Test"))) (Member Near CDecl Static Public) Void_ [Composite Class (Qualifier "0x15b4d841" (Name "X"))])
    , "X::X(int, X)" ~: parseMangledName "??0X@@QAE@HV0@@Z" ~?= Right (Function (Qualifier "X" (OpName OpConstructor)) (Member Near ThisCall Method Public) (Composite Class (Name "X")) [Int_, Composite Class (Name "X")])
    ]

debug = test
    [ "debug" ~: parseMangledName "?RemovePathBeginning@CZipArchive@@SA_NPBDAAV?$CStringT@DV?$StrTraitMFC@DV?$ChTraitsCRT@D@ATL@@@@@ATL@@P823@BEH0@Z@Z" ~?= Right (Function (Name "X Gonna") (Global Near CDecl) Void_ [Void_])
    ]

tests = TestList [ TestLabel "Name Tests" nameTests
                 , TestLabel "Type Tests" typeTests
                 , TestLabel "Pointer Tests" pointerTests
                 , TestLabel "Reference Tests" referenceTests
                 , TestLabel "RValueReference Tests" rValueReferenceTests
                 , TestLabel "Operator Tests" operatorTests
                 , TestLabel "Type Replacement Tests" typeReplacementTests
                 , TestLabel "Name Replacement Tests" nameReplacementTests 
                 , TestLabel "Template Tests" templateTests
                 , TestLabel "Template Name Tests" templateNameTests
                 ]

main = runTestTT debug
