module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    , ("a", [("a", 1), ("A", 2), ("a", 7)]) ==> [1, 7]
    ]

splitTestCases
  = [ (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    , ("! ", "!Hello, World!!")
        ==> ("! !!", ["", "Hello,", "World", "", ""])
    ]

combineTestCases
  = [ (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A", "B"])
        ==> ["A", ".", "B"]
    , (" ", ["", "A"])
        ==> ["", " ", "A"]
    , (", !!!!", ["Hello", "", "World"])
        ==> ["Hello", ",", "", " ", "World", "!", "!!!"]
    ]

getKeywordDefsTestCases
  = [ ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    ]

expandTestCases
  = [ ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
    ]

enhancedExpandTestCases
  = [ ("Welcome to $town, where $name was born in $birth-date.","$name William \
       \Shakespeare\n$birth-date 1564\n$town Stratford upon \
       \Avon\n#\n$birth-date 1840\n$town Stinsford\n$name \
       \Thomas Hardy\n#\n$name Charles Dickens\n$town \
       \Landport\n$birth-date 1812")
      ==> "Welcome to Stratford upon Avon, where William Shakespeare \
      \was born in 1564.-----Welcome to Stinsford, where Thomas Hardy \
      \was born in 1840.-----Welcome to Landport, where Charles Dickens \
      \was born in 1812.-----"
  ]

allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "split"   (uncurry split)
                         splitTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases

    , TestCase "enhancedExpand" (uncurry enhancedExpand)
                                enhancedExpandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
