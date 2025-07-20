# FP Exam 2024 WS

## Overview

| # | Points | Topics                                                         |
|---|--------|----------------------------------------------------------------|
| 1 |     20 | Functions, Lists, Recursion                                    |
| 2 |     15 | Higher-Order Functions                                         |
| 3 |      5 | Lazy Evaluation                                                |
| 4 |     10 | Type Classes (& IO)                                            |
| 5 |     30 | Functors & Monads & Applicatives & Parser & Monad Transformers |
| 6 |      5 | GADTs                                                          |
| 7 |     25 | Lambda Calculus (Part 1+2+poly)                                |
| 8 |     10 | Generic Programming                                            |

## Rules

- The exam is divided into 8 exercises.

- Each exercise covers the content of one or more lectures of the course.

- There are a total of 120 points to achieve in 120 minutes.

- The amount of points of an exercise does *not* necessarily reflect the
  required time.

- Each exercise consists of

  - an exercise file, which you should edit, e.g. `Ex1FunctionsListsRecursion.hs`; and

  - may be accompanied by library code, e.g. `LibParser.hs`.

- **IMPORTANT**

  - You are **not** allowed to change any code outside of the designated submission areas.
    In particular, you are not allowed to add imports or change datatypes, which
    we provide for you. The designated submission areas look for example as followed:

        -- SUBMISSION BEGIN 1.1
        mapList :: (a -> b) -> [a] -> [b]
        mapList = undefined
        -- SUBMISSION END

    This is very important, since we developed an automated grading tool, which
    will only consider code inside of the submission areas.

    Also take care not to edit the submission markers themselves in any way, e.g.
    changing 

        -- SUBMISSION BEGIN 1.1

    to

        -- SUBMISSION BEGIN 9.9

    is forbidden.

  - You are **not** allowed to change the type signatures provided for the functions
    inside of the submission areas. For example, it is **not** allowed
    to swap the arguments of `mapList` in the above example, i.e. to change

        -- SUBMISSION BEGIN 1.1
        mapList :: (a -> b) -> [a] -> [b]
        mapList = undefined
        -- SUBMISSION END

    to

        -- SUBMISSION BEGIN 1.1
        mapList :: [a] -> (a -> b) -> [b]
        mapList = undefined
        -- SUBMISSION END
    
  - Make sure that the code in the submission areas does type check.
    Otherwise it will not be graded.

