#### 1.1.3
  - in module `ElmSyntaxPrint`
      - correctly collapse comments in pattern list, in records, type record extensions, record update
      - correct patterns parenthesized multiple times sometimes not unwrapped fully
      - TODO correct linebreak count in module without imports and module documentation and imports
      - faster

#### 1.1.2
  - in module `ElmSyntaxParserLenient`
      - corrects `port module` to `module` if no ports exist and the other way round
      - corrects infix operator symbol `===` → `==`
      - corrects infix operator symbol `!==` → `/=`
      - corrects infix operator symbol `**` → `^`
      - corrects lambda `=>` or `.` to `->`

#### 1.1.1
  - in module `ElmSyntaxParserLenient`
      - removes extra commas in explicit exposing (thanks [@jfmengels](https://github.com/jfmengels) for suggesting!)
      - corrects exposing `(...)` → `(..)`
      - moves import statements anywhere at the top level to the import section (thanks [@jfmengels](https://github.com/jfmengels) for suggesting!)
  - in module `ElmSyntaxPrint`
      - faster

### 1.1.0
  - add module `ElmSyntaxParserLenient`

#### 1.0.2
  - fix linebreak count for modules with documentation comment but without imports

#### 1.0.1
  - fix type function `a -> (b -> c) -> d` being printed without the parens
  - fix type construct `A a` being printed in one line even though an argument is not on the same line
