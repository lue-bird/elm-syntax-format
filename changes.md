#### 1.1.5
  - in module `ElmSyntaxParserLenient`
      - allow any indentation for `}`, `]`, `)`, `=`, `:`, `,`, `|`, `->`, `..`, `then`, `else`, `of`, `as`, `exposing`, `in`, `where`, operators, `(` after exposing, `module` after `port`, exposing elements, list elements, on True, in parens, after tuple `,`, on False, after case ->, between case and of, lambda argument patterns, lambda result, after operator, expression after =, type after :, type after type alias =, first variant after type =
  - in module `ElmSyntaxPrint`
      - correct character in """ string literal that spans multiple UTF-16 codes
      - TODO find uot why some """ in triple double equals string doesn't get escaped (? solved)

#### 1.1.4
  - in module `ElmSyntaxPrint`
      - correct triple double quoted string literal escaping behavior
      - faster

#### 1.1.3
  - in module `ElmSyntaxParserLenient`
      - now merges consecutive `|`s in choice type declaration
      - now after import removes empty `exposing ()`
  - in module `ElmSyntaxPrint`
      - correctly collapse comments in pattern list, in records, type record extensions, record update
      - correct patterns parenthesized multiple times sometimes not unwrapped fully
      - correct linebreak count in module without imports and module documentation and comments
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
