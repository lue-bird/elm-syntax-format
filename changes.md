#### 1.1.6
  - in module `ElmSyntaxParserLenient`
      - actually corrects `(...)` in all places
  - in module `ElmSyntaxPrint`
      - faster

#### 1.1.5
  - in module `ElmSyntaxParserLenient`
      - fix bug where multi-line expressions after an infix operator were sometimes indented to little
      - merges consecutive `->` in function type
      - allow any indentation with `)`, `]`, `}`, `,`, `->`, `=`, `|`, `:`, `::`, `in`, `of`, `as`, `then`, `else`, `exposing`, `where`, `..`, infix operators, list elements, record field names, record field values, exposing elements, `(` after exposing, on True, on False, result after case `->`, between `case` and `of`, parameters before `=`/`=`, type after `->`, pattern after `::`, name after pattern `as`, module name after start of module header, module name after `import`, `module` after `port`, type after `:`, in parens, type after type alias declaration `=`, type declaration name, type alias declaration name, variant name, let first declaration, let result after `in`, variant pattern arguments, lambda result, expression after value/function declaration `=`, tuple part after `,`, expression after infix operator
      - faster
  - in module `ElmSyntaxPrint`
      - correct character in """ string literal that spans multiple UTF-16 codes

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
