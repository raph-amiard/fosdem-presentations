---
author:
- Pierre-Marie de Rodat
- Raphaël Amiard
title: Easy Ada tooling with Libadalang
titlepage-note: |
 Title notes...
institute: Software Engineers at AdaCore
theme: metropolis
...

## The need

### In three bullet points

- A library that allows users to query/alter data about Ada sources
- Both low & high level APIS:
    * What is the type of this expression?
    * How many references to this variable?
    * Give me the source location of this token
    * Rename this entity
    * Etc.
- Multi-language: Easy binding generation to other languages/ecosystems
    * Today: Python, Ada, C
- Easy scripting: Be able to create a prototype quickly & interactively

## The need - IDEs

![Syntax & block highlighting](gps-block-highlighting.png){ width=70% }

## The need - IDEs

![Cross references](fosdem-xrefs.png){ width=90% }

## The need - IDEs

![Refactoring](fosdem-refactoring.png){ width=90% }

## The need - command line tools

```ada
procedure Main is
   type my_int is new Integer range 1 .. 10;
   Var : my_int := 12;
begin
   null;
end Main;
```

```bash
$ ./my_custom_lal_checker main.adb
main.adb:2:9: type name should start with uppercase letter
main.adb:3:3: variable name should start with lowercase letter
```
## Why not ASIS/GNAT?

### Challenges for ASIS's GNAT implementation

- Incremental: don't recompute everything when the code changes
- Error recovery: ability to compute partial results on incorrect code
- Long running: be able to run for 3 days without crashing your machine

GNAT based ASIS implementation is ill suited to those challenges.

### API problems

- ASIS API is too low level/too difficult to change
- Desire for a more modern, higher level API

## Why not blank slate implementation of ASIS?

- ASIS specifies a complicated API
- A lot of work to create a new implementation
- And then, it is still not what we want! We still need to:
    - Change most parts of the API.
    - Add a lot of operations (refactoring API, higher level semantic queries,
      etc..)
    - Specify how error recovery works with ASIS
    - ...

So better to start from scratch :)

## API Part 1: Tokens

```ada
--  main.adb
procedure Main is null;
```

```python
ctx = lal.AnalysisContext()
unit = ctx.get_from_file('main.adb')
for token in unit.root.tokens:
    print 'Token: {}'.format(token)
```

Outputs:
```
Token: <Token Procedure u'procedure' at 1:1-1:10>
Token: <Token Identifier u'Main' at 1:11-1:15>
Token: <Token Is u'is' at 1:16-1:18>
Token: <Token Null u'null' at 1:19-1:23>
Token: <Token Semicolon u';' at 1:23-1:24>
```

## API Part 2: Syntax

```ada
procedure Main is
   A : Integer := 12;
   B, C : Integer := 15;
begin
   A := B + C;
end Main;
```

```python
for object_decl in unit.root.findall(lal.ObjectDecl):
    print object_decl.sloc_range, object_decl.text
```

Outputs:
```
2:4-2:22 A : Integer := 12;
3:4-3:25 B, C : Integer := 15;
```

## API Part 3: Semantic

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    function Double (I : Integer) return Integer is (I * 2);
    function Double (I : Float) return Float is (I * 2.0);
begin
    Put_Line (Integer'Image (Double (12)));
end Main;
```

```python
double_call = unit.root.find(
    lambda n: n.is_a(lal.CallExpr) and n.f_name.text == 'Double'
)

print double_call.f_name.p_referenced_decl.text
```

Outputs:
```
function Double (I : Integer) return Integer is (I * 2);
```

## API Part 4: Tree rewriting (not finished yet!)

```ada
procedure Main is
begin
    Put_Line ("Hello world");
end Main;
```

Let's rewrite:
```python
call = unit.root.findall(lal.CallExpr) # Find the call
diff = ctx.start_rewriting() # Start a rewriting
param_diff = diff.get_node(call.f_suffix[0]) # Get the param of the call
# Replace the expression of the parameter with a new node
param_diff.f_expr = lal.rewriting.StringLiteral('"Bye world"')
diff.apply()
```

Outputs:
```ada
procedure Main is
begin
    Put_Line ("Bye world");
end Main;
```

## An example

```python
import sys
import libadalang as lal

def check_ident(ident):
    if ident.text[0].isupper():
        print '{}:{}: variable name "{}" should be capitalized'.format(
            ident.unit.filename, ident.sloc_range.start, ident.text
        )

ctx = lal.AnalysisContext()
for filename in sys.argv[1:]:
    u = ctx.get_from_file(filename)
    for d in u.diagnostics:
        print '{}:{}'.format(filename, d)
    if u.root:
        for decl in u.root.findall(lal.ObjectDecl):
            for ident in decl.f_ids:
                check_ident(ident)
```

# Technical prototypes/demos

## Syntax highlighter/Xref explorer

![Libadalang based highlighter](lal-highlight.png){ width=90% }

## Syntax based static analyzers

```python
def has_same_operands(binop):
    def same_tokens(left, right):
        return len(left) == len(right) and all(
            le.is_equivalent(ri) for le, ri in zip(left, right)
        )
    return same_tokens(list(binop.f_left.tokens), list(binop.f_right.tokens))

def interesting_oper(op):
    return not op.is_a(lal.OpMult, lal.OpPlus, lal.OpDoubleDot,
                       lal.OpPow, lal.OpConcat))

for b in unit.root.findall(lal.BinOp):
    if interesting_oper(b.f_op) and has_same_operands(b):
        print 'Same operands for {} in {}'.format(b, source_file)
```

Those 20 lines of code found 1 bug in GNAT, 3 bugs in CodePeer, and 1 bug in
GPS (despite extensive testing and static analysis).

More info [on our blog](http://blog.adacore.com/going-after-the-low-hanging-bug)

## Semantic based static analyzers

```ada
with Ada.Text_IO; use Ada.Text_IO;
 
procedure Main is
   Input : File_Type;
begin
   Open (File => Input, Mode => In_File, Name => "input.txt");

   while not End_Of_File (Input) loop
      declare
         Line : String := Get_Line (Input);  <--- WARNING: File might be closed
      begin
         Put_Line (Line);
         Close (Input);    <--- WARNING: File might be closed
      end;
   end loop;
end Main;
```

- Very simple and targeted abstract interpretation
- DSL to specify new checkers
- Work in progress! Repository here [https://github.com/AdaCore/lal-checkers](https://github.com/AdaCore/lal-checkers)

## Copy paste detector

- Done with Python API too
- Very lightweight (few hundreds lines of code)
- Full article here: [https://blog.adacore.com/a-usable-copy-paste-detector-in-few-lines-of-python](https://blog.adacore.com/a-usable-copy-paste-detector-in-few-lines-of-python)

## Applications

- Inside Adacore: change semantic engine in GPS, new versions of GNATmetric,
  GNATStub, GNATpp
- Outside: clients using it in production for various needs such as:
    * Code instrumentation
    * Automatic refactorings
    * Generation of serializers/deserializers

## Conclusion

- Sources are on GitHub: [https://github.com/AdaCore/libadalang](https://github.com/AdaCore/libadalang)
- Come open issues and create pull requests!
- API is still a moving target
