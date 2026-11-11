// Self-contained adaptations of examples in the pinned Coalton manual.
export const config = {name: 'Lisp + Coalton', storageKey: 'miricl.coalton.sessions.v1',
  draftKey: 'miricl.coalton.draft.v1', defaultMode: 'lisp',
  modes: ['lisp', 'expression', 'definitions'],
  modeLabels: {lisp: 'Lisp', expression: 'Coalton expression', definitions: 'Coalton definitions'},
  manualBase: 'https://coalton-lang.github.io/manual/',
  busyStatus: 'Evaluating…', rejectedMessage: 'Evaluation failed.', cancelledMessage: 'Evaluation cancelled.'};

export const examples = [
  {id: 'closures', category: 'HIGHER-ORDER FUNCTIONS', title: 'A function that makes functions', mode: 'expression',
    code: `(let ((make-adder (fn (amount) (fn (x) (+ amount x)))))
  (map (make-adder 2) (make-list 2 3 4 5)))`, expected: '(4 5 6 7)',
    note: 'Capture a value in a closure, then map the resulting function over a list.',
    source: 'Fixed-Arity Functions and Manual Currying', manual: ''},
  {id: 'fibonacci', category: 'TAIL RECURSION', title: 'The hundredth Fibonacci number', mode: 'expression',
    code: `(the Integer
  (rec % ((i 100) (a 0) (b 1))
    (cond
      ((== 0 i) a)
      ((== 1 i) b)
      (True (% (- i 1) b (+ a b))))))`, expected: '354224848179261915075',
    note: 'Use a tail-recursive loop and arbitrary-precision integers.',
    source: 'Tail-Recursion and rec', manual: ''},
  {id: 'reverse', category: 'PATTERN MATCHING', title: 'Reverse a list with rec', mode: 'expression',
    code: `(rec % ((result Nil) (remaining (make-list 1 2 3 4 5)))
  (match remaining
    ((Nil) result)
    ((Cons x xs) (% (Cons x result) xs))))`, expected: '(5 4 3 2 1)',
    note: 'Deconstruct a list with patterns and accumulate its elements in reverse order.',
    source: 'Tail-Recursion and rec', manual: ''},
  {id: 'optional', category: 'ALGEBRAIC DATA TYPES', title: 'Look through nested optionals', mode: 'expression',
    code: `(match (Some (Some 42))
  ((Some (Some value)) value)
  (_ 0))`, expected: '42',
    note: 'Match nested constructors, with a wildcard for the remaining cases.',
    source: 'Pattern Matching', manual: ''},
  {id: 'lisp-interop', category: 'COMMON LISP INTEROP', title: 'Borrow a Lisp formatter', mode: 'expression',
    code: `(lisp (-> String) ()
  (cl:format cl:nil "~R" 2026))`, expected: 'two thousand twenty-six',
    note: 'Call Common Lisp from a typed Coalton expression and return its string result.',
    source: 'Inline Lisp', manual: ''},
  {id: 'tree', category: 'TYPES AND RECURSION', title: 'Define and fold a polymorphic tree', mode: 'lisp',
    code: `(cl:progn (cl:in-package :coalton-user) (cl:values))

(coalton-toplevel
  (define-type (ReplTree :a)
    (ReplLeaf :a)
    (ReplBranch (ReplTree :a) :a (ReplTree :a)))
  (declare sum-repl-tree ((ReplTree Integer) -> Integer))
  (define (sum-repl-tree tree)
    (match tree
      ((ReplLeaf value) value)
      ((ReplBranch left value right)
       (+ (sum-repl-tree left) (+ value (sum-repl-tree right)))))))

(coalton
  (sum-repl-tree
    (ReplBranch (ReplLeaf 10) 20
      (ReplBranch (ReplLeaf 30) 40 (ReplLeaf 50)))))

(cl:progn (cl:in-package :cl-user) (cl:values))`, expected: '150',
    note: 'Define a parameterized data type and a recursive function, then call it from Lisp. The definitions remain in this session.',
    source: 'Data Types and Pattern Matching', manual: ''},
];
