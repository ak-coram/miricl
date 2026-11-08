#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Script, createContext} from 'node:vm';
import {Miricl} from '../scripts/miricl.mjs';
const vm = await Miricl.instantiate(fs.readFileSync(process.argv[2] ?? 'miricl.wasm'));
const context = createContext({vm, source: ''});
const evaluate = new Script('vm.evaluate(source).output');
const check = source => {
  context.source = source;
  assert.equal(evaluate.runInContext(context, {timeout: 2000}), 'T\n', source);
};
for (const source of [
  `(and (handler-case (exp -1000d0) (floating-point-underflow () t))
    (let ((miricl:*trap-floating-point-underflow* nil))
      (and (eql (exp -1000d0) 0d0) (eql (expt 1d-300 2) 0d0)
        (handler-case (exp 1000d0) (floating-point-overflow () t))))
    (handler-case (exp -1000d0) (floating-point-underflow () t)))`,
  `(progn (deftype coerced-flonum () '(double-float * *))
    (let ((value 5d0))
      (and (eql (coerce 5 'coerced-flonum) 5d0)
        (eq value (coerce value '(double-float 0 10)))
        (eql (coerce 1/2 '(single-float (0) (1))) 0.5)
        (eql (coerce 1/2 '(float * *)) 0.5)
        (handler-case (coerce 5 '(double-float 0 4)) (type-error () t))
        (handler-case (coerce #C(1 2) 'coerced-flonum) (type-error () t)))))`,
  `(let ((*read-default-float-format* 'double-float))
    (and (string-equal (write-to-string 2d-7) "2.0e-7")
      (string-equal (write-to-string 17.29d0) "17.29")
      (string-equal (format nil "~A" 0.1d0) "0.1")))`,
  `(let ((*read-default-float-format* 'double-float))
    (every (lambda (x) (let ((y (read-from-string (write-to-string x))))
      (and (eql x y) (= (float-sign x) (float-sign y)))))
      (list 0d0 -0d0 0.1d0 17.29d0 2d-7 1d7 1d-3
        least-positive-single-float least-positive-double-float
        least-positive-normalized-single-float least-positive-normalized-double-float
        most-positive-single-float most-positive-double-float)))`,
  `(let ((*read-default-float-format* 'double-float) (seed 1729))
    (loop for type in '(single-float double-float)
      for bits in '(24 53) for low in '(-149 -1074) for high in '(104 970) always
      (loop repeat 64
        for significand = (progn (setq seed (mod (+ (* seed 6364136223846793005) 1) (expt 2 64)))
          (1+ (mod seed (1- (expt 2 bits)))))
        for exponent = (+ low (mod (ash seed -32) (1+ (- high low))))
        for x = (scale-float (coerce significand type) exponent)
        always (eql x (read-from-string (write-to-string x))))))`,
  `(and (= (float-radix 1.0) 2) (= (float-digits 1.0) 24) (= (float-digits 1d0) 53)
    (= (float-precision 0.0) 0) (= (float-precision least-positive-single-float) 1)
    (= (float-precision least-positive-double-float) 1))`,
  `(and (equal (multiple-value-list (integer-decode-float 1.0)) '(8388608 -23 1))
    (equal (multiple-value-list (integer-decode-float least-positive-double-float)) '(1 -1074 1))
    (equal (multiple-value-list (decode-float -0.0)) '(0.0 0 -1.0)))`,
  `(and (eql (float-sign -0.0) -1.0) (eql (float-sign -0d0 1.0) -1d0)
    (eql (float-sign 1.0 -0d0) 0d0) (= (float-sign (scale-float -0d0 (expt 2 100))) -1d0)
    (= (float-sign (scale-float -1.0 (- (expt 2 100)))) -1.0))`,
  `(and (= (scale-float least-positive-double-float 1074) 1d0)
    (= (scale-float 1d0 -1074) least-positive-double-float)
    (= (scale-float least-positive-single-float 149) 1.0)
    (= (scale-float 1.0 -149) least-positive-single-float))`,
  `(every (lambda (x)
    (multiple-value-bind (s e sign) (decode-float x)
      (and (typep s (type-of x)) (typep sign (type-of x)) (or (zerop s) (<= 0.5 s)) (< s 1)
        (= (scale-float s e) (abs x)))))
    (list 0.0 -0d0 0.1 0.1d0 least-positive-single-float least-positive-double-float
      least-positive-normalized-single-float least-positive-normalized-double-float
      most-positive-single-float most-positive-double-float))`,
  `(and (handler-case (scale-float 1.0 128) (floating-point-overflow (c)
      (and (eq (arithmetic-error-operation c) 'scale-float)
        (equal (arithmetic-error-operands c) '(1.0 128)))))
    (handler-case (scale-float 1d0 (expt 2 100)) (floating-point-overflow () t)))`,
  `(and (constantp 'single-float-epsilon) (constantp 'most-positive-double-float)
    (> (+ 1.0 single-float-epsilon) 1.0) (< (- 1d0 double-float-negative-epsilon) 1d0)
    (= (+ 1.0 (scale-float single-float-epsilon -1)) 1.0))`,
]) check(source);
for (const fn of ['decode-float', 'integer-decode-float', 'float-radix', 'float-digits', 'float-precision', 'float-sign']) {
  check(`(handler-case (${fn} 1) (type-error () t))`);
  check(`(handler-case (${fn}) (program-error () t))`);
}
check('(= 42 (+ 20 22))');
console.log('Guest float properties: IEEE limits, precision, decomposition, signed zero, subnormals, extreme scaling, conditions, and recovery pass.');
