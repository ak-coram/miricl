#!/usr/bin/env node
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {Miricl} from '../scripts/miricl.mjs';

const args=process.argv.slice(2);
const fixtures=`(defparameter *bit-left* (1- (ash 1 8192)))
  (defparameter *bit-right* (1+ (ash 1 16384)))
  (defparameter *bit-operation* nil) (defparameter *bit-result* nil)`;
const workloads=[
  ['small-shift',100000,'ash','32768 15','*bit-result*','1073741824'],
  ['small-logand',100000,'logand','32769 32768','*bit-result*','32768'],
  ['small-ldb',100000,'ldb','(byte 8 8) 65535','*bit-result*','255'],
  ['small-ldb-test',100000,'ldb-test','(byte 8 8) 65535','*bit-result*','T'],
  ['small-dpb',100000,'dpb','85 (byte 8 8) 1','*bit-result*','21761'],
  ['small-deposit',100000,'deposit-field','65535 (byte 8 8) 1','*bit-result*','65281'],
  ['wide-ldb',1000,'ldb','(byte 1024 1000) *bit-left*','(integer-length *bit-result*)','1024'],
  ['wide-dpb',1000,'dpb','-1 (byte 1024 2000) 0','(integer-length *bit-result*)','3024'],
  ['large-shift',1000,'ash','*bit-left* 8193','(integer-length *bit-result*)','16385'],
  ['negative-right-shift',1000,'ash','(- *bit-right*) -8192','(integer-length *bit-result*)','8193'],
  ['large-logxor',1000,'logxor','*bit-left* *bit-right*','(integer-length *bit-result*)','16385'],
  ['large-logtest',1000,'logtest','*bit-left* *bit-right*','(not (null *bit-result*))','T'],
  ['large-length',1000,'integer-length','*bit-right*','*bit-result*','16385'],
  ['large-population',1000,'logcount','*bit-left*','*bit-result*','8192'],
];
const body=(count,values,result)=>`(progn (dotimes (i ${count})
  (setq *bit-result* (funcall *bit-operation* ${values}))) ${result})`;
if(args.includes('--sbcl-source')){
  console.log(`(declaim (optimize (speed 3) (safety 1)))\n${fixtures}`);
  for(const [name,count,operator,values,result,expected] of workloads){
    console.log(`(setq *bit-operation* #'${operator})
      (let ((fn (compile nil '(lambda () ${body(count,values,result)}))))
        (assert (eql ${expected} (funcall fn)))
        (format t "${name} (${count} calls): ~,3F ms~%"
          (loop repeat 3 minimize (let ((start (get-internal-real-time)))
            (assert (eql ${expected} (funcall fn)))
            (* 1000.0d0 (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))`);
  }
}else{
  const binary=args.find(arg=>!arg.startsWith('--'))??'miricl.wasm';
  const vm=await Miricl.instantiate(fs.readFileSync(binary));vm.evaluate(fixtures);
  const results={};
  for(const [name,count,operator,values,result,expected] of workloads){
    vm.evaluate(`(setq *bit-operation* #'${operator}) (defun bit-run () ${body(count,values,result)})`);
    const run=()=>assert.equal(vm.evaluate('(bit-run)').output,`${expected}\n`);run();
    const times=[];for(let i=0;i<3;i++){
      const start=performance.now();run();times.push(performance.now()-start);
    }
    results[name]={calls:count,milliseconds:Math.min(...times)};
  }
  console.log(JSON.stringify({binary,results},null,2));
}
