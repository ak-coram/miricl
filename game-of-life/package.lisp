(defpackage :life
  (:use :cl)
  (:export :world :world-width :world-height :world-cells :world-generation
           :*world* :empty-world :paint :next-generation :seed
           :ascii-grid :print-grid :tick :render :start))
