;;------------------------------Header------------------------------
;;Garinn Morton II                                                 |
;;gmorton2@csu.fullerton.edu                                       |
;;                                                                 |
;;This file contains all functions to run Blocks World RBS,        |
;;and test code commented out to be able to run said functions     |
;;------------------------------------------------------------------

(setq rbs-closed nil) ;; Mom state's go on Closed.  No moms yet.
(setq rbs-open nil) ;; We could've added the start state here.
(setq rbs-parentages nil)
;;===========================================================================append start state to open
(setq rbs-open
      (append
       (list
        '((:state rbs-477) (:ss t) (:ham-dist 9);; SS (Start-State) node.
          (:wmem 
           ((is-clear C)
            (is-on C A)
            (is-clear B)
            (is-on B D)
            (is-ontable A)
            (is-ontable D)
            (hand-empty)))))))

;;======================================================================================================start rule set
(setq rbs-rules
      (list
       '((:rule pickup-X)  ;; one of our State-Space operations
	 (:if (and (is-clear $X)  ;; NB, nil not needed if using "cadr".
		   (is-ontable $X)
		   (hand-empty)))
	 (:then (new-state-delta-to-add  ;; Built from copy of current state WMEM.
		 (is-gripping $X))))

       '((:rule putdown-X)
	 (:if (and (is-gripping $X)))
	 
	 (:then
	  (new-state-delta-to-add
	   (hand-empty)
	   (is-ontable $X)
	   (is-clear $X))))

       '((:rule stack-X-on-Y)
	 (:if (and (is-clear $Y)
		   (is-gripping $X)))
	 (:then
	  (new-state-delta-to-add
	   (is-on $X $Y)
	   (hand-empty)
	   (is-clear $X))))

       '((:rule unstack-X-off-Y)
	 (:if (and (is-on $X $Y)
		   (is-clear $X)
		   (hand-empty)))
	 (:then
	  (new-state-delta-to-add
	   (is-gripping $X)
	   (is-clear $Y))))))


;;=============================================================================================create goal state

(setq rbs-goal
      '((:state rbs-478) (:ham-dist 0)
        (:wmem 
         ((is-clear A)
          (is-on A B)
          (is-on B C)
          (is-on C D)
          (is-ontable D)
          (hand-empty)))))

;;======================================================================================written functions for rbs-1-step
;;get kids->given a rule get its unifers
;;once we have unifer we have to take the unifer to transform the rule or create a ground rule with the given unifer
;;each wmem is a different state(board position in checkers)

;;for each kid check to see if the ham dist from closed list is non zero(remove closed kids)

;; ----------------------------------------------------------------------------------------------------------- is-rbs-var
(defun is-rbs-var (rx)
  "Return t if X is a symbol whose name starts with a '$' char, else nil."
  (eq ?$ (if (symbolp rx)
             (aref (symbol-name rx) 0)
           ?a))) ;; Use the wrong char if rx is not a symbol.
;; ----------------------------------------------------------------------------------------------------------- pairlis
(defun pairlis (ra rb)
  "Return a list of pairs, walking both lists simultaneously.
   Stop when either list runs out."
  (when (and (listp ra) (listp rb)) ;; If they're both lists...
    (let (retl)
      (while (and ra rb) ;; more in both lists?
        (setq retl
              (append (list (list (car ra) (car rb)))
                      retl)) ;; Build retl backwards.
        ;; Advance both lists.
        (setq ra (cdr ra)
              rb (cdr rb)))
      (reverse retl))))
;; ------------------------------------------------------------------------------------------------------------ cs-lhs-unify
(defun cs-lhs-unify (rexpr-1 rexpr-2)
  "Unify expr-1 (maybe has vars) with a constant expr-2.
   Return t plus the var-val pairs if any, else nil if no match.
   Unify only the top-level of the lists, check with eq, or is-rbs-var."

  (when (= (length rexpr-1) (length rexpr-2))
    (let ((var-val-pairs
           (mapcar #'(lambda (xpair)
                       (cond
                        ((eq (car xpair) (cadr xpair)) t) ;; Ret T for match.
                        ((is-rbs-var (car xpair)) xpair) ;; Ret var-val pair.
                        (t nil))) ;; This xpair doesn't match.
                   (pairlis rexpr-1 rexpr-2))))
      ;; dbg (print var-val-pairs)
      (if (not (member nil var-val-pairs)) ;; All pairs match?
          (append '(t) ;; Prepend a T element -- indicates full match.
                  (remove t var-val-pairs)))))) ;; Remove any T elts in pairs.

;; ---------------------------------------------------------------------------------------------------------  get-all-rule-unifiers-1
(defun get-all-rule-unifiers-1 (rpre-cond rwmem)
  "Get all unifiers for this pre-cond pattern in the wmem list."
  (let (unifiers ;; init to nil.
        (raw-unifiers
         (mapcar #'(lambda (ex) (cs-lhs-unify rpre-cond ex))
                 rwmem)))
    ;;dbg (print raw-unifiers)
    ;; Add only unique unifiers to final unifier list.
    (while raw-unifiers
      (let ((unifier (pop raw-unifiers)))
        (when (and (not (null unifier))
                   (not (member unifier unifiers)))
          (push unifier unifiers))))
    unifiers))
;; --------------------------------------------------------------------------------------------------------------  asubst
(defun asubst (ralist rxlist)
  "Return a new xlist with any alist key replaced with its value.
   Substitutes only at the top-level."
  (let ((retv nil))
    (while rxlist
      (let* ((elt (pop rxlist))
             (apair (assoc elt ralist)))
        (if (not apair)
            (push elt retv)
          (let ((val (cadr apair)))
            (push val retv)))))
    (reverse retv)))
;; ---------------------------------------------------------------------------------------------------------  get-all-rule-unifiers

(defun get-all-rule-unifiers (rpre-conds rwmem)
  (when rpre-conds
    (let* ((retv nil) ;; Qual'd unifiers so far, in reverse.
           (pre-cond-1 (pop rpre-conds)) ;; To get set of unifiers.
           (unifiers ;; Get unifiers for 1st pre-cond.
            (get-all-rule-unifiers-1 pre-cond-1 rwmem)))
      ;; Check each other pre-cond, maybe extend a unifier.
      (while unifiers ;; Check each unifier with rest of pre-conds.
        (let* ((unifier (pop unifiers)) ;; Unifier to check w pre-conds.
               ;;(dbg-1 (print (list :1 unifier))) ;; dbg
               (var-vals (cdr unifier)) ;; Remove 't.
               (pre-conds rpre-conds) ;; Get pre-conds list we'll walk.
               (unifier-matches t)) ;; Default, unifier works.
          (while pre-conds ;; Check each pre-cond with this unifier.
            (let* ((pre-cond (pop pre-conds))
                   (reduced-cond ;; Subst unifier values for pre-cond vars.
                    (asubst unifier pre-cond)))
              (let ((more-unifiers
                     (get-all-rule-unifiers-1 reduced-cond rwmem)))
                (if more-unifiers ;; For stack-X-on-Y.
                    (let ((extra-var-val (cadr (car more-unifiers))))
                      (when extra-var-val ;; an extra var-val?
                        (setq unifier (append unifier (list extra-var-val)))
                        ))
                  ;; Else reduced-cond doesn't match WMEM, unifier fails.
                  (setq unifier-matches nil)))))
          ;; Have checked all pre-conds with this unifier.
          (when unifier-matches
            (push unifier retv)
            )))
      (reverse retv))))


;;====================================================================================================get ham distance     
(defun ham-dist (ralst rblst)
  "Return count of exprs in 1st list not in 2nd, and vice versa, else 0."
  (let ((sum 0))
    (when (and (listp ralst) (listp rblst))
      (mapcar #'(lambda (ax) ;; Check if every elt of A list is in B list.
                  (when (not (member ax rblst))
                    (cl-incf sum))) ;;MIA so increment the sum.
              ralst)
      (mapcar #'(lambda (bx) ;; Check if every elt of B list is in A list.
                  (when (not (member bx ralst))
                    (cl-incf sum))) ;;MIA so increment the sum.
              rblst))
    sum))

;; (ham-dist '((is-clear C)
;;             (is-on C A)
;;             (is-clear B)
;;             (is-on B D)
;;             (is-ontable A)
;;             (is-ontable D)
;;             (hand-empty))
;; 	  '((is-clear A)
;;           (is-on A B)
;;           (is-on B C)
;;           (is-on C D)
;;           (is-ontable D)
;;           (hand-empty)))
;; 9




;; Return count of all missing in opposite list.

(defun replace-vars-w-vals (rexpr ralist)
  "Replace each var occurrence in the expr with its val in the alist."
  (cond
   ((eq nil rexpr) nil) ;; Just nil here?  Return it.
   ((listp rexpr) ;; Not a Basis Step, so recur on both car and cdr.
    ;; Create new cons cell with replaced LHS and RHS sub-trees.
    (cons (replace-vars-w-vals (car rexpr) ralist)
          (replace-vars-w-vals (cdr rexpr) ralist)))
   ((symbolp rexpr) ;; Basis Step, symbol?
    ;; Return val if rexpr is a var in the alist, else ret rexpr.
    (let ((val (cadr (assoc rexpr ralist))))
      (if val val rexpr)))
   (t rexpr))) ;; Basis Step, non-symbol.


(defun get-rule-conds (gnd-rule)
  "Grabs a grounded rule or non-grounded rule pre-conditions"
  (let ((retv (cdr (cadr (assoc :if gnd-rule)))));; a list key-val mech
    retv))

;;-------------------------------------------------------------------------------------->get-rule-conds-test
;; (get-rule-conds '((:if (and
;;                         (is-clear B)
;;                         (is-gripping C)))
;;                   (:then (new-state-delta-to-add
;;                           (is-on C B)
;;                           (hand-empty)
;;                           (is-clear C)))))

;; ((is-clear B) (is-gripping C))

;;------------------------------------------------------------------------------------->get-rule-conds-test

(defun remove-gnd-rule-conds (gnd-conds wmem)
  "Removes the grounded rules pre condition from working memory"
  (let ((middle-wmem wmem))
    (while gnd-conds
      (let ((con (pop gnd-conds)))
	(setq middle-wmem (remove con middle-wmem))))    
    middle-wmem))

;;------------------------------------------------------------------------------------->remove-gnd-rule-conds not working
;; (remove-gnd-rule-conds '((a b) (c c))
;; 		       '((a b) (b b) (c c)))

;; --> ((b b))

;;------------------------------------------------------------------------------------->remove-gnd-rule-conds not working

(defun get-rule-adds (gnd-rule)
  "Grabs a grounded rules post-condtions"
  (let ((retv (cdr (cadr (assoc :then gnd-rule)))))
    retv))

;;-------------------------------------------------------------------------------------->get-rule-adds-test

;; (get-rule-adds '((:if (and
;;                         (is-clear B)
;;                         (is-gripping C)))
;;                   (:then (new-state-delta-to-add
;;                           (is-on C B)
;;                           (hand-empty)
;;                           (is-clear C)))))



;; --> ((is-on C B) (hand-empty) (is-clear C))
;;-------------------------------------------------------------------------------------->get-rule-adds-test


(defun add-gnd-rule-adds(gnd-adds middle-wmem)
  "Adds the grounded rules post-conditions to working memory"
  (setq kid-WMEM (append gnd-adds middle-wmem))
  kid-WMEM)

;;-------------------------------------------------------------------------------------->add-gnd-rule-adds-test
;; (add-gnd-rule-adds '((new-state-delta-to-add
;;                           (is-on C B)
;;                           (hand-empty)
;;                           (is-clear C)))
;; 		   '((a b) (is-gripping C)))
;; --> (((new-state-delta-to-add (is-on C B) (hand-empty) (is-clear C))) (a b) (is-gripping C))

;;-------------------------------------------------------------------------------------->add-gnd-rule-adds-test

(defun add-parentage (kid mom)
  "adds a kids mom state to a parentage list"
  (let ((kid-mom-pair (list kid mom)))
    (let ((one-pair-list (list kid-mom-pair)))
      (setq rbs-parentages (append one-pair-list rbs-parentages)))))

(defun make-kid-wmem (mom-wmem gnd-rule)
  "creates kid's working memory from mom's working memory and grounded rule"
  (let ((gnd-rule-conds (get-rule-conds gnd-rule)))
    (let ((middle-wmem (remove-gnd-rule-conds gnd-rule-conds mom-wmem)))
      (let ((gnd-adds (get-rule-adds gnd-rule)))
	(let ((kid-wmem (add-gnd-rule-adds gnd-adds middle-wmem)))
	  kid-wmem)))))

;;-------------------------------------------------------------------------------------->make-kid-wmem-test
;; (make-kid-wmem '((is-clear C)
;; 		 (is-gripping C)
;;                  (is-on C A)
;;                  (is-clear B)
;;                  (is-on B D)
;;                  (is-ontable A)
;;                  (is-ontable D)
;;                  (hand-empty))
;; 	       '((:if (and
;;                        (is-clear B)
;;                        (is-gripping C)))
;;                  (:then (new-state-delta-to-add
;;                          (is-on C B)
;;                          (hand-empty)
;;                          (is-clear C)))))


;; (((is-on C B) (hand-empty) (is-clear C)) (is-clear C) (is-on C A) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty))


;;-------------------------------------------------------------------------------------->make-kid-wmem-test

(defun make-kid (mom gnd-rule)
  "creates a kid from a mom and a grounded rule"
  (let ((kid-wmem (make-kid-wmem (cadr (assoc :wmem mom)) gnd-rule)))
    (let ((new-kid (list
		    (list :state (gensym 'rbs-))
		    (list :ham-dist (ham-dist kid-wmem (cadr (assoc :wmem rbs-goal))))
		    (list :wmem kid-wmem)
		    (list :mom-state (cadr (assoc :state mom))))))
      (add-parentage new-kid mom)
      new-kid)))

;;------------------------------------------------------------------------------------------------------------------------------->make-kid-test & add-parentage

;; (make-kid '((:state rbs-14) (:ham-dist 9)
;; 	    (:wmem ((is-gripping B)
;; 		    (is-clear C)
;; 		    (is-on C A)
;; 		    (is-ontable A)
;; 		    (is-ontable D)))
;; 	    (:mom-state rbs-477))
;; 	  '((:rule putdown-X)
;;             (:if  (is-gripping B))
;;             (:then
;; 		    (new-state-delta-to-add
;; 		     (hand-empty)
;; 		     (is-ontable B)
;; 		     (is-clear B)))))

;; ((:state rbs-78) (:ham-dist 10)
;;  (:wmem ((hand-empty)
;; 	 (is-ontable B)
;; 	 (is-clear B)
;; 	 (is-gripping B)
;; 	 (is-clear C)
;; 	 (is-on C A)
;; 	 (is-ontable A)
;; 	 (is-ontable D)))
;;  (:mom-state rbs-14))


;; rbs-parentages
;; --> ((((:state rbs-234) (:ham-dist 9) (:wmem ...) (:mom-state rbs-477)) ((:state rbs-477) (:ss t) (:fig 2-3) (:wmem ...))))


;; --> ((:state rbs-233)
;;  (:ham-dist 9)
;;  (:wmem (((is-on C B) (hand-empty) (is-clear C)) (is-clear C) (is-on C A) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty)))
;;  (:mom-state rbs-477))

;;-------------------------------------------------------------------------------------------------------------------------------->make-kid-test & add-parentage


(defun make-gnd-rule (rule rule-unifier)
  "creates a gorunded rule from a rule and its rule unifiers"
  (let ((gnd-rule (replace-vars-w-vals rule rule-unifier)))
    gnd-rule))


(defun get-state-wmem (mom)
  "grabs a mom node working memory"
  (let ((retv (cadr (assoc :wmem mom))))
    retv))

;;-------------------------------------------------------------------------------------->get-state-wmem-test
;; (get-state-wmem '((:state rbs-477) (:ss t) (:fig 2-3) ;; SS (Start-State) node.
;;                   (:wmem 
;;                   ((is-clear C)
;;                   (is-on C A)
;;                   (is-clear B)
;;                   (is-on B D)
;;                   (is-ontable A)
;;                   (is-ontable D)
;;                   (hand-empty)))))

;; --> ((is-clear C) (is-on C A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty))
;;-------------------------------------------------------------------------------------->get-state-wmem-test


(defun get-kids-for-rules (mom-state)
  "creates kids for all rules"
  (let ((mom-wmem (get-state-wmem mom-state))
	(kid-states nil))
    (let ((rules rbs-rules))
      (while rules
	(let ((rule (pop rules)))
	  (let ((rule-conds (get-rule-conds rule)))
	    (let ((rule-unifiers (get-all-rule-unifiers rule-conds mom-wmem)))
	      (while rule-unifiers
		(let ((unifier (pop rule-unifiers)))
		  (let ((gnd-rule (make-gnd-rule rule unifier)))
		    (setq kid-states (append (list (make-kid mom-state gnd-rule)) kid-states))))))))))
    kid-states))

;;----------------------------------------------------------------------------------------------------------------->get-kids-for-rules & make-gnd-rule test
;; (print
;;  (get-kids-for-rules '((:state rbs-321) (:ham-dist 9)
;; 		       (:wmem
;; 			((hand-empty)
;; 			 (is-ontable B)
;; 			 (is-clear B)
;; 			 (is-clear C)
;; 			 (is-on C A)
;; 			 (is-ontable A)
;; 			 (is-ontable D)))
;; 		       (:mom-state rbs-14)))

;;  )



;; (print
;;  (get-kids-for-rules '((:state rbs-477) (:ss t) (:ham-dist 5);; SS (Start-State) node.
;; 		       (:wmem 
;; 			((is-clear C)
;; 			 (is-on C A)
;; 			 (is-clear B)
;; 			 (is-on B D)
;; 			 (is-ontable A)
;; 			 (is-ontable D)
;; 			 (hand-empty)))))

;;  )

;; (((:state rbs-4325) (:ham-dist 9)
;;   (:wmem
;;    ((is-gripping C)
;;     (is-clear B)
;;     (is-on B D)
;;     (is-ontable A)
;;     (is-ontable D)))
;;   (:mom-state rbs-477)) ((:state rbs-4324) (:ham-dist 9) (:wmem ((is-gripping B) (is-clear C) (is-on C A) (is-ontable A) (is-ontable D))) (:mom-state rbs-477)))
;; (((:state rbs-4325) (:ham-dist 9) (:wmem (... ... ... ... ...)) (:mom-state rbs-477)) ((:state rbs-4324) (:ham-dist 9) (:wmem (... ... ... ... ...)) (:mom-state rbs-477)))


;; --> (((:state rbs-282) (:ham-dist 9) (:wmem (((is-gripping C) (is-clear A)) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))
;;     ((:state rbs-281) (:ham-dist 9) (:wmem (((is-gripping B) (is-clear D)) (is-clear C) (is-on C A) (is-ontable A) (is-ontable D))) (:mom-state rbs-477)))

;;----------------------------------------------------------------------------------------------------------------->get-kids-for-rules & make-gnd-rule test

(defun is-on-open (kid-state)
  "checks to see if kid is on the open list" 
  (let* ((open rbs-open)
	 (retv nil))
    (while open
      (let* ((oldy (pop open))
             (oldy-wmem (cadr (assoc :wmem oldy)))
	     (kid-state-wmem (cadr (assoc :wmem kid-state)))
             (dist (ham-dist kid-state-wmem oldy-wmem)))
	(when (= 0 dist)
          (setq open nil 
		retv t))))
    retv))

;; (is-on-open '((a b) (b b))
;; 	    '(:wmem ((a b) (b b))))



(defun is-on-closed (kid-state)
  "checks to see if kid is on the closed list"
  (let* ((closed rbs-closed)
	 (retv nil))
    (while closed
      (let* ((oldy (pop closed))
             (oldy-wmem (cadr (assoc :wmem oldy)))
	     (kid-state-wmem (cadr (assoc :wmem kid-state)))
             (dist (ham-dist kid-state-wmem oldy-wmem)))
	(when (= 0 dist)
          (setq closed nil 
		retv t)))) 
    retv))

;; (push '((:state rbs-35859) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear B) (is-clear A) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-35856)) rbs-closed)
;; rbs-closed



;; (is-on-closed '((:state rbs-35859) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear B) (is-clear A) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-35856)))


(defun add-mom-new-kids-to-open (new-kid-states)
  "adds the mom's new found kids to open list"
  (setq rbs-open (append new-kid-states rbs-open))
  rbs-open)

(defun get-mom-new-kids (mom-state)
  "removes any kids that are found on open or closed list to get unique new kids"
  (let* ((all-kid-states (get-kids-for-rules mom-state))
	 (new-kids all-kid-states))  
    (while all-kid-states
      (let ((kid-state (pop all-kid-states)))
	(if (is-on-closed kid-state)
	    (setq new-kids (remove kid-state new-kids)))
	(if (is-on-open kid-state)
	    (setq new-kids (remove kid-state new-kids)))))
    new-kids))

;;(print
;; (get-mom-new-kids '((:state rbs-477) (:ss t) (:ham-dist 5);; SS (Start-State) node.
;; 		       (:wmem 
;; 			((is-clear C)
;; 			 (is-on C A)
;; 			 (is-clear B)
;; 			 (is-on B D)
;; 			 (is-ontable A)
;; 			 (is-ontable D)
;; 			 (hand-empty)))))


;; (((:state rbs-166) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))
;;  ((:state rbs-165) (:ham-dist 10) (:wmem ((is-gripping B) (is-clear D) (is-clear C) (is-on C A) (is-ontable A) (is-ontable D))) (:mom-state rbs-477)))
;; (((:state rbs-166) (:ham-dist 8) (:wmem (... ... ... ... ... ...)) (:mom-state rbs-477)) ((:state rbs-165) (:ham-dist 10) (:wmem (... ... ... ... ... ...)) (:mom-state rbs-477)))



(defun add-mom-to-closed (mom-state)
  "adds mom node to closed list"
  (setq rbs-closed (push mom-state rbs-closed))
  (setq rbs-open (remove mom-state rbs-open))
  rbs-closed)


(defun get-next-mom (goal-state)
  "finds the best new mom from open list"
  (let* ((open rbs-open)
	 (min nil))
    (while open ;;still elems on open?
      ;;get elem off open
      (let* ((elem (pop open))
	     (elem-wmem (cadr (assoc :wmem elem)))
	     (goal-wmem (cadr (assoc :wmem goal-state)))
             ;;calc elem-goal ham-dist
             (ham (ham-dist elem-wmem goal-wmem)))
	;;(print (list :ham ham :elem elem))
	;;chekc if elem better than current min
	(if (or (eq nil min)
		(< ham (cadr (assoc :ham-dist min))))
            ;;if so update current min
            (setq min elem))))
    min))


;; (get-next-mom 

;; 	      '((:state rbs-478) (:ham-dist 0)
;; 		(:wmem 
;; 		 ((is-clear A)
;; 		  (is-on A B)
;; 		  (is-on B C)
;; 		  (is-on C D)
;; 		  (is-ontable D)
;; 		  (hand-empty)))))



(defun print-root-goal-path (goal) ;;print parentage..?
  "prints the root to node path"
  (let ((kid goal)) 
    (while kid
      (print kid)
      (setq kid (cadr (assoc kid rbs-parentages))))))



(defun rbs-1-step (goal-state)
  "Take 1 RBS State Space Search step.  Uses global rbs-* vars."
  (let ((next-mom-state (get-next-mom goal-state)))
    (let ((new-closed (add-mom-to-closed next-mom-state)))
      (let ((new-kid-states (get-mom-new-kids next-mom-state)))
	(let ((new-open (add-mom-new-kids-to-open new-kid-states)))
	  next-mom-state)))))

;; (rbs-1-step rbs-goal)

;; (:next-mom ((:state rbs-477) (:ss t) (:ham-dist 9) (:wmem ((is-clear C) (is-on C A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty)))))

;; (:new-closed (((:state rbs-477) (:ss t) (:ham-dist 9) (:wmem ((is-clear C) (is-on C A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D) (hand-empty))))))

;; (:new-kids (((:state rbs-35852) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))
;; 	    ((:state rbs-35851) (:ham-dist 10) (:wmem ((is-gripping B) (is-clear D) (is-clear C) (is-on C A) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))))

;; (:new-open (((:state rbs-35852) (:ham-dist 8) (:wmem ((is-gripping C) (is-clear A) (is-clear B) (is-on B D) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))
;; 	    ((:state rbs-35851) (:ham-dist 10) (:wmem ((is-gripping B) (is-clear D) (is-clear C) (is-on C A) (is-ontable A) (is-ontable D))) (:mom-state rbs-477))))


(defun is-goal (state)
  "checks to see if the current state is the goal state"
  (let ((dist (cadr (assoc :ham-dist state))))
    (when dist
      (= 0 dist))))


;; (is-goal '((:state rbs-477) (:ss t) (:ham-dist 0);; SS (Start-State) node.
;;           (:wmem 
;;            ((is-clear C)
;;             (is-on C A)
;;             (is-clear B)
;;             (is-on B D)
;;             (is-ontable A)
;;             (is-ontable D)
;;             (hand-empty)))))




;; (is-goal nil)
;; ;; nil

(defun run-rbs ();;start-state goal-state rules)
  "runs entire program and prints the root to goal path once it has been discovered"
  (let* ((step t))
    (while step
      (let ((mom (rbs-1-step rbs-goal)))
	(if (is-goal mom)
	    (progn
	      (setq step nil)
	      (print-root-goal-path mom)))))))

(run-rbs)






